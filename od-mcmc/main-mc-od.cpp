
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   main.cpp
 * Author: urata
 *
 * Created on May 16, 2016, 2:04 PM
 * ADD May 31, for Sioux Falls
 */

#include <stdio.h>
#include "MT.h"
#include <time.h>
#include <math.h>
#include <stdlib.h>
//#include "igamma.h"
#include <cmath>

#define NODES 24
#define LINKS 576   //24*24
#define TRY 1 // number of sampling

// exogenous parameter
const double BETA   = 0.030;
const double ALPHA3 = 0.0030; // ALPHA3/ALPHA1 = 10000 ~ 500000, small dif of OD cost: 1/100
const double ALPHA1 = 1.0;
const double SIGMA0 = 0.245946;   // Calculated for balancing
const int OBJ = LINKS - NODES;  // for cell choice

// 構造体定義
struct link{
	int onode; 
	int dnode; 
	double cost; 
};

// 関数プロトタイプ
void fileinod(int *od);
void fileinod00(double *od);
struct link *fileinli(void);
double linkcost(int onode, int dnode, link *li);
int fact(int n);
double logfact(int n);
double logfact3(int n);
int poisson(double ave);
int logpoisson(double ave, double *psamp);
double energy (int *oi, int *dj, double c, int *oiz, int *djz, double cz, double *odz);
double ermsq (int *od, double *odz);
double ermsqod (int *oi, int *oiz);
void randombec(double *sortbec);
void quick(double *row, int left, int right);
void atdif(double *sortbec, int oave, double *volm);
double rand_normal(double mu, double sigma);
double rand_truncated_normal(double mu, double sigma);
//double r_cdf_truncated_normal(double x, double mu, double sigma);
double cdf_truncated_normal(double x, double mu, double sigma);
double r2_cdf_truncated_normal(double x, double mu, double sigma);
int sumod(int *oiz);
int cellchoice();

// グローバル変数
static struct link li[LINKS];
static int od[NODES*NODES];
static double odz[NODES*NODES];   // sampled(final)
static double odzo[NODES*NODES];   // sampled from ave of O
static double odzd[NODES*NODES];   // sampled from ave of D
static double odp[NODES*NODES];    // initial sample & accepted sample
//static double odac[NODES*NODES];  // accepted
static double sortbec[NODES];
static double volm[NODES-1];
static double psamp;

int main(void) {
    int h, i, j, k;
    int accept=0;
    int oi[NODES]={}, dj[NODES]={}, oave, dave;
    int oip[NODES]={}, djp[NODES]={};
    double ene, enep, ratio, c=0.0, cp=0.0;
    init_genrand((unsigned)time(NULL));
    double out[TRY], outO[TRY], outD[TRY], outE[TRY], outC[TRY], outOD[TRY];
    //double odt[NODES*NODES];
    char fname[50];
    int rem, add;
    FILE *fw;

    ////////////////////////////////////////////////////////////////
    // INITIAL SETTING
    fileinod(od);
        // printf("%d %d %d \n", od[0], od[1], od[2]);
    struct link *LI = fileinli();
        // printf("%d %d %d \n", (LI+1)->onode, (LI+1)->dnode, (LI+1)->cost);
 
    //* Parameter total O & D & cost of true value 
    for(i = 0; i < NODES; i++){
        for(j = 0; j < NODES; j++){
            oi[i] += od[i*NODES+j];
            dj[i] += od[i+j*NODES];
            c += od[i*NODES+j] * linkcost(i+1, j+1, LI);    // 104950
        }
    }
    // printf("%d %d %d \n", oi[0], oi[1], oi[2]);
    // printf("%d %d %d %d \n", dj[0], dj[1], dj[2], c);
    //for(i = 0; i < NODES*NODES; i++){
    //    odt[i] = double(od[i]);
    //}
    //printf("ENT of TRUE OD: %.3f", energy(oi, dj, c, oi, dj, c, odt));

    //* Initial OD table 
    fileinod00(odp);
        // printf("%f %f %f \n", odp[0], odp[1], odp[3]);
    for(i = 0; i < NODES; i++){ 
        for(j = 0; j < NODES; j++){
            oip[i] += odp[i*NODES+j];
            djp[i] += odp[i+j*NODES];
            cp += odp[i*NODES+j] * linkcost(i+1, j+1, LI);
        }
    }
        // printf("%d %d %d \n", oip[0], oip[1], oip[2]);
        // printf("%d %d %d %d \n", djp[0], djp[1], djp[2], cp);

    enep = energy(oi, dj, c, oip, djp, cp, odp);
    //enep = -9999999.9;
        // printf("enep: %.3f\n", enep);
        // printf("\n %f", exp(800));
    //printf("INITIAL error: O=%f, D=%f, OD=%f", ermsqod(oi, oip), ermsqod(dj, djp), ermsq(od, odp));
    //printf("%d, %d, %d, %d\n", cellchoice(), cellchoice(), cellchoice(), cellchoice());
    
    ///////////////////////////////////////////////////////////////
    
    for(h=0; h < TRY; h++){
        out[h]   = -99.0;
        outO[h]  = -99.0;
        outD[h]  = -99.0;
        outE[h]  = -99.0;
        outC[h]  = -99.0;
        //psamp = 0.0;
        int oiz[NODES]={}, djz[NODES]={};
        double cz=0.0;

        for (i = 0; i < LINKS; i++){
            *(odz + i) = *(odp + i);    //update (accepted sample)
        }
        
        if(h % 3 == 0){ // Exchange
            do{
                rem = cellchoice();
            } while(odz[rem]==0);
            add = cellchoice();
            odz[rem] -= 1;
            odz[add] += 1;
        } else if(h % 3 == 1){  // Add
            add = cellchoice();
            odz[add] += 1;
        } else {    // Remove
            do{
                rem = cellchoice();
            } while(odz[rem]==0);
            odz[rem] -= 1;            
        }
        
/*        for(i = 0; i < NODES; i++){
            for(j = 0; j < NODES; j++){
                if(i != j){
                    sigp = (odp[i*NODES+j]!=0.0)*(log(1.5 + odp[i*NODES+j]/7)) + (odp[i*NODES+j]==0.0)*SIGMA0;
                    odz[i*NODES+j] = round(rand_truncated_normal(odp[i*NODES+j], sigp));  // ADD Sigma
                    po += r2_cdf_truncated_normal(odz[i*NODES+j], odp[i*NODES+j], sigp);  // ADD Sigma
                    sigz = (odz[i*NODES+j]!=0.0)*(log(1.5 + odz[i*NODES+j]/7)) + (odz[i*NODES+j]==0.0)*SIGMA0;
                    // printf("SIG: (%d) %.5f\n", (odz[i*NODES+j]==0.0), sigz);
                    pprev += r2_cdf_truncated_normal(odp[i*NODES+j], odz[i*NODES+j], sigz);  // ADD Sigma
                    // printf("%.2f %.2f %.2f %.2f %.1f %.1f\n", sigp, sigz, pprev, po, odp[i*NODES+j], odz[i*NODES+j]);
                } else {
                    odz[i*NODES+j] = 0.0;
                }
                //printf("\n");
            }
        }
            //printf("po:%.3f pprev:%.3f \n", po, pprev);
*/
        
        // total O & D & cost of sample 
        for(i = 0; i < NODES; i++){ 
            for(j = 0; j < NODES; j++){
                oiz[i] += odz[i*NODES+j];
                djz[i] += odz[i+j*NODES];
                cz += odz[i*NODES+j] * linkcost(i+1, j+1, LI);
            }
        }
            // printf("\n%d %d %d ", oiz[0], oiz[1], oiz[2]);
            // printf("%d %d %d %.3f\n", oiz[0], djz[1], djz[2], cz);
        ene = energy(oi, dj, c, oiz, djz, cz, odz);
            // printf("\n %.7f %.7f %.7f", ene, enep, ene / enep);
        outOD[h] = double(sumod(oiz));
      
        if(BETA*(ene-enep) > 0){ // Accept *log(1)=0)
              accept++;
               printf("Dene: %.2f, cost: %.1f \n", BETA*(ene - enep), cz);    //write
              enep = ene;
               printf("[%d]OK!\n", h);
              out[h] = ermsq(od, odz);
              outO[h] = ermsqod(oi, oiz);
              outD[h] = ermsqod(dj, djz);
              outE[h] = ene;
              outC[h] = cz;
                   
              sprintf(fname, "D:/od-mcmc/c-result/res-od/odtable-%d.csv", h);
              if ((fw = fopen(fname, "w")) != NULL){
                for (i = 0; i < LINKS; i++){
                    fprintf(fw, "%f\n", *(odz + i));
                    *(odp + i) = *(odz + i);    //update (accepted sample)
                }
                fclose(fw);
            }
        } else {
            ratio = exp(BETA*(ene - enep));
                 printf("Ratio: %.3f, Dene: %.1f(%.1f) cost:%.1f\n", ratio, BETA*(ene - enep), ene, cz);  //write
            if(isnan(ratio)==0){
                if(ratio >= genrand_real3()){   // Accept
                    accept++;
                    enep = ene;
                     printf("[%d] POK!!\n", h);
                    out[h] = ermsq(od, odz);
                    outO[h] = ermsqod(oi, oiz);
                    outD[h] = ermsqod(dj, djz);
                    outE[h] = ene;
                    outC[h] = cz;
              
                    sprintf(fname, "D:/od-mcmc/c-result/res-od/odtable-%d.csv", h);
                    if ((fw = fopen(fname, "w")) != NULL){
                        for (i = 0; i < LINKS; i++){
                            fprintf(fw, "%f\n", *(odz + i));
                            *(odp + i) = *(odz + i);    //update (accepted sample)
                        }
                        fclose(fw);
                    }
                }
            } else {
                printf("obtain NA...\n");
            }
        }
    }
        
    if ((fw = fopen("D:/od-mcmc/c-result/errsd.csv", "w")) != NULL){
	for (h = 0; h < TRY; h++){
		fprintf(fw, "%f\n", *(out + h));
	}
	fclose(fw);
    }
    if ((fw = fopen("D:/od-mcmc/c-result/errsd-o.csv", "w")) != NULL){
	for (h = 0; h < TRY; h++){
		fprintf(fw, "%f\n", *(outO + h));
	}
	fclose(fw);
    }
    if ((fw = fopen("D:/od-mcmc/c-result/errsd-d.csv", "w")) != NULL){
	for (h = 0; h < TRY; h++){
		fprintf(fw, "%f\n", *(outD + h));
	}
	fclose(fw);
    }
    if ((fw = fopen("D:/od-mcmc/c-result/energy.csv", "w")) != NULL){
	for (h = 0; h < TRY; h++){
		fprintf(fw, "%f\n", *(outE + h));
	}
	fclose(fw);
    }
    if ((fw = fopen("D:/od-mcmc/c-result/cost.csv", "w")) != NULL){
	for (h = 0; h < TRY; h++){
		fprintf(fw, "%f\n", *(outC + h));
	}
	fclose(fw);
    }
    if ((fw = fopen("D:/od-mcmc/c-result/totalod.csv", "w")) != NULL){
	for (h = 0; h < TRY; h++){
		fprintf(fw, "%f\n", *(outOD + h));
	}
	fclose(fw);
    }
    
    printf("\nACCEPT:%d ", accept);
    printf("\ntrue.cost:%.2f %d %d %d", c, oi[1], dj[2], sumod(oi));

    return 0;
}

/* ERROR of MEAN SQUARE */
double ermsq (int *od, double *odz){
    int i;
    double err = 0.0;
    double sd;
    
    for(i=0; i < NODES*NODES; i++){
        err += pow(odz[i]-od[i], 2);
    }
    sd = pow((err/(NODES*NODES)), 0.5);
    // printf("\n%.5f", pow(3/4, 0.5));
    // printf("\n%.4f ",sd);
    return sd;
}

/* ERROR of MEAN SQUARE of Origins & Destination Demand */
double ermsqod (int *oi, int *oiz){
    int i;
    double err = 0.0;
    double sd;
    
    for(i=0; i < NODES; i++){
        err += pow(oiz[i]-oi[i], 2);
    }
    sd = pow((err/(NODES)), 0.5);
    // printf("\n%.5f", pow(3/4, 0.5));
    // printf("\n%.4f ",sd);
    return sd;
}

// Calculate total OD
int sumod(int *oiz){
    int total=0, i;
    for(i = 0; i < NODES; i++){
        total += oiz[i];
    }
    return total;
}

/* CALCULATE energy term  (equation (11) of resume at May 31)*/
double energy (int *oi, int *dj, double c, int *oiz, int *djz, double cz, double *odz){
    int total = 0;
    double conbi = 0.0;
    int i, rodz;
    double peo = 0.0, ped = 0.0, penal, count;
    
    for(i = 0; i < NODES*NODES; i++){
        rodz = odz[i];
        total += rodz;
        conbi += logfact(rodz);
    }
    // printf("\ntotal:%d conbi:%.1f logfact(total):%.1f", total, conbi, logfact(total));
    count = logfact3(total) + logfact3(total-1) + logfact3(total-2) - conbi;
            
    for(i = 0; i < NODES; i++){
        peo += pow(*(oiz + i) - *(oi + i), 2);
        ped += pow(*(djz + i) - *(dj + i), 2);
    }
    penal = ALPHA3 * pow(cz - c, 2) + ALPHA1 * peo + ALPHA1 * ped;
      printf("peo:%.1f ped:%.1f c:%.1f penal:%.1f\n", ALPHA1 * peo, ALPHA1 * ped, ALPHA3 * pow(cz - c, 2), penal);    //write
    
    return count - penal;
}

/* FACTORIAL function for poisson distribution */
int fact(int n){
    int m;
    if(n==0){
        return 1;
    }
    else {
        m = fact(n-1);
        return n * m;
    }
}

/* LOG (FACTORIAL function) */
double logfact(int n){
    double m;
    if(n==0){
        return 0.0;
    }
    else {
        m = logfact(n-1);
        return log(n) + m;
    }
}

/* LOG (FACTORIAL function) and three way split*/
double logfact3(int n){
    double m;
    if(n==0){
        return 0.0;
    } else if(n==1){
        return 0.0;
    } else if(n==2){
        return log(2);
    } else {
        m = logfact3(n-3);
        return log(n) + m;
    }
}

/* PICKUP a travel cost of OD */
double linkcost(int onode, int dnode, link *li){
    int i=0;
    double cost= -9999.9;   //path cost between onode and dnode
    while(cost == -9999.9 && i < LINKS){
        if(onode == (li+i)->onode){
            if(dnode == (li+i)->dnode){
                cost = (li+i)->cost;
            }
        }
        i++;
    }
    return cost;
}

// CELL choice for exchange/add/remove
int cellchoice(){
    int ch;
    ch = int(floor(OBJ * genrand_real3()));
    if(ch!=OBJ){
        return ch + ch/NODES + 1;
    } else {
        return cellchoice();
    }
}

// Normal Distribution  // http://www.sat.t.u-tokyo.ac.jp/~omi/random_variables_generation.html#Gauss
double rand_normal(double mu, double sigma){  //
    //double sigma = 1.0; //σ=1.0 fixed
    double z = sqrt(-2.0*log(genrand_real3())) * sin(2.0*M_PI*genrand_real3() );
    return mu + sigma * z;
 }

// Truncated Normal Distribution
double rand_truncated_normal(double mu, double sigma){  //double sigma
    double x = rand_normal(mu, sigma);
    if(mu!=0.0){
        if(x <= -0.5 || x > 2.0 * mu){
     //   if(x <= 0 || x > 2*mu){        
            return rand_truncated_normal(mu, sigma);
        } else {
            return x;
        }
    } else {
        if(x <= -0.5 || x > 1){
    //    if(x <= 0 || x > 2){        
            return rand_truncated_normal(mu, sigma);
        } else {
            return x;
        }
    }
    printf("ERROR![rand_truncated_normal:%.3f-%.3f]\n", mu, sigma);
}

// c.d.f [Truncated Normal Distribution]
double r2_cdf_truncated_normal(double x, double mu, double sigma){  //double sigma
    double p;   //sigma = 1
    if(mu != 0.0){
        p = (cdf_truncated_normal(x+0.5,mu,sigma) - cdf_truncated_normal(x-0.5,mu,sigma))/(cdf_truncated_normal(2*mu,mu,sigma) - cdf_truncated_normal(-0.5,mu,sigma));
    } else {
        if(x = 1.0){
            p = (cdf_truncated_normal(x,mu,sigma) - cdf_truncated_normal(x-0.5,mu,sigma))/(cdf_truncated_normal(1,mu,sigma) - cdf_truncated_normal(-0.5,mu,sigma));
        } else {
            p = (cdf_truncated_normal(x+0.5,mu,sigma) - cdf_truncated_normal(x-0.5,mu,sigma))/(cdf_truncated_normal(1,mu,sigma) - cdf_truncated_normal(-0.5,mu,sigma));
        }
    }
    //printf("P:%.3f ",p);
    return log(p);
}

double cdf_truncated_normal(double x, double mu, double sigma){
    return 0.5 * (1 + erf((x-mu)/pow(2*sigma*sigma, 0.5)));
}

/* READ a OD table file */
void fileinod(int *od){
    FILE *fp;
    char *fname = "odtable-row2.csv";  // should be row style not table
    int ret, cell;
    int i =0;

    fp = fopen(fname, "r");
    if(fp==NULL){
        printf("file %s can't be opened\n", fname);
    }
    
    while((ret = fscanf(fp, "%d", &cell))!=EOF){
        od[i] = cell;
        i++;
    }

    fclose(fp);
}

void fileinod00(double *od){
    FILE *fp;
    char *fname = "odtable-row-ini2.csv";  // should be row style not table
    double cell;
    int i =0, ret;
    
    fp = fopen(fname, "r");
    if(fp==NULL){
        printf("file %s can't be opened\n", fname);
    }
    
    while((ret = fscanf(fp, "%lf", &cell))!=EOF){
        od[i] = cell;
        i++;
    }
    fclose(fp);
}

/* READ a link file */
struct link *fileinli(void){
    FILE *fp;
    char *fname = "veh-time2.csv";
    int ret, onode, dnode;
    double cost;
    int i =0;
    
    fp = fopen(fname, "r");
    if(fp==NULL){
        printf("file %s can't be opened\n", fname);
    }
    
    while((ret = fscanf(fp, "%d, %d, %lf", &onode, &dnode, &cost))!=EOF){
        li[i].onode = onode;
        li[i].dnode = dnode;
        li[i].cost = cost;
        i++;
    }
    fclose(fp);
    return li;
}