
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

//#ifndef 
//#define 
#include <stdio.h>
#include "MT.h"
#include <time.h>
#include <math.h>
#include <stdlib.h>
#include <cmath>
#include <map>
#include "fw.h"

#define NODES 24
#define LINKS 576   //24*24
#define TRY 350000   // number of sampling
#define MEM 25000   // number of sampling

// exogenous parameter
const double BETA   = 0.08;
const double ALPHA3 = 10e-11; // ALPHA3/ALPHA1 = 10000 ~ 500000, small dif of OD cost: 1/100
const double ALPHA1 = 2.00;
const int OBJ = LINKS - NODES;  // for cell choice

// 構造体定義
struct odpair{
	int onode; 
	int dnode; 
	double cost; 
};

// 関数プロトタイプ
//void fileinod(int *od);
void fileinod00(int *od);
struct odpair *fileinli(void);
double linkcost(int onode, int dnode, odpair *li);
int fact(int n);
double logfact(int n);
double logfact3(int n);
double energy (int *oi, int *dj, double c, int *oiz, int *djz, double cz, vector<int> &odz);
double ermsq (vector<int> &od, vector<int> &odz);
double ermsqod (int *oi, int *oiz);
int sumod(int *oiz);
int cellchoice();
double ent(vector<int> &odz);

// グローバル変数
static struct odpair li[LINKS];
       
// main
int main(void) {
    volatile int h, hh, hhh; //コンパイラによる最適化防止
    int i, j, k, co=1, mrow;
    int accept=0;
    int oi[NODES]={}, dj[NODES]={};
    int oip[NODES]={}, djp[NODES]={};
    int odpi[NODES*NODES];    // initial sample & accepted sample
    double ene, enep, ratio, c=0.0, cp=0.0;
    init_genrand((unsigned)time(NULL));
    char fname[50];
    int rem, add, *outOD;
    double *out, *outO, *outD, *outE, *outC, *outEN;
    int inc=0, dec=0;   // number of accepted
        // メモリ確保
        outOD = new int[MEM];
        out = new double[MEM];
        outO = new double[MEM];
        outD = new double[MEM];
        outE = new double[MEM];
        outC = new double[MEM];
        outEN = new double[MEM];
    // FrankWorfe
    int linknum, ODNUM;
    map<int, Node> nodes;
    vector<int> start;      //スタートノード
    vector<int> end;        //エンドノード
    vector<int> od;         //OD交通量
    vector<int> odp;
    vector<int> odz;
    FILE *fw;
    // Read
    struct odpair *LI = fileinli();

    ////////////////////////////////////////////////////////////////
    // True OD table
    //fileinod(od);
    linkread(nodes, &linknum);
    iniod(start, end, od);
    fwolfe(nodes, start, end, od, &linknum);
    ODNUM = od.size();
        //cout << "ODNUM "<< ODNUM << endl;
    
    for (int i = 0; i<ODNUM; i++){
        (LI+i)->onode = start[i];
        (LI+i)->dnode = end[i];
        (LI+i)->cost = dijkstrafortable(start[i], end[i], nodes);
        //printf("%d st:%d cost:%.2f\n", i, start[i], (LI+i)->cost);
    }

    for(i = 0; i < NODES; i++){
        for(j = 0; j < NODES; j++){
            oi[i] += od[i*NODES+j];     // constraint condition
            dj[i] += od[i+j*NODES];
            c += od[i*NODES+j] * linkcost(i+1, j+1, LI);    // 
            // cout << start[i*NODES+j] << ", " << end[i*NODES+j] << ", " << i+1 << ", " << j+1 << endl;
        }
    }
    //cout << "COST: " << c << endl;
    
    //* Initial OD table 
    fileinod00(odpi);   //randomにする
        // printf("%d %d %d \n", odpi[0], odpi[1], odpi[3]);
    for(i=0; i<NODES*NODES; i++){
        odp.push_back(odpi[i]);
    }
        
    fwolfe(nodes, start, end, odp, &linknum);   //link cost recalculate (table:odp))
    for (int i = 0; i<ODNUM; i++){
        (LI+i)->cost = dijkstrafortable(start[i], end[i], nodes);   // upload OD travel time
        //printf("%d st:%d cost:%.2f\n", i, start[i], (LI+i)->cost);
    }
        
    for(i = 0; i < NODES; i++){ 
        for(j = 0; j < NODES; j++){
            oip[i] += odp[i*NODES+j];
            djp[i] += odp[i+j*NODES];
            cp += odp[i*NODES+j] * linkcost(i+1, j+1, LI);
        }
    }

    enep = energy(oi, dj, c, oip, djp, cp, odp);
    cout << "COST: " << c << ", INI:" << cp << ", Energy:" << enep << endl;
    //printf("INITIAL error: O=%f, D=%f, OD=%f", ermsqod(oi, oip), ermsqod(dj, djp), ermsq(od, odp));

    for (i = 0; i < LINKS; i++){
        odz.push_back(odp[i]);        // odp:previous accepted table  odz:this iteration table
    }
    
    ///////////////////////////////////////////////////////////////
    hh = 0;    
    for(h=0; h < TRY; h++){
    //    cout << h << endl;
        out[hh]   = -99.0;
    //    outO[hh]  = -99.0;
    //    outD[hh]  = -99.0;
    //    outE[hh]  = -99.0;
    //    outC[hh]  = -99.0;
        int oiz[NODES]={}, djz[NODES]={};
        double cz= 0.0;
        int it = 0; //number of iteration
        int it_limit = 5;
        
        for (i = 0; i < LINKS; i++){
            odz[i] = odp[i];        //update (accepted sample)
        }

        if(h % 3 == 0){ // Exchange
            while(it < it_limit){
                rem = cellchoice();
                if(odz[rem]!=0){
                    add = cellchoice();
                    odz[rem] -= 1;
                    odz[add] += 1;
                }
                it += 1;
            }
        } else if(h % 3 == 1){  // Add
            while(it < it_limit){
                add = cellchoice();
                odz[add] += 1;
                it += 1;
            }
       } else {    // Remove
            while(it < it_limit){
                rem = cellchoice();
                if(odz[rem]!=0){
                    odz[rem] -= 1;
                }
                it += 1;
            }
        }

        fwolfe(nodes, start, end, odz, &linknum);   //link cost recalculate (table:odp))
        for (i = 0; i<ODNUM; i++){
            (LI+i)->cost = dijkstrafortable(start[i], end[i], nodes);   // upload OD travel time
            //printf("%d st:%d cost:%.2f\n", i, start[i], (LI+i)->cost);
        }
        
        // total O & D & cost of sample 
        for(i = 0; i < NODES; i++){ 
            for(j = 0; j < NODES; j++){
                oiz[i] += odz[i*NODES+j];
                djz[i] += odz[i+j*NODES];
                cz += odz[i*NODES+j] * linkcost(i+1, j+1, LI);
            }
        }

        ene = energy(oi, dj, c, oiz, djz, cz, odz);
        outOD[hh] = sumod(oiz);
        outC[hh] = cz;
        outE[hh] = ene;
        outEN[hh] = ent(odz);
        outO[hh] = ermsqod(oi, oiz);
        outD[hh] = ermsqod(dj, djz);
                      
        if(BETA*(ene-enep) > 0){ // Accept *log(1)=0)
              accept++;
              if(h % 3 == 1){
                  inc++;
              } else if(h % 3 == 2){
                  dec++;
              }
              //  printf("Dene: %.2f, cost: %.1f \n", BETA*(ene - enep), cz);    //write
              enep = ene;
               printf("[%d]OK!\n", h);
              out[hh] = ermsq(od, odz);
                  
              sprintf(fname, "D:/od-mcmc/c-result/res-od/odtable-%d.csv", h);//+TRY*2
              if ((fw = fopen(fname, "w")) != NULL){
                for (i = 0; i < LINKS; i++){
                    fprintf(fw, "%d\n", odz[i]);
                    odp[i] = odz[i];    //update (accepted sample)
                }
                fclose(fw);
            }
        } else {
            ratio = exp(BETA*(ene - enep));
                // printf("Dene: %.1f cost:%.1f, Ratio: %.3f\n", BETA*(ene - enep), cz, ratio);  //write
            if(isnan(ratio)==0){
                if(ratio >= genrand_real3()){   // Accept
                    accept++;
                    if(h % 3 == 1){
                      inc++;
                    } else if(h % 3 == 2){
                      dec++;
                    }
                    enep = ene;
                     printf("[%d] POK!!\n", h);
                    out[hh] = ermsq(od, odz);
               
                    sprintf(fname, "D:/od-mcmc/c-result/res-od/odtable-%d.csv", h);//+TRY*2
                    if ((fw = fopen(fname, "w")) != NULL){
                        for (i = 0; i < LINKS; i++){
                            fprintf(fw, "%d\n", odz[i]);
                            odp[i] = odz[i];    //update (accepted sample)
                        }
                        fclose(fw);
                    }
                }
            } else {
                printf("obtain NA...\n");
            }
        }
       
        if((hh+1 == MEM)||(h == TRY-1)){
            mrow = MEM;
            if(h == TRY -1){
                mrow = TRY % MEM;
                if(mrow == 0) mrow = MEM;
            }
            sprintf(fname, "D:/od-mcmc/c-result/errsd%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                    fprintf(fw, "%f\n", *(out + hhh));
                }
                fclose(fw);
            }
            sprintf(fname, "D:/od-mcmc/c-result/errsd-o%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                    fprintf(fw, "%f\n", *(outO + hhh));
                }
                fclose(fw);
            }
            sprintf(fname, "D:/od-mcmc/c-result/errsd-d%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                	fprintf(fw, "%f\n", *(outD + hhh));
                }
                fclose(fw);
            }
            sprintf(fname, "D:/od-mcmc/c-result/energy%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                	fprintf(fw, "%f\n", *(outE + hhh));
                }
                fclose(fw);
             }
            sprintf(fname, "D:/od-mcmc/c-result/cost%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                    fprintf(fw, "%f\n", *(outC + hhh));
                }
                fclose(fw);
            }
            sprintf(fname, "D:/od-mcmc/c-result/totalod%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                    fprintf(fw, "%d\n", *(outOD + hhh));
                }
                fclose(fw);
            }
            sprintf(fname, "D:/od-mcmc/c-result/entropy%d.csv", co);
            if ((fw = fopen(fname, "w")) != NULL){
                for (hhh = 0; hhh < mrow; hhh++){
                    fprintf(fw, "%f\n", *(outEN + hhh));
                }
                fclose(fw);
            }
            hh = -1;
            co++;
            printf("\nACCEPT:%d (Inc:%d Dec:%d)\n", accept, inc, dec);
        }
        hh++;
    }
    
    printf("\nACCEPT:%d (Inc:%d Dec:%d)", accept, inc, dec);
    printf("\nTRUE: cost:%.2f sumod:%d entropy:%.3f", c, sumod(oi), ent(od));

    // メモリ解放
    delete[] outOD;
    delete[] out;
    delete[] outO;
    delete[] outD;
    delete[] outE;
    delete[] outC;
    delete[] outEN;    
    return 0;
}

/* ERROR of MEAN SQUARE */
double ermsq (vector<int> &od, vector<int> &odz){
    int i;
    double err = 0.0;
    double sd;
    
    for(i=0; i < NODES*NODES; i++){
        err += pow(odz[i]-od[i], 2.0);
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
        err += pow(oiz[i]-oi[i], 2.0);
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
double energy (int *oi, int *dj, double c, int *oiz, int *djz, double cz, vector<int> &odz){
    int total = 0;
    double conbi = 0.0;
    int i, rodz;
    double peo = 0.0, ped = 0.0, penal, count;
    
    for(i = 0; i < NODES*NODES; i++){
        rodz = odz[i];
        total += rodz;
        conbi += logfact(rodz);
    }
    // printf("TOTAL:%d conbi:%.1f logfact(total):%.1f\n", total, conbi, logfact(total));
    count = logfact3(total) + logfact3(total-1) + logfact3(total-2) - conbi;
            
    for(i = 0; i < NODES; i++){
        peo += pow(*(oiz + i) - *(oi + i), 2.0);
        ped += pow(*(djz + i) - *(dj + i), 2.0);
    }
    penal = ALPHA3 * pow(cz - c, 2) + ALPHA1 * peo + ALPHA1 * ped;
    //  printf("ent:%.1f peo:%.1f ped:%.1f Dc:%.1f\n", count, ALPHA1 * peo, ALPHA1 * ped, ALPHA3 * pow(cz - c, 2));    //write
    
    return count - penal;
}

/* CALCULATE energy term  (equation (11) of resume at May 31)*/
double ent (vector<int> &odz){
    int total = 0;
    double conbi = 0.0;
    int i, rodz;
    double count;
    
    for(i = 0; i < NODES*NODES; i++){
        rodz = odz[i];
        total += rodz;
        conbi += logfact(rodz);
    }
    // printf("\ntotal:%d conbi:%.1f logfact(total):%.1f", total, conbi, logfact(total));
    count = logfact3(total) + logfact3(total-1) + logfact3(total-2) - conbi;
            
    return count;
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
double linkcost(int onode, int dnode, odpair *li){
    int i=0;
    double cost= -9999.9;   //path cost between onode and dnode
    while(cost == -9999.9 && i < LINKS){
        if(onode == (li+i)->onode){
            if(dnode == (li+i)->dnode){
                cost = (li+i)->cost;
               // printf("%.1f \n", cost);
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
/*
double rand_normal(double mu, double sigma){  //
    //double sigma = 1.0; //σ=1.0 fixed
    double z = sqrt(-2.0*log(genrand_real3())) * sin(2.0*M_PI*genrand_real3() );
    return mu + sigma * z;
 }*/

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

void fileinod00(int *od){
    FILE *fp;
     char *fname = "odtable-row-ini2.csv";  // should be row style not table
    //  char *fname = "odtable-119994.csv";  // should be row style not table
    int cell;
    int i =0, ret;
    
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

/* READ a link file */
struct odpair *fileinli(void){
    FILE *fp;
    char *fname = "veh-time3.csv";
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
        //printf("%f\n", li[i].cost);
    }
    fclose(fp);
    return li;
}
