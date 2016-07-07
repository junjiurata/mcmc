/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   main.cpp
 * Author: iryolabo
 *
 * Created on 2016/06/24, 10:58
 */

//#include <iostream>
//#include <stdio.h>
//#include <math.h>
//#include <vector>
//#include <map>
//#include "Csv.h"
#include "fw.h"
using namespace std;

const int MAX = 1;
const double EPS = 10e-2;

// #define DEBUG

// ネットワークにノードとリンクを追加する
void addLink(map<int, Node>& nodes, int from, int to, double a, double b){
	// from, toノードが初出であれば，新たに作成する
	if (nodes.count(from) == 0) {
		nodes.insert(map<int, Node>::value_type(from, Node()));
	}
	if (nodes.count(to) == 0) {
//            nodes.insert(map<int, Node>::value_type(from, Node()));
//            2016-06-24 furuta 
                nodes.insert(map<int, Node>::value_type(to, Node()));
	}
	nodes[from].toNode.push_back(to);
	nodes[from].links.push_back(Link(a, b));
}

// All-or-nothing交通量をご破算する．
void clearY(map<int, Node>& nodes) {
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		for (vector<Link>::iterator j = i->second.links.begin(); j != i->second.links.end(); j++) {
			j->y = 0.0;
		}
	}
}

// リンクの交通量と旅行時間を出力する
void printLinkData(map<int, Node>& nodes) {
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		int m = i->second.toNode.size();
		for (int j = 0; j < m; j++) {
			cout << i->first << " -> " << i->second.toNode[j]
				<< "; (交通量,旅行時間) = " << i->second.links[j].x;
			cout << ", " << i->second.links[j].tt() << endl;
		}
	}
}

// 交通量の更新（指定したαで交通量xとyをまぜてそれを新しいxにする）
// al = 0 で xが100%, al = 1 で y = 100%
void updateX(map<int, Node>& nodes, double al) {
#ifdef DEBUG
	cerr << "交通量を更新…" << endl;
#endif
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		int m = i->second.toNode.size();
		for (int j = 0; j < m; j++) {
			i->second.links[j].x = i->second.links[j].mixXY(al);
#ifdef DEBUG
			cerr << i->first << " -> " << i->second.toNode[j]
				<< "; (交通量,旅行時間) = " << i->second.links[j].x;
			cerr << ", " << i->second.links[j].tt() << endl;
#endif
		}
	}
}

// ダイクストラ法で最短経路を計算し，そこに指定交通量を加える
void dijkstra(int start, int end, map<int, Node>& nodes, int traf){
#ifdef DEBUG
	cerr << "ダイクストラ法" << endl;
                cout << "O:" << start << " D:" << end << endl;
#endif
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		i->second.isDetermined = false;
		i->second.isInf = true;
		i->second.minCost = 0.0;
	}
	nodes[start].minCost = 0.0;
	nodes[start].isInf = false;
	while (1){
//		cout << "nodes.size=" << nodes.size() << endl;
		int doneNode = -1;
		for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
			if (i->second.isDetermined) {
//				cerr << i->first << " is determined\n";
			}
			else if (i->second.isInf) {
//				cerr << i->first << " is ∞\n";
			}
			else if (doneNode == -1) {
				doneNode = i->first;
//				cerr << "doneNode=-1 =>" << doneNode << endl;
			}
			else if (i->second.minCost < nodes[doneNode].minCost) {
				doneNode = i->first;
//				cerr << "doneNode=" << doneNode << endl;
			}
		}
		if (doneNode == -1) break;  // どういう状況？
		nodes[doneNode].isDetermined = true;
#ifdef DEBUG
		cerr << "ノード " << doneNode << " のコストが " << nodes[doneNode].minCost << "に確定" << endl;
#endif
		// ノードコストの更新
		for (unsigned int i = 0; i<nodes[doneNode].toNode.size(); i++){
			int toNode = nodes[doneNode].toNode[i];
			double cost = nodes[doneNode].minCost + nodes[doneNode].links[i].tt();
			if (nodes[toNode].isInf|| cost < nodes[toNode].minCost){
				nodes[toNode].minCost = cost;
				nodes[toNode].isInf = false;
				nodes[toNode].fromNode = doneNode;
			}
		}
	}
	// 先行ポインタをたどる
	int cur = end;
#ifdef DEBUG
            cerr << "最短経路を逆順にたどっています…" << endl;
#endif
	while (cur != start) {
#ifdef DEBUG
		cerr << cur << " ";
#endif
		int prev = nodes[cur].fromNode;
		// たどった先のリンクのリストをチェックし，たどった元へのリンクを探す
		bool isFound = false;
		for (unsigned int i = 0; i<nodes[prev].toNode.size(); i++){
			if (nodes[prev].toNode[i] == cur) {
				nodes[prev].links[i].y += traf;
				isFound = true;
				break;
			}
		}
		if (!isFound) {
			cerr << "ノードが見つかりませんね" << endl;
			exit(1);
		}
		cur = prev;
	}
#ifdef DEBUG
	cerr << cur << endl;
#endif
}

// ダイクストラ法で最短経路を計算し，そこに指定交通量を加える
double dijkstrafortable(int start, int end, map<int, Node>& nodes){
#ifdef DEBUG
	cerr << "ダイクストラ法fortable" << endl;
                cout << "O:" << start << " D:" << end << endl;
#endif
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		i->second.isDetermined = false;
		i->second.isInf = true;
		i->second.minCost = 0.0;
	}
	nodes[start].minCost = 0.0;
	nodes[start].isInf = false;
	while (1){
//		cout << "nodes.size=" << nodes.size() << endl;
		int doneNode = -1;
		for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
			if (i->second.isDetermined) {
//				cerr << i->first << " is determined\n";
			}
			else if (i->second.isInf) {
//				cerr << i->first << " is ∞\n";
			}
			else if (doneNode == -1) {
				doneNode = i->first;
//				cerr << "doneNode=-1 =>" << doneNode << endl;
			}
			else if (i->second.minCost < nodes[doneNode].minCost) {
				doneNode = i->first;
//				cerr << "doneNode=" << doneNode << endl;
			}
		}
		if (doneNode == -1) break;  // どういう状況？: 更新対象がない状態
		nodes[doneNode].isDetermined = true;
#ifdef DEBUG
		cerr << "ノード " << doneNode << " のコストが " << nodes[doneNode].minCost << "に確定" << endl;
#endif
                if(doneNode == end){    // dijkstra 終端ノードが確定したら打ち切り
                    return nodes[doneNode].minCost;
                    break;
                }
		// ノードコストの更新
		for (unsigned int i = 0; i<nodes[doneNode].toNode.size(); i++){
			int toNode = nodes[doneNode].toNode[i];
			double cost = nodes[doneNode].minCost + nodes[doneNode].links[i].tt();
			if (nodes[toNode].isInf|| cost < nodes[toNode].minCost){
				nodes[toNode].minCost = cost;
				nodes[toNode].isInf = false;
				nodes[toNode].fromNode = doneNode;
			}
		}
	}
}

// 目的関数の値を計算する 
double obj(double al, map<int, Node> nodes) {
	double f = 0.0;
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		for (vector<Link>::iterator j = i->second.links.begin(); j != i->second.links.end(); j++) {
			f += j->ttI(al);
		}
	}
	return f;
}

//目的関数に対する黄金分割法
double gold(map<int, Node>& nodes) {
	// 0と1のあいだで探索する
	double x1 = 0.0;
	double x2 = 1.0;
	// 黄金分割比
	const double s = (sqrt(5.0) - 1.0) / 2.0;

	double g1 = x2 - s*(x2 - x1);
	double g2 = x1 + s*(x2 - x1);

	double f1 = obj(g1, nodes);
	double f2 = obj(g2, nodes);

	for (int n = 0; n < MAX; n++) {
		// 収束条件
		if (fabs(x1 - x2) < EPS) break;
		if (f1 > f2) {
			x1 = g1;
			g1 = g2;
			g2 = x1 + s*(x2 - x1);
			f1 = f2;
			f2 = obj(g2, nodes);
		}
		else {
			x2 = g2;
			g2 = g1;
			g1 = x2 - s*(x2 - x1);
			f2 = f1;
			f1 = obj(g1, nodes);
		}

	}
	return (x1 + x2) / 2;
}

//収束判定
double convergence(map<int, Node>& nodes, double al, double linknum){
	double errsum = 0;
	for (map<int, Node>::iterator i = nodes.begin(); i != nodes.end(); i++) {
		int m = i->second.toNode.size();
		for (int j = 0; j < m; j++) {
			double err = i->second.links[j].x - i->second.links[j].mixXY(al);
			double Err = err / i->second.links[j].mixXY(al);
			errsum += fabs(Err);
		}
	}
	double errave = errsum / linknum;
	return errave;
}

void linkread(map<int, Node>& nodes, int *LINKNUM){
        // 外部方からのデータ入力
        // linkデータ 始点ノード,終点ノード，旅行時間パラa，旅行時間パラb の要素を持つ
        Csv link("SiouxLinks2_mody.csv", ',' , true);
    
	//リンクの数の読み込み
	*LINKNUM = link.getNumLines();
        cout << *LINKNUM << endl;
	//リンクデータの読み込み
	vector<int> fromnode;
	vector<int> tonode;
	vector<double> Costa;
	vector<double> Costb;

        int st;
        std::string sst;
        double cos;
        std::string scos;
	for (int i = 0; i < *LINKNUM; i++){
                sst = link.getData(i,"ST");
                st = std::atoi(sst.c_str());
		fromnode.push_back(st);
                
                sst = link.getData(i,"EN");
                st = std::atoi(sst.c_str());
                tonode.push_back(st);
                
		scos = link.getData(i,"costA");
                cos = std::atof(scos.c_str());
                Costa.push_back(cos);
                
		scos = link.getData(i,"costB");
                cos = std::atof(scos.c_str());
		Costb.push_back(cos);
                
		addLink(nodes, fromnode[i], tonode[i], Costa[i], Costb[i]);// 最終ノードからはリンクが出ていないので初期化しない
	}
}

void iniod(vector<int> &start, vector<int> &end, vector<int> &odflow){
        // odデータ スタートノード，エンドノード，OD交通量 の要素を持つ
        Csv od("odtable-row3.csv", ',' , true);

	//OD交通量の読み込み
	int ODNUM = od.getNumLines();
        cout << ODNUM << endl;
	//vector<int> START;      //スタートノード
	//vector<int> END;        //エンドノード
	//vector<double> ODFLOW;  //OD交通量
        int ori;
        std::string sori;
        double tra;
        std::string stra;
	for (int i = 0; i < ODNUM; i++){
                sori = od.getData(i,"ORIG");
                ori = std::atoi(sori.c_str());
		start.push_back(ori);
                
                sori = od.getData(i,"DEST");
                ori = std::atoi(sori.c_str());
		end.push_back(ori);
                
                stra = od.getData(i,"TRAF");
                tra = std::atof(stra.c_str());
		odflow.push_back(tra);
	}
}

void fwolfe(map<int, Node>& nodes, vector<int> &start, vector<int> &end, vector<int> &odflow, int *LINKNUM){
        int ODNUM = start.size();
        // cout << " ODNUM:" << ODNUM << endl;
        // 最短経路を自由流旅行時間（交通量０のとき）で計算し，All-or-nothingを行う

	clearY(nodes);

        for (int i = 0; i<ODNUM; i++){
            if(odflow[i]!=0){
		dijkstra(start[i], end[i], nodes, odflow[i]);
            }
	}

	// 初期リンク交通量を計算
	updateX(nodes, 1.0);
        
	// ここからFrank-Wolfeのループ
	for (int k = 0; k<MAX; k++) {
		// 最短経路を計算しAll-or-nothingを行う
		clearY(nodes);
		for (int i = 0; i<ODNUM; i++){
                    if(odflow[i]!=0){
			dijkstra(start[i], end[i], nodes, odflow[i]);
                    }
		}
		// 黄金分割法により最適なαを計算する
		double alpha = gold(nodes);
		//収束の判定
		double ERR = convergence(nodes, alpha, *LINKNUM);
		if (ERR < EPS){
                        printf("%d回目で", k);
                        cout << "収束しました。\n";
			break;
		}
#ifdef DEBUG
		cerr << "α = " << alpha << endl;
#endif
		// 交通量をアップデートする
		updateX(nodes, alpha);
	}
	//printLinkData(nodes);
        //printf("DIJKSTRA\n");
}

