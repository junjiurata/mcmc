/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   fw.h
 * Author: urata
 *
 * Created on July 5, 2016, 5:26 PM
 */

#ifndef FW_H
#define FW_H
#include <iostream>
#include <stdio.h>
#include <math.h>
#include <vector>
#include <map>
#include "Csv.h"

class Link {
public:
	// コンストラクタ
	Link(double _a, double _b) {
		a = _a; b = _b;
		x = 0.0;
		y = 0.0;
	}
	// リンクパフォーマンス関数
	double a;
	double b;
	// 交通量
	double x;
	// All-or-Nothingの交通量
	double y;
	// 旅行時間をxによって計算する
	double tt() {
		return a + b*x*x*x*x;
	}
	// 指定したalphaで交通量を混ぜる
	double mixXY(double alpha) {
		return (1 - alpha)*x + alpha*y;
	}
	// 旅行時間の積分を指定したalphaで計算する
	double ttI(double alpha) {
		double z = mixXY(alpha);
		return a*z + 0.2*b*z*z*z*z*z;
	}
};

class Node{
public:
	Node() {
		toNode.clear();
		links.clear();
	}
	vector<int> toNode;
	vector<Link> links;
	bool isDetermined;
	bool isInf;
	double minCost;
	int fromNode;
};

void addLink(map<int, Node>& nodes, int from, int to, double a, double b);
void clearY(map<int, Node>& nodes);
void printLinkData(map<int, Node>& nodes);
void updateX(map<int, Node>& nodes, double al);
void dijkstra(int start, int end, map<int, Node>& nodes, double traf);
double dijkstrafortable(int start, int end, map<int, Node>& nodes);
double obj(double al, map<int, Node> nodes);
double gold(map<int, Node>& nodes);
double convergence(map<int, Node>& nodes, double al, double linknum);
void linkread(map<int, Node>& nodes);   // CHECK
void iniod(vector<int> &start, vector<int> &end, vector<double> &odflow);
void fwolfe(map<int, Node>& nodes, vector<int> &start, vector<int> &end, vector<double> &odflow);

#endif /* FW_H */

