/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   csv.h
 * Author: iryolabo
 *
 * Created on 2016/06/24, 11:34
 */

#ifndef _INCLUDE_CSV
#define _INCLUDE_CSV

#ifdef _DEBUG
#pragma comment (lib,"Common/Error/Debug/error.lib")
#else 
#pragma comment (lib,"Common/Error/Release/error.lib")
#endif

//#include <Common/Error/error.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <sstream>
#include <vector>
#include <map>
using namespace std;

class Csv {
private:
	vector<vector<string> > data;
	map<string,int> header;
	vector<string>  headerInv;
	char sep;
	int  numLines;
	void initialise(istream& in, char _sep, bool isFirstRowHeader);

public:
	// Specify input file stream and separator
	// to construct an instance
	// If the third arg is true, first row is recognised as
	// the name of columns. Otherwise, the name of column is
	// automatically set.
	Csv(istream& in, char _sep, bool isFirstRowHeader);
	Csv(const char* filename, char _sep, bool isFirstRowHeader);

	// Check if the column name specified exists in the data
	bool isColName(string colName);
	int colNum(string colName);
	int getNumLines();

	// Get the name of a specified column
	string getColName(int col);

	// Get specified row and column
	string getData(int row, string colName);
	string getData(int row, int col);

	// Note: the number of columns may be different among rows
	int getNumColNames();
	int getNumCols(int row);

	string getAllHeaders();

};

vector<string> getSepColLine(string strline,char sep);

#endif


