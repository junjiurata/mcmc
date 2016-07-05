/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

#include "Csv.h"
#include "Error.h"

vector<string> getSepColLine(string strline,char sep) {
	stringstream inputStream;
	inputStream << strline;
	string separated;
	vector<string> ans;
	while(getline(inputStream,separated,sep)) {
		// CR+LFの改行文字に備えて，余計なCRを消す
		separated.erase(separated.find_last_not_of("\r")+1);
		separated.erase(separated.find_last_not_of("\n")+1);
		separated.erase(separated.find_last_not_of("\r")+1);
		ans.push_back(separated);
	}
	return ans;
}

void Csv::initialise(istream& in, char _sep, bool isFirstRowHeader) {
	sep = _sep;
	string strline;
	numLines = 0;
	// If the first row is headers, read it.
	if (isFirstRowHeader) {
		if (in.eof()) {
			errorStop("No more data for header in the file.");
		}
		getline(in, strline);
		headerInv = getSepColLine(strline, sep);
		for (int i = 0; i<(int)headerInv.size(); i++) {
			header[headerInv[i]] = i;
		}
	}
	while (!in.eof()) {
		getline(in, strline);
		// Skip blank lines
		if (strline.size() > 0) {
			if ((strline.at(0) != '\n') && (strline.at(0) != '\r')) {
				data.push_back(getSepColLine(strline, sep));
			}
		}
	}
}

Csv::Csv (istream& in,char _sep, bool isFirstRowHeader) {
	initialise(in, _sep, isFirstRowHeader);
}

Csv::Csv(const char* filename, char _sep, bool isFirstRowHeader) {
	ifstream inFile(filename);
	if (inFile.fail()) {
		stringstream s;
		s << "Csv.cpp: Cannot open file : " << filename;
		errorStop(s.str());
	}
	initialise(inFile, _sep, isFirstRowHeader);
	inFile.close();		
}

string Csv::getData(int row, string colName) {
	int col = header[colName];
	if (getNumCols(row) <= col) {
		stringstream s;
		s << "Too big column number" << endl;
		s << row << "," << col << endl;
		errorStop(s.str());
	}
	return data[row][col];
}

string Csv::getData(int row, int col) {
	if (getNumCols(row) <= col) {
		stringstream s;
		s << "Too big column number" << endl;
		s << row << "," << col << endl;
		errorStop(s.str());
	}
	return data[row][col];
}

bool Csv::isColName(string colName) {
	return (header.count(colName) == 1);
}

int Csv::colNum(string colName) {
	if (!isColName(colName)) {
		stringstream s;
		s << "No such column: " << colName << endl;
		errorStop(s.str());
	}
	return header[colName];
}

string Csv::getColName(int col) {
	if (getNumColNames() <= col) {
		stringstream s;
		s << "Too big column number" << endl;
		s << col << endl;
		errorStop(s.str());
	}
	return headerInv[col];
}

string Csv::getAllHeaders() {
	string ans;
	for (int i=0;i<(int)header.size();i++) {
		for (map<string,int>::iterator it = header.begin();
			 it != header.end(); it++) {
			if (it->second == i) {
				ans += it->first;
				if (i < (int)(header.size()-1)) ans += sep;
				break;
			}
		}
	}
	return ans;
}

int Csv::getNumLines() {
	return data.size();
}

int Csv::getNumColNames() {
	return (int)headerInv.size();
}

int Csv::getNumCols(int row) {
	return data[row].size();
}
