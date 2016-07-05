/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Error.h
 * Author: iryolabo
 *
 * Created on 2016/07/01, 14:30
 */

/*
	エラー発生
	NOERRSTOP を定義しないでコンパイルすると，終了前にgetcharによって文字入力待ちになる．

*/

#ifndef COMMON_ERROR_H_
#define COMMON_ERROR_H_
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>

using namespace std;
void errorStop(const stringstream message);
void errorStop(const string message);
void errorStop(const char* message);
void errorStop();
void exitWait(int n);

#endif


