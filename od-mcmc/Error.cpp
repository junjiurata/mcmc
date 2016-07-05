/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

#include "error.h"

void errorStop_base(const char* message) {
	cerr << message << endl;
#ifndef NOERRSTOP
	getchar();
#endif
	exit(1);
}

void errorStop(const char* message) {
	errorStop_base(message);
}

void errorStop(const string message) {
	errorStop_base(message.c_str());
}

void errorStop(const stringstream message) {
	errorStop(message.str());
}

void errorStop() {
	errorStop("Error. Stop.");
}

void exitWait(int n) {
	cerr << "End of the program: press any key." << endl;
#ifndef NOERRSTOP
	getchar();
#endif
	exit(n);
}