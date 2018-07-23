#ifndef UTILS_H
#define UTILS_H

#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <regex>
#include <vector>

using namespace std;

extern vector<string> split(string input, char separator);
extern bool contains(vector<string> xm, string s);
extern string trim(char *s);
extern string replace_all(string input, const string& tpl, const string& val);
extern bool is_pam_mkhomedir_used();

#endif // UTIL_H
