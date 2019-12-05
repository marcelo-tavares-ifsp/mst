#ifndef UTILITES_H
#define UTILITES_H

#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <regex>
#include <vector>
#include <QString>

using namespace std;

extern vector<string> split(string input, char separator);
extern bool contains(vector<string> xm, string s);
extern string trim(char *s);
extern string replace_all(string input, const string& tpl, const string& val);
extern bool is_pam_mkhomedir_used();
extern vector<string>::iterator _set_intersection(const vector<string>& v1,
                                                  const vector<string>& v2,
                                                  vector<string>& output);
extern void _set_intersection_x(vector<string> &v1,
                                vector<string> &v2,
                                vector<string>& output,
                                function<bool(const string&, const string&)> sort_function);
extern vector<int> _parse_resolution(QString resolution);
extern string to_std_string(QString qs);
extern QString to_qstring(string str);
extern void cp(const string& src, const string& dst);

#endif // UTILITES_H
