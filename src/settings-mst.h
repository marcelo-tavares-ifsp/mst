#ifndef SETTINGSMST_H
#define SETTINGSMST_H

#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <regex>

using namespace std;

struct Xrandr_monitor
{
    string interface;
    vector<string> resolutions;
};

class Settings_mst
{
public:
    Settings_mst();
    static vector<string> run_xrandr();
    static vector<Xrandr_monitor> parse_xrandr();
private:
    static void trim(char *s);
};

#endif // SETTINGSMST_H
