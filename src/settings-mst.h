#ifndef SETTINGSMST_H
#define SETTINGSMST_H

#include <iostream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <QByteArray>

#include "utils.h"

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
    static vector<string> run_ls_devices();
    static void parse_ls_devices(vector<string> *mice, vector<string> *keybds);
    static bool loop_answer(string device);
};

#endif // SETTINGSMST_H
