#ifndef COMMANDMANAGER_H
#define COMMANDMANAGER_H

#include <string>
#include <string.h>
#include <vector>
#include <QLoggingCategory>
#include "common/utilites/utilites.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(command_manager_category)

struct xrandrMonitor
{
    string interface;
    vector<string> resolutions;
};

class CommandManager
{
public:
    static bool is_running(const char *processName);
    static bool kill_process(const char *processName);
    static int xset_dpms();
    static int xset_soff();
    static vector<xrandrMonitor> get_interfaces_from_xrandr();
    static vector<int> get_awesome_version();
    static bool config_vgl();

    static void get_devices_from_ls(vector<string>& mice, vector<string>& keybds);

private:    
    CommandManager(){}
};

#endif // COMMANDCONTROLLER_H
