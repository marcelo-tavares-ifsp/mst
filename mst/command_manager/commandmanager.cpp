#include "commandmanager.h"

#include <unistd.h>
#include <linux/reboot.h>
#include <sys/reboot.h>

Q_LOGGING_CATEGORY(command_manager_category, "mst.command_manager")

// static methods ///////////////////////////////////////////////////////////////

static vector<string> run_xrandr()
{
    static const char *COMMAND = "xrandr";
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* file;

    if ((file = popen(COMMAND, "r")) != NULL)
    {
        vector<string> result;
        while (fgets(buf, BUF_SZ, file) != NULL)
        {
            result.push_back(trim(buf));
        }
        pclose(file);
        return result;
    }
    else
    {
        qCritical(command_manager_category()) << "Could not execute xrandr";
        throw "Could not execute xrandr";
    }
}

/**
 * @brief run_ls_devices -- get a list of input
 *      devices.
 * @return a list of devices.
 */
static vector<string> run_ls_devices()
{
    static const char *COMMAND = "ls /dev/input/by-path/";
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* file;

    if ((file = popen(COMMAND, "r")) != NULL)
    {
        vector<string> result;
        while (fgets(buf, BUF_SZ, file) != NULL)
        {
            result.push_back(trim(buf));
        }
        pclose(file);
        return result;
    }
    else
    {
        qCritical(command_manager_category())
                << "Could not execute ls /dev/input/by-path/";
        throw "Could not execute ls /dev/input/by-path/";
    }
}

// public methods ///////////////////////////////////////////////////////////////

bool CommandManager::is_running(const char *processName)
{
    char cmd[100] = "pgrep -c ";
    strcat(cmd, processName);
    return (system(cmd) == 0);
}

bool CommandManager::kill_process(const char *processName)
{
    char cmd[100] = "pkill ";
    strcat(cmd, processName);
    return (system(cmd));
}

int CommandManager::xset_dpms()
{
    char cmd[100] = "xset -dpms";
    return system(cmd);
}

int CommandManager::xset_soff()
{
    char cmd[100] = "xset s off";
    return system(cmd);
}

/**
 * @brief CommandManager::get_interfaces_from_xrandr -- create a list of available
 *      monitors with supported resolutions.
 * @return a list of monitors.
 */
vector<xrandrMonitor> CommandManager::get_interfaces_from_xrandr()
{
    vector<string> data = run_xrandr();
    vector<xrandrMonitor> result;
    regex r1("^(.*) connected.*");
    regex r2("^([0-9]+x[0-9]+).*");
    smatch sm;
    int state = 0;
    xrandrMonitor currentMonitor;

    for (uint32_t idx = 0; idx < data.size();)
    {
        qInfo(command_manager_category()) << "line: " << data[idx].c_str();
        if (data[idx].length() == 0)
        {
            idx++;
            continue;
        }

        switch (state)
        {
        case 0:
            if (regex_match(data[idx], sm, r1))
            {
                currentMonitor.interface = sm[1];
                state = 1;
                qInfo(command_manager_category()) << "[state 0] -> [state 1]";
                qInfo(command_manager_category()) << string(sm[1]).c_str();
            }
            idx++;
            break;
        case 1:
            if (regex_match(data[idx], sm, r2))
            {
                currentMonitor.resolutions.push_back(sm[1]);
                idx++;
                qInfo(command_manager_category()) << string(sm[1]).c_str();
            }
            else
            {
                result.push_back(currentMonitor);
                currentMonitor.interface = "";
                currentMonitor.resolutions.clear();
                state = 0;
                qInfo(command_manager_category()) << "[state 1] -> [state 0]";
            }
            break;
        }
    }

    if (currentMonitor.interface != "")
        result.push_back(currentMonitor);

    return result;
}

/**
 * @brief CommandManager::get_devices_from_ls -- divide a list of devices
 *          by categories.
 * @param mice -- Output list of mice.
 * @param keybds -- Output list of keyboards.
 */
void CommandManager::get_devices_from_ls(vector<string>& mice, vector<string>& keybds)
{
    vector<string> data = run_ls_devices();
    regex r1("^(.*-event-kbd)$");
    regex r2("^(.*-event-mouse)$");
    smatch sm;

    for (string line : data)
    {
        if (line.length() == 0)
            continue;

        if(regex_match(line, sm, r1))
        {
            keybds.push_back(sm[1]);
        }

        if(regex_match(line, sm, r2))
        {
            mice.push_back(sm[1]);
        }
    }
}

void CommandManager::reboot_autoboot()
{
    sync();
    setuid(0);
    reboot(RB_AUTOBOOT);
}
