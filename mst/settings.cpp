#include <QLoggingCategory>
#include "settings.h"

Q_LOGGING_CATEGORY(settings_category, "mst.settings")

Settings_mst::Settings_mst() {}

vector<string> Settings_mst::run_xrandr()
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
        qCritical(settings_category) << "Could not execute xrandr";
        throw "Could not execute xrandr";
    }
}

/**
 * @brief Settings_mst::parse_xrandr -- create a list of available
 *      monitors with supported resolutions.
 * @return a list of monitors.
 */
vector<Xrandr_monitor> Settings_mst::parse_xrandr()
{
    vector<string> data = run_xrandr();
    vector<Xrandr_monitor> result;
    regex r1("^(.*) connected.*");
    regex r2("^([0-9]+x[0-9]+).*");
    int state = 0;
    Xrandr_monitor monitor;
    smatch sm;

    for (uint32_t idx = 0; idx < data.size();)
    {
        qDebug(settings_category) << "line: " << data[idx].c_str();
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
                monitor.interface = sm[1];
                state = 1;
                qDebug(settings_category) << "[state 0] -> [state 1]";
                qDebug(settings_category) << string(sm[1]).c_str();
            }
            idx++;
            break;
        case 1:
            if (regex_match(data[idx], sm, r2))
            {
                monitor.resolutions.push_back(sm[1]);
                idx++;
                qDebug(settings_category) << string(sm[1]).c_str();
            }
            else
            {
                result.push_back(monitor);
                monitor.interface = "";
                monitor.resolutions.clear();
                state = 0;
                qDebug(settings_category) << "[state 1] -> [state 0]";
            }
            break;
        }
    }

    if (monitor.interface != "")
        result.push_back(monitor);

    return result;
}

/**
 * @brief Settings_mst::run_ls_devices -- get a list of input
 *      devices.
 * @return a list of devices.
 */
vector<string> Settings_mst::run_ls_devices()
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
        qCritical(settings_category)
                << "Could not execute ls /dev/input/by-path/";
        throw "Could not execute ls /dev/input/by-path/";
    }
}

/**
 * @brief Settings_mst::parse_ls_devices -- divide a list of devices
 *          by categories.
 * @param mice -- Output list of mice.
 * @param keybds -- Output list of keyboards.
 */
void Settings_mst::parse_ls_devices(vector<string> *mice, vector<string> *keybds)
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
            keybds->push_back(sm[1]);
        }

        if(regex_match(line, sm, r2))
        {
            mice->push_back(sm[1]);
        }
    }
}

/**
 * @brief open_input_dev -- Open an input device file.
 * @param name -- Name of the input device file;
 * @return Opened file in the read-only mode.
 */
static FILE* open_input_dev(string name) {
    string str = "cat '/dev/input/by-path/" + name + "'";
    static const char *COMMAND = str.c_str();
    FILE* file = popen(COMMAND, "r");
    if (file == NULL) {
        qCritical(settings_category)
                << "Could not execute 'cat /dev/input/by-path/"
                << name.c_str() << "'";
        throw "Could not execute 'cat /dev/input/by-path/" + name + "'";
    }

    return file;
}

