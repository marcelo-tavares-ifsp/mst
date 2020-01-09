#ifndef PLATFORM_H
#define PLATFORM_H

#include <vector>
#include <QString>
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(platform_category)

using namespace std;

struct xrandrMonitor
{
    string interface;
    vector<string> resolutions;
};

class Platform
{
public:
    Platform();
    
    static bool process_is_running(const QString& process_name);
    static bool process_kill(const QString& process_name);
    static void system_reboot();
    static int xset_dpms();
    static int xset_soff();
    static vector<xrandrMonitor> xrandr_get_monitors();
    static void get_input_devices(vector<string>& mice, vector<string>& keybds);
};

#endif // PLATFORM_H
