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

/**
 * @brief The Platform_exception class This class describes an
 *     exception that can be thrown by Platform methods.
 */
class Platform_exception: public runtime_error {
public:
    Platform_exception(const QString& what)
        : runtime_error(what.toStdString()) {
        // Do nothing.
    }
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

    static void fs_mkdir(const QString& path);
    static void fs_mkdir(const string& path);
    static void fs_rm(const QString& path);
    static void fs_cp(const QString& src, const QString& dst);

    static void system_set_default_runlevel(const QString& target);

    static void exec(const QString& command);
};

#endif // PLATFORM_H
