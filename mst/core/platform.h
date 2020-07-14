#ifndef PLATFORM_H
#define PLATFORM_H

#include <vector>
#include <QString>
#include <QLoggingCategory>
#include "types/xrandr_monitor.h"

Q_DECLARE_LOGGING_CATEGORY(platform_category)

using namespace std;

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
    static vector<XRandr_monitor> xrandr_get_monitors();
    static void get_input_devices(QVector<string> &mice,
                                  QVector<string> &keybds);

    static bool pam_is_mkhomedir_used();

    static void fs_mkdir(const QString& path);
    static void fs_mkdir(const string& path);
    static void fs_rm(const QString& path);
    static void fs_cp(const QString& src, const QString& dst);

    static void system_set_default_runlevel(const QString& target);

    static int exec(const QString& command);
};

namespace platform {

QVector<QString> run_xrandr();
vector<string> run_ls_devices();

}

#endif // PLATFORM_H
