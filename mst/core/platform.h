#ifndef PLATFORM_H
#define PLATFORM_H

#include <vector>
#include <QString>
#include <QLoggingCategory>
#include "types/xrandr_monitor.h"

Q_DECLARE_LOGGING_CATEGORY(platform_category)

/**
 * @brief The Platform_exception class This class describes an
 *     exception that can be thrown by Platform methods.
 */
class Platform_exception: public std::runtime_error {
public:
    Platform_exception(const QString& what)
        : std::runtime_error(what.toStdString()) {
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
    static std::vector<XRandr_monitor> xrandr_get_monitors();
    static void get_input_devices(QVector<QString> &mice,
                                  QVector<QString> &keybds);

    static bool pam_is_mkhomedir_used();

    static void fs_mkdir(const QString& path);
    static void fs_mkdir(const std::string& path);
    static void fs_rm(const QString& path);
    static void fs_cp(const QString& src, const QString& dst);

    static void system_set_default_runlevel(const QString& target);

    static int exec(const QString& command);

    static struct passwd* getpwnam(const QString& name);
    static void chown(const QString& path, uid_t uid, gid_t gid,
	bool is_recursive = false);
};

namespace platform {

QVector<QString> popen_read(QString command);
QVector<QString> run_xrandr();
QVector<QString> run_ls_devices();

}

#endif // PLATFORM_H
