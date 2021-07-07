#ifndef PLATFORM_H
#define PLATFORM_H

#include <vector>
#include <QString>
#include <QLoggingCategory>
#include <QProcess>
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

/**
 * @brief The Platform class contains platform-specific tools.
 */
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

    /**
     * @brief system_service_active_p -- Check if @a service is active.
     * @param service_name -- A service to check.
     * @return True if the service is active, false if not.
     */
    static bool system_service_active_p(const QString& service_name);

    static void system_enable_service(const QString& service_name);
    static void system_disable_service(const QString& service_name);

    /**
     * @brief system_release -- Get the OS release string.
     * @return the system release string or nullptr if the system release
     *     cannot be properly detected.
     */
    static QString system_release();

    /**
     * @brief system_stop_service -- Stop a service.
     * @param service_name
     */
    static void system_stop_service(const QString& service_name);
    /**
     * @brief system_start_service -- Start a service.
     * @param service_name
     */
    static void system_start_service(const QString& service_name);

    static int exec(const QString& command);

    static struct passwd* getpwnam(const QString& name);
    static void chown(const QString& path, uid_t uid, gid_t gid,
	bool is_recursive = false);
};

namespace platform {

/**
 * @brief INPUT_DEVICES_PATH -- Path to search input devices.
 */
extern const QString& INPUT_DEVICES_PATH;

/**
 * @brief platform::popen_read -- execute a program and read output from it.
 * @param program -- a program to execute.
 * @param arguments -- arguments of a program.
 * @param channel -- ProcessChannel to read (StandardOutput by default.)
 * @return QVector of output lines.
 */
QVector<QString> popen_read(const QString& program,
                            const QStringList& arguments,
                            QProcess::ProcessChannel channel = QProcess::StandardOutput);

/**
 * @brief run_xrandr -- Return the output of 'xrandr' command.
 * @return QVector of strings.
 * @throws Platform_exception
 */
QVector<QString> run_xrandr();

/**
 * @brief run_ls_devices -- get a list of input
 *      devices.
 * @return a list of devices.
 * @throws Platform_exception
 */
QVector<QString> run_ls_devices();

}

#endif // PLATFORM_H
