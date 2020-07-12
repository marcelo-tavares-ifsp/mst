#include "platform.h"

#include <unistd.h>
#include <linux/reboot.h>
#include <sys/reboot.h>
#include <QLoggingCategory>
#include <QString>

#include "core/xrandr_monitor/xrandr_monitor.h"
#include "core/utilites/utilites.h"

Q_LOGGING_CATEGORY(platform_category, "mst.platform")

Platform::Platform()
{

}

//// Helper procedures.

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
        qCritical(platform_category) << "Could not execute xrandr";
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
        qCritical(platform_category)
                << "Could not execute ls /dev/input/by-path/";
        throw "Could not execute ls /dev/input/by-path/";
    }
}

//// Methods.

bool Platform::process_is_running(const QString& process_name)
{
    QString command = "pgrep -c " + process_name;
    return (system(command.toStdString().c_str()) == 0);
}

bool Platform::process_kill(const QString& process_name)
{
    QString command = "pkill " + process_name;
    return system(command.toStdString().c_str());
}

int Platform::xset_dpms()
{
    char cmd[100] = "xset -dpms";
    return system(cmd);
}

int Platform::xset_soff()
{
    char cmd[100] = "xset s off";
    return system(cmd);
}

/**
 * @brief Platform::xrandr_get_monitors -- create a list of available
 *      monitors with supported resolutions.
 * @return a list of monitors.
 */
vector<XRandr_monitor> Platform::xrandr_get_monitors()
{
    vector<string> data = run_xrandr();
    vector<XRandr_monitor> result;
    regex r1("^(.*) connected.*");
    regex r2("^([0-9]+x[0-9]+).*");
    smatch sm;
    int state = 0;
    XRandr_monitor currentMonitor;

    for (uint32_t idx = 0; idx < data.size();)
    {
        qInfo(platform_category) << "line: " << data[idx].c_str();
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
                qInfo(platform_category) << "[state 0] -> [state 1]";
                qInfo(platform_category) << string(sm[1]).c_str();
            }
            idx++;
            break;
        case 1:
            if (regex_match(data[idx], sm, r2))
            {
                currentMonitor.resolutions.push_back(sm[1]);
                idx++;
                qInfo(platform_category) << string(sm[1]).c_str();
            }
            else
            {
                result.push_back(currentMonitor);
                currentMonitor.interface = "";
                currentMonitor.resolutions.clear();
                state = 0;
                qInfo(platform_category) << "[state 1] -> [state 0]";
            }
            break;
        }
    }

    if (currentMonitor.interface != "")
        result.push_back(currentMonitor);

    return result;
}

/**
 * @brief Platform::get_input_devices -- divide a list of devices
 *          by categories.
 * @param mice -- Output list of mice.
 * @param keybds -- Output list of keyboards.
 */
void Platform::get_input_devices(vector<string>& mice, vector<string>& keybds)
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

/**
 * @brief is_pam_mkhomedir_used -- check if a PAM module 'pam_mkhomedir' is
 *      used (that is, whether or not '/etc/skel' is copied to users home
 *      directories.)
 * @return 'true' if the PAM module is used, 'false' otherwise.
 */
bool Platform::pam_is_mkhomedir_used()
{
    QString cmd = "[ -d /etc/skel ] && [ $(ls -a1 /etc/skel | wc -l) -gt 2 ]";
    return Platform::exec(cmd) != 0;
}

/**
 * @brief Platform::fs_mkdir -- Create a directory with all needed
 *     subdirectories.
 * @param path -- A path to create.
 */
void Platform::fs_mkdir(const QString& path)
{
    if (Platform::exec("mkdir -p '" + path + "'") != 0)
    {
        qCritical(platform_category)
             << "Could not create a directory: "
             << path.toStdString().c_str();
        throw Platform_exception("Could not create a directory: '"
                                 + path + "'");
    }
}

/**
 * @brief Platform::fs_mkdir -- Create a directory with all needed
 *     subdirectories.
 * @param path -- A path to create.
 */
void Platform::fs_mkdir(const string& path)
{
    Platform::fs_mkdir(QString::fromStdString(path));
}

/**
 * @brief Platform::fs_rm -- Remove a file.
 * @param file -- A file to remove.
 * @throws Platform_exception on an error.
 */
void Platform::fs_rm(const QString& file)
{
    if (Platform::exec("rm '" + file + "'") != 0)
    {
        QString message = "Could not delete '" + file + "'";
        qCritical(platform_category) << message.toStdString().c_str();
        throw Platform_exception(message);
    }
}

/**
 * @brief Platform::fs_cp -- Copy a file SRC to a destination DST.
 * @param src -- Source file to copy.
 * @param dst -- Destination for copying.
 * @throws Platform_exception on an error.
 */
void Platform::fs_cp(const QString &src, const QString &dst)
{
    if (Platform::exec("cp '" + src + "' '" + dst + "'") != 0)
    {
        QString message = "Could not copy: '" + src + "' -> '" + dst + "'";
        throw Platform_exception(message);
    }
}

/**
 * @brief Platform::system_set_default_runlevel -- Set default runlevel for the
 *     system.
 * @param target -- A runlevel to set (without ".target" suffix.)
 * @throws Platform_exception on an error.
 */
void Platform::system_set_default_runlevel(const QString& target)
{
    if (Platform::exec("systemctl set-default " + target + ".target") != 0) {
        QString msg = "Could not set default target: " + target;
        qCritical(platform_category) << msg.toStdString().c_str();
        throw Platform_exception(msg);
    }
}

/**
 * @brief Platform::exec -- Execute a command.
 * @param command -- A command to execute.
 * @returns return code of the command.
 */
int Platform::exec(const QString &command)
{
    return system(command.toStdString().c_str());
}

/**
 * @brief Platform::system_reboot -- Reboot the system.
 */
void Platform::system_reboot()
{
    sync();
    if (setuid(0) != 0) {
        throw Platform_exception("Cound not set UID");
    }
    reboot(RB_AUTOBOOT);
}
