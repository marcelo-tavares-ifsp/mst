#include "platform.h"

#include <unistd.h>
#include <linux/reboot.h>
#include <sys/reboot.h>
#include <pwd.h> // getpwnam
#include <QLoggingCategory>
#include <QString>
#include <QRegularExpression>
#include <QRegularExpressionMatch>
#include <QFileInfo>
#include <QDir>
#include <QProcess>

#include "platform.h"
#include "core/types/xrandr_monitor.h"
#include "core/utilites/utilites.h"

using namespace std;
using namespace platform;

Q_LOGGING_CATEGORY(platform_category, "mst.core.platform")

const QString& platform::INPUT_DEVICES_PATH = "/dev/input/by-path/";

Platform::Platform()
{

}

//// Helper procedures.

QVector<QString> platform::popen_read(const QString& program,
                                      const QStringList& arguments,
                                      QProcess::ProcessChannel channel)
{
    QProcess process;
    process.start(program, arguments, QIODevice::ReadWrite);
    process.setReadChannel(channel);

    if (! process.waitForStarted()) {
        throw Platform_exception("Failed to run '" + program + "': "
                                 + process.errorString());
    }

    if (! process.waitForFinished()) {
        throw Platform_exception("Failed to run '" + program + "': "
                                 + process.errorString());
    }

    QVector<QString> result;
    QString line;

    while ((line = process.readLine()) != nullptr) {
        result.push_back(line.simplified());
    }

    process.close();

    return result;
}

QVector<QString> platform::run_xrandr()
{
    return platform::popen_read("xrandr", QStringList());
}

QVector<QString> platform::get_input_devices()
{
    QDir dir(INPUT_DEVICES_PATH);
    QFileInfoList list = dir.entryInfoList(QDir::NoDotAndDotDot | QDir::Dirs);
    QVector<QString> result;
    foreach (QFileInfo entry_info, list) {
        result.append(entry_info.fileName());
    }
    return result;
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
    QVector<QString> data = run_xrandr();
    vector<XRandr_monitor> result;
    QRegularExpression r1("^(.*) connected.*");
    QRegularExpression r2("^([0-9]+x[0-9]+).*");
    int state = 0;
    XRandr_monitor currentMonitor;
    QRegularExpressionMatch match;

    for (int idx = 0; idx < data.size();) {
        qInfo(platform_category) << "line: " << data[idx];
        if (data[idx].length() == 0) {
            idx++;
            continue;
        }

        switch (state)
        {
        case 0:
            match = r1.match(data[idx]);
            if (match.hasMatch()) {
                currentMonitor.interface = match.captured(1);
                state = 1;
                qInfo(platform_category) << "[state 0] -> [state 1]";
                qInfo(platform_category) << match.captured(1);
            }
            idx++;
            break;
        case 1:
            match = r2.match(data[idx]);
            if (match.hasMatch()) {
                currentMonitor.resolutions.push_back(
                            match.captured(1));
                idx++;
                qInfo(platform_category) << match.captured(1);
            } else {
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

void platform::parse_devices(QVector<QString> devices,
                             QVector<QString>& mice,
                             QVector<QString>& keyboards)
{
    QRegularExpression r1("^(.*-event-kbd)$");
    QRegularExpression r2("^(.*-event-mouse)$");
    QRegularExpressionMatch match;

    for (QString line : devices) {
        if (line.length() == 0)
            continue;
        match = r1.match(line);
        if(match.hasMatch()) {
            keyboards.push_back(match.captured(1));
        }
        match = r2.match(line);
        if(match.hasMatch()) {
            mice.push_back(match.captured(1));
        }
    }
}

/**
 * @brief Platform::get_input_devices -- divide a list of devices
 *          by categories.
 * @param mice -- Output list of mice.
 * @param keybds -- Output list of keyboards.
 */
void Platform::get_input_devices(QVector<QString>& mice,
                                 QVector<QString>& keybds)
{
    QVector<QString> data = platform::get_input_devices();
    parse_devices(data, mice, keybds);
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
    if (! QDir().mkpath(path)) {
        qCritical(platform_category)
                << "Could not create a directory: "
                << path;
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
    if (! QFile::remove(file)) {
        QString message = "Could not delete '" + file + "'";
        qCritical(platform_category) << message;
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
    QFileInfo file(dst);
    if (file.isFile() && file.exists()) {
        qWarning(platform_category).noquote().nospace()
                << "File '" << dst << "' is going to be re-written by '"
                << src << "'";
        QFile::remove(dst);
    }

    qInfo(platform_category).noquote().nospace()
            << "Copying '" << src << "' -> '" << dst << "' ...";
    if (QFile::copy(src, dst) != true)
    {
        QString message = "Could not copy: '" + src + "' -> '" + dst + "'";
        qCritical().noquote() << message;
        throw Platform_exception(message);
    }
    qInfo(platform_category).noquote().nospace()
            << "Copying '" << src << "' -> '" << dst << "' ... done";
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
 * @brief Platform::system_enable_service -- Enable a system service.
 * @param service_name -- The name of a service.
 */
void Platform::system_enable_service(const QString& service_name)
{
    Platform::exec("systemctl enable " + service_name);
}

bool Platform::system_service_active_p(const QString& service_name)
{
    return Platform::exec("systemctl is-active --quiet " + service_name) == 0;
}

/**
 * @brief Platform::system_disable_service -- Disable a system service.
 * @param service_name -- The name of a service.
 */
void Platform::system_disable_service(const QString& service_name)
{
    Platform::exec("systemctl disable " + service_name);
}

void Platform::system_stop_service(const QString& service_name)
{
    Platform::exec("systemctl stop " + service_name);
}

void Platform::system_start_service(const QString& service_name)
{
    Platform::exec("systemctl start " + service_name);
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

QString Platform::system_release()
{
    QFile lsb_release_file("/etc/lsb-release");
    if (lsb_release_file.exists()) {
        // DISTRIB_DESCRIPTION="Ubuntu 20.04.2 LTS"
        QRegExp re("DISTRIB_DESCRIPTION=\"(.*)\"\n");
        lsb_release_file.open(QIODevice::ReadOnly);
        QString line;
        while ((line = lsb_release_file.readLine()) != nullptr) {
            if (re.exactMatch(line)) {
                return re.cap(1);
            }
        }
    }

    QFile system_release_file("/etc/system-release");
    if (system_release_file.exists()) {
        // ALT Linux 9 w/o installed 'lsb_release' command.
        system_release_file.open(QIODevice::ReadOnly);
        QString result = system_release_file.readLine();
        return result.simplified();
    }

    return nullptr;
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

/**
 * @brief Platform::getpwnam -- Get the passwd entry for the given user name.
 * @param name -- Username to get passwd entry for.
 * @return A pointer to a passwd entry.
 */
struct passwd* Platform::getpwnam(const QString& name)
{
    return ::getpwnam(name.toStdString().c_str());
}

/**
 * @brief Platform::chown -- Change owner of a file specified by the path.
 * @param path -- Path to a file.
 * @param uid -- User ID.
 * @param gid -- Group ID.
 * @param is_recursive -- If set to 'true' change owner of the directories
 *     recursively.
 */
void Platform::chown(const QString& path, uid_t uid, gid_t gid,
                     bool is_recursive)
{
    qInfo(platform_category) << "Changing owner of " << path << " ...";
    if (::chown(path.toStdString().c_str(), uid, gid) != 0) {
        throw Platform_exception("Could not apply 'chown' on " + path);
    }

    QFileInfo file_info(path);
    if (file_info.isDir() && is_recursive) {
        QDir root(path);
        root.setFilter(QDir::Files
                       | QDir::AllDirs
                       | QDir::Hidden
                       | QDir::NoDotAndDotDot);
        QFileInfoList list = root.entryInfoList();
        for (QFileInfo finfo : list) {
            chown(finfo.absoluteFilePath(), uid, gid, true);
        }
    }
}

bool platform::is_graphics_available()
{
    return system("xset -q > /dev/null 2>&1") == 0;
}

