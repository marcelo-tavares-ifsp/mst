/* main.cpp -- This file contains the MST entry point.
 *
 * Copyright (C) 2018-2020 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2018-2020 Artyom V. Poptsov <a@gkaz.ru>
 * Copyright (C) 2018-2019 Anton Plekhanov <plehunov.anton9@gmail.com>
 * Copyright (C) 2019 Daniil Zemlyanoy <lfybssd@gmail.com>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "ui/install_window/installwindow.h"
#include <QApplication>
#include <QFile>
#include <QDir>
#include <QScopedPointer>
#include <QTextStream>
#include <QDateTime>
#include <QLoggingCategory>
#include <unistd.h>
#include <QDebug>
#include <QFile>
#include <QString>
#include <QTextStream>
#include <QCommandLineParser>
#include <QTranslator>

#include "config.h"
#include "core/template_manager.h"
#include "core/platform.h"

using namespace std;

/**
 * @brief m_logFile -- A pointer to logging file.
 */
QScopedPointer<QFile>   m_logFile;

/**
 * @brief MST_CONFIG_FILE -- The default MST configuration file.
 */
const QString MST_CONFIG_FILE = "/etc/mst";

const QString MST_SEATS_CONFIG_FILE = "/etc/mst-seats";

/**
 * @brief MST_LOG_FILE -- The default MST logging file.
 */
const QString MST_LOG_FILE   = "/var/log/mst.log";

/**
 * @brief MST_TEMPLTE_DIR -- Template directory path.
 */
const QString MST_TEMPLATE_DIR = INSTALLATION_PREFIX "/var/lib/mst/";

/**
 * @brief messageHandler -- The MST default logging handler.
 * @param type -- A log message type.
 * @param context -- A log message context.
 * @param msg -- A log message.
 */
void messageHandler(QtMsgType type, const QMessageLogContext &context,
                    const QString &msg)
{
    QTextStream out(m_logFile.data());
    out << QDateTime::currentDateTime().toString("yyyy-MM-dd hh:mm:ss.zzz ");

    switch (type)
    {
    case QtInfoMsg:     out << "INF "; break;
    case QtDebugMsg:    out << "DBG "; break;
    case QtWarningMsg:  out << "WRN "; break;
    case QtCriticalMsg: out << "CRT "; break;
    case QtFatalMsg:    out << "FTL "; break;
    }

    out << context.category << ": " << msg << Qt::endl;
    out.flush();
}

void create_config_file(QFile& file) {
    if (! file.open(QIODevice::WriteOnly | QIODevice::Text))
            return;
    QTextStream out(&file);
    out << "user:multiseat";
}

/**
 * @brief list_backups -- Print the list of backups to the stdout.
 * @param mst -- A MST instance to use.
 */
static void list_backups(const MST* mst) {
    cout << "Backup directory: "
         << mst->get_backup_directory().toStdString()
         << endl;
    for (QString s : mst->list_backups()) {
        if (mst->list_backups().first() == s) {
            cout << "* ";
        } else {
            cout << "  ";
        }
        cout << s.toStdString() << endl;
    }
}

/**
 * @brief main -- The application entry point.
 * @param argc -- Count of program arguments.
 * @param argv -- Array of program arguments.
 * @return A result of execution (0 means OK, non-zero value means that some
 *     error occured.)
 */
int main(int argc, char *argv[])
{
    m_logFile.reset(new QFile(MST_LOG_FILE));
    m_logFile.data()->open(QFile::Append | QFile::Text);
    qInstallMessageHandler(messageHandler);

    QSharedPointer<QCoreApplication> a;
    bool is_graphic_mode = platform::is_graphics_available();

    if (is_graphic_mode) {
        a.reset(new QApplication(argc, argv));
    } else {
        a.reset(new QCoreApplication(argc, argv));
    }

    QTranslator translator;
    bool ok = translator.load(":/i18n/mst_" + QLocale().system().name());
    if (ok) {
        a->installTranslator(&translator);
   } else {
        cerr << "ERROR: Could not load translations" << endl;
    }

    QCommandLineParser parser;
    parser.setApplicationDescription("Multiseat configurator");
    parser.addHelpOption();
    parser.addVersionOption();
    QCommandLineOption list_backups_option(
                QStringList() << "list-backups",
                QCoreApplication::translate("main",
                                           "Show the list of system backups"));
    QCommandLineOption rollback_option(
                QStringList() << "R" << "rollback",
                QCoreApplication::translate("main",
                                           "Rollback changes in the system"));
    QCommandLineOption start_option(
                QStringList() << "s" << "start",
                QCoreApplication::translate("main",
                                            "Start mstd."));
    QCommandLineOption stop_option(
                QStringList() << "S" << "stop",
                QCoreApplication::translate("main",
                                            "Stop mstd."));
    QCommandLineOption status_option(
                QStringList() << "status",
                QCoreApplication::translate("main",
                                            "Show MST status"));
    QCommandLineOption debug_allow_empty_devices(
                QStringList() << "debug-allow-empty-devices",
                QCoreApplication::translate("main",
                                            "Allow empty devices"));
    QCommandLineOption debug_allow_device_collisions(
                QStringList() << "debug-allow-device-collisions",
                QCoreApplication::translate("main",
                                            "Allow device collisions"));
    QCommandLineOption debug_allow_missing_components(
                QStringList() << "debug-allow-missing-components",
                QCoreApplication::translate("main",
                                            "Allow missing components."));
    QCommandLineOption list_input_devices(
                QStringList() << "list-input-devices",
                QCoreApplication::translate("main",
                                            "Print a list input devices."));
    QCommandLineOption list_components(
                QStringList() << "list-components",
                QCoreApplication::translate("main",
                                            "Print the list of components."));

    parser.addOption(list_backups_option);
    parser.addOption(list_input_devices);
    parser.addOption(list_components);
    parser.addOption(rollback_option);
    parser.addOption(start_option);
    parser.addOption(stop_option);
    parser.addOption(status_option);
    parser.addOption(debug_allow_empty_devices);
    parser.addOption(debug_allow_device_collisions);
     parser.addOption(debug_allow_missing_components);
    parser.process(*a);

    if (parser.isSet(list_input_devices)) {
        for (QString dev : platform::get_input_devices()) {
            cout << dev.toStdString() << endl;
        }
        return 0;
    }

    Configuration config;
    config.load(MST_CONFIG_FILE, MST_SEATS_CONFIG_FILE);
    Template_manager::get_instance()->set_template_dir(MST_TEMPLATE_DIR);

    if (parser.isSet(debug_allow_missing_components)) {
        config.allow_missing_components(true);
    }

    if (geteuid() != 0)
    {
        if (is_graphic_mode) {
            QMessageBox messageBox;
            messageBox.critical(
                        0,
                        QCoreApplication::translate("main",
                                                    "Error"),
                        QCoreApplication::translate(
                            "main",
                            "MST must be run as a superuser"));
        } else {
            cerr << QCoreApplication::translate(
                        "main",
                        "MST must be run as a superuser").toStdString()
                 << endl;
        }
        return 1;
    }

    MST* mst = MST::get_instance();
    mst->set_configuration(config);

    if (parser.isSet(status_option)) {
        cout << "mstd: " << (mst->running_p() ? "running" : "stopped")
             << endl;
        int32_t seat_count = mst->get_seats().size();
        if (seat_count > 0) {
            cout << seat_count << " seat(s):" << endl;
            for (auto seat : mst->get_seats()) {
                cout << "  " << *seat << endl;
            }
        }
        return 0;
    }

    if (parser.isSet(start_option)) {
        mst->start();
        return 0;
    }

    if (parser.isSet(stop_option)) {
        mst->stop();
        return 0;
    }

    if (parser.isSet(list_backups_option)) {
        list_backups(mst);
        return 0;
    }

    if (parser.isSet(list_components)) {
	cout << "xrandr:           " << PATH_TO_XRANDR << endl
	     << "pgrep:            " << PATH_TO_PGREP << endl
	     << "pkill:            " << PATH_TO_PKILL << endl
	     << "xset:             " << PATH_TO_XSET << endl
	     << "systemctl:        " << PATH_TO_SYSTEMCTL << endl
	     << "awesome:          " << PATH_TO_AWESOME << endl
	     << "lightdm:          " << PATH_TO_LIGHTDM << endl
	     << "dm-tool:          " << PATH_TO_DM_TOOL << endl
	     << "vgclient:         " << PATH_TO_VGLCLIENT << endl
	     << "vglserver_config: " << PATH_TO_VGLSERVER_CONFIG << endl
	     << "X:                " << PATH_TO_X << endl
	     << "Xephyr:           " << PATH_TO_XEPHYR << endl
	     << "bash:             " << PATH_TO_BASH << endl;
	return 0;
    }

    if (parser.isSet(debug_allow_device_collisions)) {
        config.allow_device_collisions(true);
    }

    if (parser.isSet(debug_allow_empty_devices)) {
        config.allow_empty_devices(true);
    }

    if (parser.isSet(rollback_option)) {
        mst->disable();
        return 0;
    }

    if (is_graphic_mode) {
        char* display = getenv("DISPLAY");
        cerr << "Starting on display " << display << " ..." << endl;
        InstallWindow w;
        w.show();
        return a->exec();
    } else {
        cerr << QCoreApplication::translate(
                    "main",
                    "Currently MST does not support console mode.")
                .toStdString()
             << endl;
        return 1;
    }
}
