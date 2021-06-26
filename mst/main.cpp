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

#include "core/template_manager.h"

using namespace std;

/**
 * @brief m_logFile -- A pointer to logging file.
 */
QScopedPointer<QFile>   m_logFile;

/**
 * @brief MST_CONFIG_FILE -- The default MST configuration file.
 */
const QString MST_CONFIG_FILE = "/etc/mst";

/**
 * @brief MST_LOG_FILE -- The default MST logging file.
 */
const QString MST_LOG_FILE   = "/var/log/mst.log";

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

    out << context.category << ": " << msg << endl;
    out.flush();
}

void create_config_file(QFile& file) {
    if (! file.open(QIODevice::WriteOnly | QIODevice::Text))
            return;
    QTextStream out(&file);
    out << "user:multiseat";
}

/**
 * @brief is_graphics_available -- check if the program is run in the graphic
 *     mode.
 * @return true if it is, false otherwise.
 */
bool is_graphics_available()
{
    return system("xset -q > /dev/null 2>&1") == 0;
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
    QSharedPointer<QCoreApplication> a;
    bool is_graphic_mode = is_graphics_available();

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
    QCommandLineOption stop_option(
                QStringList() << "S" << "stop",
                QCoreApplication::translate("main",
                                            "Stop MST."));
    QCommandLineOption status_option(
                QStringList() << "s" << "status",
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

    parser.addOption(list_backups_option);
    parser.addOption(rollback_option);
    parser.addOption(stop_option);
    parser.addOption(status_option);
    parser.addOption(debug_allow_empty_devices);
    parser.addOption(debug_allow_device_collisions);
    parser.process(*a);
    Configuration config;
    config.load(MST_CONFIG_FILE);
    Template_manager::get_instance()->set_template_dir("/var/lib/mst/");

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


    m_logFile.reset(new QFile(MST_LOG_FILE));
    m_logFile.data()->open(QFile::Append | QFile::Text);
    qInstallMessageHandler(messageHandler);
    MST* mst = MST::get_instance();
    mst->set_configuration(config);

    if (parser.isSet(status_option)) {
        cout << "mstd: " << (mst->running_p() ? "running" : "stopped")
             << endl;
        return 0;
    }

    if (parser.isSet(stop_option)) {
        mst->stop();
        return 0;
    }

    if (parser.isSet(list_backups_option)) {
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
