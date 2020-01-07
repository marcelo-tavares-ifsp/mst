#include "ui/install_window/installwindow.h"
#include <QApplication>
#include <QFile>
#include <QDir>
#include <QScopedPointer>
#include <QTextStream>
#include <QDateTime>
#include <QLoggingCategory>
#include <unistd.h>

#include "template_manager/template_manager.h"

/**
 * @brief m_logFile -- A pointer to logging file.
 */
QScopedPointer<QFile>   m_logFile;

/**
 * @brief MST_CONFIG_FILE -- The default MST configuration file.
 */
const string MST_CONFIG_FILE = "/etc/mst";

/**
 * @brief MST_LOG_FILE -- The default MST logging file.
 */
const QString MST_LOG_FILE   = "/var/log/mst.log";

void messageHandler(QtMsgType type, const QMessageLogContext &context,
                    const QString &msg);

/**
 * @brief main -- The application entry point.
 * @param argc -- Count of program arguments.
 * @param argv -- Array of program arguments.
 * @return A result of execution (0 means OK, non-zero value means that some
 *     error occured.)
 */
int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    DSV config(MST_CONFIG_FILE);
    PathManager::get_instance()->set_config(&config);
    Template_manager::get_instance()->set_template_dir("/var/lib/mst/");

    if (geteuid() != 0)
    {
        QMessageBox messageBox;
        messageBox.critical(0, "Ошибка",
                            "MST должен быть запущен от суперпользователя");
        return 1;
    }


    m_logFile.reset(new QFile(MST_LOG_FILE));
    m_logFile.data()->open(QFile::Append | QFile::Text);
    qInstallMessageHandler(messageHandler);

    InstallWindow w;
    w.show();

    return a.exec();
}

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
