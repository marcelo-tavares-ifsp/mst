#include "install_window/installwindow.h"
#include <QApplication>
#include <QFile>
#include <QDir>
#include <QScopedPointer>
#include <QTextStream>
#include <QDateTime>
#include <QLoggingCategory>
#include <unistd.h>

#include "template_manager/template_manager.h"

// Умный указатель на файл логирования
QScopedPointer<QFile>   m_logFile;

const string MST_CONFIG_FILE = "/etc/mst";

// Объявляение обработчика
void messageHandler(QtMsgType type, const QMessageLogContext &context, const QString &msg);

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    DSV config(MST_CONFIG_FILE);
    PathManager::get_instance()->set_config(&config);
    Template_manager::get_instance()->set_template_dir("/var/lib/mst/");
    if (! QDir("logs").exists()) {
        QDir().mkdir("logs");
    }
    if (geteuid() != 0)
    {
        QMessageBox messageBox;
        messageBox.critical(0, "Ошибка",
                            "MST должен быть запущен от суперпользователя");
        return 1;

    }

    // Устанавливаем файл логирования,
    // внимательно сверьтесь с тем, какой используете путь для файла
    m_logFile.reset(new QFile("/tmp/logFile.txt"));
    // Открываем файл логирования
    m_logFile.data()->open(QFile::Append | QFile::Text);
    // Устанавливаем обработчик
    qInstallMessageHandler(messageHandler);

    InstallWindow w;
    w.show();

    return a.exec();
}

// Реализация обработчика
void messageHandler(QtMsgType type, const QMessageLogContext &context, const QString &msg)
{
    if (! QDir("logs").exists()) {
        QDir().mkdir("logs");
    }

    // Открываем поток записи в файл
    QTextStream out(m_logFile.data());
    // Записываем дату записи
    out << QDateTime::currentDateTime().toString("yyyy-MM-dd hh:mm:ss.zzz ");
    // По типу определяем, к какому уровню относится сообщение
    switch (type)
    {
    case QtInfoMsg:     out << "INF "; break;
    case QtDebugMsg:    out << "DBG "; break;
    case QtWarningMsg:  out << "WRN "; break;
    case QtCriticalMsg: out << "CRT "; break;
    case QtFatalMsg:    out << "FTL "; break;
    }
    // Записываем в вывод категорию сообщения и само сообщение
    out << context.category << ": " << msg << endl;
    out.flush();    // Очищаем буферизированные данные
}
