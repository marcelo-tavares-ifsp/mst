#include <QApplication>
#include <QTextCodec>
#include <QDebug>
#include <QLoggingCategory>

#include "mainwindow.h"
#include "dsv.h"
#include "config.h"

using namespace std;

int main(int argc, char *argv[])
{
    QLoggingCategory::setFilterRules("*.debug=true");
    qSetMessagePattern("%{time} [%{category} %{type}] "
                       "%{function}:%{line}: %{message}");
    string cmd = "mkdir -p " + Config::get_instance()->get_output_dir();
    if (system(cmd.c_str()))
    {
        string msg = "Could not make an output directory: " + cmd;
        qFatal(msg.c_str());
        throw msg;
    }
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    return a.exec();
}
