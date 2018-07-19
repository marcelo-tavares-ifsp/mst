#include <QApplication>
#include <QTextCodec>

#include "mainwindow.h"
#include "dsv.h"
#include "config.h"

using namespace std;



int main(int argc, char *argv[])
{
    string cmd = "mkdir -p " + Config::get_instance()->get_output_dir();
    if (system(cmd.c_str()))
    {
        throw "Could not make an output directory: " + cmd;
    }
    //QTextCodec::setCodecForCStrings(QTextCodec::codecForName("UTF-8"));
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
    //QTextCodec::setCodecForTr(QTextCodec::codecForName("UTF-8"));

    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    return a.exec();

    cout << "Main The end!" << endl;
}
