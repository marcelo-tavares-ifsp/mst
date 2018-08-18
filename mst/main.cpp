/* main.cpp -- MST entry point.
 *
 * Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 * Copyright (C) 2018 Anton Plekhanov <plehunov.anton_9@mail.ru>
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
    QLoggingCategory::setFilterRules("*.debug=true\n"
                                     "qt.qpa.input.events=false\n"
                                     "qt.widgets.gestures=false\n"
                                     "qt.qpa.input=false");
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
