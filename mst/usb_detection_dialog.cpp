/* usb_detection_dialog.cpp -- USB detection dialog.
 *
 * Copyright (C) 2018 Mikhail Shcherbakov <whfrthmo@gmail.com>
 * Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include "usb_detection_dialog.h"
#include "ui_usb_detection_dialog.h"
#include <QThread>
#include <iostream>

Usb_detection_dialog::Usb_detection_dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Usb_detection_dialog)
{
    ui->setupUi(this);

    usb_detector_tread = new UsbDetectionThread();

    connect(usb_detector_tread, SIGNAL(usb_path_found(QString, int)), parent, SLOT(set_seat_device(QString,int)));
    connect(usb_detector_tread, SIGNAL(usb_path_found(QString, int)), this, SLOT(usb_path_found()));

    usb_detector_tread->start();
}

Usb_detection_dialog::~Usb_detection_dialog()
{
    delete ui;
    /*if (usb_detector_tread != NULL &&
            !usb_detector_tread->isFinished())
    {
        usb_detector_tread->terminate();
    }*/

    //delete usb_detector_tread;
}

void Usb_detection_dialog::on_pushButton_clicked()
{
    this->close();
}

void Usb_detection_dialog::usb_path_found()
{
    this->close();
}
