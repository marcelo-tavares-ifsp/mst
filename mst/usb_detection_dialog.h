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

#ifndef USB_DETECTION_DIALOG_H
#define USB_DETECTION_DIALOG_H

#include <QDialog>
#include "usbdetectionthread.h"

namespace Ui {
class Usb_detection_dialog;
}

class Usb_detection_dialog : public QDialog
{
    Q_OBJECT

public:
    explicit Usb_detection_dialog(QWidget *parent = 0);
    ~Usb_detection_dialog();

private slots:
    void on_pushButton_clicked();
    void usb_path_found();

private:
    Ui::Usb_detection_dialog *ui;
    UsbDetectionThread *usb_detector_tread;
};

#endif // USB_DETECTION_DIALOG_H
