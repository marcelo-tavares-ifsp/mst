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
