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
