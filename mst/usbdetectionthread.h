#ifndef USBDETECTIONTHREAD_H
#define USBDETECTIONTHREAD_H

#include <QThread>

class UsbDetectionThread : public QThread
{
    Q_OBJECT

    void run() override;

signals:
    void usb_path_found(QString, int);
};

#endif // USBDETECTIONTHREAD_H
