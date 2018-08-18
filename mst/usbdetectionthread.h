#ifndef USBDETECTIONTHREAD_H
#define USBDETECTIONTHREAD_H

#include <QThread>
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(usb_detection_thread)

class UsbDetectionThread : public QThread
{
    Q_OBJECT

    void run() override;

signals:
    void usb_path_found(QString, int);
};

#endif // USBDETECTIONTHREAD_H
