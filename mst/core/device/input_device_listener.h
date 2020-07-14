#ifndef INPUT_DEVICE_LISTENER_H
#define INPUT_DEVICE_LISTENER_H

#include <QObject>

#include "device_listener.h"

class Input_device_listener: public Device_listener
{
    Q_OBJECT

public:
    Input_device_listener(DEVICE_TYPE type, QVector<QString> devices);

private:
    QVector<QString> *devices;
    QString poll();
    bool loop_answer_device(QString device);
};

#endif // INPUT_DEVICE_LISTENER_H
