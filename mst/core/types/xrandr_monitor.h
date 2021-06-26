#ifndef XRANDR_MONITOR_H
#define XRANDR_MONITOR_H

#include <QString>
#include <QVector>

class XRandr_monitor
{
public:
    XRandr_monitor();
    QString interface;
    QVector<QString> resolutions;
};

#endif // XRANDR_MONITOR_H
