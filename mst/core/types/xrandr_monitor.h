#ifndef XRANDR_MONITOR_H
#define XRANDR_MONITOR_H

#include <QString>
#include <QVector>

/**
 * @brief The XRandr_monitor class describes an XRandr monitor.
 */
class XRandr_monitor
{
public:
    XRandr_monitor();

    /**
     * @brief interface -- Interface that is used to connect the monitor.
     */
    QString interface;

    /**
     * @brief resolutions -- Supported monitor resolutions.
     */
    QVector<QString> resolutions;
};

#endif // XRANDR_MONITOR_H
