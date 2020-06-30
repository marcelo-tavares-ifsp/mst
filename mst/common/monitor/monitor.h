#ifndef MONITOR_H
#define MONITOR_H

#include <QString>
#include <QVector>

#include "../resolution/resolution.h"
#include "../xrandr_monitor/xrandr_monitor.h"

using namespace std;

class Monitor
{
public:
    Monitor();
    Monitor(XRandr_monitor& xrandr_monitor);
    Monitor(QString interface, QVector<Resolution>& resolutions);
    bool is_enabled() const;
    QString get_interface() const;
    QVector<Resolution> get_available_resolutions() const;
    Resolution get_current_resolution() const;

    void set_enabled(bool is_enabled);
    void set_resolution(int index);

private:
    int current_resolution = 0;
    bool state = false;
    QString interface;
    QVector<Resolution> resolutions;
};

#endif // MONITOR_H
