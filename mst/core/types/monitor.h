#ifndef MONITOR_H
#define MONITOR_H

#include <QString>
#include <QVector>

#include "resolution.h"
#include "xrandr_monitor.h"

/**
 * @brief The Monitor class -- describes a monitor connected to the system.
 */
class Monitor
{
public:
    Monitor();
    Monitor(XRandr_monitor& xrandr_monitor);
    Monitor(QString interface, QVector<Resolution>& resolutions);
    /**
     * @brief is_enabled -- predicate.  Check if the monitor is enabled.
     * @return True if the monitor enabled, false otherwise.
     */
    bool is_enabled() const;
    /**
     * @brief get_interface -- get monitor interface as a string (e.g. "VGA-1".)
     * @return An interface string.
     */
    QString get_interface() const;
    /**
     * @brief get_available_resolutions -- get all available resolutions for
     * the monitor.
     * @return Resolutions vector.
     */
    QVector<Resolution> get_available_resolutions() const;
    /**
     * @brief get_current_resolution -- get current monitor resolution.
     * @return Resolution instance.
     */
    Resolution get_current_resolution() const;

    /**
     * @brief set_enabled -- enable or disable the monitor.
     * @param is_enabled
     */
    void set_enabled(bool is_enabled);

    /**
     * @brief set_resolution -- set the current resolution by its index in the
     * resolutions vector.
     * @param index
     */
    void set_resolution(int index);

private:
    /**
     * @brief current_resolution -- selected index in resolutions vector.
     */
    int current_resolution = 0;
    /**
     * @brief state -- the seat state.
     */
    bool state = false;
    /**
     * @brief interface -- interface name (e.g. "VGA-1".)
     */
    QString interface;
    /**
     * @brief resolutions -- available monitor resolutions.
     */
    QVector<Resolution> resolutions;
};

#endif // MONITOR_H
