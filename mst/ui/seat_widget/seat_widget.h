#ifndef SEAT_WIDGET_H
#define SEAT_WIDGET_H

#include <QComboBox>
#include <QListWidget>
#include <QCheckBox>
#include <QLabel>

#include "platform/platform.h"
#include "common/resolution/resolution.h"
#include "common/xrandr_monitor/xrandr_monitor.h"
#include "common/monitor/monitor.h"

/**
 * @brief The Seat_widget class -- A class that describes a monitor widget
 *     used in the Install_controller.
 */
class Seat_widget: public QWidget {
    Q_OBJECT
public:
    Seat_widget(Monitor monitor);
    ~Seat_widget();
    Monitor get_monitor();
    void paintEvent(QPaintEvent* ev);

private:
    Monitor    monitor;
    QCheckBox* monitor_state_check_box;
    QComboBox* resolution_combo_box;
    QLabel*    monitor_label;
};

#endif // SEAT_WIDGET_H
