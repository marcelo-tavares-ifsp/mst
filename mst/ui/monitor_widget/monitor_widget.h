#ifndef MONITOR_WIDGET_H
#define MONITOR_WIDGET_H

#include <QComboBox>
#include <QListWidget>
#include <QCheckBox>
#include <QLabel>

#include "platform/platform.h"
#include "common/resolution/resolution.h"
#include "common/xrandr_monitor/xrandr_monitor.h"

/**
 * @brief The Monitor_widget class -- A class that describes a monitor widget
 *     used in the Install_controller.
 */
class Monitor_widget: public QWidget {
    Q_OBJECT
public:
    Monitor_widget(XRandr_monitor& monitor);
    ~Monitor_widget();
    bool is_monitor_enabled();
    QString get_interface();
    Resolution get_selected_resolution();
    void paintEvent(QPaintEvent* ev);

private:
    QCheckBox* monitor_state_check_box;
    QComboBox* resolution_combo_box;
    QLabel*    monitor_label;
};

#endif // MONITOR_WIDGET_H
