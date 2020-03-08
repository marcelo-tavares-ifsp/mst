#ifndef MONITOR_WIDGET_H
#define MONITOR_WIDGET_H

#include <QComboBox>
#include <QListWidget>
#include <QCheckBox>
#include <QLabel>

#include "../platform/platform.h"

/**
 * @brief The Monitor_widget class -- A class that describes a monitor widget
 *     used in the Install_controller.
 */
class Monitor_widget: public QWidget {
public:
    Monitor_widget(xrandrMonitor& monitor);
    ~Monitor_widget();
    bool is_enabled();

private:
    QCheckBox* monitor_state_check_box;
    QComboBox* resolution_combo_box;
    QLabel*    monitor_label;
};

#endif // MONITOR_WIDGET_H
