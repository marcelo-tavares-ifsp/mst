#ifndef SEAT_WIDGET_H
#define SEAT_WIDGET_H

#include <QComboBox>
#include <QListWidget>
#include <QCheckBox>
#include <QLabel>
#include <QPushButton>
#include <memory>

#include "platform/platform.h"
#include "common/resolution/resolution.h"
#include "common/xrandr_monitor/xrandr_monitor.h"
#include "common/monitor/monitor.h"
#include "common/seat/seat.h"

/**
 * @brief The Seat_widget class -- A class that describes a monitor widget
 *     used in the Install_controller.
 */
class Seat_widget: public QWidget {
    Q_OBJECT
public:
    Seat_widget(shared_ptr<Seat> seat);
    ~Seat_widget();
    Monitor get_monitor();
    void paintEvent(QPaintEvent* ev);

private:
    shared_ptr<Seat> seat;
    QCheckBox* monitor_state_check_box;
    QComboBox* resolution_combo_box;
    QPushButton* device_configuration_button;
    QLabel*    monitor_label;

private slots:
    void device_configuration_button_clicked();

signals:
    void configure_seat(int seat_id);
};

#endif // SEAT_WIDGET_H
