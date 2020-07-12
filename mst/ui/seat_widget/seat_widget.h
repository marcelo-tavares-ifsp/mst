#ifndef SEAT_WIDGET_H
#define SEAT_WIDGET_H

#include <QComboBox>
#include <QListWidget>
#include <QCheckBox>
#include <QLabel>
#include <QPushButton>
#include <memory>

#include "platform/platform.h"
#include "core/resolution/resolution.h"
#include "core/xrandr_monitor/xrandr_monitor.h"
#include "core/monitor/monitor.h"
#include "core/seat/seat.h"

/**
 * @brief The Seat_widget class -- A class that describes a monitor widget
 *     used in the Install_controller.
 */
class Seat_widget: public QWidget {
    Q_OBJECT
public:
    Seat_widget(shared_ptr<Seat> seat);
    ~Seat_widget();
    void paintEvent(QPaintEvent* ev);

private:
    /**
     * @brief seat -- A shared seat pointer assigned to this widget.
     */
    shared_ptr<Seat> seat;

    QCheckBox* monitor_state_check_box;
    QComboBox* resolution_combo_box;
    QPushButton* device_configuration_button;
    QLabel*    monitor_label;

private slots:
    /**
     * @brief device_configuration_button_clicked -- handle device configuration
     * button click.
     */
    void device_configuration_button_clicked();
    /**
     * @brief monitor_resolution_changed -- handle changes in the dropdown list
     * that contains available resolutions.
     * @param index -- current resolution index.
     */
    void monitor_resolution_changed(int index);
    /**
     * @brief seat_state_changed -- handle the seat checkbox.
     * @param state
     */
    void seat_state_changed(bool state);

signals:
    void configure_seat(int seat_id);
};

#endif // SEAT_WIDGET_H
