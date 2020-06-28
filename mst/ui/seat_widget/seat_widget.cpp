#include <QVBoxLayout>
#include <QPainter>
#include <memory>

#include "ui/install_window/installwindow.h"
#include "seat_widget.h"
#include "common/resolution/resolution.h"
#include "common/utilites/utilites.h"
#include "common/xrandr_monitor/xrandr_monitor.h"
#include "common/monitor/monitor.h"

Seat_widget::Seat_widget(shared_ptr<Seat> seat)
{
    this->seat = seat;
    monitor_state_check_box = new QCheckBox();
    resolution_combo_box = new QComboBox();
    monitor_label = new QLabel(seat->get_monitor().get_interface());

    monitor_state_check_box->setText("Включить");
    monitor_state_check_box->setChecked(true);
    for (auto resolution : seat->get_monitor().get_available_resolutions()) {
        resolution_combo_box->addItem(resolution.to_string());
    }

    device_configuration_button
            = new QPushButton("Настроить устройства ввода");
    device_configuration_button->setParent(this);
    connect(device_configuration_button,
            SIGNAL(clicked()), this,
            SLOT(device_configuration_button_clicked()));

    QVBoxLayout* layout = new QVBoxLayout();
    layout->addWidget(monitor_label);
    layout->addWidget(monitor_state_check_box);
    layout->addWidget(resolution_combo_box);
    layout->addWidget(device_configuration_button);
    this->setLayout(layout);

    resolution_combo_box->setCurrentIndex(0);
}

Seat_widget::~Seat_widget()
{
    delete monitor_state_check_box;
    delete resolution_combo_box;
    delete monitor_label;
    delete device_configuration_button;
}

Monitor Seat_widget::get_monitor() {
    seat->get_monitor().set_enabled(monitor_state_check_box->isChecked());
    seat->get_monitor().set_resolution(resolution_combo_box->currentIndex());
    return seat->get_monitor();
}

void Seat_widget::device_configuration_button_clicked()
{
    emit configure_seat(seat->get_id());
}

void Seat_widget::paintEvent(QPaintEvent* ev)
{
    QStyleOption o;
    o.initFrom(this);
    QPainter p(this);
    style()->drawPrimitive(QStyle::PE_Widget, &o, &p, this);
}
