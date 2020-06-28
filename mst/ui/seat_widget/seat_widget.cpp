#include <QVBoxLayout>
#include <QPainter>

#include "seat_widget.h"
#include "common/resolution/resolution.h"
#include "common/utilites/utilites.h"
#include "common/xrandr_monitor/xrandr_monitor.h"
#include "common/monitor/monitor.h"

Seat_widget::Seat_widget(Monitor monitor)
{
    this->monitor = monitor;
    monitor_state_check_box = new QCheckBox();
    resolution_combo_box = new QComboBox();
    monitor_label = new QLabel(monitor.get_interface());

    monitor_state_check_box->setText("Включить");
    monitor_state_check_box->setChecked(true);
    for (auto resolution : monitor.get_available_resolutions()) {
        resolution_combo_box->addItem(resolution.to_string());
    }

    QVBoxLayout* layout = new QVBoxLayout();
    layout->addWidget(monitor_label);
    layout->addWidget(monitor_state_check_box);
    layout->addWidget(resolution_combo_box);
    this->setLayout(layout);

    resolution_combo_box->setCurrentIndex(0);
}

Seat_widget::~Seat_widget()
{
    delete monitor_state_check_box;
    delete resolution_combo_box;
    delete monitor_label;
}

Monitor Seat_widget::get_monitor() {
    monitor.set_enabled(monitor_state_check_box->isChecked());
    monitor.set_resolution(resolution_combo_box->currentIndex());
    return monitor;
}

void Seat_widget::paintEvent(QPaintEvent* ev)
{
    QStyleOption o;
    o.initFrom(this);
    QPainter p(this);
    style()->drawPrimitive(QStyle::PE_Widget, &o, &p, this);
}
