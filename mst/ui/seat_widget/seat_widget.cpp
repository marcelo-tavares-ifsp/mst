#include <QVBoxLayout>
#include <QPainter>
#include <memory>

#include "ui/install_window/installwindow.h"
#include "seat_widget.h"
#include "core/types/resolution.h"
#include "core/utilites/utilites.h"
#include "core/types/xrandr_monitor.h"
#include "core/types/monitor.h"

Seat_widget::Seat_widget(shared_ptr<Seat> seat)
    : seat(seat),
      monitor_state_check_box(new QCheckBox()),
      resolution_combo_box(new QComboBox()),
      monitor_label(new QLabel())
{
    monitor_state_check_box->setText(tr("Enable"));
    monitor_state_check_box->setChecked(true);
    for (auto resolution : seat->get_monitor().get_available_resolutions()) {
        resolution_combo_box->addItem(resolution.to_string());
    }

    device_configuration_button
            = new QPushButton(tr("Configure"));
    device_configuration_button->setParent(this);
    connect(device_configuration_button,
            SIGNAL(clicked()), this,
            SLOT(device_configuration_button_clicked()));
    connect(resolution_combo_box,
            SIGNAL(currentIndexChanged(int)), this,
            SLOT(monitor_resolution_changed(int)));
    connect(monitor_state_check_box,
            SIGNAL(toggled(bool)),
            this, SLOT(seat_state_changed(bool)));

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

void Seat_widget::device_configuration_button_clicked()
{
    emit configure_seat(seat->get_id());
}

void Seat_widget::monitor_resolution_changed(int index)
{
    seat->get_monitor().set_resolution(index);
}

void Seat_widget::seat_state_changed(bool state)
{
    device_configuration_button->setEnabled(state);
    monitor_label->setEnabled(state);
    resolution_combo_box->setEnabled(state);
    seat->get_monitor().set_enabled(state);
}

void Seat_widget::paintEvent(QPaintEvent* ev)
{
    (void) ev;
    QStyleOption o;
    o.initFrom(this);
    QPainter p(this);
    style()->drawPrimitive(QStyle::PE_Widget, &o, &p, this);
    this->monitor_label->setText(
                "<h1>" + seat->get_monitor().get_interface() + "</h1><br/><hr/><br/>"
                + ((seat->get_mouse().length() > 0) ? "<b>Mouse:</b> " + seat->get_mouse() : "")
                + "<br/>"
                + ((seat->get_keyboard().length() > 0) ? "<b>Keyboard</b>: " + seat->get_keyboard() : "") );
}
