#include <QVBoxLayout>
#include <QPainter>

#include "monitor_widget.h"
#include "common/resolution/resolution.h"
#include "common/utilites/utilites.h"
#include "common/xrandr_monitor/xrandr_monitor.h"

Monitor_widget::Monitor_widget(XRandr_monitor& monitor)
{
    auto split1 = [] (const string& input, char separator) -> string {
      return split(input, separator)[0];
    };
    auto rcomp = [split1] (const string& left, const string& right) -> int {
            return stoi(split1(left, 'x')) > stoi(split1(right, 'x'));
    };

    monitor_state_check_box = new QCheckBox();
    resolution_combo_box = new QComboBox();
    monitor_label = new QLabel(QString::fromStdString(monitor.interface));

    monitor_state_check_box->setText("Включить");
    monitor_state_check_box->setChecked(true);
    sort(monitor.resolutions.begin(), monitor.resolutions.end(), rcomp);
    for (auto resolution : monitor.resolutions) {
        resolution_combo_box->addItem(QString::fromStdString(resolution));
    }

    QVBoxLayout* layout = new QVBoxLayout();
    layout->addWidget(monitor_label);
    layout->addWidget(monitor_state_check_box);
    layout->addWidget(resolution_combo_box);
    this->setLayout(layout);

    resolution_combo_box->setCurrentIndex(0);
}

Monitor_widget::~Monitor_widget()
{
    delete monitor_state_check_box;
    delete resolution_combo_box;
    delete monitor_label;
}

/**
 * @brief Monitor_widget::get_interface -- Get Monitor interface.
 * @return QString interface name (e.g. "VGA-1".)
 */
QString Monitor_widget::get_interface()
{
    return this->monitor_label->text();
}

/**
 * @brief Monitor_widget::get_resolution
 * @return QString resolution in the format WIDTHxHEIGHT.
 */
Resolution Monitor_widget::get_selected_resolution()
{
    return { this->resolution_combo_box->currentText() };
}

/**
 * @brief Monitor_widget::is_enabled -- Check if the monitor is enabled.
 * @return 'true' if enabled, 'false' otherwise.
 */
bool Monitor_widget::is_monitor_enabled()
{
    return monitor_state_check_box->isChecked();
}

void Monitor_widget::paintEvent(QPaintEvent* ev)
{
    QStyleOption o;
    o.initFrom(this);
    QPainter p(this);
    style()->drawPrimitive(QStyle::PE_Widget, &o, &p, this);
}
