#include <QVBoxLayout>

#include "monitor_widget.h"
#include "common/utilites/utilites.h"

Monitor_widget::Monitor_widget(xrandrMonitor& monitor)
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

    this->setStyleSheet("border: 1px solid");
    resolution_combo_box->setStyleSheet("border: 0px");
    monitor_label->setStyleSheet("border: 0px");
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
 * @brief Monitor_widget::is_enabled -- Check if the monitor is enabled.
 * @return 'true' if enabled, 'false' otherwise.
 */
bool Monitor_widget::is_enabled()
{
    return monitor_state_check_box->isChecked();
}
