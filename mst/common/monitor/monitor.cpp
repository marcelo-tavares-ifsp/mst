#include "monitor.h"

#include "common/xrandr_monitor/xrandr_monitor.h"
#include "common/resolution/resolution.h"

Monitor::Monitor()
{

}

Monitor::Monitor(XRandr_monitor& xrandr_monitor)
{
    auto rcomp = [] (const Resolution& left, const Resolution& right) -> int {
            return left.get_width() > right.get_width();
    };

    resolutions.clear();
    this->interface = QString::fromStdString(xrandr_monitor.interface);
    for (string resolution_string : xrandr_monitor.resolutions) {
        QString resolution_qstring = QString::fromStdString(resolution_string);
        resolutions.push_back(Resolution(resolution_qstring));
    }
    sort(resolutions.begin(), resolutions.end(), rcomp);
}

bool Monitor::is_enabled() const {
    return this->state;
}

void Monitor::set_enabled(bool state) {
    this->state = state;
}

void Monitor::set_resolution(int index) {
    this->current_resolution = index;
}

Resolution Monitor::get_current_resolution() const {
    return this->resolutions[this->current_resolution];
}

QString Monitor::get_interface() const {
    return this->interface;
}

QVector<Resolution> Monitor::get_available_resolutions() const
{
   return this->resolutions;
}
