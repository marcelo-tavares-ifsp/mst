#include "monitor.h"

#include "../types/xrandr_monitor.h"
#include "../types/resolution.h"

using namespace std;

Monitor::Monitor()
{

}

static void _sort_resolutions(QVector<Resolution>& resolutions)
{
    auto rcomp = [] (const Resolution& left, const Resolution& right) -> int {
        return left.get_width() > right.get_width();
    };
    sort(resolutions.begin(), resolutions.end(), rcomp);
}

Monitor::Monitor(QString interface, QVector<Resolution> &resolutions)
{
    this->interface = interface;
    this->resolutions = resolutions;
    _sort_resolutions(this->resolutions);
}

Monitor::Monitor(XRandr_monitor& xrandr_monitor)
{
    resolutions.clear();
    this->interface = xrandr_monitor.interface;
    add_resolutions(xrandr_monitor.resolutions);
}

void Monitor::add_resolutions(const QVector<QString>& new_resolutions)
{
    for (QString resolution_string : new_resolutions) {
        Resolution resolution(resolution_string);
        if (! resolutions.contains(resolution)) {
            resolutions.push_back(resolution);
        }
    }
    _sort_resolutions(resolutions);
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
