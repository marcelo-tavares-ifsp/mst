#include "monitor.h"

#include "../xrandr_monitor/xrandr_monitor.h"
#include "../resolution/resolution.h"

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
    this->interface = QString::fromStdString(xrandr_monitor.interface);
    for (string resolution_string : xrandr_monitor.resolutions) {
        QString resolution_qstring = QString::fromStdString(resolution_string);
        resolutions.push_back(Resolution(resolution_qstring));
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
