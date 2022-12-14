#include "monitor.h"

#include "../types/xrandr_monitor.h"
#include "../types/resolution.h"

using namespace std;

Monitor::Monitor()
{

}

bool operator==(const Monitor& lhs, const Monitor& rhs) {
    return (lhs.get_interface() == rhs.get_interface())
            && (lhs.get_available_resolutions() == rhs.get_available_resolutions());
}

std::ostream& operator<< (std::ostream& os, const Monitor& monitor) {
    os << "#<Monitor " << monitor.interface.toStdString()
       << ">";
    return os;
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

void Monitor::set_resolution(const Resolution &resolution)
{
    int res_idx = 0;
    for (auto res : this->resolutions) {
        if (res == resolution) {
            set_resolution(res_idx);
            return;
        }
        res_idx++;
    }
    throw Monitor_error("Resolution is not available: "
                        + resolution.to_string());
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
