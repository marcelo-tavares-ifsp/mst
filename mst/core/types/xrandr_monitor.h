#ifndef XRANDR_MONITOR_H
#define XRANDR_MONITOR_H

#include <vector>
#include <string>

class XRandr_monitor
{
public:
    XRandr_monitor();
    std::string interface;
    std::vector<std::string> resolutions;
};

#endif // XRANDR_MONITOR_H
