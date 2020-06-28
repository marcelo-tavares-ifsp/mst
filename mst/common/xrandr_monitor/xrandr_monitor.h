#ifndef XRANDR_MONITOR_H
#define XRANDR_MONITOR_H

#include <vector>
#include <string>

using namespace std;

class XRandr_monitor
{
public:
    XRandr_monitor();
    string interface;
    vector<string> resolutions;
};

#endif // XRANDR_MONITOR_H
