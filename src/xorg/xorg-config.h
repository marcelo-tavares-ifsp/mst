#ifndef XORGCONFIG_H
#define XORGCONFIG_H

#include <vector>
#include <string>
#include <iostream>
#include <functional>
#include <numeric>

#include "xorg-monitor.h"

using namespace std;



class XorgConfig
{
private:
    /**
     * @brief monitors -- a list of known output devices.
     */
    vector<XorgMonitor> monitors;

public:
    XorgConfig();

    /**
     * @brief add_monitor -- add a new device to the config.
     * @param monitor -- a new device.
     */
    void add_monitor(XorgMonitor monitor);

    /**
     * @brief get_monitors -- get monitors registered in the config.
     * @return a vector of monitors.
     */
    const vector<XorgMonitor>& get_monitors() const;

    friend ostream& operator << (ostream& os, const XorgConfig& config);
};

#endif // XORGCONFIG_H
