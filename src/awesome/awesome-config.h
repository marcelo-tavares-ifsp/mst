#ifndef AWESOMECONFIG_H
#define AWESOMECONFIG_H

#include <vector>
#include <string>

#include <awesome-device.h>

using namespace std;

class AwesomeConfig
{
    /**
     * @brief devices -- a list of known input devices pairs.
     */
    vector<AwesomeDevice> devices;

public:
    AwesomeConfig();

    /**
     * @brief add_devices -- add a new pair of devices to the config.
     * @param devices -- a new pair of devices.
     */
    void add_devices(AwesomeDevice pair_devices);
    string get_rules();

    /**
     * @brief get_devices -- get devices registered in the config.
     * @return a vector of devices.
     */
    const vector<AwesomeDevice>& get_devices() const;

    friend ostream& operator << (ostream& os, const AwesomeConfig& config);
};

#endif // AWESOMECONFIG_H
