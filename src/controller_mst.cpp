#include "controller_mst.h"

Controller::Controller()
{

}

XorgConfig* create_xorg()
{
    string interfaces[] = {"Monitor-DVI-1", "Monitor-VGA-1"};
    XorgConfig *config = new XorgConfig();

    for(int i = 0; i < MONITORS; i++)
    {
        XorgMonitor *monitor = new XorgMonitor("monitor" + i);
        monitor->set_dimensions(1920, 1080);
        monitor->set_interface_name(interfaces[i]);
        config->add_monitor(*monitor);
    }
    return config;
}

AwesomeConfig* create_rclua()
{
    AwesomeConfig *config = new AwesomeConfig();

    for(int i = 0; i < MONITORS; i++)
    {
        AwesomeDevice *device = new AwesomeDevice(i);
        device->set_dimensions(1920, 1080);
        config->add_devices(*device);
    }
    return config;
}





