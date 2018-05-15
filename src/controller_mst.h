#ifndef CONTROLLER_MST_H
#define CONTROLLER_MST_H

#include "awesome/awesome-config.h"
#include "xorg/xorg-config.h"

const unsigned int MONITORS = 2;

class Controller
{
public:
    Controller();
    XorgConfig* create_xorg();
    AwesomeConfig* create_rclua();
};

#endif // CONTROLLER_MST_H
