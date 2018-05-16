#ifndef CONTROLLER_MST_H
#define CONTROLLER_MST_H

#include "xorg/xorg-config.h"
#include "awesome/awesome-config.h"

#include <fstream>

const unsigned int MONITORS = 2;

class Controller
{
public:
    Controller();
    void enable_mst();
    void disable_mst();

private:
    AwesomeConfig *awesome_conf;
    XorgConfig *xorg_conf;
    void make_mst();

    void create_rclua();
    void create_xorg();
    string create_bashrc();
    string create_xmst();
    string create_xinitrc();

    void write_rc_lua();
    void write_xorg();
    void write_bashrc();
    void write_xinitrc();
};

#endif // CONTROLLER_MST_H
