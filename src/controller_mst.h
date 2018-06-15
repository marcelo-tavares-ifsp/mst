#ifndef CONTROLLER_MST_H
#define CONTROLLER_MST_H

#include <fstream>

#include "xorg/xorg-config.h"
#include "awesome/awesome-config.h"
#include "mainwindow.h"
#include "ui_mainwindow.h"



/**
 * @brief The Controller class -- a class to control the multi-seat
 *      configuration.
 */
class Controller
{
public:
    Controller(int count_of_monitors);
    void enable_mst();
    void disable_mst();

private:
    int count_of_monitors;
    AwesomeConfig *awesome_conf;
    XorgConfig *xorg_conf;
    void make_mst();

    void create_rclua(int width, int height);
    void create_xorg(vector<string> interfaces, int width, int height);
    string create_bashrc();
    string create_xmst();
    string create_xinitrc();

    void write_rc_lua();
    void write_xorg();
    void write_bashrc();
    void write_xinitrc();
};

#endif // CONTROLLER_MST_H
