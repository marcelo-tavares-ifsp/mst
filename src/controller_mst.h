#ifndef CONTROLLER_MST_H
#define CONTROLLER_MST_H

#include <fstream>
#include <vector>

#include "xorg/xorg-config.h"
#include "awesome/awesome-config.h"
#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "seat.h"

using namespace std;



/**
 * @brief The Controller class -- a class to control the multi-seat
 *      configuration.
 */
class Controller
{
public:
    Controller(vector<Seat> seats);
    void enable_mst();
    void disable_mst();

private:
    vector<Seat> seats;
    AwesomeConfig *awesome_conf;
    XorgConfig *xorg_conf;
    void make_mst();

    string create_bashrc();
    string create_xmst();
    string create_xinitrc();

    void write_rc_lua();
    void write_xorg();
    void write_bashrc();
    void write_xinitrc();
};

#endif // CONTROLLER_MST_H
