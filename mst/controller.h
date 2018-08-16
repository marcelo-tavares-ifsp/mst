#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <fstream>
#include <vector>
#include <QLoggingCategory>

#include "xorg/xorg-config.h"
#include "awesome/awesome.h"
#include "mainwindow.h"
#include "seat.h"
#include "config.h"

Q_DECLARE_LOGGING_CATEGORY(controller_category)

using namespace std;

/**
 * @brief The Controller class -- a class to control the multi-seat
 *      configuration.
 */
class Controller
{
public:
    Controller(vector<Seat> seats);
    void generate_files();
    void enable_mst();
    void disable_mst();
    static void stop_mst();
    static void create_backup();
    static void restore_backup();

private:
    vector<Seat> seats;
    Awesome *awesome_conf;
    XorgConfig *xorg_conf;
    void make_mst();

    string create_bashrc();
    string create_xmst();
    string create_xinitrc();

    void make_rc_lua();
    void make_xorg();
    void make_bashrc();
    void make_xinitrc();
    void make_sudoers();
    void make_lightdm_conf();
    void make_getty_service();

    void install_files();
};

#endif // CONTROLLER_H
