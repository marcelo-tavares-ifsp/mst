#include "controller.h"
#include "dsv.h"

#include "config.h"
#include "utils.h"

Controller::Controller(vector<Seat> seats) : seats(seats)
{
}

void Controller::make_mst()
{
    make_rc_lua();
    make_xorg();
    make_bashrc();
    make_xinitrc();
}

void Controller::enable_mst()
{
    make_mst();
    printf("Multiseat enabled.\n");
}

void Controller::disable_mst()
{

}

///////////////////////////////////////////////////////////////////////////////

void Controller::make_rc_lua()
{
    string out_file = Config::get_instance()->get_output_dir() + "/rc.lua";
    fstream rclua_pattern;
    fstream rclua;

    awesome_conf = new AwesomeConfig(seats);
    rclua_pattern.open("/usr/share/mst/rc.lua.pattern", ios::in);
    rclua.open(out_file, ios::out);

    string str;

    cout << "[debug] writing '" + out_file + "' ..." << endl;

    while(getline(rclua_pattern, str))
    {
        if(str == "-- $MST_AUTOSTART$")
        {
            rclua << *awesome_conf;
        }
        else if(str == "-- $MST_AWFUL_RULES$")
        {
            rclua << awesome_conf->get_rules();
        }
        else
        {
            rclua << str << endl;
        }
    }
    rclua.close();
    rclua_pattern.close();
    cout << "[debug] writing '" + out_file + "' ... done" << endl;
}

void Controller::make_xorg()
{
    string out_file = Config::get_instance()->get_output_dir() + "/xorg.conf";
    xorg_conf = new XorgConfig(seats);

    fstream xorg;
    xorg.open(out_file, ios::out);
    xorg << *xorg_conf;
    xorg.close();
}

void Controller::make_bashrc()
{
    string out_file = Config::get_instance()->get_output_dir() + "/.bashrc";
    string in_file
            = Config::get_instance()->get_usr_share_dir() + "/bashrc.template";
    ofstream bashrc(out_file);
    ifstream bashrc_template(in_file);

    for (string line; getline(bashrc_template, line); )
    {
        string tmp = replace_all(line, "{{tty}}", "6");
        bashrc << tmp << endl;
    }

    bashrc.close();
    bashrc_template.close();
}

void Controller::make_xinitrc()
{
    string out_file = Config::get_instance()->get_output_dir() + "/xinitrc";
    string in_file
            = Config::get_instance()->get_usr_share_dir() + "/xinitrc.template";
    ofstream xinitrc(out_file, ios::binary);
    ifstream xinitrc_template(in_file, ios::binary);
    xinitrc << xinitrc_template.rdbuf();
    xinitrc.close();
    xinitrc_template.close();

    out_file = Config::get_instance()->get_output_dir() + "/xmst";
    in_file  = Config::get_instance()->get_usr_share_dir() + "/xmst.template";
    ofstream xmst(out_file);
    ifstream xmst_template(in_file);
    xmst << xmst_template.rdbuf();
    xmst.close();
    xmst_template.close();
}
