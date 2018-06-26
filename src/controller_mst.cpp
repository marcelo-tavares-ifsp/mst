#include "controller_mst.h"



Controller::Controller(vector<Seat> seats) : seats(seats)
{
}

void Controller::make_mst()
{
    write_rc_lua();
    write_xorg();
    write_bashrc();
    write_xinitrc();
}

void Controller::enable_mst()
{
    make_mst();
    printf("Multiseat enabled.\n");
}

void Controller::disable_mst()
{

}

string Controller::create_bashrc()
{
    string bash;
    bash += string("if [ -z \"$DISPLAY\" ] && [ $(tty) = /dev/tty");
    bash += to_string(6);
    bash += string(" ]; then\n");
    bash += string("    startx\n");
    bash += string("fi\n");
    return bash;
}

string Controller::create_xinitrc()
{
    string xinit;
    xinit += string("[ -f ~/.xmst ] && source ~/.xmst\n");
    return xinit;
}

string Controller::create_xmst()
{
    string xmst;
    xmst += string("exec awesome\n");
    return xmst;
}

///////////////////////////////////////////////////////////////////////////////

void Controller::write_rc_lua()
{
    awesome_conf = new AwesomeConfig(seats);

    fstream rclua_pattern;
    rclua_pattern.open("/root/mst/src/mst_files/rc.lua.pattern", ios::in);
    fstream rclua;
    rclua.open("/root/mst/src/test_files/home/multiseat/config/awesome/rc.lua", ios::out);

    string str;

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
}

void Controller::write_xorg()
{
    xorg_conf = new XorgConfig(seats);

    fstream xorg;
    xorg.open("/root/mst/src/test_files/etc/X11/xorg.conf", ios::out);
    xorg << *xorg_conf;
    xorg.close();
}

void Controller::write_bashrc()
{
    fstream bashrc;
    bashrc.open("/root/mst/src/test_files/home/multiseat/bash.rc", ios::out);
    bashrc << create_bashrc();
    bashrc.close();
}

void Controller::write_xinitrc()
{
    fstream xinitrc;
    xinitrc.open("/root/mst/src/test_files/home/multiseat/xinitrc", ios::out);
    xinitrc << create_xinitrc();
    xinitrc.close();

    fstream xmst;
    xmst.open("/root/mst/src/test_files/home/multiseat/xmst", ios::out);
    xmst << create_xmst();
    xmst.close();
}
