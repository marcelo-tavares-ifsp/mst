#include "controller_mst.h"

Controller::Controller()
{

}

void Controller::make_mst()
{
    create_rclua();
    create_xorg();

    write_rc_lua();
    write_xorg();
    write_bashrc();
    write_xinitrc();
}

void Controller::enable_mst()
{

}

void Controller::disable_mst()
{

}

void Controller::create_xorg()
{
    string interfaces[] = {"Monitor-DVI-1", "Monitor-VGA-1"};
    xorg_conf = new XorgConfig();

    for(int i = 0; i < MONITORS; i++)
    {
        XorgMonitor *monitor = new XorgMonitor("monitor" + i);
        monitor->set_dimensions(1920, 1080);
        monitor->set_interface_name(interfaces[i]);
        xorg_conf->add_monitor(*monitor);
    }
}

void Controller::create_rclua()
{
    awesome_conf = new AwesomeConfig();

    for(int i = 0; i < MONITORS; i++)
    {
        AwesomeDevice *device = new AwesomeDevice(i);
        device->set_dimensions(1920, 1080);
        awesome_conf->add_devices(*device);
    }
}

string Controller::create_bashrc()
{
    string bash;
    bash += string("if [ -z \"$DISPLAY\" ] && [ $(tty) = /dev/tty");
    bash += 6;
    bash += string(" ]; then\n");
    bash += string("startx\n");
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
    fstream rclua_pattern;
    rclua_pattern.open("D:\\Programming\\Git\\mst\\src\\mst_files\\rc.lua.pattern", ios::in);
    fstream rclua;
    rclua.open("D:\\Programming\\Git\\mst\\src\\test_files\\home\\multiseat\\config\\awesome\\rc.lua", ios::out);

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
    fstream xorg;
    xorg.open("D:\\Programming\\Git\\mst\\src\\test_files\\etc\\X11\\xorg.conf", ios::out);
    xorg << *xorg_conf;
    xorg.close();
}

void Controller::write_bashrc()
{
    fstream bashrc;
    bashrc.open("D:\\Programming\\Git\\mst\\src\\test_files\\home\\multiseat\\bash.rc", ios::out);
    bashrc << create_bashrc();
    bashrc.close();
}

void Controller::write_xinitrc()
{
    fstream xinitrc;
    xinitrc.open("D:\\Programming\\Git\\mst\\src\\test_files\\home\\multiseat\\xinitrc", ios::out);
    xinitrc << create_xinitrc();
    xinitrc.close();

    fstream xmst;
    xmst.open("D:\\Programming\\Git\\mst\\src\\test_files\\home\\multiseat\\xmst", ios::out);
    xmst << create_xmst();
    xmst.close();
}
