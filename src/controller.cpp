#include <string>

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
    make_sudoers();
}

void Controller::enable_mst()
{
    make_mst();
    install_files();
    printf("Multiseat enabled.\n");
}

void Controller::disable_mst()
{

}

static void cp(const string& src, const string& dst)
{
    string cmd = "cp " + src + " " + dst;
    cout << "[debug] executing: " << cmd << endl;
    if (system(cmd.c_str()))
    {
        throw "Could not execute command: "
            + src + " -> " + dst;
    }
}

void Controller::install_files()
{
    Config* config = Config::get_instance();
    const string output_dir = config->get_output_dir();
    const string mst_user   = config->get_mst_user();
    const string mst_user_home = "/home/" + mst_user + "/";
    auto install = [output_dir](const string& src, const string& dst) -> void {
      cp(output_dir + "/" + src, dst);
    };
    install("rc.lua",    mst_user_home + "/.config/awesome/");
    install("xorg.conf", "/etc/X11/xorg.conf");
    install(".bashrc",   mst_user_home);
    install(".xinitrc",   mst_user_home);
    install(".xmst",      mst_user_home);
    if (is_pam_mkhomedir_used())
    {
        string skel = "/etc/skel";
        install("rc.lua",    skel + "/.config/awesome/");
        install(".bashrc",   skel);
        install(".xinitrc",   skel);
        install(".xmst",      skel);
    }
    install("sudoers",   Config::get_instance()->get_sudoers_config());

}

///////////////////////////////////////////////////////////////////////////////

void Controller::make_rc_lua()
{
    string out_file = Config::get_instance()->get_output_dir() + "/rc.lua";
    fstream rclua_pattern;
    fstream rclua;

    awesome_conf = new AwesomeConfig(seats);
    rclua_pattern.open("/usr/share/mst/rc.lua.template", ios::in);
    rclua.open(out_file, ios::out);

    string str;

    cout << "[debug] writing '" + out_file + "' ..." << endl;

    stringstream awesome_autostart;
    awesome_autostart << *awesome_conf;

    while(getline(rclua_pattern, str))
    {

        str = replace_all(str, "{{mst_autostart}}",
                          awesome_autostart.str());
        str = replace_all(str, "{{mst_awful_rules}}",
                          awesome_conf->get_rules());

        rclua << str << endl;
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
        string tmp = replace_all(line, "{{tty}}", "1");
        bashrc << tmp << endl;
    }

    bashrc.close();
    bashrc_template.close();
}

void Controller::make_xinitrc()
{
    string out_file = Config::get_instance()->get_output_dir() + "/.xinitrc";
    string in_file
            = Config::get_instance()->get_usr_share_dir() + "/xinitrc.template";
    ofstream xinitrc(out_file, ios::binary);
    ifstream xinitrc_template(in_file, ios::binary);
    xinitrc << xinitrc_template.rdbuf();
    xinitrc.close();
    xinitrc_template.close();

    out_file = Config::get_instance()->get_output_dir() + "/.xmst";
    in_file  = Config::get_instance()->get_usr_share_dir() + "/xmst.template";
    ofstream xmst(out_file);
    ifstream xmst_template(in_file);
    xmst << xmst_template.rdbuf();
    xmst.close();
    xmst_template.close();
}

void Controller::make_sudoers()
{
    const string user = Config::get_instance()->get_mst_user();
    const string out_file = Config::get_instance()->get_output_dir()
            + "/sudoers";
    const string in_file = Config::get_instance()->get_usr_share_dir()
            + "/sudoers.template";
    ofstream out(out_file);
    ifstream in(in_file);
    string line;
    getline(in, line);
    string result = replace_all(replace_all(line, "{{user}}", user),
                                "{{mst}}",
                                "/root/src/mst/scripts/mst-start-dm");
    out << result << endl;
    out.close();
    in.close();
}
