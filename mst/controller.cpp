/* controller.cpp -- MST controller.
 *
 * Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 * Copyright (C) 2018 Anton Plekhanov <plehunov.anton_9@mail.ru>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <string>
#include <QLoggingCategory>

#include "controller.h"
#include "dsv.h"
#include "config.h"
#include "utils.h"

Q_LOGGING_CATEGORY(controller_category, "mst.controller")

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
    make_lightdm_conf();
    make_getty_service();
    make_udev_rules();
}

bool Controller::is_mst_running()
{
    return (system("pgrep -c Xephyr") == 0);
}

/**
 * @brief Controller::stop_mst -- Stop multiseat if it is running.
 * @throws an error message if stopping is failed.
 */
void Controller::stop_mst()
{
    if (system("pkill Xephyr"))
    {
        const string msg = "Could not stop MST ('pkill Xephyr' failed.)";
        qCritical(controller_category) << msg.c_str();
        throw msg;
    }
}

void Controller::generate_files()
{
    string out = Config::get_instance()->get_output_dir();
    make_mst();
    qDebug(controller_category) << "files generated: " << out.c_str();
}

void Controller::enable_mst()
{
    install_files();
    if (system("systemctl set-default multi-user.target"))
    {
        qCritical(controller_category) << "Could not enable MST in systemd.";
        throw "Could not enable MST in systemd.";
    }
    qInfo(controller_category) << "multiseat enabled.";
}

void Controller::disable_mst()
{
    if (system("systemctl set-default graphical.target"))
    {
        qCritical(controller_category) << "Could not disable MST in systemd.";
        throw "Could not disable MST in systemd.";
    }

    string cmd = "rm '" + Config::get_instance()->get_sudoers_config() + "'";

    if (system(cmd.c_str()))
    {
        string message = "Could not delete "
                + Config::get_instance()->get_sudoers_config() + ".";
        qCritical(controller_category) << message.c_str();
        throw message;
    }
}

static void cp(const string& src, const string& dst)
{
    string cmd = "cp " + src + " " + dst;
    qDebug(controller_category) << "executing: " << cmd.c_str();
    if (system(cmd.c_str()))
    {
        qCritical(controller_category)
                << "Could not execute command: "
                << src.c_str() << " -> " << dst.c_str();
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

    string cmd = "mkdir -p " + mst_user_home + ".config/awesome/";
    if (system(cmd.c_str()))
    {
        qCritical(controller_category)
             << "Could not create a directory: " << cmd.c_str();
        throw "Could not create a directory: " + cmd;
    }
    install("rc.lua",    mst_user_home + ".config/awesome/");
    install("xorg.conf", "/etc/X11/xorg.conf");
    install(".bashrc",   mst_user_home);
    install(".xinitrc",   mst_user_home);
    install(".xmst",      mst_user_home);
    install("lightdm-mst.conf", "/etc/lightdm/");
    install("getty@.service",   "/lib/systemd/system/getty@.service");
    if (is_pam_mkhomedir_used())
    {
        string skel = "/etc/skel";
        cmd = "mkdir -p " + skel + "/.config/awesome/";
        if (system(cmd.c_str()))
        {
            qCritical(controller_category)
                    << "Could not create a directory: " << cmd.c_str();
            throw "Could not create a directory: " + cmd;
        }
        install("rc.lua",    skel + "/.config/awesome/");
        install(".bashrc",   skel);
        install(".xinitrc",   skel);
        install(".xmst",      skel);
    }
    install("sudoers",   Config::get_instance()->get_sudoers_config());

    install("99-mst.rules", "/etc/udev/rules.d/99-mst.rules");
    install("systemd-udevd.service", "/etc/systemd/system");

}

///////////////////////////////////////////////////////////////////////////////

void Controller::make_rc_lua()
{
    string out_file = Config::get_instance()->get_output_dir() + "/rc.lua";
    fstream rclua_pattern;
    fstream rclua;

    const vector<int> version = Awesome::get_version();
        awesome_conf = new Awesome(seats);
        if (version[0] == 3)
        {
            qDebug(controller_category) << "Using rc.lua.template for Awesome 3";
            rclua_pattern.open("/usr/share/mst/rc.lua.template", ios::in);
        }
        else
        {
             qDebug(controller_category) << "Using rc.lua.template for Awesome 4";
            rclua_pattern.open("/usr/share/mst/rc.lua.4.template", ios::in);
    }
    rclua.open(out_file, ios::out);

    string str;

        qDebug(controller_category)
                << "writing '" << out_file.c_str() << "' ...";

    while(getline(rclua_pattern, str))
    {

        str = replace_all(str, "{{mst_autostart}}",
                          awesome_conf->make_autostart());
        str = replace_all(str, "{{mst_awful_rules}}",
                          awesome_conf->make_rules());

        rclua << str << endl;
    }
    rclua.close();
    rclua_pattern.close();
    qDebug(controller_category)
            << "writing '" << out_file.c_str() << "' ... done";
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
                                "/usr/local/bin/mst-start-dm");
    out << result << endl;
    out.close();
    in.close();
}

/**
 * @brief Controller::make_lightdm_conf -- Generate a LightDM configuration
 *          file.
 *
 * TODO: Use actual number of seats.
 */
void Controller::make_lightdm_conf()
{
    const string out_file = Config::get_instance()->get_output_dir()
            + "/lightdm-mst.conf";
    const string in_file = Config::get_instance()->get_usr_share_dir()
            + "/lightdm-mst.conf.template";
    ofstream out(out_file);
    ifstream in(in_file);
    out << in.rdbuf();
    out.close();
    in.close();
}

void Controller::make_getty_service()
{
    const string in_file = Config::get_instance()->get_usr_share_dir()
            + "/getty@.service.template";
    const string out_file = Config::get_instance()->get_output_dir()
            + "/getty@.service";
    const string user = Config::get_instance()->get_mst_user();
    ifstream in(in_file);
    ofstream out(out_file);
    for (string line; getline(in, line); )
    {
        string tmp = replace_all(line, "{{user}}", user);
        out << tmp << endl;
    }
    in.close();
    out.close();
}

void Controller::make_udev_rules()
{
    const string out_file = Config::get_instance()->get_output_dir()
            + "/99-mst.rules";

    ofstream out(out_file);

    for (int idx = 0; idx < seats.size(); ++idx)
    {
        out << "ACTION==\"add\", ";
        out << "KERNEL==\"sd[b-z][0-9]\", ";
        out << "DEVPATH==\"" << seats[idx].usb << "/*\", ";
        out << "RUN+=\"/usr/local/bin/mst-mount /dev/%k " << idx + 1 << "\""
            << endl;
    }

    out.close();
}

void Controller::make_udev_service()
{
    const string out_file = Config::get_instance()->get_output_dir()
            + "/systemd-udevd.service";
    const string in_file = Config::get_instance()->get_usr_share_dir()
            + "/systemd-udevd.service.template";
    ofstream out(out_file);
    ifstream in(in_file);
    out << in.rdbuf();
    out.close();
    in.close();
}

void Controller::create_backup()
{
    const string user = Config::get_instance()->get_mst_user();
    const string usr_dir = Config::get_instance()->get_usr_share_dir();
    const string cmd = "/usr/local/bin/mk_backup.sh " + user;
    if (system(cmd.c_str()))
    {
        qCritical(main_window_category)
                << "Could not create a backup: "
                << cmd.c_str();
        throw "Could not create a backup: " + cmd;
    }
}

void Controller::restore_backup()
{
    const string user = Config::get_instance()->get_mst_user();
    const string cmd = "/usr/local/bin/apl_backup.sh " + user;

    if (system(cmd.c_str()))
    {
        qCritical(main_window_category) << "Could not restore a backup copy.";
        throw "Could not restore a backup copy.";
    }
}
