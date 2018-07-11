#include "config.h"

#include "dsv.h"

const string Config::MST_CONFIG_FILE  = "/etc/mst";
Config* Config::instance = NULL;

Config::Config()
{

}

Config* Config::get_instance()
{
    if (! instance)
    {
        instance = new Config();
        instance->mst_conf = new DSV(MST_CONFIG_FILE);
        instance->mst_user = instance->mst_conf->get("user");
        instance->usr_share_dir = "/usr/share/mst";
        instance->output_dir
                = "/home/" + instance->mst_user + "/.local/share/mst/output";

        instance->awesome_config
                = "/home/" + instance->mst_user + "/.config/awesome/rc.lua";
    }

    return instance;
}

const string Config::get_mst_user() const
{
    return mst_user;
}

const string Config::get_output_dir() const
{
    return output_dir;
}

const string Config::get_usr_share_dir() const
{
    return usr_share_dir;
}

const string Config::get_awesome_config() const
{
    return awesome_config;
}
