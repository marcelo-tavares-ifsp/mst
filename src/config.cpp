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
    }

    return instance;
}

const string Config::get_mst_user() const
{
    return mst_user;
}
