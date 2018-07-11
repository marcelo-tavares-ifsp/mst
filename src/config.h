#ifndef CONFIG_H
#define CONFIG_H

#include <string>

#include "dsv.h"

class Config
{
public:
    static const string MST_CONFIG_FILE;

    static Config* get_instance();
    const string get_mst_user() const;
    const string get_usr_share_dir() const;
    const string get_output_dir() const;
    const string get_awesome_config() const;

private:
    Config();
    static Config* instance;
    DSV* mst_conf = NULL;
    string mst_user;
    string usr_share_dir;
    string output_dir;
    // configuration files
    string awesome_config;

};

#endif // CONFIG_H
