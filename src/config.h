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

private:
    Config();
    static Config* instance;
    DSV* mst_conf = NULL;
    string mst_user;

};

#endif // CONFIG_H
