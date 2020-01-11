#ifndef AWESOME_CONFIG_H
#define AWESOME_CONFIG_H

#include <string>
#include <vector>
#include <sstream>

#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "component.h"
#include <QLoggingCategory>

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(component_awesome_category)

namespace awesome {

//// Constants.

static const QString RC_LUA_FILE      = "rc.lua";
static const QString RC_LUA_TPL_FILE  = "rc.lua";
static const QString RC_LUA4_TPL_FILE = "rc.lua4";

//// The main class.

class Awesome : public Component
{
public:
    Awesome(Configuration& config);
    void configure() override;
    QString get_version() override;
    void enable() override {
        /* Do nothing. */
    }
    void disable() override {
        /* Do nothing. */
    }

    void prepare_rclua_template(Template& rclua_template);
};

//// Helper procedures.

extern string make_xephyr_autostart();
extern string make_xephyr_rules(uint32_t sSize);
extern string make_xephyr_screens(vector<Seat> seats);

extern string get_awesome_raw_version();
extern vector<int> get_awesome_version();

}

#endif // AWESOME_CONFIG_H
