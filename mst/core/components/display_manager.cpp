#include "display_manager.h"

#include "component.h"
#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"
#include "core/platform.h"

Q_LOGGING_CATEGORY(display_manager_category, "mst.dm")

using namespace display_manager;

Display_manager::Display_manager(Configuration& config) : Component(config)
{

}

void Display_manager::configure()
{
    component_configuration.add(LIGHTDM_FILE, "/etc/lightdm/",
                                prepare_lightdm_template());
}

Template display_manager::prepare_lightdm_template()
{
    Template tpl = Template_manager::get_instance()->get_template(LIGHTDM_FILE);
    return tpl;
}

static void _configure_x11() {
    if (Platform::xset_dpms() || Platform::xset_soff())
    {
        qCritical(display_manager_category) << "Could not configure X11.";
        throw "Could not configure X11.";
    }
}

void Display_manager::enable() {
    QString lightdm_cmd = "/usr/sbin/lightdm --config ";// + config_path;
    if (Platform::exec(lightdm_cmd) != 0) {
        throw Component_error("Could not start lightdm");
    }

    _configure_x11();
}

void Display_manager::add_seat(int seat_number) {
    QString lightdm_cmd = "/usr/bin/dm-tool add-local-x-seat " + seat_number;
    if (Platform::exec(lightdm_cmd) != 0) {
        throw Component_error("Could not configure seats");
    }
}

void Display_manager::add_seats(int count) {
    for (int idx = 1; idx <= count; ++idx) {
        add_seat(idx);
    }
}
