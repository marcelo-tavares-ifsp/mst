#include "display_manager.h"

#include "component.h"
#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"
#include "../platform/platform.h"

Q_LOGGING_CATEGORY(display_manager_category, "mst.dm")

using namespace display_manager;

Display_manager::Display_manager(Configuration& config) : Component(config)
{

}

void Display_manager::configure(const QString &output_dir)
{
    QString out_file_name = output_dir + "/" + LIGHTDM_FILE;
    Template tpl = prepare_lightdm_template();
    tpl.substitute(out_file_name.toStdString());
}

Template display_manager::prepare_lightdm_template()
{
    Template tpl = Template_manager::get_instance()->get_template(
                LIGHTDM_FILE.toStdString());
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
    string lightdm_cmd = "/usr/sbin/lightdm --config ";// + config_path;
    if (system(lightdm_cmd.c_str()))
    {
        qCritical(display_manager_category) << "Could not start lightdm: " << lightdm_cmd.c_str();
        throw "Could not start lightdm: " + lightdm_cmd;
    }
    _configure_x11();
}

void Display_manager::add_seat(int seat_number) {
    string lightdm_cmd = "/usr/bin/dm-tool add-local-x-seat " + seat_number;
    if (system(lightdm_cmd.c_str()))
    {
        qCritical(display_manager_category)
                << "Could not configure seats: " << lightdm_cmd.c_str();
        throw "Could not configure seats: " + lightdm_cmd;
    }
}

void Display_manager::add_seats(int count) {
    for (int idx = 1; idx <= count; ++idx) {
        add_seat(idx);
    }
}
