#include <QString>

#include "system.h"

#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"
#include "core/path_manager.h"
#include "core/platform.h"


using namespace sys;

System::System(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

void System::configure()
{
    component_configuration.add(BASHRC_FILE,
                                "{{home}}/.bashrc",
                                prepare_bashrc_template());

    component_configuration.add(GETTY_FILE,
                                "/lib/systemd/system/getty@.service",
                                prepare_getty_template());

    component_configuration.add(SEATS_CONFIG,
                                "/etc/mst-seats",
                                prepare_seat_configuration_template());
}

void System::enable()
{
    Platform::exec("systemctl enable mstd");
}

void System::disable()
{
    Platform::exec("systemctl disable mstd");
}

Template System::prepare_seat_configuration_template()
{
    Template seat_template("{{device_path}} {{seat_number}}");
    QString config_contents;
    for (shared_ptr<Seat> seat : config.get_seats()) {
        seat_template
                .set("device_path", seat->get_usb())
                .set("seat_number", QString::number(seat->get_id() + 1));
        config_contents += seat_template.substitute() + "\n";
    }

    return Template(config_contents);
}

Template sys::prepare_getty_template()
{
    const QString user = Path_manager::get_instance()->get_mst_user();
    Template tpl = Template_manager::get_instance()->get_template(GETTY_FILE);
    tpl.set("user", user);
    return tpl;
}

/**
 * @brief ConfigManager::make_bashrc -- Generate ".bashrc" file for multiseat
 *     user.
 */
Template sys::prepare_bashrc_template()
{
    Template bashrc_template = Template_manager::get_instance()->get_template(
                BASHRC_FILE);
    bashrc_template.set("tty", "1");
    return bashrc_template;
}
