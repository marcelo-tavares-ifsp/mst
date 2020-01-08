#include "display_manager.h"

Q_LOGGING_CATEGORY(display_manager_category, "mst.dm")

Display_manager::Display_manager()
{

}

static void _configure_x11() {
    if (CommandManager::xset_dpms() || CommandManager::xset_soff())
    {
        qCritical(display_manager_category) << "Could not configure X11.";
        throw "Could not configure X11.";
    }
}

void Display_manager::start() {
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
