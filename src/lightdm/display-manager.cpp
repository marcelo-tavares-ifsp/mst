#include "display-manager.h"

#include <stdlib.h>

using namespace std;

DisplayManager::DisplayManager(string config_path) : config_path(config_path) {

}

static void _configure_x11() {
    system("xset -dpms");
    system("xset s off");
}

void DisplayManager::start() {
    string lightdm_cmd = "/usr/sbin/lightdm --config " + config_path;
    system(lightdm_cmd.c_str());
    _configure_x11();
}

void DisplayManager::add_seat(int seat_number) {
    string lightdm_cmd = "/usr/bin/dm-tool add-local-x-seat " + seat_number;
    system(lightdm_cmd.c_str());
}

void DisplayManager::add_seats(int count) {
    for (int idx = 1; idx <= count; ++idx) {
        add_seat(idx);
    }
}
