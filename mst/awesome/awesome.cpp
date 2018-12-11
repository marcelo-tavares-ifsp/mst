#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string>
#include <regex>
#include <fstream>

#include <QLoggingCategory>

#include "awesome.h"

Q_LOGGING_CATEGORY(awesome_category, "mst.awesome")

using namespace std;

Awesome::Awesome(vector<Seat> seats) : seats(seats)
{
}

/**
 * @brief Awesome::get_version -- get Awesome version.
 * @throws an error message on error.
 * @return awesome version as a string.
 */
const string Awesome::get_raw_version()
{
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* f = popen("awesome --version", "r");
    if (f != NULL)
    {
        if (fgets(buf, BUF_SZ, f) != NULL)
        {
            return string(buf);
        }
    }
    else
    {
        const string msg = "Could not get Awesome version.";
        qCritical(awesome_category) << msg.c_str();
        throw msg;
    }
    return NULL;
}

const vector<int> Awesome::get_version()
{
    const string raw_version = get_raw_version();
    regex r1("awesome v([0-9]+).([0-9]+)*");
    smatch sm;
    if (regex_search(raw_version, sm, r1))
    {
        vector<int> result(2);
        result[0] = atoi(string(sm[1]).c_str());
        result[1] = atoi(string(sm[2]).c_str());
        return result;
    }
    else
    {
        const string msg = "Could not parse Awesome version: " + raw_version;
        qCritical(awesome_category) << msg.c_str();
        throw msg;
    }
}

static string _make_xephyr_autostart(const Awesome& config)
{
    stringstream result;

    for (uint32_t idx = 0; idx < config.seats.size(); idx++)
    {
        string mouse_dev = "/dev/input/by-path/" + config.seats[idx].mouse;
        string keybd_dev = "/dev/input/by-path/" + config.seats[idx].keyboard;
        result << "os.execute(\"sudo Xephyr -softCursor -ac -br "
               << " -mouse \'evdev,5,device=" << mouse_dev << "\'"
               << " -keybd \'evdev,,device=" << keybd_dev << "\'"
               << " -screen "
               << config.seats[idx].width  << "x"
               << config.seats[idx].height << " :"
               << idx + 1 << " &\")" << endl;
    }
    return result.str();
}

string Awesome::make_rules()
{
    string rules;
    for (uint32_t idx = 1; idx <= seats.size(); idx++)
    {
        rules += string("{ rule = { class = \"Xephyr\", name = \"Xephyr on :");
        rules += to_string(idx);
        rules += string(".0 (ctrl+shift grabs mouse and keyboard)\" },\n ");
        rules += string("properties = { floating = true, fullscreen = true, screen = ");
        rules += to_string(idx);
        rules += string("} },\n");
    }
    return rules;
}

string Awesome::make_autostart()
{
    string result;
    result += _make_xephyr_autostart(*this);
    result += "os.execute(\"unclutter &\")\n";

    // TODO: 10s waiting seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop.
    result += (string) "os.execute(\"sleep 10; "
            + "sudo /usr/local/bin/mst-start-dm "
            + to_string(seats.size()) + " &\")\n";

    return result;
}
