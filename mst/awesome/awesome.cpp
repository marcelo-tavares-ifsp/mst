#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string>
#include <regex>

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

static void _print_xephyr(ostream& os, const Awesome& config)
{
    for (int idx = 0; idx < config.seats.size(); idx++)
    {
        string mouse_dev = "/dev/input/by-path/" + config.seats[idx].mouse;
        string keybd_dev = "/dev/input/by-path/" + config.seats[idx].keyboard;
        os << string("os.execute(\"sudo Xephyr -softCursor -ac -br ")
           << " -mouse \'evdev,5,device=" << mouse_dev << "\'"
           << " -keybd \'evdev,,device="  << keybd_dev << "\'"
           << " -screen "
           << config.seats[idx].width      << string("x")
           << config.seats[idx].height     << string(" :")
           << idx + 1                      << string(" &\")")   << endl;
    }
}

static void _print_unclutter(ostream& os, const Awesome& config)
{
    os << string("os.execute(\"unclutter &\")") << endl;
}

static void _print_script(ostream& os, const Awesome& config)
{
    // TODO: 10s waiting seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop.
    os << "os.execute(\"sleep 10; "
       << "sudo /usr/local/bin/mst-start-dm "
       << config.seats.size() << " &\")" << endl;
}

string Awesome::make_rules()
{
    string rules;
    for (int idx = 1; idx <= seats.size(); idx++)
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

ostream& operator << (ostream& os, const Awesome& config)
{
    _print_xephyr(os, config);
    _print_unclutter(os, config);
    _print_script(os, config);
    return os;
}
