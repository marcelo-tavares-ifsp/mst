#include <string>
#include <iostream>
#include <fstream>
#include <map>

#include "dsv.h"
#include "../utilites/utilites.h"

using namespace std;

static void _parse_x(ifstream& in, map<string, string>& out)
{
    for (string line = ""; getline(in, line);) {
        vector<string> data = split(line, ':');
        out[data[0]] = data[1];
    }
}

/**
 * @brief DSV::DSV
 * @param file_name
 * @throws DSV_exception
 */
DSV::DSV(const QString& file_name)
    : file_name(file_name)
{
    ifstream in(file_name.toStdString());
    if (! in.is_open()) {
        throw DSV_exception("Could not open config file: "
                            + file_name.toStdString());
    }

    _parse_x(in, data);
    in.close();
}

/**
 * @brief DSV::get -- get a parameter value from a config.
 * @param name -- a parameter name to search for.
 * @return the parameter value.
 */
const QString DSV::get(const QString& name) const
{
    return QString::fromStdString(data.at(name.toStdString()));
}

/**
 * @brief DSV::put -- put a new parameter value to a config.
 * @param name -- name of the parameter.
 * @param value -- value of the parameter.
 */
void DSV::put(string name, string value)
{
    data[name] = value;
}

/**
 * @brief DSV::save -- save DSV data to a file.
 *
 * XXX: This method does not escape special characters.
 */
void DSV::save()
{
    ofstream out(file_name.toStdString(), ios::trunc);
    for (auto const& record : data) {
        out << record.first << ":" << record.second << endl;
    }
}
