/* dsv.cpp -- Ad-Hoc implementation of DSV parser/writer.
 *
 * Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <string>
#include <iostream>
#include <fstream>
#include <map>

#include "dsv.h"
#include "utils.h"

using namespace std;

static void _parse_x(ifstream& in, map<string, string>& out)
{
    for (string line = ""; getline(in, line); )
    {
        vector<string> data = split(line, ':');
        out[data[0]] = data[1];
    }
}

DSV::DSV(string file_name)
    : file_name(file_name)
{
    ifstream in(file_name);
    if (! in.is_open())
    {
        throw "Could not open config file: " + file_name;
    }

    _parse_x(in, data);
    in.close();
}

/**
 * @brief DSV::get -- get a parameter value from a config.
 * @param name -- a parameter name to search for.
 * @return the parameter value.
 */
const string DSV::get(const string name) const
{
    return data.at(name);
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
    ofstream out(file_name, ios::trunc);
    for (auto const& record : data)
    {
        out << record.first << ":" << record.second << endl;
    }
}
