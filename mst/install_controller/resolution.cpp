#include "resolution.h"

#include <vector>
#include <QPair>
#include <string>

using namespace std;

/**
 * @brief _parse_resolution -- parse resolution in "WIDTHxHEIGTH" format.
 * @param resolution -- resolution string.
 * @return QPair with the 1st element set to display width and the 2nd
 *      set to the heigth.
 */
QPair<int, int> Resolution::parse_string(QString resolution)
{
    QStringList list = resolution.split("x");
    return { list.at(0).toInt(), list.at(0).toInt() };
}

Resolution::Resolution() {
    this->resolution.first  = 0;
    this->resolution.second = 0;
}

Resolution::Resolution(const Resolution &other) {
    this->resolution.first  = other.get_width();
    this->resolution.second = other.get_height();
}

Resolution::Resolution(QString resolution_string)
{
    this->resolution = parse_string(resolution_string);
}

Resolution::Resolution(int width, int height) {
    this->resolution.first  = width;
    this->resolution.second = height;
}

int Resolution::get_width() const {
    return resolution.first;
}

int Resolution::get_height() const {
    return resolution.second;
}

QPair<int, int> Resolution::get_pair() const {
    return resolution;
}
