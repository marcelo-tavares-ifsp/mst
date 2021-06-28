#include "resolution.h"

#include <vector>
#include <QPair>
#include <string>

using namespace std;

std::ostream& operator<< (std::ostream& os, const Resolution& resolution) {
    os << "#<Resolution " << resolution.to_string().toStdString() << ">";
    return os;
}

bool operator==(const Resolution& lhs, const Resolution& rhs) {
    return (lhs.get_height() == rhs.get_height())
            && (lhs.get_width() == rhs.get_width());
}

/**
 * @brief _parse_resolution -- parse resolution in "WIDTHxHEIGTH" format.
 * @param resolution -- resolution string.
 * @return QPair with the 1st element set to display width and the 2nd
 *      set to the heigth.
 * @throws Resolution_error when resolution format is malformed.
 */
QPair<int, int> Resolution::parse_string(QString resolution)
{
    static QRegExp regex("([0-9]+)x([0-9]+)");
    if (regex.exactMatch(resolution)) {
        regex.indexIn(resolution);
        return { regex.cap(1).toInt(), regex.cap(2).toInt() };
    } else {
        throw Resolution_error("Wrong format: " + resolution);
    }
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
