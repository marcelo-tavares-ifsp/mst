#ifndef RESOLUTION_H
#define RESOLUTION_H

#include <QObject>

using namespace std;

class Resolution
{
public:
   Resolution();
   Resolution(const Resolution& other);
   Resolution(QString resolution_string);
   Resolution(int width, int height);
   int get_width() const;
   int get_height() const;

   /**
    * @brief get_pair -- get resolution width and height as a QPair.
    * @return QPair
    */
   QPair<int, int> get_pair() const;

   /**
    * @brief parse_string -- parse a common resolution string (e.g. "640x480".)
    * @param resolution_string
    * @return QPair with width and height of the resolution.
    */
   static QPair<int, int> parse_string(QString resolution_string);

   QString to_string() const {
       return QString::number(resolution.first)
               + "x" + QString::number(resolution.second);
   }

   operator std::string() const {
       return resolution.first + "x" + resolution.second;
   }

   Resolution& operator=(const Resolution& rhs) {
       resolution.first = rhs.get_width();
       resolution.second = rhs.get_height();
       return *this;
   }

private:
   QPair<int, int> resolution;
};

#endif // RESOLUTION_H
