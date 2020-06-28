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
   QPair<int, int> get_pair() const;

   static QPair<int, int> parse_string(QString resolution_string);

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
