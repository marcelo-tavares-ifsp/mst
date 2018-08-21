#ifndef VGL_H
#define VGL_H

#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(vgl_category)

class Vgl
{
public:
    Vgl();

    static void configure();
    static void unconfigure();
};

#endif // VGL_H
