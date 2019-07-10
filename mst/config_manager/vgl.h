#ifndef VGL_H
#define VGL_H

#include "command_manager/commandmanager.h"
#include <QLoggingCategory>
#include <string>
#include <stdlib.h>

Q_DECLARE_LOGGING_CATEGORY(vgl_category)

class VGL
{
public:
    VGL();

    static void configure();
    static void unconfigure();
};

#endif // VGL_H
