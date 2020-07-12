#ifndef TEMPLATE_MANAGER_H
#define TEMPLATE_MANAGER_H

#include "types/template.h"

using namespace std;

class Template_manager
{
public:
    static const QString TEMPLATE_FILE_EXTENSION;

    Template get_template(const QString& name);

    static Template_manager* get_instance();
    void set_template_dir(const QString& templates_dir);

private:
    QString templates_dir;
};

#endif // TEMPLATE_MANAGER_H
