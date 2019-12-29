#ifndef TEMPLATE_MANAGER_H
#define TEMPLATE_MANAGER_H

#include <string>
#include "template.h"

using namespace std;

class Template_manager
{
public:
    static const string TEMPLATE_FILE_EXTENSION;

    Template get_template(const string& name);

    static Template_manager* get_instance();
    void set_template_dir(const string& templates_dir);

private:
    string templates_dir;
};

#endif // TEMPLATE_MANAGER_H
