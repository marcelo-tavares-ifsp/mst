#include "template_manager.h"

#include <string>
#include <QFile>

using namespace std;

const string Template_manager::TEMPLATE_FILE_EXTENSION = ".template";


//Template_manager::Template_manager(const string& templates_dir)
//    : templates_dir(templates_dir)
//{
//    /* Do nothing */
//}

void Template_manager::set_template_dir(const string &templates_dir)
{
    this->templates_dir = templates_dir;
}

/**
 * @brief Template_manager::get_template -- Get a Template instance by a name.
 * @param name -- Template name to search.
 * @return A new Template instance.
 */
Template Template_manager::get_template(const string& name)
{
    string file_name = templates_dir + "./" + name
            + Template_manager::TEMPLATE_FILE_EXTENSION;
    QFile file(QString::fromStdString(file_name));
    return Template(file);
}

/**
 * @brief Template_manager::get_instance -- Get the Template_manager instance.
 * @return -- A static Template_manager instance.
 */
Template_manager* Template_manager::get_instance()
{
    static Template_manager* instance = NULL;
    if (instance == NULL) {
        instance = new Template_manager();
    }
    return instance;
}
