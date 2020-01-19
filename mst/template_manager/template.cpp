#include "template.h"

#include <QTextStream>

using namespace std;

const QString Template::TEMPLATE_BEGIN = "{{";
const QString Template::TEMPLATE_END   = "}}";

/**
 * @brief Template::Template -- Create a template based on a string.
 * @param template_string -- A string to use.
 */
Template::Template(const QString& template_string)
    : template_string(template_string)
{
    /* Do nothing */
}

/**
 * @brief Template::Template -- Create a template based on a C string.
 * @param template_string -- A string to use.
 */
Template::Template(char const * template_string)
    : template_string(template_string)
{
    /* Do nothing */
}

/**
 * @brief Template::Template -- Create a template based on a file.
 * @param file -- A file to read.
 */
Template::Template(QFile& file)
    : template_string("")
{
    file.open(QFile::ReadOnly);
    QTextStream stream(&file);
    template_string.append(stream.readAll());
}

/**
 * @brief Template::substitute -- Substitute values.
 * @return A string with substituted values.
 */
QString Template::substitute()
{

    QString output(this->template_string);
    foreach (auto key, substitutions.keys()) {
        string::size_type pos = 0;
        QString pattern(Template::TEMPLATE_BEGIN + key
                        + Template::TEMPLATE_END);
        output.replace(pattern, substitutions[key]);
    }

    return output;
}

/**
 * @brief Template::substitute Substitute values and store data to a file.
 * @param output_file -- A file to use.
 */
void Template::substitute(QFile& output_file)
{
    output_file.open(QFile::WriteOnly | QIODevice::Text);
    output_file.write(substitute().toStdString().c_str());
    output_file.close();
}

/**
 * @brief Template::substitute Substitute values and store data to a file.
 * @param output_file_name -- A file name to use.
 */
void Template::substitute(const QString& output_file_name)
{
    QFile output_file(output_file_name);
    this->substitute(output_file);
}

/**
 * @brief Template::set -- Set a substitution for a value.
 * @param key -- Key to use.
 * @param value -- Value to use.
 * @return This template.
 */
Template& Template::set(const QString &key, const QString& value)
{
    this->substitutions[key] = value;
    return *this;
}
