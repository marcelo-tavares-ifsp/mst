#ifndef TEMPLATE_H
#define TEMPLATE_H

#include <string>
#include <QFile>
#include <QMap>

using namespace std;

/**
 * @brief The Template_error class -- describes a generic template error.
 */
class Template_error : public runtime_error {
public:
    Template_error(QString what)
        : runtime_error(what.toStdString()) {
        // Do nothing.
    }
};

class Template
{
public:
    static const QString TEMPLATE_BEGIN;
    static const QString TEMPLATE_END;

    Template() {
        /* Do nothing. */
    }
    Template(QFile &file);
    Template(const QString& template_string);
    Template(char const * template_string);

    Template& set(const QString& key, const QString& value);
    QString substitute();
    void substitute(QFile &output_file);
    void substitute(const QString& output_file_name);


private:
    QMap<QString, QString> substitutions;
    QString template_string;
};

#endif // TEMPLATE_H
