#ifndef TEMPLATE_H
#define TEMPLATE_H

#include <string>
#include <QFile>
#include <QMap>

using namespace std;

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
    string replace_all(const string& tpl,
                       const string& val);

    Template& set(const QString& key, const QString& value);
    string substitute();
    void substitute(QFile &output_file);
    void substitute(const QString& output_file_name);


private:
    QMap<QString, QString> substitutions;
    QString template_string;
};

#endif // TEMPLATE_H
