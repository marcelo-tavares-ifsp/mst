#ifndef DSV_H
#define DSV_H

#include <QString>
#include <map>
#include <string>

class DSV_exception : public std::runtime_error
{
public:
    DSV_exception(const std::string& what)
        : runtime_error(what)
    {
        /* Do nothing */
    }
};

class DSV
{
public:
    DSV(const QString &file_name);
    void put(std::string key, std::string value);
    const QString get(const QString& key) const;
    void save();

private:
    QString file_name;
    std::map<std::string, std::string> data;
};

#endif // DSV_H
