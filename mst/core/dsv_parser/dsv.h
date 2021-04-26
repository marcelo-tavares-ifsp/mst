#ifndef DSV_H
#define DSV_H

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
    DSV(std::string file_name);
    void put(std::string key, std::string value);
    const std::string get(const std::string key) const;
    void save();

private:
    std::string file_name;
    std::map<std::string, std::string> data;
};

#endif // DSV_H
