#ifndef DSV_H
#define DSV_H

#include <map>
#include <string>

using namespace std;

class DSV_exception : public runtime_error
{
public:
    DSV_exception(const string& what)
        : runtime_error(what)
    {
        /* Do nothing */
    }
};

class DSV
{
public:
    DSV(string file_name);
    void put(string key, string value);
    const string get(const string key) const;
    void save();

private:
    string file_name;
    map<string, string> data;
};

#endif // DSV_H
