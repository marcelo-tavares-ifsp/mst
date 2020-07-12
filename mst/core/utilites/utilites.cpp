#include "utilites.h"

vector<string> split(string input, char separator) {
    int p = input.find(separator);
    vector<string> result;
    result.push_back(input.substr(0, p));
    result.push_back(input.substr(p + 1));
    return result;
}

bool contains(vector<string> xm, string s)
{
    for(string::size_type i = 0; i < xm.size(); i++)
    {
        if(xm[i] == s)
        {
            return true;
        }
    }
    return false;
}

/**
 * @brief ltrim -- trim a string from the beginning.  The input
 *                  string is modified.
 * @param s -- an input string.
 */
static inline void ltrim_x(string &s) {
    s.erase(s.begin(), find_if(s.begin(), s.end(), [](int ch) {
        return ! isspace(ch);
    }));
}

/**
 * @brief rtrim -- trim a string from the end.  The input string is modified.
 * @param s -- an input string.
 */
static inline void rtrim_x(string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return ! isspace(ch);
    }).base(), s.end());
}

/**
 * @brief trim -- trim an input C string STR on both sides.
 * @param str -- an input string.
 * @return a new C++ string.
 */
string trim(char* str) {
    string result(str);
    ltrim_x(result);
    rtrim_x(result);
    return result;
}

/**
 * @brief _set_intersection -- Get intersection of two vectors, return the
 *          resulting vector iterator.
 * @param v1 -- The first input vector.
 * @param v2 -- The seconds input vector.
 * @param r  -- The resulting vector.
 * @return -- A vector iterator.
 */
vector<string>::iterator _set_intersection(const vector<string>& v1,
                                           const vector<string>& v2,
                                           vector<string>& output)
{
    return set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(),
                            output.begin());
}

void _set_intersection_x(vector<string>& v1, vector<string>& v2,
                         vector<string>& output, function<bool(const string&, const string&)> sort_function) {
    sort(v1.begin(), v1.end(), sort_function);
    sort(v2.begin(), v2.end(), sort_function);
    auto it = _set_intersection(v1, v2, output);
    output.resize(it->size());

}

string to_std_string(QString qs)
{
    return qs.toUtf8().constData();
}

QString to_qstring(string str)
{
    return QString::fromStdString(str);
}