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
    for(int i = 0; i < xm.size(); i++)
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
 * @brief is_pam_mkhomedir_used -- check if a PAM module 'pam_mkhomedir' is
 *      used (that is, whether or not '/etc/skel' is copied to users home
 *      directories.)
 * @return 'true' if the PAM module is used, 'false' otherwise.
 */
bool is_pam_mkhomedir_used()
{
    char cmd[58] = "[ -d /etc/skel ] && [ $(ls -a1 /etc/skel | wc -l) -gt 2 ]";
    return system(cmd) == 0;
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

/**
 * @brief _parse_resolution -- parse resolution in "WIDTHxHEIGTH" format.
 * @param resolution -- resolution string.
 * @return vector with the 1st element set to display width and the 2nd
 *      set to the heigth.
 */
vector<int> _parse_resolution(QString resolution)
{
    vector<string> strs = split(resolution.toUtf8().constData(), 'x');
    return {  atoi(strs[0].c_str()), atoi(strs[1].c_str()) };
}

string to_std_string(QString qs)
{
    return qs.toUtf8().constData();
}

QString to_qstring(string str)
{
    return QString::fromStdString(str);
}
