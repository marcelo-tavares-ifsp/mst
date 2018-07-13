#include "utils.h"



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
 * @brief replace_all -- replace all occurences of a template TPL in a string
 *      INPUT with value VAL.
 * @param input -- an input string.
 * @param tpl -- a template to be replaced.
 * @param val -- a value to place instead of the template.
 * @return a string with inserted values.
 */
string replace_all(string input, const string& tpl, const string& val)
{
    string::size_type pos = 0;
    while ((pos = input.find(tpl, pos)) != string::npos)
    {
        input.replace(pos, tpl.size(), val);
        ++pos;
    }
    return input;
}

/**
 * @brief is_pam_mkhomedir_used -- check if a PAM module 'pam_mkhomedir' is
 *      used (that is, whether or not '/etc/skel' is copied to users home
 *      directories.)
 * @return 'true' if the PAM module is used, 'false' otherwise.
 */
bool is_pam_mkhomedir_used()
{
    char* cmd = "[ -d /etc/skel ] && [ $(ls -a1 /etc/skel | wc -l) -gt 2 ]";
    return system(cmd) == 0;
}
