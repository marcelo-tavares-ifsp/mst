#include "utilites.h"

vector<string> split(string input, char separator) {
    int p = input.find(separator);
    vector<string> result;
    result.push_back(input.substr(0, p));
    result.push_back(input.substr(p + 1));
    return result;
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


