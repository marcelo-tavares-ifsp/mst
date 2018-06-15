#include "utils.h"

using namespace std;

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

void trim(char *s)
{
     int i=0,j;
     while((s[i]==' ')||(s[i]=='\t'))
     {
          i++;
     }
     if(i>0)
     {
          for(j=0; j < strlen(s); j++)
          {
              s[j]=s[j+i];
          }
          s[j]='\0';
     }
     i=strlen(s)-1;
     while((s[i]==' ')||(s[i]=='\t'))
     {
          i--;
     }
     if(i < (strlen(s)-1))
     {
          s[i+1]='\0';
     }
}
