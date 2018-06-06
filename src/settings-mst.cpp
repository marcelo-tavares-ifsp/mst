#include "settings-mst.h"

using namespace std;

Settings_mst::Settings_mst()
{

}

static void trim(char *s)
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

vector<string> Settings_mst::run_xrandr()
{
    static const char *COMMAND = "xrandr";
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* file;

    if ((file = popen(COMMAND, "r")) != NULL)
    {
        vector<string> result(10);
        while (fgets(buf, BUF_SZ, file) != NULL)
        {
            trim(buf);
            result.push_back(buf);
        }
        pclose(file);
        return result;
    }
    else
    {
        throw "Could not execute xrandr";
    }
}

vector<Xrandr_monitor> Settings_mst::parse_xrandr()
{
    vector<string> data = run_xrandr();
    vector<Xrandr_monitor> result;
    regex r1("^(.*) connected(.*)\n");
    regex r2("^([0-9]+x[0-9]+).*\n");
    int state = 0;
    Xrandr_monitor monitor;
    smatch sm;

    for (string line : data)
    {
        if (line.length() == 0)
            continue;

        switch (state)
        {
        case 0:
            if (regex_match(line, sm, r1))
            {
                monitor.interface = sm[1];
                state = 1;
            }
            break;
        case 1:
            if (regex_match(line, sm, r2))
            {
                monitor.resolutions.push_back(sm[1]);
            }
            else
            {
                result.push_back(monitor);
                monitor.interface = "";
                monitor.resolutions.clear();
                state = 0;
            }
            break;
        }
    }
    result.push_back(monitor);
    return result;
}



vector<string> Settings_mst::run_ls_devices()
{
    static const char *COMMAND = "ls /dev/input/by-path/";
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* file;

    if ((file = popen(COMMAND, "r")) != NULL)
    {
        vector<string> result(20);
        while (fgets(buf, BUF_SZ, file) != NULL)
        {
            trim(buf);
            result.push_back(buf);
        }
        pclose(file);
        return result;
    }
    else
    {
        throw "Could not execute ls /dev/input/by-path/";
    }
}

void Settings_mst::parse_ls_devices(vector<string> *mice, vector<string> *keybds)
{
    vector<string> data = run_ls_devices();
    regex r1("^(.*-event-kbd)\n$");
    regex r2("^(.*-event-mouse)\n$");
    smatch sm;

    for (string line : data)
    {
        if (line.length() == 0)
            continue;



        if(regex_match(line, sm, r1))
        {
            keybds->push_back(sm[1]);
        }

        if(regex_match(line, sm, r2))
        {
            mice->push_back(sm[1]);
        }
    }
}

/**
 * @brief open_input_dev -- Open an input device file.
 * @param name -- Name of the input device file;
 * @return Opened file in the read-only mode.
 */
static FILE* open_input_dev(string name) {
    string str = "cat '/dev/input/by-path/" + name + "'";
    static const char *COMMAND = str.c_str();
    FILE* file = popen(COMMAND, "r");
    if (file == NULL) {
        throw "Could not execute 'cat /dev/input/by-path/" + name + "'";
    }

    return file;
}

void Settings_mst::loop_answer(vector<string> data)
{
    for (auto line : data)
    {
        const int BUF_SZ = 10;
        char buf[BUF_SZ];
        FILE* file = open_input_dev(line);

        while (fgets(buf, BUF_SZ, file) != NULL)
        {
            cout << buf;
        }
        pclose(file);
    }
}
