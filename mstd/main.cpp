#include <QCoreApplication>

#include <signal.h>
#include <libudev.h>

void signal_handler(int sig)
{
    switch (sig) {
    case SIGHUP:
    case SIGTERM:
        exit(0);
    }
}

void init_daemon()
{
    if (getppid() == 1) {
        return; // Already a daemon
    }

    pid_t pid = fork();
    if (pid < 0) {
        exit(1);
    } else if (pid > 0) {
        exit(0);
    }

    setsid();

    signal(SIGCHLD,SIG_IGN); /* ignore child */
    signal(SIGTSTP,SIG_IGN); /* ignore tty signals */
    signal(SIGTTOU,SIG_IGN);
    signal(SIGTTIN,SIG_IGN);

    signal(SIGHUP,signal_handler); /* catch hangup signal */
    signal(SIGTERM,signal_handler); /* catch kill signal */
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    return a.exec();
}
