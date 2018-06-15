#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <vector>

#include "interface_settings.h"

using namespace std;

namespace Ui {
class MainWindow;
}

struct Desktop
{
    string mouse;
    string interface;
    string keyboard;
    Resolution resolution;
};

struct Resolution
{
    int width;
    int heigth;
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    vector<Desktop> global_desktops;
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private slots:

    void on_btn_next_2_clicked();

    void on_btn_next_1_clicked();

    void on_btn_next_3_clicked();

private:
    Ui::MainWindow *ui;
};

#endif // MAINWINDOW_H
