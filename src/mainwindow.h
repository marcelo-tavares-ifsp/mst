#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"
#include "controller_mst.h"
#include "settings-mst.h"
#include "utils.h"

#include <QString>
#include <set>
#include <algorithm>

using namespace std;

namespace Ui {
class MainWindow;
}

class Seat
{
public:
    string mouse;
    string interface;
    string keyboard;
    static int width;
    static int heigth;
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    vector<QWidget *> widgets;
    vector<Seat> global_seats;
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private slots:

    void on_btn_next_2_clicked();

    void on_btn_next_1_clicked();

    void on_btn_next_3_clicked();

    void on_btn_back_2_clicked();

    void on_btn_back_1_clicked();

    void on_interface_clicked();

private:
    Ui::MainWindow *ui;

    void clear_layout();
    void fill_layout();
    void fill_global_seats();
    void get_resolution();
    void save_resolution();
    bool is_layout_changed(vector<Seat> seats,
                                  QList<QListWidgetItem*> list);
};

#endif // MAINWINDOW_H
