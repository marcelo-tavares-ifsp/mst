#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"
#include "controller.h"
#include "settings.h"
#include "input-device-listener.h"
#include "utils.h"
#include "seat_calibration_dialog.h"
#include "controller.h"
#include "seat.h"
#include "input-device-listener.h"

#include <QThreadPool>
#include <QString>
#include <QObject>
#include <QDialog>
#include <QMessageBox>
#include <algorithm>
#include <QListWidgetItem>
#include <QMainWindow>
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(main_window_category)

using namespace std;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    vector<QWidget *> widgets;
    vector<Seat> global_seats;
    vector<string> list_mice;
    vector<string> list_keybs;
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

public Q_SLOTS:

    void on_btn_next_2_clicked();

    void on_btn_next_1_clicked();

    void on_btn_next_3_clicked();

    void on_btn_back_2_clicked();

    void on_btn_back_1_clicked();

    void on_interface_clicked();

    void set_seat_device(QString, int);

    void on_btn_next_4_clicked();

    void on_stop_mst_clicked();
    
    void on_install_button_clicked();

private slots:
    void on_pushButton_clicked();

private:
    Ui::MainWindow *ui;

    bool check_collision_seats();
    bool check_fill_seats();
    void clear_layout();
    void fill_layout();
    void fill_global_seats();
    void get_resolution();
    void save_resolution();
    bool is_layout_changed(vector<Seat> seats,
                                  QList<QListWidgetItem*> list);
};

#endif // MAINWINDOW_H
