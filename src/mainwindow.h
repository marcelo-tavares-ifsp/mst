#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <vector>

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
    int check_size;
    vector<Seat> global_seats;
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private slots:

    void on_btn_next_2_clicked();

    void on_btn_next_1_clicked();

    void on_btn_next_3_clicked();

    void on_btn_back_2_clicked();

    void on_btn_back_1_clicked();

private:
    Ui::MainWindow *ui;

    //void save_resolution(Ui::MainWindow *ui);
};

#endif // MAINWINDOW_H
