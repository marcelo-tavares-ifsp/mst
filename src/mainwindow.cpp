#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "controller_mst.h"
#include "settings-mst.h"
#include "utils.h"

#include <QString>

using namespace std;

int Seat::width;
int Seat::heigth;



MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->stackedWidget->setCurrentIndex(0);
}

MainWindow::~MainWindow()
{
    delete ui;
}

static void add_resolutions_to_cb(vector<string> resol, QComboBox *cb)
{
    for(string s : resol)
    {
        cb->addItem(QString::fromStdString(s));
    }
}

void get_resolution(QComboBox *cb_resolution, QListWidget *lw_interface)
{
    if (cb_resolution->count() != 0 || lw_interface->count() != 0)
    {
        return;
    }

    lw_interface->addItem(QString::fromStdString("test_3")); // test

    vector<Xrandr_monitor> xm = Settings_mst::parse_xrandr();
    int xm_size = xm.size();
    vector<string> resol;
    vector<string>::iterator it;

    if(xm_size > 1)
    {
        for (int idx = 1; idx < xm_size; idx++)
        {
            sort(resol.begin(), resol.end());
            sort(xm[idx].resolutions.begin(), xm[idx].resolutions.end());
            it = set_intersection(resol.begin(), resol.end(),
                                  xm[idx].resolutions.begin(), xm[idx].resolutions.end(),
                                  resol.begin());
            resol.resize(it-resol.begin());
        }
    }
    else
    {
        lw_interface->addItem(QString::fromStdString(xm[0].interface));
        resol = xm[0].resolutions;
    }

    add_resolutions_to_cb(resol, cb_resolution);

    lw_interface->addItem(QString::fromStdString("test_1")); // test
    lw_interface->addItem(QString::fromStdString("test_5")); // test
}

void MainWindow::on_btn_next_1_clicked()
{
    get_resolution(ui->cb_resolution, ui->lw_interface);

    ui->stackedWidget->setCurrentIndex(1);
}

void MainWindow::on_btn_next_2_clicked()
{
    //save_resolution(ui);
    QString Qtmp = ui->cb_resolution->currentText();
    string tmp = Qtmp.toUtf8().constData();
    vector<string> strs = split(tmp, 'x');

    Seat::width = atoi(strs[0].c_str());
    Seat::heigth = atoi(strs[1].c_str());




    if (global_seats.size() == 0 || check_size != ui->lw_interface->selectedItems().count())
    {
        global_seats.clear();
        for (auto item : ui->lw_interface->selectedItems())
        {
            Seat seat;
            seat.interface = item->text().toUtf8().constData();
            global_seats.push_back(seat);
        }
        check_size = global_seats.size();
    }

    ////////////////////////////////////////////////////////////

    for (auto widget : widgets)
    {
        ui->gridLayout->removeWidget(widget);
    }

    widgets.clear();

    int sz = global_seats.size();
    for (int idx = 0; idx < sz; ++idx) {
        QPushButton *Qpb = new QPushButton("btn" + idx, this);
        Qpb->setText(QString::fromStdString(global_seats[idx].interface));
        widgets.push_back(Qpb);
        cout << idx << endl;
    }

    for (auto widget : widgets)
    {
        ui->gridLayout->addWidget(widget);
    }

    ui->stackedWidget->setCurrentIndex(2);
}

void MainWindow::on_btn_next_3_clicked()
{


    cout << "The End!" << endl;
}

void MainWindow::on_btn_back_2_clicked()
{
    ui->stackedWidget->setCurrentIndex(1);
}

void MainWindow::on_btn_back_1_clicked()
{
    ui->stackedWidget->setCurrentIndex(0);
}

////////static functions///////////////////////////////////////////////////

static void save_resolution(Ui::MainWindow *ui)
{
    QString Qtmp = ui->cb_resolution->currentText();
    string tmp = Qtmp.toUtf8().constData();
    vector<string> strs = split(tmp, 'x');

    Seat::width = atoi(strs[0].c_str());
    Seat::heigth = atoi(strs[1].c_str());
}
