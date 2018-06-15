#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "controller_mst.h"
#include "settings-mst.h"
#include "utils.h"
#include "interface_settings.h"

#include <QString>
// #include <boost/algorithm/string.hpp>

using namespace std;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    ui->btn_next_3->hide();
    ui->btn_next_2->hide();
    ui->cb_resolution->hide();
    ui->lw_interface->hide();
    ui->lbl_resolution->hide();
    ui->lbl_interface->hide();
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
//    get_resolution(ui->cb_resolution, ui->lw_interface);

//    ui->lbl_hello->hide();
//    ui->btn_next_1->hide();

    interface_settings is;
    is.show();
    this->hide();

//    ui->lw_interface->show();
//    ui->btn_next_2->show();
//    ui->cb_resolution->show();
//    ui->lbl_resolution->show();
//    ui->lbl_interface->show();
}

void MainWindow::on_btn_next_2_clicked()
{
     QString tmpS = ui->cb_resolution->currentText();
     string tmp = tmpS.toUtf8().constData();
     vector<string> strs = split(tmp, 'x');

//     global_desktops.re
//     global_desktops.resolution.width = atoi(strs[0].c_str());
//     global_desktops.resolution.heigth = atoi(strs[1].c_str());

    QList<QListWidgetItem *> selected_items = ui->lw_interface->selectedItems();
    int i = 0;
    for (auto item : selected_items)
    {
        Desktop desktop;
        desktop.interface = item->text().toUtf8().constData();
        global_desktops.push_back(desktop);
    }

    for (int i = 0; i < global_desktops.size(); i++)
    {
        cout << global_desktops[i].interface << endl;
    }

    ui->btn_next_2->hide();
    ui->cb_resolution->hide();
    ui->lw_interface->hide();
    ui->lbl_resolution->hide();
    ui->lbl_interface->hide();

    ui->btn_next_3->show();
}

void MainWindow::on_btn_next_3_clicked()
{


    cout << "The End!" << endl;
}
