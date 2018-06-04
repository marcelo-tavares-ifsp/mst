#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "controller_mst.h"
#include "settings-mst.h"

#include <QString>

using namespace std;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    ui->btn_next_2->hide();
    ui->cb_resolution->hide();
    ui->lw_inter->hide();
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

void get_resolution(QComboBox *cb_resol, QListWidget *lw_inter)
{
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
        lw_inter->addItem(QString::fromStdString(xm[0].interface));
        resol = xm[0].resolutions;
    }

    add_resolutions_to_cb(resol, cb_resol);
}

void MainWindow::on_btn_next_1_clicked()
{
    ui->lbl_hello->hide();
    ui->btn_next_1->hide();

    ui->lw_inter->show();
    ui->btn_next_2->show();
    ui->cb_resolution->show();

    get_resolution(ui->cb_resolution, ui->lw_inter);
}
