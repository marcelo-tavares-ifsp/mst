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

bool Contains(vector<string> xm, string s)
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

void get_resolution(QComboBox *cb_resol, QListWidget *lw_inter)
{
    vector<Xrandr_monitor> xm = Settings_mst::parse_xrandr();
    int xm_size = xm.size();
    vector<string> resol;
    bool flag = true;


    if(xm_size == 1)
    {
        for(int i = 0; i < xm_size; i++)
        {

            int xm_resolv_size = xm[i].resolutions.size();
            for(int n = 0; n < xm_resolv_size; n++)
            {

                flag = true;
                for(int j = 0; j < xm_size - 1; j++)
                {
                    if(!(Contains(xm[j + 1].resolutions, xm[i].resolutions[n])))
                    {
                        flag = false;
                    }
                }

                if(flag && !(Contains(resol, xm[i].resolutions[n])))
                {
                    resol.push_back(xm[i].resolutions[n]);
                }
            }
            lw_inter->addItem(QString::fromStdString(xm[i].interface));
        }
    }
    else
    {
        lw_inter->addItem(QString::fromStdString(xm[0].interface));
        resol = xm[0].resolutions;
    }



    for(int i = 0; i < resol.size(); i++)
    {
        cb_resol->addItem(QString::fromStdString(resol[i]));
    }

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
