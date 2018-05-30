#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "controller_mst.h"
#include "settings-mst.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    ui->btn_next_2->hide();

}

bool Contains(Xrandr_monitor xm, string s)
{
    for(int i = 0; i < xm.resolutions.size(); i++)
    {
        if(xm.resolutions[i] == s)
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

void MainWindow::on_pushButton_clicked()
{
    ui->lbl_hello->hide();
    ui->btn_next_1->hide();

    ui->btn_next_2->show();
    ui->cb_1->show();
    ui->cb_2->show();
    ui->cb_3->show();
    ui->cb_resolution->show();

    vector<Xrandr_monitor> xm = Settings_mst::parse_xrandr();
    ui->cb_1->text(xm[0].interface);
    ui->cb_2->text(xm[1].interface);

    for(int i = 0; i < xm[0].resolutions.size(); i++)
    {
        if(Contains(xm[1], xm[0].resolutions[i]))
        {
            ui->cb_resolution->addItem(xm[0].resolutions[i]);
        }
    }
}
