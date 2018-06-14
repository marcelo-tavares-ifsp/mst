#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "controller_mst.h"
#include "settings-mst.h"

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

void get_resolution(QComboBox *cb_resolution, QListWidget *lw_interface)
{
    lw_interface->addItem(QString::fromStdString("test_3"));
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

    lw_interface->addItem(QString::fromStdString("test_1"));
    lw_interface->addItem(QString::fromStdString("test_5"));
}

void MainWindow::on_btn_next_1_clicked()
{
    ui->lbl_hello->hide();
    ui->btn_next_1->hide();

    ui->lw_interface->show();
    ui->btn_next_2->show();
    ui->cb_resolution->show();
    ui->lbl_resolution->show();
    ui->lbl_interface->show();

    get_resolution(ui->cb_resolution, ui->lw_interface);
}

void MainWindow::on_btn_next_2_clicked()
{
    // QString tmpS = ui->cb_resolution->currentText();
    // string tmp = tmpS.toUtf8().constData;// split('x');
    // vector<string> strs;
    // boost::split(strs,tmp,boost::is_any_of("\t"));

    // global_resolution.width = atoi(tmp[0]);
    // global_resolution.heigth = atoi(tmp[1]);

//    QList<QListWidgetItem *> selected_items = ui->lw_interface->selectedItems();
//    global_desktops = new Desktop[ui->lw_interface->count()];
//    int i = 0;
//    for (auto item : selected_items)
//    {
//        global_desktops[i] = Desktop();
//        global_desktops[i].interface = item->text().toUtf8().constData();
//        i++;
//    }

//    for (int i = 0; i < global_desktops)
//    {
//        cout << item->interface << endl;
//    }

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
