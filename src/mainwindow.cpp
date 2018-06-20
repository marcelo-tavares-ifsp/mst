#include <QThreadPool>

#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "input-device-listener.h"

using namespace std;

int Seat::width;
int Seat::heigth;
vector<string> list_mice;
vector<string> list_keybs;



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

void MainWindow::on_btn_next_1_clicked()
{
    get_resolution();

    ui->stackedWidget->setCurrentIndex(1);
}

void MainWindow::on_btn_next_2_clicked()
{
    save_resolution();

    if (is_layout_changed(global_seats, ui->lw_interface->selectedItems()))
    {
        global_seats.clear();
        fill_global_seats();

        clear_layout();
        widgets.clear();

        fill_layout();
    }

    Settings_mst::parse_ls_devices(&list_mice, &list_keybs);

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

void MainWindow::on_interface_clicked()
{
    QPushButton *button = (QPushButton *) sender();
    string keyboard;
    string mouse;

//    while (1)
//    {
//        for (auto mouse : list_mice)
//        {
//            Settings_mst::loop_answer_mouse(mouse);
//        }
//        for (auto device : list_keybs)
//        {
//            Settings_mst::loop_answer(device);
//        }
//    }
    Input_device_listener listener(list_mice, Input_device_listener::MOUSE);
    QObject::connect(&listener, SIGNAL(device_found(string, Input_device_listener::DEVICE_TYPE)),
                     this, SLOT(set_seat_device(string, Input_device_listener::DEVICE_TYPE)));

    QThreadPool::globalInstance()->start(&listener);


    for (auto seat : global_seats)
    {
        if (seat.interface == button->text().toUtf8().constData())
        {
            seat.keyboard = keyboard;
            seat.mouse = mouse;
            break;
        }
    }

    cout << mouse << endl;
    cout << button->text().toUtf8().constData() << endl;
}

void MainWindow::set_seat_device(string device,
                                 Input_device_listener::DEVICE_TYPE type)
{
    cout << device << endl;
    cout << type << endl;
}



////////static functions///////////////////////////////////////////////////



void MainWindow::save_resolution()
{
    QString Qtmp = ui->cb_resolution->currentText();
    string tmp = Qtmp.toUtf8().constData();
    vector<string> strs = split(tmp, 'x');

    Seat::width = atoi(strs[0].c_str());
    Seat::heigth = atoi(strs[1].c_str());
}

void MainWindow::fill_layout()
{
    int sz = global_seats.size();
    for (int idx = 0; idx < sz; ++idx)
    {
        QPushButton *Qpb = new QPushButton("btn" + idx, this);
        Qpb->setText(QString::fromStdString(global_seats[idx].interface));
        connect(Qpb, SIGNAL(clicked()), this, SLOT(on_interface_clicked()));
        widgets.push_back(Qpb);
        ui->gridLayout->addWidget(Qpb);
    }
}

/**
 * @brief is_layout_changed -- predicate that checks if new list of seats
 *      differs from the previous setup.
 * @param seats -- old list of seats.
 * @param list -- new list of seats.
 * @return the result of comparison.
 */
bool MainWindow::is_layout_changed(vector<Seat> seats,
                              QList<QListWidgetItem*> list)
{
    bool result = true;
    if (! seats.empty())
    {
        vector<string> a;
        vector<string> b;
        vector<string> c;

        for (QListWidgetItem* elem : list)
        {
            a.push_back(elem->text().toUtf8().constData());
        }
        for (Seat s : seats)
        {
            b.push_back(s.interface);
        }

        set_symmetric_difference(a.begin(), a.end(),
                                 b.begin(), b.end(), back_inserter(c));
        result = c.size() > 0;
    }

    return result;
}

void MainWindow::fill_global_seats()
{
    for (auto item : ui->lw_interface->selectedItems())
    {
        Seat seat;
        seat.interface = item->text().toUtf8().constData();
        global_seats.push_back(seat);
    }
}

void MainWindow::clear_layout()
{
    for (auto widget : widgets)
    {
        ui->gridLayout->removeWidget(widget);
        delete widget;
    }
}

static void add_resolutions_to_cb(vector<string> resol, QComboBox *cb)
{
    for(string s : resol)
    {
        cb->addItem(QString::fromStdString(s));
    }
}

/**
 * @brief _set_intersection -- Get intersection of two vectors, return the
 *          resulting vector.
 * @param v1 -- The first input vector.
 * @param v2 -- The seconds input vector.
 * @param r  -- The resulting vector.
 * @return -- A vector iterator.
 */
vector<string>::iterator _set_intersection(vector<string> v1,
                                           vector<string> v2,
                                           vector<string> r)
{
    return set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(),
                            r.begin());
}

void MainWindow::get_resolution()
{
    if (ui->cb_resolution->count() != 0 || ui->lw_interface->count() != 0)
    {
        return;
    }

    ui->lw_interface->addItem(QString::fromStdString("test_3")); // test

    vector<Xrandr_monitor> xm = Settings_mst::parse_xrandr();
    int xm_size = xm.size();
    vector<string> resol;
    vector<string>::iterator it;

    if (xm_size == 0)
    {
        throw "Could not get Xrandr output.";
    }

    resol = xm[0].resolutions;
    ui->lw_interface->addItem(QString::fromStdString(xm[0].interface));

    if(xm_size > 1)
    {
        for (int idx = 1; idx < xm_size; idx++)
        {
            sort(resol.begin(), resol.end());
            sort(xm[idx].resolutions.begin(), xm[idx].resolutions.end());
            it = _set_intersection(resol, xm[idx].resolutions, resol);
            resol.resize(it->size());
            ui->lw_interface->addItem(QString::fromStdString(xm[idx].interface));
        }
    }

    add_resolutions_to_cb(resol, ui->cb_resolution);

    ui->lw_interface->addItem(QString::fromStdString("test_1")); // test
    ui->lw_interface->addItem(QString::fromStdString("test_5")); // test
}
