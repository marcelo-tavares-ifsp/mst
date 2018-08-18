/* mainwindow.cpp -- MST main window.
 *
 * Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 * Copyright (C) 2018 Anton Plekhanov <plehunov.anton_9@mail.ru>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <QDebug>
#include <QLoggingCategory>

#include "ui_mainwindow.h"
#include "mainwindow.h"
#include "config.h"
#include "reboot_dialog.h"
#include "version.h"

Q_LOGGING_CATEGORY(main_window_category, "mst.main_window")

static Controller *con;

int Seat::width;
int Seat::height;
QPushButton *button;
Input_device_listener *mouse_listener;
Input_device_listener *keybd_listener;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->stackedWidget->setCurrentIndex(0);

    if (Controller::is_mst_running())
    {
        qDebug(main_window_category) << "MST is running";
        QPushButton *btn = new QPushButton("stop_mst", this);
        btn->setText(QString::fromStdString("Остановить MST"));
        connect(btn, SIGNAL(clicked()), this, SLOT(on_stop_mst_clicked()));
        widgets.push_back(btn);
        ui->gridLayout_1->addWidget(btn);
    }

    ui->lbl_version->setText(QString::fromStdString(VERSION));
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_btn_next_4_clicked()
{
    Controller::restore_backup();
    con->disable_mst();

    Reboot_Dialog rebootDialog;

    rebootDialog.setModal(true);
    rebootDialog.exec();
}

void MainWindow::on_stop_mst_clicked()
{
    Controller::stop_mst();
}

/**
 * @brief MainWindow::on_btn_next_1_clicked -- the handler of the button
 *      that appears on the main window first.
 */

void MainWindow::on_btn_next_1_clicked()
{
    get_resolution();

    ui->stackedWidget->setCurrentIndex(1);

    ui->cb_resolution->setCurrentIndex(ui->cb_resolution->count() - 1);
    ui->lw_interface->selectAll();
}

void MainWindow::on_btn_next_2_clicked()
{
    save_resolution();

    if (is_layout_changed(global_seats, ui->lw_interface->selectedItems()))
    {
        qDebug(main_window_category) << "Layout changed";
        global_seats.clear();
        fill_global_seats();

        clear_layout();
        widgets.clear();

        fill_layout();
    }

    if (list_mice.size() == 0)
    {
        Settings_mst::parse_ls_devices(&list_mice, &list_keybs);
    }

    ui->stackedWidget->setCurrentIndex(2);
}

/**
 * @brief MainWindow::on_btn_next_3_clicked -- the "next" button on the panel
 *      where input devices are assigned to the seats.
 */
void MainWindow::on_btn_next_3_clicked()
{
    if (check_collision_seats())
    {
        if (check_fill_seats())
        {
            con = new Controller(global_seats);
            con->generate_files();
            ui->stackedWidget->setCurrentIndex(3);
            qDebug(main_window_category)
                    << "going to the 3rd panel...";
        }
        else
        {
           QMessageBox::information(this, "Необходимо заполнить!",
              "У каждого монитора должна быть мышь и клавиатура!", QMessageBox::Ok);
        }
    }
    else
    {
        QMessageBox::information(this, "Коллизия!",
           "У каждого монитора должна быть уникальная мышь и клавиатура!",
                                 QMessageBox::Ok);
    }
}

void MainWindow::on_btn_back_2_clicked()
{
    ui->stackedWidget->setCurrentIndex(1);

    for (auto seat: global_seats)
    {
        seat.keyboard = "";
        seat.mouse = "";
    }
}

void MainWindow::on_btn_back_1_clicked()
{
    ui->stackedWidget->setCurrentIndex(0);
}

void MainWindow::on_interface_clicked()
{
    button = (QPushButton *) sender();

    clear_interface(button->text().toUtf8().constData());

    mouse_listener = new Input_device_listener(list_mice, Input_device_listener::MOUSE);
    connect(mouse_listener, SIGNAL(device_found(QString, int)),
                this, SLOT(set_seat_device(QString, int)));

    keybd_listener = new Input_device_listener(list_keybs, Input_device_listener::KEYBOARD);
    connect(keybd_listener, SIGNAL(device_found(QString, int)),
                this, SLOT(set_seat_device(QString, int)));

    QThreadPool::globalInstance()->start(mouse_listener);
    QThreadPool::globalInstance()->start(keybd_listener);

    connect(&scd, SIGNAL(cancel()), mouse_listener, SLOT(cancel()));
    connect(&scd, SIGNAL(cancel()), keybd_listener, SLOT(cancel()));

    scd.setModal(true);
    scd.exec();
}

void MainWindow::on_install_button_clicked()
{
    try
    {
        Controller::create_backup();
    }
    catch (string msg)
    {
        cout << msg << endl;
    }
    con->enable_mst();
    cout << "[debug] multiseat enabled." << endl;

    ui->install_button->setEnabled(false);
    Reboot_Dialog rebootDialog;

    rebootDialog.setModal(true);
    rebootDialog.exec();
}

void MainWindow::set_seat_device(QString device, int type)
{
    string d = device.toUtf8().constData();

    qDebug(main_window_category)
            << "Device assigned: " << d.c_str() << " (" << type << ")";

    string device_interface = button->text().toUtf8().constData();
    for (int i = 0; i < global_seats.size(); i++)
    {
        if (global_seats[i].interface == device_interface)
        {
            if (type == Input_device_listener::DEVICE_TYPE::KEYBOARD)
            {
                global_seats[i].keyboard = d;
            }
            else
            {
                global_seats[i].mouse = d;
            }

            qDebug(main_window_category)
                    << "Seat interface: '" << global_seats[i].interface.c_str()
                    << "'; keyboard: '" << global_seats[i].keyboard.c_str()
                    << "'; mouse: '" << global_seats[i].mouse.c_str() << "'";

            if (strlen(global_seats[i].keyboard.c_str()) > 3 && strlen(global_seats[i].mouse.c_str()) > 3)
            {
                scd.close();
            }
        }
    }
}



////////static functions///////////////////////////////////////////////////



bool MainWindow::check_collision_seats()
{
    auto is_equal = [this](int i, int j) -> bool {
        return (global_seats[i].keyboard == global_seats[j].keyboard)
                || (global_seats[i].mouse == global_seats[j].mouse);
    };

    int count_seats = global_seats.size();
    if (count_seats > 1)
    {
        for (int i = 0; i < count_seats; i++)
        {
            for (int j = 1; j < count_seats; j++)
            {
                if (i == j)
                    continue;
                if (is_equal(i, j))
                {
                    return false;
                }
            }
        }
    }

    return true;
}

bool MainWindow::check_fill_seats()
{
    for (auto seat : global_seats)
    {
        if (seat.keyboard == "" || seat.mouse == "")
        {
            return false;
        }
    }
    return true;
}

/**
 * @brief _parse_resolution -- parse resolution in "WIDTHxHEIGTH" format.
 * @param resolution -- resolution string.
 * @return vector with the 1st element set to display width and the 2nd
 *      set to the heigth.
 */
static vector<int> _parse_resolution(QString resolution)
{
    vector<string> strs = split(resolution.toUtf8().constData(), 'x');
    return {  atoi(strs[0].c_str()), atoi(strs[1].c_str()) };
}

void MainWindow::save_resolution()
{
    vector<int> resolution
            = _parse_resolution(ui->cb_resolution->currentText());

    Seat::width = resolution[0];
    Seat::height = resolution[1];
}

void MainWindow::fill_layout()
{
    int sz = global_seats.size();
    for (int idx = 0; idx < sz; ++idx)
    {
        QPushButton *Qpb = new QPushButton("btn" + idx, this);
        Qpb->setText(QString::fromStdString(global_seats[idx].interface));
        Qpb->setFocusPolicy(Qt::NoFocus);
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
        if (! s.empty())
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
        // Information is already filled in.
        return;
    }

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
}



void MainWindow::on_pushButton_clicked()
{
    this->close();
}

void MainWindow::on_lw_interface_itemSelectionChanged()
{
    if (ui->lw_interface->selectedItems().count() == 0)
    {
        ui->btn_next_2->setDisabled(true);
    }
    else
    {
        ui->btn_next_2->setEnabled(true);
    }
}

void MainWindow::clear_interface(string interface)
{
    for (int i = 0; i < global_seats.size(); i++)
    {
        if (global_seats[i].interface == interface)
        {
            global_seats[i].keyboard = "";
            global_seats[i].mouse = "";
            return;
        }
    }
}
