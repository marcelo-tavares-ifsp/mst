/* reboot_dialog.cpp -- MST reboot dialog.
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

#include "seat_calibration_dialog.h"

#include <iostream>

using namespace std;

Seat_calibration_dialog::Seat_calibration_dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Seat_calibration_dialog)
{
    ui->setupUi(this);
}

Seat_calibration_dialog::~Seat_calibration_dialog()
{
    delete ui;
}

void Seat_calibration_dialog::on_pushButton_2_clicked()
{
    emit cancel();
    this->close();
}
