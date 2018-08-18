/* reboot_dialog.cpp -- MST reboot dialog.
 *
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

#include "reboot_dialog.h"
#include "ui_reboot_dialog.h"
#include <unistd.h>
#include <linux/reboot.h>
#include <sys/reboot.h>

Reboot_Dialog::Reboot_Dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Reboot_Dialog)
{
    ui->setupUi(this);
}

Reboot_Dialog::~Reboot_Dialog()
{
    delete ui;
}

void Reboot_Dialog::on_btn_now_reboot_clicked()
{
    sync();
    setuid(0);
    reboot(RB_AUTOBOOT);
}

void Reboot_Dialog::on_btn_late_reboot_clicked()
{
    this->close();
}
