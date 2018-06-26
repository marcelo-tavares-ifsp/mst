#include "seat_calibration_dialog.h"



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

void Seat_calibration_dialog::on_lineEdit_textChanged(const QString &arg1)
{
    ui->buttonBox->setEnabled(true);
}

void Seat_calibration_dialog::on_buttonBox_2_clicked(QAbstractButton *button)
{
    this->close();
}
