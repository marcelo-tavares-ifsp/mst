#ifndef SEAT_CALIBRATION_DIALOG_H
#define SEAT_CALIBRATION_DIALOG_H

#include "ui_seat_calibration_dialog.h"

#include <QDialog>



namespace Ui {
class Seat_calibration_dialog;
}

class Seat_calibration_dialog : public QDialog
{
    Q_OBJECT

public:
    explicit Seat_calibration_dialog(QWidget *parent = 0);
    ~Seat_calibration_dialog();

private slots:
    void on_lineEdit_textChanged(const QString &arg1);

    void on_buttonBox_2_clicked(QAbstractButton *button);

private:
    Ui::Seat_calibration_dialog *ui;
};

#endif // SEAT_CALIBRATION_DIALOG_H
