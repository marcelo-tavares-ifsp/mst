#ifndef SEAT_CALIBRATION_DIALOG_H
#define SEAT_CALIBRATION_DIALOG_H

#include <QDialog>
#include <QAbstractButton>

#include "ui_seat_calibration_dialog.h"

namespace Ui {
class Seat_calibration_dialog;
}

class Seat_calibration_dialog : public QDialog
{
    Q_OBJECT

public:
    explicit Seat_calibration_dialog(QWidget *parent = 0);
    ~Seat_calibration_dialog();

signals:
    void cancel();

private slots:
    void on_lineEdit_textChanged(const QString &arg1);

    void on_pushButton_clicked();

    void on_pushButton_2_clicked();

private:
    Ui::Seat_calibration_dialog *ui;
};

#endif // SEAT_CALIBRATION_DIALOG_H
