#ifndef CALIBRATIONDIALOG_H
#define CALIBRATIONDIALOG_H

#include <QDialog>
#include <QLoggingCategory>
#include "../mst/core/device/device_listener.h"

Q_DECLARE_LOGGING_CATEGORY(calibration_dialog_category)

namespace Ui {
class CalibrationDialog;
}

class CalibrationDialog : public QDialog
{
    Q_OBJECT

public:
    explicit CalibrationDialog(QWidget *parent, QString interface, DEVICE_TYPE type);
    ~CalibrationDialog();

signals:
    void cancel();

private slots:
    void on_btnCancel_clicked();
    void work_done();

private:
    Ui::CalibrationDialog *ui;
    QString interface;
};

#endif // CALIBRATIONDIALOG_H
