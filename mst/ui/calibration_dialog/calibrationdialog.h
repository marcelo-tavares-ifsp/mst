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
    explicit CalibrationDialog(QWidget *parent, QString interface);
    ~CalibrationDialog();

signals:
    void cancel();
    void device_configured(DEVICE_TYPE type, QString name);

private slots:
    void on_btnCancel_clicked();
    void device_found(QString name, DEVICE_TYPE type);

private:
    Ui::CalibrationDialog *ui;
    QString interface;
    Device_listener* listener = nullptr;
    QVector<QString> mice;
    QVector<QString> keyboards;

    void attach_signals(Device_listener* listener);
    void set_title(DEVICE_TYPE type);
    void set_view(DEVICE_TYPE type);
};

#endif // CALIBRATIONDIALOG_H
