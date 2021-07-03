#include "calibrationdialog.h"
#include "ui_calibrationdialog.h"

Q_LOGGING_CATEGORY(calibration_dialog_category, "mst.ui.calibration_dialog")

CalibrationDialog::CalibrationDialog(QWidget *parent, QString interface, DEVICE_TYPE type) :
    QDialog(parent),
    ui(new Ui::CalibrationDialog)
{
    ui->setupUi(this);
    this->setWindowTitle(interface + ": "
                         + tr("Seat Device Configuration")
                         + " — " + device::describe(type));

    switch (type) {
    case DEVICE_TYPE::KEYBOARD:
        ui->lblInstuction->setText(
                    "Пожалуйста, нажимайте на кнопки той клавиатуры,"
                    " которую хотите использовать для выбранного рабочего"
                    " места.");
        break;

    case DEVICE_TYPE::MOUSE:
         ui->lblInstuction->setText(
                     "Пожалуйста, нажимайте на кнопки той мыши,"
                     " которую хотите использовать для выбранного рабочего"
                     " места.");
        break;

    case DEVICE_TYPE::USB:
        ui->lblInstuction->setText(
                    "Пожалуйста вставьте USB-устройство в тот USB-разъём,"
                    " который хотите использовать для выбранного рабочего"
                    " места.");
                break;
            }
}

CalibrationDialog::~CalibrationDialog()
{
    delete ui;
}

void CalibrationDialog::on_btnCancel_clicked()
{
    this->close();
    emit cancel();
}

void CalibrationDialog::work_done()
{
    qInfo(calibration_dialog_category()) << "work_done";
    this->close();
}
