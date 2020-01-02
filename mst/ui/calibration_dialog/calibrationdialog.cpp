#include "calibrationdialog.h"
#include "ui_calibrationdialog.h"

Q_LOGGING_CATEGORY(calibration_dialog_category, "mst.calibration_dialog")

CalibrationDialog::CalibrationDialog(QWidget *parent, QString instruction) :
    QDialog(parent),
    ui(new Ui::CalibrationDialog)
{
    ui->setupUi(this);
    this->setWindowTitle("Калибровка устройства");
    ui->lblInstuction->setText(instruction);
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
