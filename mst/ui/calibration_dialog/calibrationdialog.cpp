#include "calibrationdialog.h"
#include "ui_calibrationdialog.h"

#include <core/device/input_device_listener.h>
#include <core/device/usb_device_listener.h>

#include <core/mst.h>

#include <QThreadPool>

Q_LOGGING_CATEGORY(calibration_dialog_category, "mst.ui.calibration_dialog")

CalibrationDialog::CalibrationDialog(QWidget *parent, QString interface) :
    QDialog(parent),
    ui(new Ui::CalibrationDialog),
    interface(interface)
{
    ui->setupUi(this);
    set_view(DEVICE_TYPE::MOUSE);
    MST::get_instance()->get_devices(mice, keyboards);
    listener = new Input_device_listener(DEVICE_TYPE::MOUSE, mice);
    attach_signals(listener);
    QThreadPool::globalInstance()->start(listener);
    qInfo(calibration_dialog_category()) << "Mouse input listener was started";

//    switch (type) {
//    case DEVICE_TYPE::KEYBOARD:
//        ui->lblInstuction->setText(
//                    "Пожалуйста, нажимайте на кнопки той клавиатуры,"
//                    " которую хотите использовать для выбранного рабочего"
//                    " места.");
//        break;

//    case DEVICE_TYPE::MOUSE:
//         ui->lblInstuction->setText(
//                     "Пожалуйста, нажимайте на кнопки той мыши,"
//                     " которую хотите использовать для выбранного рабочего"
//                     " места.");
//        break;

//    case DEVICE_TYPE::USB:
//        ui->lblInstuction->setText(
//                    "Пожалуйста вставьте USB-устройство в тот USB-разъём,"
//                    " который хотите использовать для выбранного рабочего"
//                    " места.");
//                break;
//            }
}

CalibrationDialog::~CalibrationDialog()
{
    delete ui;
}

void CalibrationDialog::set_title(DEVICE_TYPE type)
{
    setWindowTitle(interface + ": "
                   + tr("Seat Device Configuration")
                   + " — " + device::describe(type));
}

void CalibrationDialog::set_view(DEVICE_TYPE type)
{
    set_title(type);
    ui->device_configration->setCurrentIndex(type);
}

void CalibrationDialog::on_btnCancel_clicked()
{
    this->close();
    emit cancel();
}

void CalibrationDialog::device_found(QString name, DEVICE_TYPE type)
{
    emit device_configured(type, name);

    switch (type) {
    case DEVICE_TYPE::MOUSE:
        set_view(DEVICE_TYPE::KEYBOARD);
        listener = new Input_device_listener(DEVICE_TYPE::KEYBOARD,
                                             keyboards);
        attach_signals(listener);
        QThreadPool::globalInstance()->start(listener);
        qInfo(calibration_dialog_category()) << "Keyboard input listener was started";
        break;

    case DEVICE_TYPE::KEYBOARD:
        set_view(DEVICE_TYPE::USB);
        ui->device_configration->setCurrentIndex(2);
        listener = new USB_device_listener(DEVICE_TYPE::USB);
        attach_signals(listener);
        QThreadPool::globalInstance()->start(listener);
        qInfo(calibration_dialog_category()) << "USB listener was started";
        break;

    case DEVICE_TYPE::USB:
        close();
        break;
    }
}

void CalibrationDialog::attach_signals(Device_listener* listener)
{
    qDebug(calibration_dialog_category(), "Attaching signals to slots ...");

    connect(listener, SIGNAL(device_found(QString, DEVICE_TYPE)),
            this,     SLOT(device_found(QString, DEVICE_TYPE)));
    connect(this,     SIGNAL(cancel()),
            listener, SLOT(cancel()));

    qDebug(calibration_dialog_category(), "Attaching signals to slots ... done");
}
