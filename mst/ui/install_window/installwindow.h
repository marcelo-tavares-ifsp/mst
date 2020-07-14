#ifndef INSTALLWINDOW_H
#define INSTALLWINDOW_H

#include <QMainWindow>
#include <QThreadPool>
#include <QMessageBox>
#include "ui/calibration_dialog/calibrationdialog.h"
#include "ui/reboot_dialog/rebootdialog.h"
#include "core/installcontroller.h"
#include "core/device/device_listener.h"
#include "core/device/input_device_listener.h"
#include "core/device/usb_device_listener.h"
#include "version.h"

Q_DECLARE_LOGGING_CATEGORY(install_window_category)

namespace Ui {
class InstallWindow;

/**
 * @brief The Page enum -- InstallWindow pages.
 */
enum Page {
  START_PAGE          = 0,
  INTERFACES          = 1,
  CONFIGURATION_END   = 2
};

}

class InstallWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit InstallWindow(QWidget *parent = nullptr);
    ~InstallWindow();

private slots:
    void on_btnBeginInstall_clicked();

    void on_btnBackToStart_clicked();

    void on_btnBackToInterface_clicked();

    void on_btnContinueToEnd_clicked();

    void on_btnBackToDevices_clicked();

    void on_btnCancel_clicked();

    void on_btnEndInstall_clicked();

    void on_btnExit_clicked();

    void on_btnBreak_clicked();

    void on_btnBackup_clicked();

    void on_about_triggered();

    void on_btnConfigurationEnd_clicked();

public slots:
    void configure_seat(int seat_id);

private:
    Ui::InstallWindow *ui;
    InstallController *inst_controller;

    void initial_listeners();
    void initial_calibration_dialog(Device_listener* device_listener);
    void attach_signals(Device_listener* listener, CalibrationDialog* cd);
    void show_page(int number);
};

#endif // INSTALLWINDOW_H
