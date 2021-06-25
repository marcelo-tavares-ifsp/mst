#ifndef INSTALLWINDOW_H
#define INSTALLWINDOW_H

#include <QMainWindow>
#include <QThreadPool>
#include <QMessageBox>
#include "ui/calibration_dialog/calibrationdialog.h"
#include "ui/reboot_dialog/reboot_dialog.h"
#include "core/mst.h"
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
  CONFIGURATION       = 1,
  CONFIGURATION_END   = 2
};

}

class InstallWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit InstallWindow(QWidget *parent = nullptr);
    ~InstallWindow();

public slots:
    void on_about_triggered();
    void configure_seat(int seat_id);

private slots:
    void on_button_begin_configuration_clicked();
    void on_button_install_mst_clicked();
    void on_button_back_to_main_screen_clicked();
    void on_button_back_to_configuration_clicked();
    void on_button_cancel_clicked();
    void on_button_exit_clicked();
    void on_button_stop_mst_clicked();
    void on_button_restore_backup_clicked();
    void on_button_next_to_installation_clicked();


private:
    Ui::InstallWindow *ui;
    MST *inst_controller;

    void initial_listeners();
    void initial_calibration_dialog(Device_listener* device_listener);
    void attach_signals(Device_listener* listener, CalibrationDialog* cd);
    void show_page(int number);
};

#endif // INSTALLWINDOW_H
