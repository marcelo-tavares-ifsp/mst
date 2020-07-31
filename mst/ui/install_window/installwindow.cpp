#include "installwindow.h"
#include "ui_installwindow.h"

#include "../about_dialog/about_dialog.h"

Q_LOGGING_CATEGORY(install_window_category, "mst.install_window")

InstallWindow::InstallWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::InstallWindow)
{
    ui->setupUi(this);
    inst_controller = InstallController::get_instance();

    if (! inst_controller->is_mst_running()) {
        qInfo(install_window_category()) << "MST is running";
        ui->btnBreak->setEnabled(false);
    }

    ui->Version->setText(QString::fromStdString(VERSION));

    show_page(0);
}

InstallWindow::~InstallWindow()
{
    delete ui;
}

/**
 * @brief InstallWindow::show_page -- change main window page
 * @param number -- number of page to show
 */
void InstallWindow::show_page(int number)
{
    ui->stackedWidget->setCurrentIndex(number);
    qDebug(install_window_category()) << number + " page is shown";
}

void InstallWindow::configure_seat(int seat_id)
{
    qInfo(install_window_category()) << seat_id << " was selected";
    inst_controller->prepare_for_device_configuration(seat_id);

    initial_listeners();
}

// Continue Buttons Handlers //////////////////////////////////////////////////

/**
 * @brief InstallWindow::on_btnBeginInstall_clicked -- The 1st page of
 *     configuration dialog.
 */
void InstallWindow::on_btnBeginInstall_clicked()
{
    inst_controller->load_seat_configuration_page(this, ui->hbox_seats);
    show_page(Ui::Page::INTERFACES);
}

/**
 * @brief InstallWindow::on_btnContinueToEnd_clicked -- Theird page to Fourth page
 */
void InstallWindow::on_btnContinueToEnd_clicked()
{
    if (inst_controller->config_is_valid())
    {
        show_page(Ui::Page::CONFIGURATION_END);
        InstallController* con = InstallController::get_instance();
        con->begin_install();
        qDebug(install_window_category) << "going to the 3rd panel...";
    }
    else
    {
        QMessageBox::information(this, "Необходимо заполнить!",
                      "У каждого монитора должна быть мышь и клавиатура! Они не должны пересекаться!", QMessageBox::Ok);
        // TODO: Show error msg!
    }
}

/**
 * @brief InstallWindow::on_btnEndInstall_clicked -- Begin install button
 */
void InstallWindow::on_btnEndInstall_clicked()
{
    try
      {
          inst_controller->create_backup();
      }
      catch (string msg)
      {
          cout << msg << endl;
      }

    inst_controller->begin_install(); // TODO: Dialog OK/Cancel
    inst_controller->install_files();
    inst_controller->enable_mst();
    ui->btnEndInstall->setEnabled(false);

    Reboot_dialog* rd = new Reboot_dialog(this);

    rd->setModal(true);
    rd->exec();

    this->close();
}

// Back and Cancel Buttons Handlers ///////////////////////////////////////////

void InstallWindow::on_btnBackToStart_clicked()
{
    show_page(Ui::Page::START_PAGE);
}

void InstallWindow::on_btnBackToInterface_clicked()
{
    show_page(Ui::Page::INTERFACES);
}

void InstallWindow::on_btnBackToDevices_clicked()
{
    show_page(Ui::Page::INTERFACES);
}

void InstallWindow::on_btnCancel_clicked()
{
    this->close(); // TODO: Dialog OK/Cancel
}

void InstallWindow::on_btnExit_clicked()
{
    this->close(); // TODO: Dialog OK/Cancel
}

void InstallWindow::on_btnBreak_clicked()
{
    inst_controller->begin_stop(); // TODO: Dialog OK/Cancel
}

void InstallWindow::on_btnBackup_clicked()
{
    inst_controller->begin_uninstall(); // TODO: Dialog OK/Cancel

    Reboot_dialog* rd = new Reboot_dialog(this);

    rd->setModal(true);
    rd->exec();
}

// private methods ///////////////////////////////////////////////////////////////

void InstallWindow::initial_listeners()
{
    Device_listener* mouse_listener
            = new Input_device_listener(DEVICE_TYPE::MOUSE,
                                        inst_controller->get_list_of_mice());
    Device_listener* keybd_listener
            = new Input_device_listener(DEVICE_TYPE::KEYBOARD,
                                        inst_controller->get_list_of_keybs());
    Device_listener* usb_listener
            = new USB_device_listener(DEVICE_TYPE::USB);

    QThreadPool::globalInstance()->start(mouse_listener);
    qInfo(install_window_category()) << "MOUSE was started";
    initial_calibration_dialog(mouse_listener);

    QThreadPool::globalInstance()->start(keybd_listener);
    qInfo(install_window_category()) << "KEYBOARD was started";
    initial_calibration_dialog(keybd_listener);

    QThreadPool::globalInstance()->start(usb_listener);
    qInfo(install_window_category()) << "USB was started";
    initial_calibration_dialog(usb_listener);
}

void InstallWindow::initial_calibration_dialog(Device_listener* device_listener)
{
    CalibrationDialog* cd = new CalibrationDialog(this, inst_controller->get_instruction(device_listener));

    attach_signals(device_listener, cd);

    cd->setModal(true);
    cd->exec();
}

void InstallWindow::attach_signals(Device_listener* listener, CalibrationDialog* cd)
{
    connect(listener, SIGNAL(device_found(QString, DEVICE_TYPE)),
        inst_controller, SLOT(set_seat_device(QString, DEVICE_TYPE)));

    connect(listener, SIGNAL(work_done()), cd, SLOT(work_done()));
    connect(cd, SIGNAL(cancel()), listener, SLOT(cancel()));
}


void InstallWindow::on_about_triggered()
{
    About_dialog* ad = new About_dialog(this);
    ad->setModal(true);
    ad->show();
}

void InstallWindow::on_btnConfigurationEnd_clicked()
{
    if (inst_controller->config_is_valid())
    {
        show_page(Ui::Page::CONFIGURATION_END);
        InstallController* con = InstallController::get_instance();
        con->begin_install();
        qDebug(install_window_category) << "going to the 3rd panel...";
    }
    else
    {
        QMessageBox::information(this, "Необходимо заполнить!",
                      "У каждого монитора должна быть мышь и клавиатура! Они не должны пересекаться!", QMessageBox::Ok);
        // TODO: Show error msg!
    }
}
