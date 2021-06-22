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
        qInfo(install_window_category()) << "MST is not running";
        ui->button_stop_mst->setEnabled(false);
    } else {
        qInfo(install_window_category()) << "MST is running";
    }

    ui->Version->setText(QString::fromStdString(VERSION));

    show_page(Ui::Page::START_PAGE);
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
    qDebug(install_window_category()) << number << " page is shown";
}

void InstallWindow::configure_seat(int seat_id)
{
    qInfo(install_window_category()) << seat_id << " was selected";
    inst_controller->prepare_for_device_configuration(seat_id);

    initial_listeners();
}

// Continue Buttons Handlers //////////////////////////////////////////////////

/**
 * @brief InstallWindow::on_button_begin_configuration_clicked -- The 1st page of
 *     configuration dialog.
 */
void InstallWindow::on_button_begin_configuration_clicked()
{
    inst_controller->load_seat_configuration_page(this, ui->hbox_seats);
    show_page(Ui::Page::CONFIGURATION);
}

/**
 * @brief InstallWindow::on_button_install_mst_clicked -- Begin install button
 */
void InstallWindow::on_button_install_mst_clicked()
{
    try {
        inst_controller->create_backup();
        inst_controller->begin_install(); // TODO: Dialog OK/Cancel
        inst_controller->install_files();
        inst_controller->enable_mst();
        ui->button_next_to_installation->setEnabled(false);

        Reboot_dialog* rd = new Reboot_dialog(this);

        rd->setModal(true);
        rd->exec();

        this->close();
    } catch (string msg) {
        qWarning(install_window_category()) << QString::fromStdString(msg);
        QMessageBox::information(
                    this,
                    QApplication::translate("main", "Unable to make a backup"),
                    QApplication::translate("main",
                                            "Aborting installation"),
                    QMessageBox::Ok);
    }
}

// Back and Cancel Buttons Handlers ///////////////////////////////////////////

void InstallWindow::on_button_back_to_main_screen_clicked()
{
    show_page(Ui::Page::START_PAGE);
}

void InstallWindow::on_button_back_to_configuration_clicked()
{
    show_page(Ui::Page::CONFIGURATION);
}

void InstallWindow::on_button_cancel_clicked()
{
    this->close(); // TODO: Dialog OK/Cancel
}

void InstallWindow::on_button_exit_clicked()
{
    this->close(); // TODO: Dialog OK/Cancel
}

void InstallWindow::on_button_stop_mst_clicked()
{
    inst_controller->begin_stop(); // TODO: Dialog OK/Cancel
}

void InstallWindow::on_button_restore_backup_clicked()
{
    inst_controller->begin_uninstall(); // TODO: Dialog OK/Cancel

    Reboot_dialog* rd = new Reboot_dialog(this);

    rd->setModal(true);
    rd->exec();
}

// private methods ///////////////////////////////////////////////////////////////

void InstallWindow::initial_listeners()
{
    qDebug(install_window_category(), "initial_listeners: Creating and starting I/O listeners ...");
    Device_listener* mouse_listener
            = new Input_device_listener(DEVICE_TYPE::MOUSE,
                                        inst_controller->get_mice());
    Device_listener* keyboard_listener
            = new Input_device_listener(DEVICE_TYPE::KEYBOARD,
                                        inst_controller->get_keyboards());
    Device_listener* usb_listener
            = new USB_device_listener(DEVICE_TYPE::USB);

    QThreadPool::globalInstance()->start(mouse_listener);
    qInfo(install_window_category()) << "Mouse input listener was started";
    initial_calibration_dialog(mouse_listener);

    QThreadPool::globalInstance()->start(keyboard_listener);
    qInfo(install_window_category()) << "Keyboard input listener was started";
    initial_calibration_dialog(keyboard_listener);

    QThreadPool::globalInstance()->start(usb_listener);
    qInfo(install_window_category()) << "USB listener was started";
    initial_calibration_dialog(usb_listener);
    qDebug(install_window_category(), "initial_listeners: Creating and starting I/O listeners ... done");
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
    qDebug(install_window_category(), "Attaching signals from listeners to slots ...");
    connect(listener, SIGNAL(device_found(QString, DEVICE_TYPE)),
        inst_controller, SLOT(set_seat_device(QString, DEVICE_TYPE)));

    connect(listener, SIGNAL(work_done()), cd, SLOT(work_done()));
    connect(cd, SIGNAL(cancel()), listener, SLOT(cancel()));
    qDebug(install_window_category(), "Attaching signals from listeners to slots ... done");
}


void InstallWindow::on_about_triggered()
{
    About_dialog* ad = new About_dialog(this);
    ad->setModal(true);
    ad->show();
}

void InstallWindow::on_button_next_to_installation_clicked()
{
    if (inst_controller->config_is_valid())
    {
        qInfo(install_window_category) << "Configuration is valid";
        show_page(Ui::Page::CONFIGURATION_END);
        InstallController* con = InstallController::get_instance();
        con->begin_install();
        qDebug(install_window_category) << "Going to the 3rd panel...";
    }
    else
    {
        QMessageBox::information(
                    this,
                    QApplication::translate("main", "Incomplete configuration"),
                    QApplication::translate("main",
                                            "Each seat must have configured"
                                            " unique keyboard and mouse."),
                    QMessageBox::Ok);
    }
}
