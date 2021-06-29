#include "installwindow.h"
#include "ui_installwindow.h"

#include "../about_dialog/about_dialog.h"

#include <ui/seat_widget/seat_widget.h>

Q_LOGGING_CATEGORY(install_window_category, "mst.ui.install_window")

InstallWindow::InstallWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::InstallWindow),
    current_seat_id(-1)
{
    ui->setupUi(this);
    mst = MST::get_instance();

    if (! mst->running_p()) {
        qInfo(install_window_category()) << "MST is not running";
        if (mst->running_p()) {
            ui->button_stop_mst->setText(tr("Stop multiseat"));
        } else if (mst->config_is_valid()) {
            ui->button_stop_mst->setText(tr("Start multiseat"));
        } else {
            ui->button_stop_mst->setEnabled(false);
        }
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
    mst->reset_devices(seat_id);
    current_seat_id = seat_id;
    initial_listeners();
}

void InstallWindow::load_seat_configuration_page()
{
    for (auto w : widgets) {
        delete w;
    }
    widgets.clear();

    mice.clear();
    keyboards.clear();
    mst->get_devices(mice, keyboards);

    for (auto seat : mst->get_seats()) {
        QWidget* widget = new Seat_widget(seat);
        connect(widget, SIGNAL(configure_seat(int)), this, SLOT(configure_seat(int)));
        widgets.push_back(widget);
        ui->hbox_seats->addWidget(widget);
    }
}

// Continue Buttons Handlers //////////////////////////////////////////////////

/**
 * @brief InstallWindow::on_button_begin_configuration_clicked -- The 1st page of
 *     configuration dialog.
 */
void InstallWindow::on_button_begin_configuration_clicked()
{
    mst->load_seats();
    load_seat_configuration_page();
    show_page(Ui::Page::CONFIGURATION);
}

/**
 * @brief InstallWindow::on_button_install_mst_clicked -- Begin install button
 */
void InstallWindow::on_button_install_mst_clicked()
{
    try {
        mst->configure(); // TODO: Dialog OK/Cancel
        qInfo(install_window_category()) << "Creating backup ...";
        mst->create_backup();
        qInfo(install_window_category()) << "Creating backup ... done";
        qInfo(install_window_category()) << "Installing ... ";
        mst->install();
        qInfo(install_window_category()) << "Installing ... done";
        qInfo(install_window_category()) << "Enabling components ...";
        mst->enable();
        qInfo(install_window_category()) << "Enabling components ... done";
        ui->button_next_to_installation->setEnabled(false);

        Reboot_dialog* rd = new Reboot_dialog(this);

        rd->setModal(true);
        rd->exec();
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
    if (mst->running_p()) {
        ui->button_stop_mst->setText(tr("Multiseat is stopping ..."));
        mst->stop(); // TODO: Dialog OK/Cancel
        if (! mst->running_p()) {
            //ui->button_stop_mst->setEnabled(false);
            if (mst->config_is_valid()) {
                ui->button_stop_mst->setText(tr("Start multiseat"));
            }
        }
    } else if (mst->config_is_valid()) {
        ui->button_stop_mst->setText(tr("Multiseat is starting ..."));
        mst->start();
        if (mst->running_p()) {
            ui->button_stop_mst->setText(tr("Stop multiseat"));
        }
    }
}

void InstallWindow::on_button_restore_backup_clicked()
{
    mst->uninstall(); // TODO: Dialog OK/Cancel

    Reboot_dialog* rd = new Reboot_dialog(this);

    rd->setModal(true);
    rd->exec();
}

// private methods ///////////////////////////////////////////////////////////////

void InstallWindow::initial_listeners()
{
    qDebug(install_window_category(), "initial_listeners: Creating and starting I/O listeners ...");
    Device_listener* mouse_listener
            = new Input_device_listener(DEVICE_TYPE::MOUSE, mice);
    Device_listener* keyboard_listener
            = new Input_device_listener(DEVICE_TYPE::KEYBOARD, keyboards);
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


static QString _get_instruction(Device_listener * device_listener)
{
    QString instruction = "";

    switch (device_listener->type) {
    case DEVICE_TYPE::KEYBOARD:
        instruction = "Пожалуйста, нажимайте на кнопки той клавиатуры,"
                      " которую хотите использовать для выбранного рабочего"
                      " места.";
        break;
    case DEVICE_TYPE::MOUSE:
        instruction = "Пожалуйста, нажимайте на кнопки той мыши,"
                      " которую хотите использовать для выбранного рабочего"
                      " места.";
        break;
    case DEVICE_TYPE::USB:
        instruction = "Пожалуйста вставьте USB-устройство в тот USB-разъём,"
                      " который хотите использовать для выбранного рабочего"
                      " места.";
        break;
    }

    return instruction;
}

void InstallWindow::initial_calibration_dialog(Device_listener* device_listener)
{
    CalibrationDialog* cd = new CalibrationDialog(this, _get_instruction(device_listener));

    attach_signals(device_listener, cd);

    cd->setModal(true);
    cd->exec();
}

void InstallWindow::set_seat_device(QString device, DEVICE_TYPE type)
{
    mst->set_device(current_seat_id, device, type);
    ui->hbox_seats->update();
}

void InstallWindow::attach_signals(Device_listener* listener, CalibrationDialog* cd)
{
    qDebug(install_window_category(), "Attaching signals from listeners to slots ...");
    connect(listener, SIGNAL(device_found(QString, DEVICE_TYPE)),
        this, SLOT(set_seat_device(QString, DEVICE_TYPE)));

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
    if (mst->config_is_valid())
    {
        qInfo(install_window_category) << "Configuration is valid";
        show_page(Ui::Page::CONFIGURATION_END);
        MST* con = MST::get_instance();
        con->configure();
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
