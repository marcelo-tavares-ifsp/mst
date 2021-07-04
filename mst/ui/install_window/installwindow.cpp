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
    mst->reset_devices(seat_id - 1);
    current_seat_id = seat_id - 1;

    QString interface = mst->get_seats()[current_seat_id]->get_monitor().get_interface();
    CalibrationDialog* cd = new CalibrationDialog(this, interface);

    connect(cd, SIGNAL(device_configured(DEVICE_TYPE, QString)),
            this, SLOT(device_configured(DEVICE_TYPE, QString)));

    cd->setModal(true);
    cd->exec();
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

void InstallWindow::device_configured(DEVICE_TYPE type, QString name)
{
    mst->set_device(current_seat_id, name, type);
    ui->hbox_seats->update();
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
