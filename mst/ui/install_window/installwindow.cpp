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

    set_show_page(0);
}

InstallWindow::~InstallWindow()
{
    delete ui;
}

/**
 * @brief InstallWindow::set_show_page -- change install page
 * @param number -- number of page install
 */
void InstallWindow::set_show_page(int number)
{
    ui->stackedWidget->setCurrentIndex(number);
    qDebug(install_window_category()) << number + " page is show";
}

void InstallWindow::on_btninterface_clicked()
{
    QPushButton* button = (QPushButton *) sender();
    string name_interface = to_std_string(button->text());
    qInfo(install_window_category()) << name_interface.c_str() << " was selected";
    inst_controller->prepare_for_connect_interface(name_interface);    

    initial_listeners();
}

// Continue Buttons Handlers //////////////////////////////////////////////////

/**
 * @brief InstallWindow::on_btnBeginInstall_clicked -- First page to Second page
 */
void InstallWindow::on_btnBeginInstall_clicked()
{
    inst_controller->load_interface_page(ui->cbResolution, ui->lwMonitors);
    set_show_page(1);
}

/**
 * @brief InstallWindow::on_btnContinueToDevices_clicked -- Second page to Theird page
 */
void InstallWindow::on_btnContinueToDevices_clicked()
{
    inst_controller->save_interfaces(ui->cbResolution, ui->lwMonitors);
    vector<QWidget *> buttons = inst_controller->load_device_page(ui->vlDevices);

    for (auto btn : buttons)
    {
        btn->setParent(ui->pageDevices);
        connect(btn, SIGNAL(clicked()), this, SLOT(on_btninterface_clicked()));
        ui->vlDevices->addWidget(btn);
    }

    set_show_page(2);
}

/**
 * @brief InstallWindow::on_btnContinueToEnd_clicked -- Theird page to Fourth page
 */
void InstallWindow::on_btnContinueToEnd_clicked()
{
    if (inst_controller->config_is_valid())
    {
        set_show_page(3);
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

    RebootDialog* rd = new RebootDialog(this);

    rd->setModal(true);
    rd->exec();

    this->close();
}

// Back and Cancel Buttons Handlers ///////////////////////////////////////////

void InstallWindow::on_btnBackToStart_clicked()
{
    set_show_page(0);
}

void InstallWindow::on_btnBackToInterface_clicked()
{
    set_show_page(1);
}

void InstallWindow::on_btnBackToDevices_clicked()
{
    set_show_page(2);
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

    RebootDialog* rd = new RebootDialog(this);

    rd->setModal(true);
    rd->exec();
}

// private methods ///////////////////////////////////////////////////////////////

void InstallWindow::initial_listeners()
{
    vector<string>* mice;
    vector<string>* keybs;
    vector<string>* usbs;

    mice = new vector<string>(inst_controller->get_list_of_mice());
    keybs = new vector<string>(inst_controller->get_list_of_keybs());
    usbs = new vector<string>();

    InputDeviceListener* mouse_listener = new InputDeviceListener(DEVICE_TYPE::MOUSE, * mice);
    InputDeviceListener* keybd_listener = new InputDeviceListener(DEVICE_TYPE::KEYBOARD, * keybs);
    InputDeviceListener* usb_listener = new InputDeviceListener(DEVICE_TYPE::USB, * usbs);

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

void InstallWindow::initial_calibration_dialog(InputDeviceListener* device_listener)
{
    CalibrationDialog* cd = new CalibrationDialog(this, inst_controller->get_instruction(device_listener));

    attach_signals(device_listener, cd);

    cd->setModal(true);
    cd->exec();
}

void InstallWindow::attach_signals(InputDeviceListener* listener, CalibrationDialog* cd)
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
