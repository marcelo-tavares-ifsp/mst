#include "interface_settings.h"
#include "ui_interface_settings.h"

interface_settings::interface_settings(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::interface_settings)
{
    ui->setupUi(this);
}

interface_settings::~interface_settings()
{
    delete ui;
}

void interface_settings::on_btn_next_2_clicked()
{

}

void interface_settings::on_pushButton_clicked()
{

}
