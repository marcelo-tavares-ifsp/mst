#-------------------------------------------------
#
# Project created by QtCreator 2018-12-16T10:50:20
#
#-------------------------------------------------

QT       += core gui
LIBS += -ludev

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = mst
TEMPLATE = app

PRE_TARGETDEPS += version

QMAKE_EXTRA_TARGETS += version

version.commands += \
    echo \'const string VERSION = \" $$VERSION-$$system("git rev-parse --short HEAD")\";\' \
        > version.h

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++11

SOURCES += \
        main.cpp \
        install_window/installwindow.cpp \
    install_controller/installcontroller.cpp \
    configuration/configuration.cpp \
    common/utilites/utilites.cpp \
    command_manager/commandmanager.cpp \
    config_manager/configmanager.cpp \
    path_manager/pathmanager.cpp \
    calibration_dialog/calibrationdialog.cpp \
    common/dsv_parser/dsv.cpp \
    config_manager/xorg_config.cpp \
    config_manager/vgl.cpp \
    config_manager/display_manager.cpp \
    reboot_dialog/rebootdialog.cpp \
    input_device_listener/inputdevicelistener.cpp \
    config_manager/awesome.cpp \
    template_manager/template_manager.cpp \
    template_manager/template.cpp

HEADERS += \
        install_window/installwindow.h \
    install_controller/installcontroller.h \
    configuration/configuration.h \
    common/utilites/utilites.h \
    command_manager/commandmanager.h \
    config_manager/configmanager.h \
    path_manager/pathmanager.h \
    calibration_dialog/calibrationdialog.h \
    common/dsv_parser/dsv.h \
    config_manager/xorg_config.h \
    config_manager/vgl.h \
    config_manager/display_manager.h \
    reboot_dialog/rebootdialog.h \
    version.h \
    input_device_listener/inputdevicelistener.h \
    config_manager/awesome.h \
    template_manager/template_manager.h \
    template_manager/template.h

FORMS += \
        install_window/installwindow.ui \
    calibration_dialog/calibrationdialog.ui \
    reboot_dialog/rebootdialog.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target
