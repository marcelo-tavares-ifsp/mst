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
    ui/install_window/installwindow.cpp \
    ui/calibration_dialog/calibrationdialog.cpp \
    ui/reboot_dialog/rebootdialog.cpp \
    install_controller/installcontroller.cpp \
    configuration/configuration.cpp \
    common/utilites/utilites.cpp \
    path_manager/pathmanager.cpp \
    common/dsv_parser/dsv.cpp \
    config_manager/component.cpp \
    config_manager/vgl.cpp \
    config_manager/display_manager.cpp \
    input_device_listener/inputdevicelistener.cpp \
    config_manager/awesome.cpp \
    template_manager/template_manager.cpp \
    template_manager/template.cpp \
    ui/about_dialog/about_dialog.cpp \
    config_manager/xorg.cpp \
    config_manager/udev.cpp \
    config_manager/sudo.cpp \
    config_manager/system.cpp \
    platform/platform.cpp \
    config_manager/component_manager.cpp

HEADERS += \
    ui/install_window/installwindow.h \
    ui/calibration_dialog/calibrationdialog.h \
    ui/reboot_dialog/rebootdialog.h \
    install_controller/installcontroller.h \
    configuration/configuration.h \
    common/utilites/utilites.h \
    config_manager/component.h \
    path_manager/pathmanager.h \
    common/dsv_parser/dsv.h \
    config_manager/vgl.h \
    config_manager/display_manager.h \
    version.h \
    input_device_listener/inputdevicelistener.h \
    config_manager/awesome.h \
    template_manager/template_manager.h \
    template_manager/template.h \
    ui/about_dialog/about_dialog.h \
    config_manager/xorg.h \
    config_manager/udev.h \
    config_manager/sudo.h \
    config_manager/system.h \
    platform/platform.h \
    config_manager/component_manager.h

FORMS += \
    ui/install_window/installwindow.ui \
    ui/calibration_dialog/calibrationdialog.ui \
    ui/reboot_dialog/rebootdialog.ui \
    ui/about_dialog/about_dialog.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

RESOURCES += \
    resources.qrc
