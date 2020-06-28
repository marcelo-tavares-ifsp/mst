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

versiontarget.target = version.h
versiontarget.commands = \
    [ ! -e ../.git ] || echo \'const string VERSION = \"\
$$system("[ ! -e '../.git' ] || git describe --abbrev=0")-\
$$system("[ ! -e '../.git' ] || git rev-parse --short HEAD")\";\' \
   > version.h;
versiontarget.depends = FORCE
dist.depends = version.h

PRE_TARGETDEPS += version.h

QMAKE_EXTRA_TARGETS += versiontarget

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++11 strip

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
    components/component.cpp \
    components/vgl.cpp \
    components/display_manager.cpp \
    input_device_listener/inputdevicelistener.cpp \
    components/awesome.cpp \
    template_manager/template_manager.cpp \
    template_manager/template.cpp \
    ui/about_dialog/about_dialog.cpp \
    components/xorg.cpp \
    components/udev.cpp \
    components/sudo.cpp \
    components/system.cpp \
    platform/platform.cpp \
    components/component_manager.cpp \
    ui/monitor_widget/monitor_widget.cpp \
    install_controller/resolution.cpp

HEADERS += \
    ui/install_window/installwindow.h \
    ui/calibration_dialog/calibrationdialog.h \
    ui/reboot_dialog/rebootdialog.h \
    install_controller/installcontroller.h \
    configuration/configuration.h \
    common/utilites/utilites.h \
    components/component.h \
    path_manager/pathmanager.h \
    common/dsv_parser/dsv.h \
    components/vgl.h \
    components/display_manager.h \
    input_device_listener/inputdevicelistener.h \
    components/awesome.h \
    template_manager/template_manager.h \
    template_manager/template.h \
    ui/about_dialog/about_dialog.h \
    components/xorg.h \
    components/udev.h \
    components/sudo.h \
    components/system.h \
    platform/platform.h \
    components/component_manager.h \
    version.h \
    ui/monitor_widget/monitor_widget.h \
    install_controller/resolution.h

FORMS += \
    ui/install_window/installwindow.ui \
    ui/calibration_dialog/calibrationdialog.ui \
    ui/reboot_dialog/rebootdialog.ui \
    ui/about_dialog/about_dialog.ui

isEmpty(PREFIX) {
    PREFIX = /usr/
}
DEFINES += INSTALLATION_PREFIX=\\\"$${PREFIX}\\\"

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

DISTFILES +=
