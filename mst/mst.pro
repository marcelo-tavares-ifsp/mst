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
    core/installcontroller.cpp \
    core/configuration.cpp \
    core/utilites/utilites.cpp \
    core/dsv_parser/dsv.cpp \
    core/component.cpp \
    core/components/vgl.cpp \
    core/components/display_manager.cpp \
    core/components/awesome.cpp \
    core/template_manager.cpp \
    core/types/template.cpp \
    ui/about_dialog/about_dialog.cpp \
    core/components/xorg.cpp \
    core/components/udev.cpp \
    core/components/sudo.cpp \
    core/components/system.cpp \
    core/platform.cpp \
    core/component_manager.cpp \
    core/types/resolution.cpp \
    core/types/xrandr_monitor.cpp \
    core/types/monitor.cpp \
    core/types/seat.cpp \
    ui/seat_widget/seat_widget.cpp \
    core/path_manager.cpp \
    core/device/device.cpp \
    core/device/device_listener.cpp \
    core/device/usb_device_listener.cpp \
    core/device/input_device_listener.cpp

HEADERS += \
    ui/install_window/installwindow.h \
    ui/calibration_dialog/calibrationdialog.h \
    ui/reboot_dialog/rebootdialog.h \
    core/installcontroller.h \
    core/configuration.h \
    core/utilites/utilites.h \
    core/component.h \
    core/dsv_parser/dsv.h \
    core/components/vgl.h \
    core/components/display_manager.h \
    core/components/awesome.h \
    core/template_manager.h \
    core/types/template.h \
    ui/about_dialog/about_dialog.h \
    core/components/xorg.h \
    core/components/udev.h \
    core/components/sudo.h \
    core/components/system.h \
    core/platform.h \
    core/component_manager.h \
    version.h \
    core/types/resolution.h \
    core/types/xrandr_monitor.h \
    core/types/monitor.h \
    core/types/seat.h \
    ui/seat_widget/seat_widget.h \
    core/path_manager.h \
    core/device/device.h \
    core/device/device_listener.h \
    core/device/usb_device_listener.h \
    core/device/input_device_listener.h

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
