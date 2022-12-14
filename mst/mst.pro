#-------------------------------------------------
#
# Project created by QtCreator 2018-12-16T10:50:20
#
#-------------------------------------------------
include(../mst-vars.pri)

QT       += core gui
LIBS += -ludev

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = mst
TEMPLATE = app

executable.files = mst
executable.path = $${PREFIX}/bin/

MST_VERSION = $${VERSION}
MST_HASH    = $$system("[ ! -e '../.git' ] || git rev-parse --short HEAD")
!isEmpty(MST_HASH) {
    MST_VERSION_FULL = $${MST_VERSION}-$${MST_HASH}
} else {
    MST_VERSION_FULL = $${MST_VERSION}-$${MST_HASH}
}



PATH_TO_XRANDR = $$system(which xrandr)
isEmpty(PATH_TO_XRANDR) {
    warning("xrandr not found")
} else {
    message("xrandr binary: " $${PATH_TO_XRANDR})
}

PATH_TO_PGREP = $$system(which pgrep)
isEmpty(PATH_TO_PGREP) {
    warning("pgrep not found")
} else {
    message("pgrep binary: " $${PATH_TO_PGREP})
}

PATH_TO_PKILL = $$system(which pkill)
isEmpty(PATH_TO_PKILL) {
    warning("pkill not found")
} else {
    message("pkill binary: " $${PATH_TO_PKILL})
}

PATH_TO_XSET = $$system(which xset)
isEmpty(PATH_TO_XSET) {
    warning("xset not found")
} else {
    message("xset binary: " $${PATH_TO_XSET})
}

PATH_TO_SYSTEMCTL = $$system(which systemctl)
isEmpty(PATH_TO_SYSTEMCTL) {
    warning("systemctl not found")
    PATH_TO_SYSTEMCTL = "/usr/bin/systemctl"
    warning("assuming default systemctl path: " $${PATH_TO_SYSTEMCTL})
} else {
    message("systemctl binary: " $${PATH_TO_SYSTEMCTL})
}

PATH_TO_AWESOME = $$system(which awesome)
isEmpty(PATH_TO_AWESOME) {
    warning("awesome not found")
} else {
    message("awesome binary: " $${PATH_TO_AWESOME})
}

PATH_TO_LIGHTDM = $$system(which lightdm)
isEmpty(PATH_TO_LIGHTDM) {
    warning("lightdm not found")
} else {
    message("lightdm binary: " $${PATH_TO_LIGHTDM})
}

PATH_TO_DM_TOOL = $$system(which dm-tool)
isEmpty(PATH_TO_DM_TOOL) {
    warning("dm-tool not found")
} else {
    message("dm-tool binary: " $${PATH_TO_DM_TOOL})
}

PATH_TO_VGLCLIENT = $$system(which vglclient)
isEmpty(PATH_TO_VGLCLIENT) {
    warning("vglclient not found")
} else {
    message("vglclient binary: " $${PATH_TO_VGLCLIENT})
}

PATH_TO_VGLSERVER_CONFIG = $$system(which vglserver_config)
isEmpty(PATH_TO_VGLSERVER_CONFIG) {
    warning("vglserver_config not found")
} else {
    message("vglserver_config binary: " $${PATH_TO_VGLSERVER_CONFIG})
}

PATH_TO_X = $$system(which X)
isEmpty(PATH_TO_X) {
    warning("X server not found")
} else {
    message("X server binary: " $${PATH_TO_X})
}

PATH_TO_XEPHYR = $$system(which Xephyr)
isEmpty(PATH_TO_XEPHYR) {
    warning("Xephyr not found")
} else {
    message("Xephyr binary: " $${PATH_TO_XEPHYR})
}

PATH_TO_BASH = $$system(which bash)
isEmpty(PATH_TO_BASH) {
    warning("Bash not found")
} else {
    message("Bash binary: " $${PATH_TO_BASH})
}

QMAKE_SUBSTITUTES = config.h.in
DISTFILES += config.h.in

PRE_TARGETDEPS += config.h

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++11 strip lrelease
CONFIG += silent

SOURCES += \
    core/component.cpp \
    core/component_manager.cpp \
    core/components/awesome.cpp \
    core/components/bash.cpp \
    core/components/display_manager.cpp \
    core/components/pam.cpp \
    core/components/polkit.cpp \
    core/components/sudo.cpp \
    core/components/system.cpp \
    core/components/udev.cpp \
    core/components/vgl.cpp \
    core/components/xorg.cpp \
    core/configuration.cpp \
    core/device/device.cpp \
    core/device/device_listener.cpp \
    core/device/input_device_listener.cpp \
    core/device/usb_device_listener.cpp \
    core/dsv_parser/dsv.cpp \
    core/mst.cpp \
    core/platform.cpp \
    core/template_manager.cpp \
    core/types/monitor.cpp \
    core/types/resolution.cpp \
    core/types/seat.cpp \
    core/types/template.cpp \
    core/types/xrandr_monitor.cpp \
    core/utilites/utilites.cpp \
    main.cpp \
    ui/about_dialog/about_dialog.cpp \
    ui/calibration_dialog/calibrationdialog.cpp \
    ui/install_window/installwindow.cpp \
    ui/reboot_dialog/reboot_dialog.cpp \
    ui/seat_widget/seat_widget.cpp

HEADERS += \
    core/component.h \
    core/component_manager.h \
    core/components/awesome.h \
    core/components/bash.h \
    core/components/display_manager.h \
    core/components/pam.h \
    core/components/polkit.h \
    core/components/sudo.h \
    core/components/system.h \
    core/components/udev.h \
    core/components/vgl.h \
    core/components/xorg.h \
    core/configuration.h \
    core/device/device.h \
    core/device/device_listener.h \
    core/device/input_device_listener.h \
    core/device/usb_device_listener.h \
    core/dsv_parser/dsv.h \
    core/mst.h \
    core/platform.h \
    core/template_manager.h \
    core/types/monitor.h \
    core/types/resolution.h \
    core/types/seat.h \
    core/types/template.h \
    core/types/xrandr_monitor.h \
    core/utilites/utilites.h \
    ui/about_dialog/about_dialog.h \
    ui/calibration_dialog/calibrationdialog.h \
    ui/install_window/installwindow.h \
    ui/reboot_dialog/reboot_dialog.h \
    ui/seat_widget/seat_widget.h \
    config.h

FORMS += \
    ui/install_window/installwindow.ui \
    ui/calibration_dialog/calibrationdialog.ui \
    ui/reboot_dialog/rebootdialog.ui \
    ui/about_dialog/about_dialog.ui

isEmpty(PREFIX) {
    PREFIX = /
}
DEFINES += INSTALLATION_PREFIX=\\\"$${PREFIX}\\\"

INSTALLS += \
    executable

DISTFILES +=

RESOURCES = \
    ../resources.qrc
