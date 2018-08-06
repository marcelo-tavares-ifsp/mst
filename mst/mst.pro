#-------------------------------------------------
#
# Project created by QtCreator 2018-04-28T11:15:51
#
#-------------------------------------------------


QMAKE_CXXFLAGS += -std=c++11

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

version.commands += \
    echo \'const string VERSION = \"$$system("git rev-parse HEAD")\";\' \
        > version.h

TARGET = mst
TEMPLATE = app

PRE_TARGETDEPS += version

QMAKE_EXTRA_TARGETS += version

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0


SOURCES += \
        main.cpp \
        mainwindow.cpp \
    lightdm/display-manager.cpp \
    xorg/xorg-config.cpp \
    awesome/awesome-config.cpp \
    settings-mst.cpp \
    utils.cpp \
    input-device-listener.cpp \
    seat_calibration_dialog.cpp \
    seat.cpp \
    dsv.cpp \
    config.cpp \
    controller.cpp \
    reboot_dialog.cpp

HEADERS += \
        mainwindow.h \
    lightdm/display-manager.h \
    xorg/xorg-config.h \
    awesome/awesome-config.h \
    settings-mst.h \
    utils.h \
    input-device-listener.h \
    seat_calibration_dialog.h \
    seat.h \
    dsv.h \
    config.h \
    controller.h \
    reboot_dialog.h \
    version.h

FORMS += \
        mainwindow.ui \
    seat_calibration_dialog.ui \
    reboot_dialog.ui

target.path = $$(PREFIX)/bin

INSTALLS += target
