#-------------------------------------------------
#
# Project created by QtCreator 2018-04-28T11:15:51
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = mst
TEMPLATE = app

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
    xorg/xorg-monitor.cpp \
    awesome/awesome-config.cpp \
    awesome/awesome-device.cpp \
    controller_mst.cpp

HEADERS += \
        mainwindow.h \
    lightdm/display-manager.h \
    xorg/xorg-config.h \
    xorg/xorg-monitor.h \
    awesome/awesome-config.h \
    awesome/awesome-device.h \
    controller_mst.h

FORMS += \
        mainwindow.ui
