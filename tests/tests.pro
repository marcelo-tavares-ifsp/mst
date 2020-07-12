#-------------------------------------------------
#
# Project created by QtCreator 2018-06-15T13:12:07
#
#-------------------------------------------------

QT       += testlib core

QT       -= gui

TARGET = unit_tests
CONFIG   += console
CONFIG   -= app_bundle

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

INCLUDEPATH += ../

SOURCES += \
    ../mst/core/utilites/utilites.cpp \
    unit_tests.cpp \
    test_utils.cpp \
    test_template.cpp \
    test_component.cpp \
    test_monitor.cpp \
    test_seat.cpp \
    test_resolution.cpp

SOURCES += \
    ../mst/configuration/configuration.cpp \
    ../mst/components/component.cpp \
    ../mst/components/awesome.cpp \
    test_awesome.cpp

SOURCES += \
    ../mst/template_manager/template.cpp \
    ../mst/template_manager/template_manager.cpp \
    ../mst/path_manager/pathmanager.cpp \
    ../mst/core/dsv_parser/dsv.cpp \
    ../mst/core/seat/seat.cpp \
    ../mst/core/monitor/monitor.cpp \
    ../mst/core/resolution/resolution.cpp \
    ../mst/core/xrandr_monitor/xrandr_monitor.cpp

HEADERS += \
    ../mst/configuration/configuration.h \
    ../mst/core/utilites/utilites.h \
    ../mst/core/seat/seat.h \
    ../mst/core/monitor/monitor.h \
    test_awesome.h \
    test_utils.h \
    test_template.h \
    test_component.h \
    test_monitor.h \
    test_seat.h \
    test_resolution.h

DEFINES += SRCDIR=\\\"$$PWD/\\\"

DISTFILES += \
    test_awesome_xephyr_rules.lua \
    test_awesome_xephyr_screens.lua \
    test_template.txt.template \
    test_awesome_mst_autostart.lua

DEFINES += INSTALLATION_PREFIX=\\\"\\\"
