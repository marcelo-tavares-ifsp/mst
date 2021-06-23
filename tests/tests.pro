#-------------------------------------------------
#
# Project created by QtCreator 2018-06-15T13:12:07
#
#-------------------------------------------------

QT       += testlib core

QT       -= gui

TARGET = unit_tests
CONFIG   += console
CONFIG   += testcase
CONFIG   += silent
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

INCLUDEPATH += ../mst/

SOURCES += \
    ../mst/core/utilites/utilites.cpp \
    unit_tests.cpp \
    test_utils.cpp \
    test_template.cpp \
    test_component.cpp \
    test_monitor.cpp \
    test_seat.cpp \
    test_resolution.cpp \
    test_mstd.cpp

SOURCES += \
    ../mst/core/configuration.cpp \
    ../mst/core/platform.cpp \
    ../mst/core/component.cpp \
    ../mst/core/components/awesome.cpp \
    test_awesome.cpp

SOURCES += \
    ../mst/core/types/template.cpp \
    ../mst/core/types/seat.cpp \
    ../mst/core/types/monitor.cpp \
    ../mst/core/types/resolution.cpp \
    ../mst/core/types/xrandr_monitor.cpp \
    ../mst/core/template_manager.cpp \
    ../mst/core/path_manager.cpp \
    ../mst/core/dsv_parser/dsv.cpp

HEADERS += \
    ../mst/core/configuration.h \
    ../mst/core/utilites/utilites.h \
    ../mst/core/types/seat.h \
    ../mst/core/types/monitor.h \
    test_awesome.h \
    test_utils.h \
    test_template.h \
    test_component.h \
    test_monitor.h \
    test_seat.h \
    test_resolution.h \
    test_mstd.h

DEFINES += SRCDIR=\\\"$$PWD/\\\"

DISTFILES += \
    test_awesome_xephyr_rules.lua \
    test_awesome_xephyr_screens.lua \
    test_template.txt.template \
    test_awesome_mst_autostart.lua \
    mstd-system.scm \
    mstd-config.scm \
    mstd-dm.scm \
    mstd-config-seats

DEFINES += INSTALLATION_PREFIX=\\\"\\\"

copydata.commands = cd $$PWD; \
    $(COPY) $$DISTFILES $$OUT_PWD || :

copy_guile_modules.commands = \
    $(COPY) -r ../mstd/modules $$OUT_PWD/ || :

first.depends = $(first) copydata copy_guile_modules
export(first.depends)
export(copydata.commands)
export(copy_guile_modules.commands)

QMAKE_EXTRA_TARGETS += \
    first       \
    copydata    \
    copy_guile_modules
