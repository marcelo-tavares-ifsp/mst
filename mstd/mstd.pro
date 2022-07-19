TEMPLATE = aux

CONFIG += nostrip

isEmpty(PREFIX) {
    PREFIX = /usr
}

executable.files = mstd
executable.path  = $${PREFIX}/bin/

# XXX: ALT Linux 8, 9, 10 does not have (system foreign-library) module
# in GNU Guile, even in Guile 3.0, so we need to install the module
# on such systems.
#DISTRIBUTION = $$system(lsb_release -i | cut -f2)
#contains(DISTRIBUTION, "ALT") {
#    guile_modules_system.files = \
#        modules/system/foreign-library.scm
#    guile_modules_system.path = \
#        $${PREFIX}/share/guile/site/system/
#    INSTALLS += guile_modules_system
#}

guile_modules_mst.files = \
    modules/mst/docker.scm  \
    modules/mst/system.scm  \
    modules/mst/config.scm \
    modules/mst/device-listener.scm \
    modules/mst/unmouser.scm \
    modules/mst/dm.scm

guile_modules_mst.path = $${PREFIX}/share/guile/site/mst/

guile_modules_mst_core.files = \
     modules/mst/core/config.scm  \
     modules/mst/core/log.scm   \
     modules/mst/core/seat.scm \
     modules/mst/core/docker-container.scm

guile_modules_mst_core.path = $${PREFIX}/share/guile/site/mst/core/

### (mst component) module

guile_modules_mst_component.files = \
     modules/mst/component/lightdm.scm \
     modules/mst/component/awesome.scm \
     modules/mst/component/xephyr.scm \
     modules/mst/component/docker.scm

guile_modules_mst_component.path = $${PREFIX}/share/guile/site/mst/component/

PATH_TO_GUILE = $$system(which guile)
isEmpty(PATH_TO_GUILE) {
    warning("Guile not found")
} else {
    message("Guile binary: " $${PATH_TO_GUILE})
}

PATH_TO_DOCKER = $$system(which docker)
isEmpty(PATH_TO_DOCKER) {
    warning("Docker not found")
} else {
    message("Docker binary: " $${PATH_TO_DOCKER})
}

PATH_TO_NOTIFY_SEND = $$system(which notify-send)
isEmpty(PATH_TO_NOTIFY_SEND) {
    warning("notify-send not found")
} else {
    message("notify-send binary: " $${PATH_TO_NOTIFY_SEND})
}

generate_mstd.target = mstd
generate_mstd.commands = \
    sed -e 's,[@]GUILE[@],$$PATH_TO_GUILE,g' mstd.in > mstd
    
generate_config_scm.target = modules/mst/config.scm
generate_config_scm.commands = \
    sed -e 's,[@]PATH_TO_DOCKER[@],$$PATH_TO_DOCKER,g' \
        -e 's,[@]PATH_TO_NOTIFY_SEND[@],$$PATH_TO_NOTIFY_SEND,g' \
        modules/mst/config.scm.in > modules/mst/config.scm

generate_mstd.depends = generate_config_scm
executable.depends = generate_mstd
QMAKE_EXTRA_TARGETS += generate_mstd generate_config_scm

PRE_TARGETDEPS += modules/mst/config.scm

QMAKE_CLEAN += mstd modules/mst/config.scm

###

DISTFILES = \
    mstd        \
    mstd.in \
    modules/mst/config.scm.in  \
    modules/mst/config.scm  \
    modules/mst/core/config.scm \
    modules/mst/system.scm \
    modules/mst/dm.scm

INSTALLS += \
    executable  \
    guile_modules_mst \
    guile_modules_mst_core \
    guile_modules_mst_component
