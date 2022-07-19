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

generate_mstd.target = mstd
generate_mstd.commands = \
    sed -e 's,[@]GUILE[@],$$PATH_TO_GUILE,g' mstd.in > mstd
    
executable.depends = generate_mstd
QMAKE_EXTRA_TARGETS += generate_mstd

QMAKE_CLEAN += mstd

###

DISTFILES = \
    mstd        \
    mstd.in \
    modules/mst/core/config.scm \
    modules/mst/system.scm \
    modules/mst/dm.scm

INSTALLS += \
    executable  \
    guile_modules_mst \
    guile_modules_mst_core \
    guile_modules_mst_component
