TEMPLATE = aux

CONFIG += nostrip

isEmpty(PREFIX) {
    PREFIX = /usr
}

executable.files = mstd
executable.path  = $${PREFIX}/bin/

guile_modules_mst.files = \
    modules/mst/config.scm  \
    modules/mst/docker.scm  \
    modules/mst/system.scm  \
    modules/mst/device-listener.scm \
    modules/mst/dm.scm

guile_modules_mst.path = $${PREFIX}/share/guile/site/mst/

guile_modules_mst_core.files = \
     modules/mst/core/log.scm   \
     modules/mst/core/seat.scm

guile_modules_mst_core.path = $${PREFIX}/share/guile/site/mst/core/

### (mst component) module

guile_modules_mst_component.files = \
    modules/mst/component/lightdm.scm

guile_modules_mst_component.path = $${PREFIX}/share/guile/site/mst/component/

###

DISTFILES = \
    mstd        \
    modules/mst/config.scm \
    modules/mst/docker.scm \
    modules/mst/system.scm \
    modules/mst/dm.scm

INSTALLS += \
    executable  \
    guile_modules_mst \
    guile_modules_mst_core \
    guile_modules_mst_component
