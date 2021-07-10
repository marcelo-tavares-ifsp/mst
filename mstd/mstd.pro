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
    modules/mst/dm.scm

guile_modules_mst.path = $${PREFIX}/share/guile/site/mst/

guile_modules_mst_core.files = \
     modules/mst/core/log.scm   \
     modules/mst/core/seat.scm

guile_modules_mst_core.path = $${PREFIX}/share/guile/site/mst/core/

DISTFILES = \
    mstd        \
    modules/mst/config.scm \
    modules/mst/docker.scm \
    modules/mst/system.scm \
    modules/mst/dm.scm

INSTALLS += \
    executable  \
    guile_modules_mst \
    guile_modules_mst_core
