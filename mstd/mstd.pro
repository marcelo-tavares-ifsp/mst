TEMPLATE = aux

isEmpty(PREFIX) {
    PREFIX = /usr
}

executable.files = mstd
executable.path  = $${PREFIX}/bin/

guile_modules_mst.files = \
    modules/mst/config.scm  \
    modules/mst/system.scm  \
    modules/mst/dm.scm

guile_modules_mst.path = $${PREFIX}/share/guile/site/mst/

DISTFILES = \
    mstd        \
    modules/mst/config.scm \
    modules/mst/system.scm \
    modules/mst/dm.scm

INSTALLS += \
    executable  \
    guile_modules_mst
