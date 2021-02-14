TEMPLATE = aux

isEmpty(PREFIX) {
    PREFIX = /usr
}

executable.files = mstd
executable.path  = $${PREFIX}/bin/

guile_modules_mst.files = \
    modules/mst/system.scm

guile_modules_mst.path = $${PREFIX}/share/guile/site/mst/

DISTFILES = \
    mstd        \
    modules/mst/system.scm

INSTALLS += \
    executable  \
    guile_modules_mst
