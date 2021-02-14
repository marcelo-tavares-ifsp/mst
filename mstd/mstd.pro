TEMPLATE = aux

isEmpty(PREFIX) {
    PREFIX = /usr
}

executable.files = mstd
executable.path  = $${PREFIX}/bin/

DISTFILES = mstd

INSTALLS += executable
