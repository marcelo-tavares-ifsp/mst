TEMPLATE = subdirs
SUBDIRS = src

scripts.files += \
    scripts/mst-start-dm \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh

scripts.path = $$(PREFIX)/bin

templates.files += \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/xmst.template

templates.path = $$(PREIFX)/var/lib/mst

INSTALLS += scripts templates
