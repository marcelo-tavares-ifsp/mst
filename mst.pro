TEMPLATE = subdirs
SUBDIRS = mst

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

# We need 'rpmtools' package in AltLinux to do that.
rpm.commands += \
    su - multiseat -c rpmdev-setuptree;		\
    cp mst.spec ~multiseat/RPM/SPECS;		\
    cp mst*.tar.gz ~multiseat/RPM/SOURCES/;	\
    chown multiseat: ~multiseat/RPM/;		\
    su - multiseat -c "rpmbuild -ba mst.spec"

rpm.depends = dist

QMAKE_EXTRA_TARGETS += rpm
