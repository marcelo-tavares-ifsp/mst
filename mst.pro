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
    templates/xmst.template      \
    templates/lightdm-mst.conf.template \
    getty@.service.template

templates.path = $$(PREIFX)/var/lib/mst

etc.files += \
    etc/mst

etc.path = /etc/

INSTALLS += scripts templates etc

DISTFILES += \
    scripts/mst-start-dm \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/xmst.template      \
    templates/lightdm-mst.conf.template \
    templates/getty@.service.template \
    etc/mst

# We need 'rpmtools' package in AltLinux to do that.
rpm.commands += \
    su - multiseat -c rpmdev-setuptree;		\
    cp mst.spec ~multiseat/rpmbuild/SPECS;		\
    cp mst*.tar.gz ~multiseat/rpmbuild/SOURCES/;	\
    chown multiseat: ~multiseat/rpmbuild/;		\
    su - multiseat -c \'rpmbuild -ba ~/rpmbuild/SPECS/mst.spec\'

rpm.depends = dist

QMAKE_EXTRA_TARGETS += rpm
