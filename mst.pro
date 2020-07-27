include(mst-vars.pri)

TEMPLATE = subdirs
SUBDIRS = mst tests \
    mstd

isEmpty(PREFIX) {
    PREFIX = /usr
}

scripts.files += \
    scripts/mst-start-dm \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    scripts/mst-mount	\
    scripts/mst-umount

scripts.path = $${PREFIX}/bin

templates.files += \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/rc.lua.4.template  \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/xmst.template      \
    templates/lightdm-mst.conf.template \
    templates/getty@.service.template \
    templates/systemd-udevd.service.template \
    templates/vgl.sh.template \
    templates/99-mst.rules.template

templates.path = $$(PREIFX)/var/lib/mst

templates_awesome.path = $$(PREIFX)/var/lib/mst/awesome
templates_awesome.files += \
    templates/awesome/xephyr_rules.lua.template \
    templates/awesome/xephyr_screens.lua.template \
    templates/awesome/mst_autostart.lua.template

CONFIG += nostrip
    
etc.files += \
    etc/mst

etc.path = /etc/

INSTALLS += scripts templates templates_awesome etc

DISTFILES += \
    scripts/mst-start-dm \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    scripts/mst-mount \
    scripts/mst-umount \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/rc.lua.4.template  \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/xmst.template      \
    templates/lightdm-mst.conf.template \
    templates/getty@.service.template \
    templates/systemd-udevd.service.template \
    templates/vgl.sh.template \
    templates/awesome/xephyr_rules.lua.template \
    templates/awesome/xephyr_screens.lua.template \
    templates/awesome/mst_autostart.lua.template \
    templates/99-mst.rules.template \
    etc/mst

# We need 'rpmtools' and 'rpmdevtools' package in AltLinux to do that.
rpm.commands += \
    su - multiseat -c rpmdev-setuptree;		\
    ln -s ~multiseat/RPM ~multiseat/rpmbuild;          \
    cp mst.spec ~multiseat/rpmbuild/SPECS;		\
    cp $$DIST_NAME\\.tar.gz ~multiseat/rpmbuild/SOURCES/;	\
    chown multiseat: ~multiseat/rpmbuild/;		\
    su - multiseat -c \'rpmbuild -ba ~/rpmbuild/SPECS/mst.spec\'

rpm.depends += rpm_dist

rpm_dist.commands += cd mst && make version.h && cd .. && make -j4 dist;
rpm_dist.commands += \
    cd ..;                      \
    mkdir $$DIST_NAME;    \
    cd $$DIST_NAME;      \
    tar -xzf ../mst/$$DIST_NAME\\.tar.gz;   \
    mv $$DIST_NAME mst;                     \
    cp -r ../templates ../scripts ../etc .;     \
    cd ..;                                      \
    tar -czpf $$DIST_NAME\\.tar.gz $$DIST_NAME; \
    rm -rf $$DIST_NAME

guile_udev.commands += \
    cd guile-udev \
    && autoreconf -vif \
    && ./configure --with-guilesitedir=/usr/share/guile/site/2.0 --prefix=/usr \
    && make -j4 install

QMAKE_EXTRA_TARGETS += rpm rpm_dist dist guile_udev

RESOURCES += \
    resources.qrc
