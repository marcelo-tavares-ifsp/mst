include(mst-vars.pri)

TEMPLATE = subdirs
SUBDIRS = mst tests \
    mstd

isEmpty(PREFIX) {
    PREFIX = /usr
}

scripts.files += \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    scripts/mst-mount	\
    scripts/mst-umount \
    scripts/mst-umount

scripts.path = $${PREFIX}/bin

templates.files += \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/rc.lua.4.template  \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/lightdm-mst.conf.template \
    templates/getty@.service.template \
    templates/systemd-udevd.service.template \
    templates/vgl.sh.template \
    templates/99-mst.rules.template \
    templates/10-mst.polkit.rules.template

templates.path = $$(PREIFX)/var/lib/mst

templates_awesome.path = $$(PREIFX)/var/lib/mst/awesome
templates_awesome.files += \
    templates/awesome/xephyr_rules.lua.template \
    templates/awesome/xephyr_screens.lua.template \
    templates/awesome/mst_autostart.lua.template

templates_xorg.path = $$(PREFIX)/var/lib/mst/xorg
templates_xorg.files += \
    templates/xorg/Device.template \
    templates/xorg/Monitor.template \
    templates/xorg/Option.template \
    templates/xorg/Screen.template \
    templates/xorg/ServerLayout.template

CONFIG += nostrip lrelease embed_translations
    
etc.files += \
    etc/mst

etc.path = /etc/

systemd_services.files += \
    etc/mstd.service
systemd_services.path = /lib/systemd/system

INSTALLS += scripts templates templates_awesome templates_xorg \
    etc systemd_services

DISTFILES += \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/rc.lua.4.template  \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/lightdm-mst.conf.template \
    templates/getty@.service.template \
    templates/systemd-udevd.service.template \
    templates/vgl.sh.template \
    templates/awesome/xephyr_rules.lua.template \
    templates/xorg/Device.template \
    templates/xorg/Monitor.template \
    templates/xorg/Option.template \
    templates/xorg/Screen.template \
    templates/xorg/ServerLayout.template \
    templates/awesome/xephyr_screens.lua.template \
    templates/awesome/mst_autostart.lua.template \
    templates/99-mst.rules.template \
    templates/10-mst.polkit.rules.template \
    etc/mst \
    doc/logo.png \
    i18n/mst_ru.qm

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

GUILE_VERSION=$$system("guile -c '(display (effective-version))'")

guile_udev_build.name = "Build Guile-Udev library."
guile_udev_build.commands += \
    @echo "----- Building Guile-Udev -----"; \
    cd guile-udev \
    && autoreconf -vif \
    && ./configure --with-guilesitedir=/usr/share/guile/site/$${GUILE_VERSION} \
       --prefix=/usr --libdir=$$[QT_INSTALL_LIBS]

guile_udev_install.name = "Install Guile-Udev library."
guile_udev_install.commands += \
    @echo "----- Installing Gule-Udev -----" \
    && cd guile-udev \
    && make -j4 install
guile_udev_install.depends += guile_udev_build

build_deps.name      = "Build dependencies."
build_deps.depends   += guile_udev_build
install_deps.name    = "Install dependencies."
install_deps.depends += guile_udev_install

install_vgl.name = "Install VGL"
install_vgl.commands += bash ./install_vgl.sh 2.6.4

QMAKE_EXTRA_TARGETS += rpm rpm_dist dist guile_udev_build guile_udev_install
QMAKE_EXTRA_TARGETS += build_deps install_deps install_vgl

TRANSLATIONS = i18n/mst_ru.ts
