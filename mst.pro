PROJECT_NAME = mst
VERSION      = 1.0.0
DIST_NAME    = $$PROJECT_NAME$$VERSION

TEMPLATE = subdirs
SUBDIRS = mst

scripts.files += \
    scripts/mst-start-dm \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    scripts/mst-mount

scripts.path = $$(PREFIX)/bin

templates.files += \
    templates/bashrc.template    \
    templates/rc.lua.template    \
    templates/rc.lua.4.template  \
    templates/sudoers.template   \
    templates/xinitrc.template   \
    templates/xmst.template      \
    templates/lightdm-mst.conf.template \
    templates/systemd-udevd.service.template \
    templates/getty@.service.template \
    templates/vgl.sh.template

templates.path = $$(PREIFX)/var/lib/mst

etc.files += \
    etc/mst

etc.path = /etc/

INSTALLS += scripts templates etc

DISTFILES += \
    scripts/mst-start-dm \
    scripts/mk_backup.sh \
    scripts/apl_backup.sh \
    scripts/mst-mount \
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
    etc/mst

#version.commands += \
#    echo \"Building $$system("git rev-parse HEAD") ...\"; \
#    cd mst && make version

# We need 'rpmtools' and 'rpmdevtools' package in AltLinux to do that.
rpm.commands += \
    su - multiseat -c rpmdev-setuptree;		\
    ln -s ~multiseat/RPM ~multiseat/rpmbuild;          \
    cp mst.spec ~multiseat/rpmbuild/SPECS;		\
    cp $$DIST_NAME\\.tar.gz ~multiseat/rpmbuild/SOURCES/;	\
    chown multiseat: ~multiseat/rpmbuild/;		\
    su - multiseat -c \'rpmbuild -ba ~/rpmbuild/SPECS/mst.spec\'

rpm.depends += version dist

dist.commands += cd mst && make -j4 dist;
dist.commands += \
    cd ..;                      \
    mkdir $$DIST_NAME;    \
    cd $$DIST_NAME;      \
    tar -xzf ../mst/$$DIST_NAME\\.tar.gz;   \
    mv $$DIST_NAME mst;                     \
    cp -r ../templates ../scripts ../etc .;     \
    cd ..;                                      \
    tar -czpf $$DIST_NAME\\.tar.gz $$DIST_NAME; \
    rm -rf $$DIST_NAME

QMAKE_EXTRA_TARGETS += rpm version dist
