#ifndef XORG_CONFIG_H
#define XORG_CONFIG_H

#include "core/configuration.h"
#include "../types/template.h"
#include "iostream"
#include <QLoggingCategory>

#include "component.h"

using namespace std;

namespace xorg {

//// Constants.

static const QString XORG_FILE = "xorg.conf";
static const QString XINIT_RC_FILE    = "xinitrc";
static const QString XMST_FILE        = "xmst";

class Xorg : public Component
{
public:
    Xorg(Configuration& config);
    void configure() override;
    void enable() override {
        /* Do nothing. */
    }
    void disable() override {
        /* Do nothing. */
    }
    QString get_version() override {
        // TODO:
        throw Component_error("Unimplemented");
    }
};


//// Helper procedures.
Template prepare_xinitrc_template();
Template prepare_xmst_template();

}

#endif // XORG_CONFIG_H
