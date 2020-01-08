#ifndef XORG_CONFIG_H
#define XORG_CONFIG_H

#include "configuration/configuration.h"
#include "iostream"
#include <QLoggingCategory>

#include "component.h"

using namespace std;

namespace xorg {

//// Constants.

static const QString XORG_FILE = "xorg.conf";

class Xorg : public Component
{
public:
    Xorg(Configuration& config);
    void configure(const QString &output_dir) override;
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

}

#endif // XORG_CONFIG_H
