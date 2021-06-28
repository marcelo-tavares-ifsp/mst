#ifndef VGL_H
#define VGL_H

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

#include <QLoggingCategory>
#include <string>
#include <stdlib.h>

Q_DECLARE_LOGGING_CATEGORY(vgl_category)

namespace vgl {


//// Constants.

static const QString VGL_SH_FILE = "vgl.sh";


//// The main class.

class VGL : public Component
{
public:
    VGL(Configuration& config);
    void configure() override;
    void enable() override;
    void disable() override;

    void prepare_vgl_sh_template(Template& tpl);
};


//// Helper procedures.


}

#endif // VGL_H
