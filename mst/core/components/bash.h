#ifndef BASH_H
#define BASH_H

#include "../component.h"

Q_DECLARE_LOGGING_CATEGORY(component_bash_category)

namespace bash {

const QString BASH_PROFILE_FILE = "bash_profile";
const QString BEGIN_MARK = "BEGIN: Added by MST";
const QString END_MARK   = "END: Added by MST";
const QString COMMENT_MARK = "#";

class Bash : public Component
{
public:
    Bash(Configuration& config);
    QString get_version() override;

    void configure() override;
    void install() override;

    bool installed_p(const QString& path);
};

Template prepare_bash_profile_template();

}

#endif // BASH_H
