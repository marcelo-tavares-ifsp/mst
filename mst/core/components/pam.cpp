#include "pam.h"

PAM::PAM(Configuration& config) : Component(config)
{

}

void PAM::configure()
{
//    component_configuration.add(POLKIT_TEMPLATE_FILE, "/etc/security/pam_env.conf",
//                                prepare_polkit_template());
}
