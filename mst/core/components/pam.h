#ifndef PAM_H
#define PAM_H

#include "../component.h"

class PAM : public Component
{
public:
    PAM(Configuration& config);

    void configure() override;
};

#endif // PAM_H
