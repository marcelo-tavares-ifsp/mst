#ifndef COMPONENT_H
#define COMPONENT_H

#include <unordered_map>

#include "../configuration/configuration.h"

class Component
{
public:
    Component(Configuration& config);
    virtual void configure(const QString& output_dir) = 0;
    virtual QString get_version() = 0;

protected:
    Configuration config;
};

#endif // COMPONENT_H
