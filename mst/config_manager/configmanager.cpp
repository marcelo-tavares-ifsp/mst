#include "configmanager.h"

#include "template_manager/template_manager.h"
#include "template_manager/template.h"

#include "vgl.h"
#include "udev.h"
#include "sudo.h"
#include "display_manager.h"
#include "system.h"

Q_LOGGING_CATEGORY(config_manager_category, "mst.config_manager")

/**
 * @brief ConfigManager::make_rc_lua -- Generate Awesome WM "rc.lua" file.
 *
 * Generate Awesome WM "rc.lua" file based on a template.
 *
 * @param config
 */
void ConfigManager::make_rc_lua(Configuration& config)
{
    awesome::Awesome awesome(config);
    awesome.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

/**
 * @brief ConfigManager::make_xorg -- Make Xorg configuration file based on a
 *     template.
 * @param config -- MST configuration.
 */
void ConfigManager::make_xorg(Configuration& config)
{
    xorg::Xorg xorg(config);
    xorg.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

void ConfigManager::configure_system(Configuration &config)
{
    sys::System sys(config);
    sys.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

/**
 * @brief ConfigManager::make_sudoers -- Generate sudoers file for MST.
 */
void ConfigManager::make_sudoers(Configuration& config)
{
    sudo::Sudo sudo(config);
    sudo.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

/**
 * @brief Controller::make_lightdm_conf -- Generate a LightDM configuration
 *          file.
 *
 * TODO: Use actual number of seats.
 */
void ConfigManager::make_lightdm_conf(Configuration& config)
{
    display_manager::Display_manager dm(config);
    dm.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

void ConfigManager::configure_udev(Configuration& config)
{
    udev::Udev udev(config);
    udev.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

void ConfigManager::make_vgl(Configuration& config)
{
    vgl::VGL vgl(config);
    vgl.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));

}
