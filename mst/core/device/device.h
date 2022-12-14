#ifndef DEVICE_H
#define DEVICE_H

#include <unistd.h>
#include <linux/input.h>
#include <libudev.h>
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(device_category)

namespace device {

class Device_exception : public std::runtime_error
{
  public:
    Device_exception(QString& what)
        : runtime_error(what.toStdString())
    {
        /* Do nothing */
    }
};

//// USB device scanner.

class USB_device_scanner
{
public:
    USB_device_scanner();
    ~USB_device_scanner();

    QString scan();

private:
    struct udev* udev;
    struct udev_monitor* udev_monitor;
    int monitor_fd;
};

//// Procedures.

ssize_t try_read(int fd, struct input_event* ie);

/**
 * @brief is_button_pressed -- Predicate.  Check if any button is pressed
 *     on an input device.
 * @param e -- An input event.
 * @return true if a button is pressed, false otherwise.
 */
bool is_button_pressed(struct input_event &e);

}

#endif // DEVICE_H
