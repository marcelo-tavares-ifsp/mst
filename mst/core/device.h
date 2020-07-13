#ifndef DEVICE_H
#define DEVICE_H

#include <unistd.h>
#include <linux/input.h>
#include <libudev.h>
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(device_category)

namespace device {

//// Procedures.

ssize_t try_read(int fd, struct input_event* ie);
bool is_btn_pressed(struct input_event &e);

}

#endif // DEVICE_H
