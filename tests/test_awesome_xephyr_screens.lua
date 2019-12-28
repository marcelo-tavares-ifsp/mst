if is_screen_available(1) then
    os.execute("sudo Xephyr -softCursor -ac -br
                  -mouse  'evdev,5,device=/dev/input/by-path/mouse'
                  -keybd  'evdev,,device=/dev/input/by-path/keyboard'
                  -screen 640x480 :1
                  &")
end
