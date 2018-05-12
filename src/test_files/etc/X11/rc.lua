Section "Monitor"
    Identifier		"monitor0"
EndSection	# Monitor

Section "Monitor"
    Identifier		"monitor1"
EndSection	# Monitor

Section "Device"
    Identifier		"card0"
    Option		"Monitor-DVI-1""monitor0"
    Option		"Monitor-VGA-1""monitor1"
EndSection	# Device

Section "Screen"
    Identifier		"screen0"
    Device		"card0"
    Monitor		"monitor0"
    DefaultDepth		24
    SubSection "Display"
        Depth		24
        Virtual		3840 1080
    EndSubSection	# Display
EndSection	# Screen

Section "ServerLayout"
    Identifier		"seat0"
    Screen		"screen0"
    Option		"Seat"	"seat0"
EndSection	# ServerLayout

Section "ServerLayout"
    Identifier		"seat1"
    Screen		"screen0"
    Option		"Seat"	"seat1"
EndSection	# ServerLayout

