(use-modules (srfi srfi-64)
             (mst core seat)
	     (mst config))


(test-begin "mstd-config")

(test-assert "read-seats-configuration"
  (let* ((config (read-seats-configuration "mstd-config-seats"))
         (seat1  (config-get-seat config "1"))
         (seat2  (config-get-seat config "2")))
    (and (string=? (seat-display    seat1) "1")
         (string=? (seat-interface  seat1) "LVDS-1")
         (string=? (seat-resolution seat1) "1280x720")
         (string=? (seat-keyboard   seat1) "pci-0000:00:1d.2-usb-0:2:1.0-event-kbd")
         (string=? (seat-mouse      seat1) "pci-0000:00:1d.1-usb-0:2:1.0-event-mouse")
         (string=? (seat-usb        seat1) "/devices/pci0000:00/0000:00:1d.7/usb7/7-1")
         (string=? (seat-display    seat2) "2")
         (string=? (seat-interface  seat2) "LVDS-2")
         (string=? (seat-keyboard   seat2) "pci-0000:00:1d.2-usb-0:2:1.1-event-kbd")
         (string=? (seat-mouse      seat2) "pci-0000:00:1d.1-usb-0:2:1.1-event-mouse")
         (string=? (seat-usb        seat2) "/devices/pci0000:00/0000:00:1d.7/usb7/7-2"))))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-config")

(exit result)
