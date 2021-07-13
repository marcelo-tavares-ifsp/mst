(define-module (mst device-listener)
  #:use-module (udev udev)
  #:use-module (udev monitor)
  #:use-module (udev device)
  #:use-module (mst config)
  #:use-module (mst system)
  #:use-module (mst core log)
  #:export (device-listener-start))


(define (notify-broadcast config message)
  "Broadcast a notify with a MESSAGE across all seats that are listed
in a CONFIG."
  (for-each (lambda (rec)
              (notify-send (seat-display rec) message))
            config))

(define (error-handler monitor error-message)
  (log-error "ERROR: In ~a: ~a~%" monitor error-message))


(define (device-listener-start config)
  "Start udev listener."
  (let* ((udev         (make-udev))
         (udev-monitor (make-udev-monitor udev
                                          #:timeout-sec 1
                                          #:error-callback error-handler
                                          #:filter '("block" "partition")))
         (udev-input-monitor (make-udev-monitor udev
                                                #:timeout-sec 1
                                                #:error-callback error-handler
                                                #:filter '("usb" "usb_device"))))
    (log-info "Config: ~a~%" config)

    (let ((callback
           (lambda (device)
             (when (string=? (udev-device-get-action device) "add")
                   (let* ((devpath        (udev-device-get-devpath device))
                          (display-number (device-path->display-number config
                                                                       devpath))
                          (user           (display-number->user display-number))
                          (devname (udev-device-get-property-value device
                                                                   "DEVNAME")))
                     (if user
                         (begin
                           (log-info "Mounting ~a for seat ~a (user ~a)~%"
                                     devpath display-number user)
                           (mount devname user))
                         (display "Could not determine user.")))))))

      (udev-monitor-set-callback! udev-monitor callback))

    (let ((callback
           (lambda (device)
             (let ((action  (udev-device-get-property-value device "ACTION"))
                   (devname (udev-device-get-property-value device
                                                            "DEVNAME"))
                   (devpath (udev-device-get-devpath device)))
               (cond
                ((string=? action "remove")
                 (log-info "Device ~a was removed.~%" devname)
                 (unless (device-path->display-number config devpath)
                         (notify-broadcast config
                                           (format #f
                                                   (string-append
                                                    "Seat input device ~a was removed.~%"
                                                    "Please re-connect the device and"
                                                    " reboot the computer.")
                                                   devname))))
                ((string=? action "add")
                 (log-info "Device ~a was added.~%" devname)))))))

      (udev-monitor-set-callback! udev-input-monitor callback))

    (udev-monitor-start-scanning! udev-monitor)
    (udev-monitor-start-scanning! udev-input-monitor)
    (while #t
           (sleep 1))))
