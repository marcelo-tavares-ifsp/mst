(use-modules (srfi srfi-64)
             (mst system))


(test-begin "mstd-system")

(test-assert "proc-get-pids"
  (list? (proc-get-pids)))

(test-equal "%make-command:mount"
  "sudo --user='alice' -- udisksctl mount --no-user-interaction --block-device /dev/some/device"
  (%make-command:mount "/dev/some/device" "alice"))

(test-equal "%make-command:notify-send"
  "DISPLAY=:0 /usr/bin/notify-send --urgency=critical --icon=error 'hello world'"
  (%make-command:notify-send 0 "hello world"))

(test-equal "%make-command:display-number->user"
  "who | grep '(:0)' | sed -re 's/^([^ ]+) +.* \\(:[0-9]+\\)/\\1/g' | head -1"
  (%make-command:display-number->user 0))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-system")

(exit result)
