(use-modules (srfi srfi-64)
             (mst system))


(test-begin "mstd-system")

(test-assert "proc-get-pids"
  (list? (proc-get-pids)))

(test-equal "%make-mount-command"
  "sudo --user='alice' -- udisksctl mount --no-user-interaction --block-device /dev/some/device"
  (%make-mount-command "/dev/some/device" "alice"))

(test-equal "%make-notify-send-command"
  "DISPLAY=:0 /usr/bin/notify-send --urgency=critical --icon=error 'hello world'"
  (%make-notify-send-command 0 "hello world"))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-system")

(exit result)
