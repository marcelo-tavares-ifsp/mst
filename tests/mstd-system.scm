(use-modules (srfi srfi-64)
             (mst system))


(test-begin "mstd-system")

(test-assert "proc-get-pids"
  (list? (proc-get-pids)))

(test-equal "%make-mount-command"
  "sudo --user='alice' -- udisksctl mount --no-user-interaction --block-device /dev/some/device"
  (%make-mount-command "/dev/some/device" "alice"))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-system")

(exit result)
