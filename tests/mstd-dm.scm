(use-modules (srfi srfi-64)
             (mst component lightdm))


(test-begin "mstd-dm")

(test-equal "%make-command:add-seat"
  "/usr/bin/dm-tool add-local-x-seat 1"
  (%make-command:add-seat 1))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-dm")

(exit result)
