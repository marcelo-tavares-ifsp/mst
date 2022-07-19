(use-modules (srfi srfi-64)
             (ice-9 regex)
             (mst component lightdm))


(test-begin "mstd-dm")

(test-assert "%make-command:add-seat"
  (string-match
   ".* add-local-x-seat 1"
   (%make-command:add-seat 1)))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-dm")

(exit result)
