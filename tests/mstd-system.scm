(use-modules (srfi srfi-64)
	     (mst system))


(test-begin "mstd-system")

(test-assert "proc-get-pids"
  (list? (proc-get-pids)))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-system")

(exit result)
