(use-modules (srfi srfi-64)
	     (mst config))


(test-begin "mstd-config")

(test-assert "read-seats-configuration"
  (let ((config (read-seats-configuration "mstd-config-seats")))
    config))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-config")

(exit result)
