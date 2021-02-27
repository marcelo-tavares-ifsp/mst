(use-modules (srfi srfi-64)
	     (mst config))


(test-begin "mstd-config")

(test-assert "read-seats-configuration"
  (let ((config (read-seats-configuration "mstd-config-seats")))
    (and (string=? (caar   config) "/device/path/1")
    	 (string=? (cadar  config) "1")
    	 (string=? (caadr  config)  "/device/path/2")
    	 (string=? (cadadr config) "2"))))


(define result (test-runner-fail-count (test-runner-current)))

(test-end "mstd-config")

(exit result)
