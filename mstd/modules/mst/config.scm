;;; config.scm -- MST configuration procedures.

;; Copyright (C) 2021 "AZ Company Group" LLC <https://gkaz.ru/>
;; Copyright (C) 2021 Artyom V. Poptsov <a@gkaz.ru>
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains the procedures reading the MST configuration
;; file.


;;; Code:

(define-module (mst config)
  #:use-module (ice-9 rdelim)
  #:export (read-seats-configuration))

(define (read-seats-configuration config-file)
  "Read seats configuration from a CONFIG-FILE.  Return the
configuration as an alist."
  (let ((port (open-input-file config-file)))
    (let read ((line (read-line port))
	       (data '()))
      (if (eof-object? line)
	  (reverse data)
	  (read (read-line port)
		(cons (string-split line #\space) data))))))

;;; config.scm ends here.

