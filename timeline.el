;;; timeline.el
;;; An attempt at parsing Org datetrees into timelines for interactive
;;; viewing

(require 'parsec)

(defun year () (parsec-count-as-string 4 (parsec-digit)))

(defun day-or-month () (parsec-count-as-string 2 (parsec-digit)))

(defun date () (parsec-collect-as-string
		(year)
		(parsec-ch ?-)
		(day-or-month)
		(parsec-ch ?-)
		(day-or-month)))

(defun day-of-week ()
  (parsec-re
   (regexp-opt (list "Monday" "Tuesday" "Wednesday" "Thursday"
		     "Friday" "Saturday" "Sunday"))))

(parsec-with-input "Monday"
  (parsec-parse (day-of-week)))

(defun heading () (parsec-many1-as-string (parsec-ch ?*)))

(parsec-with-input "1912-06-01" (parsec-parse (date)))

(defun heading-year ()
  "Parses an Org header (string of * followed by optional title)"
  (parsec-and
   (heading)
   (parsec-string " ")
   (year)))

(parsec-with-input "**** 2015" (parsec-parse (heading-year)))

(defun heading-year-month ()
  (parsec-and (heading-year)
	      (parsec-ch ?-)
	      (parsec-many-as-string (parsec-digit))
	      (parsec-string " ")
	      (parsec-many-as-string (parsec-letter))))

(defun heading-year-month-day ()
  (parsec-and (heading-year-month)
	      (parsec-string " ")
	      (parsec-many-as-string (parsec-letter))))

;; example:
;; * 1912
;; ** 1912-06 June
;; *** 1912-06-01 Saturday

(parsec-with-input "* 1912-06 June"
  (parsec-parse (heading-year-month-day)))
