;;; timeline.el
;;; An attempt at parsing Org datetrees into timelines for interactive viewing

(require 'parsec)

(defun year ()
  "Parses a four-digit year."
  (parsec-count-as-string 4 (parsec-digit)))

(defun day-or-month* ()
  "Parses a two-digit month or day. Internal."
  (parsec-count-as-string 2 (parsec-digit)))

(defun dash ()
  "Parses a hyphen/dash character. Internal."
  (parsec-ch ?-))

(defun day-or-month ()
  "Parses a two-digit day or month, surrounded by dashes. Internal."
  (parsec-between (dash) (dash) (day-or-month*)))

(defun date ()
  "Parses a date in ISO-8601 format. Returns a list of the components."
  (parsec-collect*
   (year)
   (day-or-month)
   (day-or-month*)))

(defun day-of-week ()
  "Parses a string containing one of the seven English-language
words for the days of the week."
  (parsec-re
   (regexp-opt (list "Monday" "Tuesday" "Wednesday" "Thursday"
		     "Friday" "Saturday" "Sunday"))))

(defun ignore-whitespace ()
  "Parses whitespace and ignores the result."
  (parsec-optional* (parsec-ch ?\s)))

(defun heading ()
  "Parses an Org-mode header."
  (parsec-many1-as-string (parsec-ch ?*)))

(defun heading-year ()
  "Parses an Org header followed by an optional date-string."
  (parsec-collect*
   (heading)
   (parsec-optional* (parsec-string " "))
   (parsec-optional-maybe (year))))

(defun heading-year-month ()
  (parsec-collect*
   (heading-year)
   (parsec-ch ?-)
   (parsec-many-as-string (parsec-digit))
   (parsec-optional* (parsec-string " "))
   (parsec-many-as-string (parsec-letter))))

(defun heading-year-month-day ()
  (parsec-and (heading-year-month)
	      (parsec-optional* (parsec-string " "))
	      (parsec-many-as-string (parsec-letter))))

(provide 'timeline)
;;; timeline.el ends here
