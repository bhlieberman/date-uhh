;;; timeline.el ---  parse Org datetrees into timelines -*- lexical-binding:t -*-

;; Author: Ben Lieberman
;; Version: 0.1
;; Package-Requires ((parsec "20180730.16"))
;; Keywords: parsing, Org-mode
;; URL: https://github.com/bhlieberman/date-uhh

;;; Commentary

;; This package provides simple facilities for parsing Org date-trees of the
;; form:
;; * 2015
;; ** 2015-06 June
;; *** 2015-06-01 Monday
;; into a visual timeline. Primarily intended for writing/research-oriented
;; projects that want to take advantage of Org's strong integration with
;; the Emacs calendar while also providing a linear view of the dates in
;; question outside the context of the Org agenda.

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
  (let ((days (list "Monday" "Tuesday" "Wednesday" "Thursday"
			     "Friday" "Saturday" "Sunday")))
    (parsec-re
     (regexp-opt days))))

(defun month ()
  "Parses a string containing one of the twelve English-language
words for the months of the year."
  (let ((months (list "January" "February" "March" "April" "May" "June"
		      "July" "August" "September" "October" "November"
		      "December")))
    (parsec-re
     (regexp-opt months))))

(defun ignore-whitespace ()
  "Parses whitespace and ignores the result."
  (parsec-optional* (parsec-ch ?\s)))

(defun heading ()
  "Parses an Org header."
  (parsec-many1-as-string (parsec-ch ?*)))

(defun heading-year ()
  "Parses an Org header of the form
   * yyyy."
  (parsec-collect*
   (heading)
   (ignore-whitespace)
   (year)))

(defun heading-year-month ()
  "Parses an Org header of the form
   * yyyy-mm <Month-name>."
  (parsec-collect*
   (heading)
   (ignore-whitespace)
   (parsec-optional* (year))
   (parsec-optional* (dash))
   (day-or-month*)
   (ignore-whitespace)
   (month)))

(defun heading-year-month-day ()
  "Parses an Org header of the form ** yyyy-mm-dd <Day-name>. Throws
away the year and month, preserving only the date number and
name."
  (parsec-collect*
   (heading)
   (ignore-whitespace)
   (parsec-and (year) (day-or-month) (day-or-month*))
   (ignore-whitespace)
   (day-of-week)))

(defun parse* (inp)
  (when inp
    (parsec-with-input inp
      (parsec-parse
       (parsec-collect*
	(heading-year)
	(parsec-optional* (parsec-eol))
	(heading-year-month)
	(parsec-optional* (parsec-eol))
	(heading-year-month-day)
	(parsec-optional* (parsec-eol-or-eof)))))))

(defun parse! (inp)
  "Runs the parser."
  (parse* inp))

(defun create-tree (inp)
   "WIP."
   (cl-destructuring-bind ((_ year) (_ month _) (_ day _)) (parse! inp)
     `(,year ,month ,day)))

(provide 'timeline)
;;; timeline.el ends here
