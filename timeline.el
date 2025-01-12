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
   (ignore-whitespace)
   (parsec-optional-maybe (year))))

(defun heading-year-month ()
  (parsec-collect*
   (heading-year)
   (parsec-ch ?-)
   (parsec-many-as-string (parsec-digit))
   (ignore-whitespace)
   (parsec-many-as-string (parsec-letter))))

(defun heading-year-month-day ()
  (parsec-and (heading-year-month)
	      (ignore-whitespace)
	      (parsec-many-as-string (parsec-letter))))

(provide 'timeline)
;;; timeline.el ends here
