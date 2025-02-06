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

(require 'dash)
(require 'parsec)

(defalias '-parse #'parsec-parse)
(defalias '-with-input #'parsec-with-input)
(defalias '-count-as-string #'parsec-count-as-string)
(defalias '-ch #'parsec-ch)
(defalias '-between #'parsec-between)
(defalias '-collect* #'parsec-collect*)
(defalias '-re #'parsec-re)
(defalias '-optional* #'parsec-optional*)
(defalias '-many1-as-string #'parsec-many1-as-string)
(defalias '-many1 #'parsec-many1)
(defalias '-many-till #'parsec-many-till)
(defalias '-many-till-as-string #'parsec-many-till-as-string)
(defalias '-eol-or-eof #'parsec-eol-or-eof)
(defalias '-eol #'parsec-eol)
(defalias '-any-ch #'parsec-any-ch)
(defalias '-and #'parsec-and)
(defalias '-digit #'parsec-digit)

(defun year ()
  "Parses a four-digit year."
  (-count-as-string 4 (-digit)))

(defun day-or-month* ()
  "Parses a two-digit month or day. Internal."
  (-count-as-string 2 (-digit)))

(defun dash ()
  "Parses a hyphen/dash character. Internal."
  (-ch ?-))

(defun day-or-month ()
  "Parses a two-digit day or month, surrounded by dashes. Internal."
  (-between (dash) (dash) (day-or-month*)))

(defun date ()
  "Parses a date in ISO-8601 format. Returns a list of the components."
  (-collect*
   (year)
   (day-or-month)
   (day-or-month*)))

(defun day-of-week ()
  "Parses a string containing one of the seven English-language
words for the days of the week."
  (let ((days (list "Monday" "Tuesday" "Wednesday" "Thursday"
			     "Friday" "Saturday" "Sunday")))
    (-re
     (regexp-opt days))))

(defun month ()
  "Parses a string containing one of the twelve English-language
words for the months of the year."
  (let ((months (list "January" "February" "March" "April" "May" "June"
		      "July" "August" "September" "October" "November"
		      "December")))
    (-re
     (regexp-opt months))))

(defun ignore-whitespace ()
  "Parses whitespace and ignores the result."
  (-optional* (-ch ?\s)))

(defun heading ()
  "Parses an Org header."
  (-many1-as-string (-ch ?*)))

(defun heading-year ()
  "Parses an Org header of the form
   * yyyy."
  (-collect*
   (heading)
   (ignore-whitespace)
   (year)))

(defun heading-year-month ()
  "Parses an Org header of the form
   * yyyy-mm <Month-name>."
  (-collect*
   (heading)
   (ignore-whitespace)
   (-optional* (year))
   (-optional* (dash))
   (day-or-month*)
   (ignore-whitespace)
   (month)))

(defun heading-year-month-day ()
  "Parses an Org header of the form ** yyyy-mm-dd <Day-name>. Throws
away the year and month, preserving only the date number and
name."
  (-collect*
   (heading)
   (ignore-whitespace)
   (-and (year) (day-or-month) (day-or-month*))
   (ignore-whitespace)
   (day-of-week)))

(defun get-text ()
  "Parses the plain text below an Org header."
  (-many-till
   (-many-till-as-string
    (-any-ch)
    (-eol))
   (heading)))

(defun parse! (inp)
  "Runs the parser."
  (if inp
      (-with-input inp
	(-parse
	 (-collect*
	  (heading-year)
	  (-optional* (-eol))
	  (get-text)
	  (-many1
	   (-collect*
	    (heading-year-month)
	    (-optional* (-eol))
	    (get-text)
	    (heading-year-month-day)
	    (get-text)
	    (-optional* (-eol-or-eof)))))))
    (error "You need to supply an input string to be parsed.")))

(defun create-tree (inp)
  "Creates a flat list of the nodes of the \"tree\" approximated by the nested list."
   (let ((tree (parse! inp)))
     (->> (-tree-seq 'listp 'identity tree)
	  (-remove 'listp))))

(provide 'timeline)
;;; timeline.el ends here
