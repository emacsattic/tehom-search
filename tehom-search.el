;;; tehom-search.el --- Support sregex.el for searching.

;; Copyright (C) 1999 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Inspired by Bob Glickstein's sregex, this package lets you compose
;; regular expressions for regex searching in a special buffer, in the
;; more readable sregex format.

;; EG, "^a*$" is written as ``bol (star "a" ) eol''

;; Special disclaimer: I wrote it on a whim after seeing sregex.  I
;; haven't added bells and whistles or actual help documentation.
;; This should be considered alpha software.

;; This requires tehom-1.el, tehom-2.el, sregex, and isearch.

;; I've copied (defmacro regex ...) from sregex in order to fix it.
;; This file will automatically load sregex first, but if you change
;; that to make sregex load later, it will not work.

;;; Usage:

;; The entry points are tehom-re-search-forward,
;; tehom-re-search-backward.

;; The "normal" regex keys, such as "*" and "?", will insert the
;; relevant component.  To quote a character, use \C-q (or whatever
;; you have bound quoted-insert to)


;;; Known Bugs:
;;
;; Better help would be very nice.
;;
;; The user variables are not customizable.


;;; Code:

(require 'sregex)     ;;But see (defmacro regex ...) in this file.
(require 'tehom-1)
(require 'tehom-2)

;;;;;;;;;;;;;;;;;;;;
;;; Internal variables

(defvar tehom-tsearch-ring nil 
  "History list for tsearch" )

(defvar tehom-tsearch-ring-position 0 
  "Current position in tsearch history." )

(defvar tehom-tsearch-ring-max 16 
  "*Maximum length of tsearch history before oldest elements are thrown away."
  )


;;;;;;;;;;;;;;;;;;;;;; 
;;History functions

( tehom-define-history-update 
  "tsearch" 
  tehom-tsearch-ring 
  tehom-tsearch-ring-max 
  equal )

(tehom-define-history-motion  
  "tsearch" 
  "search object"
  tehom-tsearch-ring
  'tehom-tsearch-ring-position
  'tehom-write-eform-contents
  nil)


;;;;;;;;;;;;;;
;;Borrowed from sregex.el in order to fix it.  It's still wrong in
;;sregex.el, so don't reload that after this.


(defmacro regex (&rest forms)
  "Construct a regex from a sequence of symbolic clauses.
Each clause may be:

- a string
  This stands for the literal string.  If it contains
  metacharacters, they will be escaped in the resulting regex
  (using `regexp-quote').

- the symbol `dot'
  This stands for a regex matching any character except newline;
  usually \".\"

- the symbol `bol'
  Matches the beginning of a line; usually \"^\"

- the symbol `eol'
  Matches the end of a line; usually \"$\"

- (opt CLAUSE)
  Matches zero or one occurrences of CLAUSE (where CLAUSE is any of
  these clauses).  In most syntaxes, this simply appends \"?\"

- (group CLAUSE ...)
  Groups the given clauses using \"\\(\" and \"\\)\" or \"(\" and \")\"
  (depending on the selected syntax)

- (backref N)
  Matches the same string previously matched by the Nth \"group\" in
  the same regex.  Here, N is a positive integer.

- (or CLAUSE ...)
  Matches any one of the subclauses by separating them with \"\\|\"
  or \"|\"

- (star CLAUSE)
  Matches any number of occurrences of CLAUSE, including zero.

- (plus CLAUSE)
  Matches one or more occurrences of CLAUSE.

- (repeat CLAUSE MIN MAX)
  Matches any number of occurrences of CLAUSE between MIN and MAX,
  inclusive.  MIN and MAX are nonnegative integers.  MAX must not
  be smaller than MIN.  MIN may be `nil' which is the same as zero.
  MAX may be `nil' to mean \"infinity.\"

- (cclass CCLAUSE ...)
  Creates a \"character class\" matching one character from the given
  set.  See below for how to construct a CCLAUSE.

- (not-cclass CCLAUSE ...)
  Creates a \"character class\" matching any one character not in the
  given set.  See below for how to construct a CCLAUSE.

The \"cclauses\" that are passed to (cclass ...) and (not-cclass ...)
have the following forms:

- a character
  Adds that character to the set.

- a string
  Adds all the characters in the string to the set.

- A pair (MIN . MAX)
  Where MIN and MAX are characters, adds the range of characters
  from MIN through MAX to the set."
  `(mapconcat 'regex--aux ,@forms nil))


;;;;;;;;;;;;;;
;;Shortcut skeletons

(define-skeleton tehom-sregex-cclass
  "Insert a character-class list, per the sregex package."

  nil
  \n > "( cclass " _ " )"
  )

(define-skeleton tehom-sregex-star
  "Insert a kleene star, per the sregex package."

  nil
  \n > "( star " _ " )"
  )

(define-skeleton tehom-sregex-plus
  "Insert a kleene plus, per the sregex package."

  nil
  \n > "( plus " _ " )"
  )

(define-skeleton tehom-sregex-opt
  "Insert an optional element, per the sregex package."

  nil
  \n > "( opt " _ " )"
  )

;;;;;;;;;;;;;;
;;Keymap

(defvar tehom-tsearch-map nil)
(unless tehom-tsearch-map
  (setq tehom-tsearch-map (copy-keymap emacs-lisp-mode-map))
  (gnus-define-keys tehom-tsearch-map
    "\C-c\C-c" gnus-edit-form-done
    "\C-c\C-k" gnus-edit-form-exit

    ;;Keys to move in the history list.  
    "\M-p" tsearch-advance
    "\M-n"  tsearch-retreat


    ;;Keys to insert the usual regex abbreviations.
    "$" ( lambda () (interactive) (insert "eol " ))
    "^" ( lambda () (interactive) (insert "bol " ))
    "." ( lambda () (interactive) (insert "dot " ))
    "[" tehom-sregex-cclass
    "*" tehom-sregex-star
    "+" tehom-sregex-plus
    "?" tehom-sregex-opt

    ))



;;;;;;;;;;;;;;
;;The entry point

( defun tehom-re-search-forward ()
  ""
  (interactive)

  ;;EXIT-FORM must be a function of one variable, the form created in
  ;;the eform buffer.

  (tehom-edit-form 
    '"" 
    "Edit a search string" 
    ( function 
      ( lambda (&rest x) 
	(let
	  (
	    ( reg (regex x) ))

	  (tsearch-update-ring form)
	  (re-search-forward reg))
	))
    tehom-tsearch-map))

( defun tehom-re-search-backward ()
  ""
  (interactive)

  ;;EXIT-FORM must be a function of one variable, the form created in
  ;;the eform buffer.

  (tehom-edit-form 
    '"" 
    "Edit a search string" 
    ( function 
      ( lambda (&rest x) 
	(let
	  (
	    ( reg (regex x) ))

	  (tsearch-update-ring form)
	  (re-search-backward reg))
	))
    tehom-tsearch-map))



(provide 'tehom-search )

;;; tehom-search.el ends here
