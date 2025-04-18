;;; em-ls.el --- implementation of ls in Lisp  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2025 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Most of the command switches recognized by GNU's ls utility are
;; supported ([(fileutils)ls invocation]).

;;; Code:

(require 'cl-lib)
(require 'esh-util)
(require 'esh-opt)
(require 'esh-proc)
(require 'esh-cmd)

;;;###esh-module-autoload
(progn
(defgroup eshell-ls nil
  "This module implements the \"ls\" utility fully in Lisp.
If it is passed any unrecognized command switches, it will revert
to the operating system's version.  This version of \"ls\" uses
text properties to colorize its output based on the setting of
`eshell-ls-use-colors'."
  :tag "Implementation of `ls' in Lisp"
  :group 'eshell-module))

;;; User Variables:

(defcustom eshell-ls-date-format "%Y-%m-%d"
  "How to display time information in `eshell-ls-file'.
This is passed to `format-time-string' as a format string.
To display the date using the current locale, use \"%b \ %e\"."
  :version "24.1"
  :type 'string)

(defcustom eshell-ls-initial-args nil
  "If non-nil, this list of args is included before any call to `ls'.
This is useful for enabling human-readable format (-h), for example."
  :type '(repeat :tag "Arguments" string))

(defcustom eshell-ls-dired-initial-args nil
  "If non-nil, args is included before any call to `ls' in Dired.
This is useful for enabling human-readable format (-h), for example."
  :type '(repeat :tag "Arguments" string))

(defun eshell-ls-enable-in-dired ()
  "Use `eshell-ls' to read directories in Dired."
  (require 'dired)
  (advice-add 'insert-directory :around #'eshell-ls--insert-directory)
  (advice-add 'dired :around #'eshell-ls--dired))

(defun eshell-ls-disable-in-dired ()
  "Stop using `eshell-ls' to read directories in Dired."
  (advice-remove 'insert-directory #'eshell-ls--insert-directory)
  (advice-remove 'dired #'eshell-ls--dired))

(defcustom eshell-ls-use-in-dired nil
  "If non-nil, use `eshell-ls' to read directories in Dired.
Changing this without using customize has no effect."
  :set (lambda (symbol value)
         (if value
             (eshell-ls-enable-in-dired)
           (eshell-ls-disable-in-dired))
         (set symbol value))
  :type 'boolean
  :require 'em-ls)

(defcustom eshell-ls-default-blocksize 1024
  "The default blocksize to use when display file sizes with -s."
  :type 'integer)

(defcustom eshell-ls-exclude-regexp nil
  "Unless -a is specified, files matching this regexp will not be shown."
  :type '(choice regexp (const nil)))

(defcustom eshell-ls-exclude-hidden t
  "Unless -a is specified, files beginning with . will not be shown.
Using this boolean, instead of `eshell-ls-exclude-regexp', is both
faster and conserves more memory."
  :type 'boolean)

(defcustom eshell-ls-use-colors t
  "If non-nil, use colors in file listings."
  :type 'boolean)

(defface eshell-ls-directory
  '((t (:inherit font-lock-function-name-face)))
  "The face used for highlighting directories."
  :version "29.1")

(defface eshell-ls-symlink
  '((t (:inherit font-lock-keyword-face)))
  "The face used for highlighting symbolic links."
  :version "29.1")

(defface eshell-ls-executable
  '((((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "Green" :weight bold)))
  "The face used for highlighting executables (not directories, though).")

(defface eshell-ls-readonly
  '((((class color) (background light)) (:foreground "Brown"))
    (((class color) (background dark)) (:foreground "Pink")))
  "The face used for highlighting read-only files.")

(defface eshell-ls-unreadable
  '((((class color) (background light)) (:foreground "Grey30"))
    (((class color) (background dark)) (:foreground "DarkGrey")))
  "The face used for highlighting unreadable files.")

(defface eshell-ls-special
  '((((class color) (background light)) (:foreground "Magenta" :weight bold))
    (((class color) (background dark)) (:foreground "Magenta" :weight bold)))
  "The face used for highlighting non-regular files.")

(defface eshell-ls-missing
  '((((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold)))
  "The face used for highlighting non-existent file names.")

(defcustom eshell-ls-archive-regexp
  (concat "\\.\\(t\\(a[rz]\\|gz\\)\\|arj\\|lzh\\|"
	  "zip\\|[zZ]\\|gz\\|bz2\\|xz\\|deb\\|rpm\\)\\'")
  "A regular expression that matches names of file archives.
This typically includes both traditional archives and compressed
files."
  :version "24.1"			; added xz
  :type 'regexp)

(defface eshell-ls-archive
  '((((class color) (background light)) (:foreground "Orchid" :weight bold))
    (((class color) (background dark)) (:foreground "Orchid" :weight bold)))
  "The face used for highlighting archived and compressed file names.")

(defcustom eshell-ls-backup-regexp
  "\\(\\`\\.?#\\|\\(\\.bak\\|~\\)\\'\\)"
  "A regular expression that matches names of backup files."
  :type 'regexp)

(defface eshell-ls-backup
  '((((class color) (background light)) (:foreground "OrangeRed"))
    (((class color) (background dark)) (:foreground "LightSalmon")))
  "The face used for highlighting backup file names.")

(defcustom eshell-ls-product-regexp
  "\\.\\(elc\\|o\\(bj\\)?\\|a\\|lib\\|res\\)\\'"
  "A regular expression that matches names of product files.
Products are files that get generated from a source file, and hence
ought to be recreatable if they are deleted."
  :type 'regexp)

(defface eshell-ls-product
  '((((class color) (background light)) (:foreground "OrangeRed"))
    (((class color) (background dark)) (:foreground "LightSalmon")))
  "The face used for highlighting files that are build products.")

(defcustom eshell-ls-clutter-regexp
  "\\(^texput\\.log\\|^core\\)\\'"
  "A regular expression that matches names of junk files.
These are mainly files that get created for various reasons, but don't
really need to stick around for very long."
  :type 'regexp)

(defface eshell-ls-clutter
  '((((class color) (background light)) (:foreground "OrangeRed" :weight bold))
    (((class color) (background dark)) (:foreground "OrangeRed" :weight bold)))
  "The face used for highlighting junk file names.")

(defsubst eshell-ls-filetype-p (attrs type)
  "Test whether ATTRS specifies a file of type TYPE."
  (if (file-attribute-modes attrs)
      (eq (aref (file-attribute-modes attrs) 0) type)))

(defmacro eshell-ls-applicable (attrs index func file)
  "Test whether, for ATTRS, the user can do what corresponds to INDEX.
ATTRS is a string of file modes.  See `file-attributes'.
If we cannot determine the answer using ATTRS (e.g., if we need
to know what group the user is in), compute the return value by
calling FUNC with FILE as an argument."
  `(let ((owner (file-attribute-user-id ,attrs))
	 (modes (file-attribute-modes ,attrs)))
     (cond ((cond ((numberp owner)
                   (= owner (file-user-uid)))
		  ((stringp owner)
                   (or (string-equal owner (eshell-user-login-name))
		       (member owner (eshell-current-ange-uids)))))
	    ;; The user owns this file.
	    (not (eq (aref modes ,index) ?-)))
	   ((eq (aref modes (+ ,index 3))
		(aref modes (+ ,index 6)))
	    ;; If the "group" and "other" fields give identical
	    ;; results, use that.
	    (not (eq (aref modes (+ ,index 3)) ?-)))
	   (t
	    ;; Otherwise call FUNC.
	    (,(eval func) ,file)))))

(defcustom eshell-ls-highlight-alist nil
  "This alist correlates test functions to color.
The format of the members of this alist is

  (TEST-SEXP . FACE)

If TEST-SEXP evals to non-nil, that face will be used to highlight the
name of the file.  The first match wins.  `file' and `attrs' are in
scope during the evaluation of TEST-SEXP."
  :type '(repeat (cons function face)))

(defvar block-size)
(defvar dereference-links)
(defvar dir-literal)
(defvar error-func)
(defvar human-readable)
(defvar ignore-pattern)
(defvar insert-func)
(defvar listing-style)
(defvar numeric-uid-gid)
(defvar reverse-list)
(defvar show-all)
(defvar show-almost-all)
(defvar show-recursive)
(defvar show-size)
(defvar sort-method)
(defvar dired-flag)

;;; Functions:

(declare-function eshell-extended-glob "em-glob" (glob))
(defvar eshell-error-if-no-glob)
(defvar eshell-glob-splice-results)

(defun eshell-ls--expand-wildcards (file)
  "Expand the shell wildcards in FILE if any."
  (if (and (atom file)
           (not (file-exists-p file)))
      (let ((eshell-error-if-no-glob t)
            ;; Ensure `eshell-extended-glob' returns a list.
            (eshell-glob-splice-results t))
        (mapcar #'file-relative-name (eshell-extended-glob file)))
    (list (file-relative-name file))))

(defun eshell-ls--insert-directory
  (orig-fun file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function uses `eshell/ls'.  If any of the switches
passed are not recognized, the operating system's version will be used
instead."
  (if (not eshell-ls-use-in-dired)
      (funcall orig-fun file switches wildcard full-directory-p)
    (let ((handler (find-file-name-handler file 'insert-directory)))
      (if handler
          (funcall handler 'insert-directory file switches
                   wildcard full-directory-p)
        (if (stringp switches)
            (setq switches (split-string switches)))
        (let (eshell-current-handles
              eshell-current-subjob-p
              font-lock-mode)
          ;; use the fancy highlighting in `eshell-ls' rather than font-lock
          (when eshell-ls-use-colors
            (font-lock-mode -1)
            (setq font-lock-defaults nil))
          (require 'em-glob)
          (let* ((insert-func 'insert)
                 (error-func 'insert)
                 (target (eshell-ls--expand-wildcards file))
                 (switches
                  (append eshell-ls-dired-initial-args
                          (and (or (consp dired-directory) wildcard) (list "-d"))
                          switches)))
            (eshell-do-ls (nconc switches (list target)))))))))


(declare-function dired-read-dir-and-switches "dired" (str))
(declare-function dired-goto-next-file "dired" ())

(defun eshell-ls--dired (orig-fun dir-or-list &optional switches)
  (interactive (dired-read-dir-and-switches ""))
  (require 'em-glob)
  (if (consp dir-or-list)
      (funcall orig-fun dir-or-list switches)
    (let ((dir-wildcard (insert-directory-wildcard-in-dir-p
                         (expand-file-name dir-or-list))))
      (if (not dir-wildcard)
          (funcall orig-fun dir-or-list switches)
        (let* ((default-directory (car dir-wildcard))
               (files (eshell-extended-glob (cdr dir-wildcard)))
               (dir (car dir-wildcard)))
          (if files
              (let ((inhibit-read-only t)
                    (buf
                     (apply orig-fun
                            (nconc (list dir) files)
                            (and switches (list switches)))))
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (dired-goto-next-file)
                    (forward-line 0)
                    (insert "  wildcard " (cdr dir-wildcard) "\n"))))
            (user-error "No files matching regexp")))))))

(defsubst eshell/ls (&rest args)
  "An alias version of `eshell-do-ls'."
  (eshell-with-buffered-print
    (let ((insert-func #'eshell-buffered-print)
          (error-func (lambda (s) (eshell-error s) (eshell-set-exit-info 2))))
      (apply 'eshell-do-ls args))))

(put 'eshell/ls 'eshell-no-numeric-conversions t)
(put 'eshell/ls 'eshell-filename-arguments t)

(declare-function eshell-glob-regexp "em-glob" (pattern))

(defun eshell-do-ls (&rest args)
  "Implementation of \"ls\" in Lisp, passing ARGS."
  ;; Process the command arguments, and begin listing files.
  (eshell-eval-using-options
   "ls" (if eshell-ls-initial-args
	    (list eshell-ls-initial-args args)
	  args)
   '((?a "all" nil show-all
	 "do not ignore entries starting with .")
     (?A "almost-all" nil show-almost-all
	 "do not list implied . and ..")
     (?c nil by-ctime sort-method
	 "sort by last status change time")
     (?d "directory" nil dir-literal
	 "list directory entries instead of contents")
     (?k "kilobytes" 1024 block-size
	 "using 1024 as the block size")
     (?h "human-readable" 1024 human-readable
	 "print sizes in human readable format")
     (?H "si" 1000 human-readable
	 "likewise, but use powers of 1000 not 1024")
     (?I "ignore" t ignore-pattern
	 "do not list implied entries matching pattern")
     (?l nil long-listing listing-style
	 "use a long listing format")
     (?n "numeric-uid-gid" nil numeric-uid-gid
	 "list numeric UIDs and GIDs instead of names")
     (?r "reverse" nil reverse-list
	 "reverse order while sorting")
     (?s "size" nil show-size
	 "print size of each file, in blocks")
     (?t nil by-mtime sort-method
	 "sort by modification time")
     (?u nil by-atime sort-method
	 "sort by last access time")
     (?x nil by-lines listing-style
	 "list entries by lines instead of by columns")
     (?C nil by-columns listing-style
	 "list entries by columns")
     (?L "dereference" nil dereference-links
	 "list entries pointed to by symbolic links")
     (?R "recursive" nil show-recursive
	 "list subdirectories recursively")
     (?S nil by-size sort-method
	 "sort by file size")
     (?U nil unsorted sort-method
	 "do not sort; list entries in directory order")
     (?X nil by-extension sort-method
	 "sort alphabetically by entry extension")
     (?1 nil single-column listing-style
	 "list one file per line")
     (nil "dired" nil dired-flag
	  "Here for compatibility with GNU ls.")
     (nil "help" nil nil
	  "show this usage display")
     :external "ls"
     :usage "[OPTION]... [FILE]...
List information about the FILEs (the current directory by default).
Sort entries alphabetically across.")
   ;; setup some defaults, based on what the user selected
   (unless block-size
     (setq block-size eshell-ls-default-blocksize))
   (unless listing-style
     (setq listing-style 'by-columns))
   (unless args
     (setq args (list ".")))
   (let ((eshell-ls-exclude-regexp eshell-ls-exclude-regexp))
     (when ignore-pattern
       (unless (eshell-using-module 'eshell-glob)
	 (error (concat "-I option requires that `eshell-glob'"
			" be a member of `eshell-modules-list'")))
       (set-text-properties 0 (length ignore-pattern) nil ignore-pattern)
       (setq eshell-ls-exclude-regexp
	     (if eshell-ls-exclude-regexp
		 (concat "\\(" eshell-ls-exclude-regexp "\\|"
			 (eshell-glob-regexp ignore-pattern) "\\)")
	       (eshell-glob-regexp ignore-pattern))))
     ;; list the files!
     (eshell-ls-entries
      (mapcar (lambda (arg)
		(cons (if (and (eshell-under-windows-p)
			       (file-name-absolute-p arg))
			  (expand-file-name arg)
			arg)
		      (eshell-file-attributes
		       arg (if numeric-uid-gid 'integer 'string))))
	      args)
      t (expand-file-name default-directory)))))

(defsubst eshell-ls-printable-size (filesize &optional by-blocksize)
  "Return a printable FILESIZE."
  (eshell-printable-size filesize human-readable
			 (and by-blocksize block-size)
			 eshell-ls-use-colors))

(defsubst eshell-ls-size-string (attrs size-width)
  "Return the size string for ATTRS length, using SIZE-WIDTH."
  (let* ((str (eshell-ls-printable-size (file-attribute-size attrs) t))
	 (len (length str)))
    (if (< len size-width)
	(concat (make-string (- size-width len) ? ) str)
      str)))

(defun eshell-ls-annotate (fileinfo)
  "Given a FILEINFO object, return a resolved, decorated FILEINFO.
This means resolving any symbolic links, determining what face the
name should be displayed as, etc.  Think of it as cooking a FILEINFO."
  (if (not (and (stringp (cadr fileinfo))
		(or dereference-links
		    (eq listing-style 'long-listing))))
      (setcar fileinfo (eshell-ls-decorated-name fileinfo))
    (let (dir attr)
      (unless (file-name-absolute-p (cadr fileinfo))
	(setq dir (file-truename
		   (file-name-directory
		    (expand-file-name (car fileinfo))))))
      (setq attr
	    (eshell-file-attributes
	     (let ((target (if dir
			       (expand-file-name (cadr fileinfo) dir)
			     (cadr fileinfo))))
	       (if dereference-links
		   (file-truename target)
		 target))))
      (if (or dereference-links
	      (string-match "^\\.\\.?$" (car fileinfo)))
	  (progn
	    (setcdr fileinfo attr)
	    (setcar fileinfo (eshell-ls-decorated-name fileinfo)))
	(cl-assert (eq listing-style 'long-listing))
	(setcar fileinfo
		(concat (eshell-ls-decorated-name fileinfo) " -> "
			(eshell-ls-decorated-name
			 (cons (cadr fileinfo) attr)))))))
  fileinfo)

(defun eshell-ls-file (fileinfo &optional size-width copy-fileinfo)
  "Output FILEINFO in long format.
FILEINFO may be a string, or a cons cell whose car is the
filename and whose cdr is the list of file attributes."
  (if (not (cdr fileinfo))
      (funcall error-func (format "%s: No such file or directory\n"
				  (car fileinfo)))
    (setq fileinfo
	  (eshell-ls-annotate (if copy-fileinfo
				  (cons (car fileinfo)
					(cdr fileinfo))
				fileinfo)))
    (let ((file (car fileinfo))
	  (attrs (cdr fileinfo)))
      (if (not (eq listing-style 'long-listing))
	  (if show-size
	      (funcall insert-func (eshell-ls-size-string attrs size-width)
		       " " file "\n")
	    (funcall insert-func file "\n"))
	(let ((line
	       (concat
		(if show-size
		    (concat (eshell-ls-size-string attrs size-width) " "))
		(format
		 (if numeric-uid-gid
		     "%s%4d %-8s %-8s "
		   "%s%4d %-14s %-8s ")
		 (or (file-attribute-modes attrs) "??????????")
		 (or (file-attribute-link-number attrs) 0)
		 (or (let ((user (file-attribute-user-id attrs)))
		       (and (stringp user)
			    (eshell-substring user 14)))
		     (file-attribute-user-id attrs)
		     "")
		 (or (let ((group (file-attribute-group-id attrs)))
		       (and (stringp group)
			    (eshell-substring group 8)))
		     (file-attribute-group-id attrs)
		     ""))
		(let* ((str (eshell-ls-printable-size (file-attribute-size attrs)))
		       (len (length str)))
		  ;; Let file sizes shorter than 9 align neatly.
		  (if (< len (or size-width 8))
		      (concat (make-string (- (or size-width 8) len) ? ) str)
		    str))
		" " (format-time-string
		     (concat
		      eshell-ls-date-format " "
		      (if (= (decoded-time-year (decode-time))
			     (decoded-time-year
                              (decode-time
			       (nth (cond
				     ((eq sort-method 'by-atime) 4)
				     ((eq sort-method 'by-ctime) 6)
				     (t 5))
                                    attrs))))
			  "%H:%M"
			" %Y")) (nth (cond
			((eq sort-method 'by-atime) 4)
			((eq sort-method 'by-ctime) 6)
			(t 5)) attrs)) " ")))
	  (funcall insert-func line file "\n"))))))

(defun eshell-ls-dir (dirinfo &optional insert-name root-dir size-width)
  "Output the entries in DIRINFO.
If INSERT-NAME is non-nil, the name of DIRINFO will be output.  If
ROOT-DIR is also non-nil, and a directory name, DIRINFO will be output
relative to that directory."
  (let ((dir (car dirinfo)))
    (if (not (cdr dirinfo))
	(funcall error-func (format "%s: No such file or directory\n" dir))
      (if dir-literal
	  (eshell-ls-file dirinfo size-width)
	(if insert-name
	    (funcall insert-func
		     (eshell-ls-decorated-name
		      (cons (concat
			     (if root-dir
				 (file-relative-name dir root-dir)
			       (expand-file-name dir)))
			    (cdr dirinfo))) ":\n"))
	(let ((entries (eshell-directory-files-and-attributes
			dir nil (and (not (or show-all show-almost-all))
				     eshell-ls-exclude-hidden
				     "\\`[^.]") t
				     ;; Asking for UID and GID as
				     ;; strings saves another syscall
				     ;; later when we are going to
				     ;; display user and group names.
				     (if numeric-uid-gid 'integer 'string))))
          (when (and show-almost-all
                     (not show-all))
            (setq entries
                  (cl-remove-if
                   (lambda (entry)
                     (member (car entry) '("." "..")))
                   entries)))
	  (when (and (not (or show-all show-almost-all))
                     eshell-ls-exclude-regexp)
	    (while (and entries (string-match eshell-ls-exclude-regexp
					      (caar entries)))
	      (setq entries (cdr entries)))
	    (let ((e entries))
	      (while (cdr e)
		(if (string-match eshell-ls-exclude-regexp (car (cadr e)))
		    (setcdr e (cddr e))
		  (setq e (cdr e))))))
	  (when (or (eq listing-style 'long-listing) show-size)
	    (let ((total 0.0))
	      (setq size-width 0)
	      (dolist (e entries)
		(if (file-attribute-size (cdr e))
		    (setq total (+ total (file-attribute-size (cdr e)))
			  size-width
			  (max size-width
			       (length (eshell-ls-printable-size
					(file-attribute-size (cdr e))
					(not
					 ;; If we are under -l, count length
					 ;; of sizes in bytes, not in blocks.
					 (eq listing-style 'long-listing))))))))
	      (funcall insert-func "total "
		       (eshell-ls-printable-size total t) "\n")))
	  (let ((default-directory (expand-file-name dir)))
	    (if show-recursive
		(eshell-ls-entries
		 (let ((e entries) (good-entries (list t)))
		   (while e
		     (unless (let ((len (length (caar e))))
			       (and (eq (aref (caar e) 0) ?.)
				    (or (= len 1)
					(and (= len 2)
					     (eq (aref (caar e) 1) ?.)))))
		       (nconc good-entries (list (car e))))
		     (setq e (cdr e)))
		   (cdr good-entries))
		 nil root-dir)
	      (eshell-ls-files (eshell-ls-sort-entries entries)
			       size-width))))))))

(defsubst eshell-ls-compare-entries (l r inx func)
  "Compare the time of two files, L and R, the attribute indexed by INX."
  (let ((lt (nth inx (cdr l)))
	(rt (nth inx (cdr r))))
    (if (equal lt rt)
	(string-lessp (directory-file-name (car l))
		      (directory-file-name (car r)))
      (funcall func rt lt))))

(defun eshell-ls-sort-entries (entries)
  "Sort the given ENTRIES, which may be files, directories or both.
In Eshell's implementation of ls, ENTRIES is always reversed."
  (if (eq sort-method 'unsorted)
      (nreverse entries)
    (sort entries
          (lambda (l r)
            (let ((result
                   (cond
                    ((eq sort-method 'by-atime)
                     (eshell-ls-compare-entries l r 4 'time-less-p))
                    ((eq sort-method 'by-mtime)
                     (eshell-ls-compare-entries l r 5 'time-less-p))
                    ((eq sort-method 'by-ctime)
                     (eshell-ls-compare-entries l r 6 'time-less-p))
                    ((eq sort-method 'by-size)
                     (eshell-ls-compare-entries l r 7 '<))
                    ((eq sort-method 'by-extension)
                     (let ((lx (file-name-extension
                                (directory-file-name (car l))))
                           (rx (file-name-extension
                                (directory-file-name (car r)))))
                       (cond
                        ((or (and (not lx) (not rx))
                             (equal lx rx))
                         (string-lessp (directory-file-name (car l))
                                       (directory-file-name (car r))))
                        ((not lx) t)
                        ((not rx) nil)
                        (t
                         (string-lessp lx rx)))))
                    (t
                     (string-lessp (directory-file-name (car l))
                                   (directory-file-name (car r)))))))
              (if reverse-list
                  (not result)
                result))))))

(defun eshell-ls-files (files &optional size-width copy-fileinfo)
  "Output a list of FILES.
Each member of FILES is either a string or a cons cell of the form
\(FILE .  ATTRS)."
  ;; Mimic behavior of coreutils ls, which lists a single file per
  ;; line when output is not a tty.  Exceptions: if -x was supplied,
  ;; or if we are the _last_ command in a pipeline.
  ;; FIXME Not really the same since not testing output destination.
  (if (or (and eshell-in-pipeline-p
	       (not (eq eshell-in-pipeline-p 'last))
	       (not (eq listing-style 'by-lines)))
	  (memq listing-style '(long-listing single-column)))
      (dolist (file files)
	(if file
	    (eshell-ls-file file size-width copy-fileinfo)))
    (let ((f files)
	  last-f
	  display-files
	  ) ;; ignore
      (while f
	(if (cdar f)
	    (setq last-f f
		  f (cdr f))
	  (unless nil ;; ignore
	    (funcall error-func
		     (format "%s: No such file or directory\n" (caar f))))
	  (if (eq f files)
	      (setq files (cdr files)
		    f files)
	    (if (not (cdr f))
		(progn
		  (setcdr last-f nil)
		  (setq f nil))
	      (setcar f (cadr f))
	      (setcdr f (cddr f))))))
      (if (not show-size)
	  (setq display-files (mapcar #'eshell-ls-annotate files))
	(dolist (file files)
	  (let* ((str (eshell-ls-printable-size (file-attribute-size (cdr file)) t))
		 (len (length str)))
	    (if (< len size-width)
		(setq str (concat (make-string (- size-width len) ? ) str)))
	    (setq file (eshell-ls-annotate file)
		  display-files (cons (cons (concat str " " (car file))
					    (cdr file))
				      display-files))))
	(setq display-files (nreverse display-files)))
      (let* ((col-vals
	      (if (eq listing-style 'by-columns)
		  (eshell-ls-find-column-lengths display-files)
		(cl-assert (eq listing-style 'by-lines))
		(eshell-ls-find-column-widths display-files)))
	     (col-widths (car col-vals))
	     (display-files (cdr col-vals))
	     (columns (length col-widths))
	     (col-index 1)
	     need-return)
	(dolist (file display-files)
	  (let ((name
		 (if (car file)
		     (if show-size
			 (concat (substring (car file) 0 size-width)
				 (eshell-ls-decorated-name
				  (cons (substring (car file) size-width)
					(cdr file))))
		       (eshell-ls-decorated-name file))
		   "")))
	    (if (< col-index columns)
		(setq need-return
		      (concat need-return name
			      (make-string
			       (max 0 (- (aref col-widths
					       (1- col-index))
					 (length name))) ? ))
		      col-index (1+ col-index))
	      (funcall insert-func need-return name "\n")
	      (setq col-index 1 need-return nil))))
	(if need-return
	    (funcall insert-func need-return "\n"))))))

(defun eshell-ls-entries (entries &optional separate root-dir)
  "Output PATH's directory ENTRIES.
Each member of ENTRIES may either be a string or a cons cell, the car
of which is the file name, and the cdr of which is the list of
attributes.
If SEPARATE is non-nil, directories name will be entirely separated
from the filenames.  This is the normal behavior, except when doing a
recursive listing.
ROOT-DIR, if non-nil, specifies the root directory of the listing, to
which non-absolute directory names will be made relative if ever they
need to be printed."
  (let (dirs files show-names need-return (size-width 0))
    (dolist (entry entries)
      (if (and (not dir-literal)
	       (or (eshell-ls-filetype-p (cdr entry) ?d)
		   (and (eshell-ls-filetype-p (cdr entry) ?l)
			(file-directory-p (car entry)))))
	  (progn
	    (unless separate
	      (setq files (cons entry files)
		    size-width
		    (if show-size
			(max size-width
			     (length (eshell-ls-printable-size
				      (file-attribute-size (cdr entry)) t))))))
	    (setq dirs (cons entry dirs)))
	(setq files (cons entry files)
	      size-width
	      (if show-size
		  (max size-width
		       (length (eshell-ls-printable-size
				(file-attribute-size (cdr entry)) t)))))))
    (when files
      (eshell-ls-files (eshell-ls-sort-entries files)
		       size-width show-recursive)
      (setq need-return t))
    (setq show-names (or show-recursive
			 (> (+ (length files) (length dirs)) 1)))
    (dolist (dir (eshell-ls-sort-entries dirs))
      (if (and need-return (not dir-literal))
	  (funcall insert-func "\n"))
      (eshell-ls-dir dir show-names
		     (unless (file-name-absolute-p (car dir)) root-dir)
		     size-width)
      (setq need-return t))))

(defun eshell-ls-find-column-widths (files)
  "Find the best fitting column widths for FILES.
It will be returned as a vector, whose length is the number of columns
to use, and each member of which is the width of that column
\(including spacing)."
  (let* ((numcols 0)
	 (width 0)
	 (widths
	  (mapcar
           (lambda (file)
             (+ 2 (length (car file))))
	   files))
	 ;; must account for the added space...
	 (max-width (+ (window-body-width nil 'remap) 2))
	 (best-width 0)
	 col-widths)

    ;; determine the largest number of columns in the first row
    (let ((w widths))
      (while (and w (< width max-width))
	(setq width (+ width (car w))
	      numcols (1+ numcols)
	      w (cdr w))))

    ;; refine it based on the following rows
    (while (> numcols 0)
      (let ((i 0)
	    (colw (make-vector numcols 0))
	    (w widths))
	(while w
	  (if (= i numcols)
	      (setq i 0))
	  (aset colw i (max (aref colw i) (car w)))
	  (setq w (cdr w) i (1+ i)))
	(setq i 0 width 0)
	(while (< i numcols)
	  (setq width (+ width (aref colw i))
		i (1+ i)))
	(if (and (< width max-width)
		 (> width best-width))
	    (setq col-widths colw
		  best-width width)))
      (setq numcols (1- numcols)))

    (cons (or col-widths (vector max-width)) files)))

(defun eshell-ls-find-column-lengths (files)
  "Find the best fitting column lengths for FILES.
It will be returned as a vector, whose length is the number of columns
to use, and each member of which is the width of that column
\(including spacing)."
  (let* ((numcols 1)
	 (width 0)
	 (widths
	  (mapcar
           (lambda (file)
             (+ 2 (length (car file))))
	   files))
	 (max-width (+ (window-body-width nil 'remap) 2))
	 col-widths
	 colw)

    ;; refine it based on the following rows
    (while numcols
      (let* ((rows (ceiling (/ (length widths)
			       (float numcols))))
	     (w widths)
	     (len (* rows numcols))
	     (index 0)
	     (i 0))
	(setq width 0)
	(unless (or (= rows 0)
		    (<= (/ (length widths) (float rows))
			(float (1- numcols))))
	  (setq colw (make-vector numcols 0))
	  (while (> len 0)
	    (if (= i numcols)
		(setq i 0 index (1+ index)))
	    (aset colw i
		  (max (aref colw i)
		       (or (nth (+ (* i rows) index) w) 0)))
	    (setq len (1- len) i (1+ i)))
	  (setq i 0)
	  (while (< i numcols)
	    (setq width (+ width (aref colw i))
		  i (1+ i))))
	(if (>= width max-width)
	    (setq numcols nil)
	  (if colw
	      (setq col-widths colw))
	  (if (>= numcols (length widths))
	      (setq numcols nil)
	    (setq numcols (1+ numcols))))))

    (if (not col-widths)
	(cons (vector max-width) files)
      (setq numcols (length col-widths))
      (let* ((rows (ceiling (/ (length widths)
			       (float numcols))))
	     (len (* rows numcols))
	     (newfiles (make-list len nil))
	     (index 0)
	     (i 0)
	     (j 0))
	(while (< j len)
	  (if (= i numcols)
	      (setq i 0 index (1+ index)))
	  (setcar (nthcdr j newfiles)
		  (nth (+ (* i rows) index) files))
	  (setq j (1+ j) i (1+ i)))
	(cons col-widths newfiles)))))

(defun eshell-ls-decorated-name (file)
  "Return FILE, possibly decorated."
  (if eshell-ls-use-colors
      (let ((face
	     (cond
	      ((not (cdr file))
	       'eshell-ls-missing)

	      ((stringp (cadr file))
	       'eshell-ls-symlink)

	      ((eq (cadr file) t)
	       'eshell-ls-directory)

	      ((not (eshell-ls-filetype-p (cdr file) ?-))
	       'eshell-ls-special)

              ((and (/= (file-user-uid) 0) ; root can execute anything
		    (eshell-ls-applicable (cdr file) 3
					  'file-executable-p (car file)))
	       'eshell-ls-executable)

	      ((not (eshell-ls-applicable (cdr file) 1
					  'file-readable-p (car file)))
	       'eshell-ls-unreadable)

	      ((string-match eshell-ls-archive-regexp (car file))
	       'eshell-ls-archive)

	      ((string-match eshell-ls-backup-regexp (car file))
	       'eshell-ls-backup)

	      ((string-match eshell-ls-product-regexp (car file))
	       'eshell-ls-product)

	      ((string-match eshell-ls-clutter-regexp (car file))
	       'eshell-ls-clutter)

	      ((not (eshell-ls-applicable (cdr file) 2
					  'file-writable-p (car file)))
	       'eshell-ls-readonly)
	      (eshell-ls-highlight-alist
	       (let ((tests eshell-ls-highlight-alist)
		     value)
		 (while tests
		   (if (funcall (caar tests) (car file) (cdr file))
		       (setq value (cdar tests) tests nil)
		     (setq tests (cdr tests))))
		 value)))))
	(if face
	    (add-text-properties 0 (length (car file))
				 (list 'font-lock-face face)
				 (car file)))))
  (car file))

(defun em-ls-unload-function ()
  (eshell-ls-disable-in-dired))

(provide 'em-ls)
;;; em-ls.el ends here
