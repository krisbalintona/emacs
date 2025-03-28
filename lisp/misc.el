;;; misc.el --- some nonstandard editing and utility commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1989, 2001-2025 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

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

;;; Code:

(eval-when-compile
  (require 'tabulated-list))

;;;###autoload
(defun copy-from-above-command (&optional arg)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point.

Also see the `duplicate-line' command."
  (interactive "P")
  (let ((cc (current-column))
	n
	(string ""))
    (save-excursion
      (beginning-of-line)
      (backward-char 1)
      (skip-chars-backward " \t\n")
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (setq n (if arg (prefix-numeric-value arg) (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (= (preceding-char) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\s))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (line-end-position)
				 (+ n (point)))))))
    (insert string)))

(defcustom duplicate-line-final-position 0
  "Where to put point after `duplicate-line' or `duplicate-dwim'.
When 0, leave point on the original line.
When 1, move point to the first new line.
When -1, move point to the last new line.
The same column is preserved after moving to a new line."
  :type '(choice (const :tag "Leave point on old line" 0)
                 (const :tag "Move point to first new line" 1)
                 (const :tag "Move point to last new line" -1)
                 (integer))
  :group 'editing
  :version "29.1")

(defun duplicate--insert-copies (n string)
  "Insert N copies of STRING at point."
  (insert (mapconcat #'identity (make-list n string))))

;;;###autoload
(defun duplicate-line (&optional n)
  "Duplicate the current line N times.
Interactively, N is the prefix numeric argument, and defaults to 1.
The user option `duplicate-line-final-position' specifies where to
move point after duplicating the line.
Also see the `copy-from-above-command' command."
  (interactive "p")
  (unless n
    (setq n 1))
  (let ((line (concat (buffer-substring (line-beginning-position)
                                        (line-end-position))
                      "\n"))
        (pos (point))
        (col (current-column)))
    (forward-line 1)
    (unless (bolp)
      (insert "\n"))
    (duplicate--insert-copies n line)
    (unless (< duplicate-line-final-position 0)
      (goto-char pos))
    (unless (eq duplicate-line-final-position 0)
      (forward-line duplicate-line-final-position)
      (move-to-column col))))

(defcustom duplicate-region-final-position 0
  "Where the region ends up after duplicating a region with `duplicate-dwim'.
When 0, leave the region in place.
When 1, put the region around the first copy.
When -1, put the region around the last copy."
  :type '(choice (const :tag "Leave region in place" 0)
                 (const :tag "Put region around first copy" 1)
                 (const :tag "Put region around last copy" -1))
  :group 'editing
  :version "30.1")

(declare-function rectangle--duplicate-right "rect" (n displacement))

;; `duplicate-dwim' preserves an active region and changes the buffer
;; outside of it: disregard the region when immediately undoing the
;; actions of this command.
(put 'duplicate-dwim 'undo-inhibit-region t)

;;;###autoload
(defun duplicate-dwim (&optional n)
  "Duplicate the current line or region N times.
If the region is inactive, duplicate the current line (like `duplicate-line').
Otherwise, duplicate the region, which remains active afterwards.
If the region is rectangular, duplicate on its right-hand side.
Interactively, N is the prefix numeric argument, and defaults to 1.
The variables `duplicate-line-final-position' and
`duplicate-region-final-position' control the position of point
and the region after the duplication."
  (interactive "p")
  (unless n
    (setq n 1))
  (cond
   ((<= n 0) nil)
   ;; Duplicate rectangle.
   ((bound-and-true-p rectangle-mark-mode)
    (rectangle--duplicate-right n
                                (if (< duplicate-region-final-position 0)
                                    n
                                  duplicate-region-final-position))
    (setq deactivate-mark nil))

   ;; Duplicate (contiguous) region.
   ((use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (text (buffer-substring beg end))
           (pt (point))
           (mk (mark)))
      (save-excursion
        (goto-char end)
        (duplicate--insert-copies n text))
      (let* ((displace (if (< duplicate-region-final-position 0)
                           n
                         duplicate-region-final-position))
             (d (* displace (- end beg))))
        (unless (zerop d)
          (push-mark (+ mk d))
          (goto-char (+ pt d)))))
    (setq deactivate-mark nil))

   ;; Duplicate line.
   (t (duplicate-line n))))

;; Variation of `zap-to-char'.

;;;###autoload
(defun zap-up-to-char (arg char &optional interactive)
  "Kill up to, but not including ARGth occurrence of CHAR.
When run interactively, the argument INTERACTIVE is non-nil.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point.
If called interactively, do a case sensitive search if CHAR
is an upper-case character."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char-from-minibuffer "Zap up to char: "
						nil 'read-char-history)
                     t))
  (let ((direction (if (>= arg 0) 1 -1))
        (case-fold-search (if (and interactive (char-uppercase-p char))
                              nil
                            case-fold-search)))
    (kill-region (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point)))))

;; These were added with an eye to making possible a more CCA-compatible
;; command set; but that turned out not to be interesting.

;;;###autoload
(defun mark-beginning-of-buffer ()
  "Set mark at the beginning of the buffer."
  (interactive)
  (push-mark (point-min)))

;;;###autoload
(defun mark-end-of-buffer ()
  "Set mark at the end of the buffer."
  (interactive)
  (push-mark (point-max)))

;;;###autoload
(defun upcase-char (arg)
  "Uppercasify ARG chars starting from point.  Point doesn't move."
  (interactive "p")
  (save-excursion
    (upcase-region (point) (progn (forward-char arg) (point)))))

;;;###autoload
(defun forward-to-word (&optional arg)
  "Move forward until encountering the beginning of the ARGth word.
ARG defaults to 1.  When called interactively, ARG is the prefix
numeric argument."
  (interactive "^p")
  (unless arg (setq arg 1))
  (or (re-search-forward (if (> arg 0) "\\W\\b" "\\b\\W") nil t arg)
      (goto-char (if (> arg 0) (point-max) (point-min)))))

;;;###autoload
(defun backward-to-word (&optional arg)
  "Move backward until encountering the end of the ARGth word.
ARG defaults to 1.  When called interactively, ARG is the prefix
numeric argument."
  (interactive "^p")
  (unless arg (setq arg 1))
  (forward-to-word (- arg)))

;;;###autoload
(defun butterfly ()
  "Use butterflies to flip the desired bit on the drive platter.
Open hands and let the delicate wings flap once.  The disturbance
ripples outward, changing the flow of the eddy currents in the
upper atmosphere.  These cause momentary pockets of higher-pressure
air to form, which act as lenses that deflect incoming cosmic rays,
focusing them to strike the drive platter and flip the desired bit.
You can type \\`M-x butterfly C-M-c' to run it.  This is a permuted
variation of `C-x M-c M-butterfly' from url `https://xkcd.com/378/'."
  (interactive)
  (if (yes-or-no-p "Do you really want to unleash the powers of the butterfly? ")
      (progn
	(switch-to-buffer (get-buffer-create "*butterfly*"))
	(erase-buffer)
	(sit-for 0)
	(animate-string "Amazing physics going on..."
			(/ (window-height) 2) (- (/ (window-width) 2) 12))
	(sit-for (* 5 (/ (abs (random)) (float most-positive-fixnum))))
	(message "Successfully flipped one bit!"))
    (message "Well, then go to xkcd.com!")
    (browse-url "https://xkcd.com/378/")))

;; A command to list dynamically loaded libraries.  This useful in
;; environments where dynamic-library-alist is used, i.e., Windows

(defvar-local list-dynamic-libraries--loaded-only-p)

(defun list-dynamic-libraries--loaded (from)
  "Compute the \"Loaded from\" column.
Internal use only."
  (if from
      (let ((name (car from))
            (path (or (cdr from) "<unknown>")))
        ;; This is a roundabout way to change the tooltip without
        ;; having to replace the default printer function
        (propertize name
                    'display (propertize name
                                         'help-echo (concat "Loaded from: " path))))
    ""))

(defun list-dynamic-libraries--refresh ()
  "Recompute the list of dynamic libraries.
Internal use only."
  (setq tabulated-list-format  ; recomputed because column widths can change
        (let ((max-id-len 7) (max-name-len 11))
          (dolist (lib dynamic-library-alist)
            (let ((id-len (length (symbol-name (car lib))))
                  (name-len (apply 'max (mapcar 'length (cdr lib)))))
              (when (> id-len max-id-len) (setq max-id-len id-len))
              (when (> name-len max-name-len) (setq max-name-len name-len))))
          (vector (list "Library" (1+ max-id-len) t)
                  (list "Loaded from" (1+ max-name-len) t)
                  (list "Candidate names" 0 t))))
  (tabulated-list-init-header)
  (setq tabulated-list-entries nil)
  (dolist (lib dynamic-library-alist)
    (let* ((id (car lib))
           (from (get id :loaded-from)))
      (when (or from
                (not list-dynamic-libraries--loaded-only-p))
        (push (list id (vector (symbol-name id)
                               (list-dynamic-libraries--loaded from)
                               (mapconcat 'identity (cdr lib) ", ")))
              tabulated-list-entries))))
  (when (not dynamic-library-alist)
    (message "No dynamic libraries found")))

;;;###autoload
(defun list-dynamic-libraries (&optional loaded-only-p buffer)
  "Display a list of all dynamic libraries known to Emacs.
\(These are the libraries listed in `dynamic-library-alist'.)
If optional argument LOADED-ONLY-P (interactively, prefix arg)
is non-nil, only libraries already loaded are listed.
Optional argument BUFFER specifies a buffer to use, instead of
\"*Dynamic Libraries*\".
The return value is always nil."
  (interactive "P")
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Dynamic Libraries*")))
  (with-current-buffer buffer
    (tabulated-list-mode)
    (setq tabulated-list-sort-key (cons "Library" nil))
    (add-hook 'tabulated-list-revert-hook 'list-dynamic-libraries--refresh nil t)
    (setq list-dynamic-libraries--loaded-only-p loaded-only-p)
    (list-dynamic-libraries--refresh)
    (tabulated-list-print))
  (display-buffer buffer)
  nil)

(provide 'misc)

;;; misc.el ends here
