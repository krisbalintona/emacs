;;; proced.el --- operate on system processes like dired  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 Free Software Foundation, Inc.

;; Author: Roland Winkler <winkler@gnu.org>
;; Keywords: Processes, Unix

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

;; Proced makes an Emacs buffer containing a listing of the current
;; system processes.  You can use the normal Emacs commands to move around
;; in this buffer, and special Proced commands to operate on the processes
;; listed.  See `proced-mode' for getting started.
;;
;; To do:
;; - Interactive temporary customizability of flags in `proced-grammar-alist'
;;
;; Thoughts and Ideas
;; - Currently, `process-attributes' returns the list of
;;   command-line arguments of a process as one concatenated string.
;;   This format is compatible with `shell-command'.  Also, under
;;   MS-Windows, the command-line arguments are actually stored as a
;;   single string, so that it is impossible to reverse-engineer it back
;;   into separate arguments.  Alternatively, `process-attributes'
;;   could (try to) return a list of strings that correspond to individual
;;   command-line arguments.  Then one could feed such a list of
;;   command-line arguments into `call-process' or `start-process'.
;;   Are there real-world applications when such a feature would be useful?
;;   What about something like `proced-restart-pid'?

;;; Code:

(defgroup proced nil
  "Proced mode."
  :group 'processes
  :group 'unix
  :prefix "proced-")

(defcustom proced-show-remote-processes nil
  "Whether processes of the remote host shall be shown.
This happens only when `default-directory' is remote."
  :version "29.1"
  :type 'boolean)

(defcustom proced-signal-function #'signal-process
  "Name of signal function.
It can be an elisp function (usually `signal-process') or a string specifying
the external command (usually \"kill\")."
  :type '(choice (function :tag "function")
                 (string :tag "command")))
(make-obsolete-variable 'proced-signal-function "no longer used." "29.1")

(defcustom proced-renice-command "renice"
  "Name of renice command."
  :version "24.3"
  :type '(string :tag "command"))

(defcustom proced-signal-list
  '( ;; signals supported on all POSIX compliant systems
    ("HUP" . "   (1.  Hangup)")
    ("INT" . "   (2.  Terminal interrupt)")
    ("QUIT" . "  (3.  Terminal quit)")
    ("ABRT" . "  (6.  Process abort)")
    ("KILL" . "  (9.  Kill - cannot be caught or ignored)")
    ("ALRM" . "  (14. Alarm Clock)")
    ("TERM" . "  (15. Termination)")
    ;; signals supported on systems conforming to POSIX 1003.1-2001
    ;; according to (info "(coreutils) Signal specifications")
    ("BUS" . "   (Access to an undefined portion of a memory object)")
    ("CHLD" . "  (Child process terminated, stopped, or continued)")
    ("CONT" . "  (Continue executing, if stopped)")
    ("FPE" . "   (Erroneous arithmetic operation)")
    ("ILL" . "   (Illegal Instruction)")
    ("PIPE" . "  (Write on a pipe with no one to read it)")
    ("SEGV" . "  (Invalid memory reference)")
    ("STOP" . "  (Stop executing / pause - cannot be caught or ignored)")
    ("TSTP" . "  (Terminal stop / pause)")
    ("TTIN" . "  (Background process attempting read)")
    ("TTOU" . "  (Background process attempting write)")
    ("URG" . "   (High bandwidth data is available at a socket)")
    ("USR1" . "  (User-defined signal 1)")
    ("USR2" . "  (User-defined signal 2)"))
  "List of signals, used for minibuffer completion."
  :type '(repeat (cons (string :tag "signal name")
                       (string :tag "description"))))

;; For which attributes can we use a fixed width of the output field?
;; A fixed width speeds up formatting, yet it can make
;; `proced-grammar-alist' system-dependent.
;; (If proced runs like top(1) we want it to be fast.)
;;
;; If it is impossible / unlikely that an attribute has the same value
;; for two processes, then sorting can be based on one ordinary (fast)
;; predicate like `<'.  Otherwise, a list of proced predicates can be used
;; to refine the sort.
;;
;; It would be neat if one could temporarily override the following
;; predefined rules.
(defcustom proced-grammar-alist
  '( ;; attributes defined in `process-attributes'
    (euid    "EUID"    "%d" right proced-< nil (euid pid) (nil t nil))
    (user    "User"    proced-format-user left proced-string-lessp nil
                       (user pid) (nil t nil))
    (egid    "EGID"    "%d" right proced-< nil (egid euid pid) (nil t nil))
    (group   "Group"   nil left proced-string-lessp nil (group user pid)
                       (nil t nil))
    (comm    "Command" nil left proced-string-lessp nil (comm pid) (nil t nil))
    (state   "Stat"    proced-format-state left proced-string-lessp nil
                       (state pid) (nil t nil))
    (ppid    "PPID"    proced-format-ppid right proced-< nil (ppid pid)
                       ((lambda (ppid)
                          (proced-filter-parents proced-process-alist ppid))
                        "refine to process parents"))
    (pgrp    "PGrp"    proced-format-pgrp right proced-< nil (pgrp euid pid)
                       (nil t nil))
    (sess    "Sess"    proced-format-sess right proced-< nil (sess pid)
                       (nil t nil))
    (ttname  "TTY"     proced-format-ttname left proced-string-lessp nil
                       (ttname pid) (nil t nil))
    (tpgid   "TPGID"   "%d" right proced-< nil (tpgid pid) (nil t nil))
    (minflt  "MinFlt"  "%d" right proced-< nil (minflt pid) (nil t t))
    (majflt  "MajFlt"  "%d" right proced-< nil (majflt pid) (nil t t))
    (cminflt "CMinFlt" "%d" right proced-< nil (cminflt pid) (nil t t))
    (cmajflt "CMajFlt" "%d" right proced-< nil (cmajflt pid) (nil t t))
    (utime   "UTime"   proced-format-time right proced-time-lessp t (utime pid)
                       (nil t t))
    (stime   "STime"   proced-format-time right proced-time-lessp t (stime pid)
                       (nil t t))
    (time    "Time"    proced-format-time right proced-time-lessp t (time pid)
                       (nil t t))
    (cutime  "CUTime"  proced-format-time right proced-time-lessp t (cutime pid)
                       (nil t t))
    (cstime  "CSTime"  proced-format-time right proced-time-lessp t (cstime pid)
                       (nil t t))
    (ctime   "CTime"   proced-format-time right proced-time-lessp t (ctime pid)
                       (nil t t))
    (pri     "Pr"      "%d" right proced-< t (pri pid) (nil t t))
    (nice    "Ni"      "%3d" 3 proced-< t (nice pid) (t t nil))
    (thcount "THCount" "%d" right proced-< t (thcount pid) (nil t t))
    (start   "Start"   proced-format-start left proced-time-lessp nil (start pid)
                       (t t nil))
    (vsize   "VSize"   proced-format-memory right proced-< t (vsize pid)
                       (nil t t))
    (rss     "RSS"     proced-format-rss right proced-< t (rss pid) (nil t t))
    (etime   "ETime"   proced-format-time right proced-time-lessp t (etime pid)
                       (nil t t))
    (pcpu    "%CPU"    proced-format-cpu right proced-< t (pcpu pid) (nil t t))
    (pmem    "%Mem"    proced-format-mem right proced-< t (pmem pid) (nil t t))
    (args    "Args"    proced-format-args left proced-string-lessp nil
                       (args pid) (nil t nil))
    ;;
    ;; attributes defined by proced (see `proced-process-attributes')
    (pid     "PID"     proced-format-pid right proced-< nil (pid)
             ((lambda (ppid) (proced-filter-children proced-process-alist ppid))
              "refine to process children"))
    ;; process tree
    (tree    "Tree"   proced-format-tree left nil nil nil nil))
  "Alist of rules for handling Proced attributes.

Each element has the form

  (KEY NAME FORMAT JUSTIFY PREDICATE REVERSE SORT-SCHEME REFINER).

Symbol KEY is the car of a process attribute.

String NAME appears in the header line.

FORMAT specifies the format for displaying the attribute values.  It can
be a string passed to `format'.  It can be a function called with one
argument, the value of the attribute.  The value nil means take as is.

If JUSTIFY is an integer, its modulus gives the width of the attribute
values formatted with FORMAT.  If JUSTIFY is positive, NAME appears
right-justified, otherwise it appears left-justified.  If JUSTIFY is `left'
or `right', the field width is calculated from all field values in the listing.
If JUSTIFY is `left', the field values are formatted left-justified and
right-justified otherwise.

PREDICATE is the predicate for sorting and filtering the process listing
based on attribute KEY.  PREDICATE takes two arguments P1 and P2,
the corresponding attribute values of two processes.  PREDICATE should
return `equal' if P1 has same rank like P2.  Any other non-nil value says
that P1 is \"less than\" P2, or nil if not.
If PREDICATE is nil the attribute cannot be sorted.

PREDICATE defines an ascending sort order.  REVERSE is non-nil if the sort
order is descending.

SORT-SCHEME is a list (KEY1 KEY2 ...) defining a hierarchy of rules
for sorting the process listing.  KEY1, KEY2, ... are KEYs appearing as cars
of `proced-grammar-alist'.  First the PREDICATE of KEY1 is evaluated.
If it yields non-equal, it defines the sort order for the corresponding
processes.  If it evaluates to `equal' the PREDICATE of KEY2 is evaluated, etc.

REFINER can be a list of flags (LESS-B EQUAL-B LARGER-B) used by the command
`proced-refine' (see there) to refine the listing based on attribute KEY.
This command compares the value of attribute KEY of every process with
the value of attribute KEY of the process at the position of point
using PREDICATE.
If PREDICATE yields non-nil, the process is accepted if LESS-B is non-nil.
If PREDICATE yields `equal', the process is accepted if EQUAL-B is non-nil.
If PREDICATE yields nil, the process is accepted if LARGER-B is non-nil.

REFINER can also be a list (FUNCTION HELP-ECHO).
FUNCTION is called with one argument, the PID of the process at the position
of point.  The function must return a list of PIDs that is used for the refined
listing.  HELP-ECHO is a string that is shown when mouse is over this field.

If REFINER is nil no refinement is done."
  :type '(repeat (list :tag "Attribute"
                       (symbol :tag "Key")
                       (string :tag "Header")
                       (choice :tag "Format"
                               (const :tag "None" nil)
                               (string :tag "Format String")
                               (function :tag "Formatting Function"))
                       (choice :tag "Justification"
                               (const :tag "left" left)
                               (const :tag "right" right)
                               (integer :tag "width"))
                       (choice :tag "Predicate"
                               (const :tag "None" nil)
                               (function :tag "Function"))
                       (boolean :tag "Descending Sort Order")
                       (repeat :tag "Sort Scheme" (symbol :tag "Key"))
                       (choice :tag "Refiner"
                               (const :tag "None" nil)
                               (list (function :tag "Refinement Function")
                                     (string :tag "Help echo"))
                               (list :tag "Refine Flags"
                                     (boolean :tag "Less")
                                     (boolean :tag "Equal")
                                     (boolean :tag "Larger"))))))

(defcustom proced-custom-attributes nil
  "List of functions defining custom attributes.
This variable extends the functionality of `proced-process-attributes'.
Each function is called with one argument, the list of attributes
of a system process.  It returns a cons cell of the form (KEY . VALUE)
like `process-attributes'.  This cons cell is appended to the list
returned by `proced-process-attributes'.
If the function returns nil, the value is ignored."
  :type '(repeat (function :tag "Attribute")))

;; Formatting and sorting rules are defined "per attribute".  If formatting
;; and / or sorting should use more than one attribute, it appears more
;; transparent to define a new derived attribute, so that formatting and
;; sorting can use them consistently.  (Are there exceptions to this rule?
;; Would it be advantageous to have yet more general methods available?)
;; Sorting can also be based on attributes that are invisible in the listing.

(defcustom proced-format-alist
  '((short user pid tree pcpu pmem start time (args comm))
    (medium user pid tree pcpu pmem vsize rss ttname state start time (args comm))
    (long user euid group pid tree pri nice pcpu pmem vsize rss ttname state
          start time (args comm))
    (verbose user euid group egid pid ppid tree pgrp sess pri nice pcpu pmem
             state thcount vsize rss ttname tpgid minflt majflt cminflt cmajflt
             start time utime stime ctime cutime cstime etime (args comm)))
  "Alist of formats of listing.
The car of each element is a symbol, the name of the format.
The cdr is a list of attribute keys appearing in `proced-grammar-alist'.
An element of this list may also be a list of attribute keys that specifies
alternatives.  If the first attribute is absent for a process, use the second
one, etc."
  :type '(alist :key-type (symbol :tag "Format Name")
                :value-type (repeat :tag "Keys"
                                    (choice (symbol :tag "")
                                            (repeat :tag "Alternative Keys"
                                                    (symbol :tag ""))))))

(defcustom proced-format 'short
  "Current format of Proced listing.
It can be the car of an element of `proced-format-alist'.
It can also be a list of keys appearing in `proced-grammar-alist'."
  :type '(choice (symbol :tag "Format Name")
                 (repeat :tag "Keys" (symbol :tag "")))
  :local t)

;; FIXME: is there a better name for filter `user' that does not coincide
;; with an attribute key?
(defcustom proced-filter-alist
  `((user (user . proced-user-name))
    (user-running (user . proced-user-name)
                  (state . "\\`[Rr]\\'"))
    (all)
    (all-running (state . "\\`[Rr]\\'"))
    (emacs (fun-all . (lambda (list)
                        (proced-filter-children list ,(emacs-pid))))))
  "Alist of process filters.
The car of each element is a symbol, the name of the filter.
The cdr is a list of elementary filters that are applied to every process.
A process is displayed if it passes all elementary filters of a selected
filter.

An elementary filter can be one of the following:
\(KEY . REGEXP)   If value of attribute KEY matches REGEXP,
                 accept this process.
\(KEY . FUN)      Apply function FUN to attribute KEY.  Accept this process,
                 if FUN returns non-nil.
\(function . FUN) For each process, apply function FUN to list of attributes
                 of each.  Accept the process if FUN returns non-nil.
\(fun-all . FUN)  Apply function FUN to entire process list.
                 FUN must return the filtered list."
  :type '(repeat (cons :tag "Filter"
                       (symbol :tag "Filter Name")
                       (repeat :tag "Filters"
                               (choice (cons :tag "Key . Regexp" (symbol :tag "Key") regexp)
                                       (cons :tag "Key . Function" (symbol :tag "Key") function)
                                       (cons :tag "Function" (const :tag "Key: function" function) function)
                                       (cons :tag "Fun-all" (const :tag "Key: fun-all" fun-all) function))))))

(defcustom proced-filter 'user
  "Current filter of proced listing.
It can be the car of an element of `proced-filter-alist'.
It can also be a list of elementary filters as in the cdrs of the elements
of `proced-filter-alist'."
  :type '(choice (symbol :tag "Filter Name")
                 (repeat :tag "Filters"
                         (choice (cons :tag "Key . Regexp" (symbol :tag "Key") regexp)
                                 (cons :tag "Key . Function" (symbol :tag "Key") function)
                                 (cons :tag "Function" (const :tag "Key: function" function) function)
                                 (cons :tag "Fun-all" (const :tag "Key: fun-all" fun-all) function))))
  :local t)

(defcustom proced-sort 'pcpu
  "Current sort scheme for proced listing.
It must be the KEY of an element of `proced-grammar-alist'.
It can also be a list of KEYs as in the SORT-SCHEMEs of the elements
of `proced-grammar-alist'."
  :type '(choice (symbol :tag "Sort Scheme")
                 (repeat :tag "Key List" (symbol :tag "Key")))
  :local t)

(defcustom proced-descend t
  "Non-nil if proced listing is sorted in descending order."
  :type '(boolean :tag "Descending Sort Order")
  :local t)

(defcustom proced-goal-attribute 'args
  "If non-nil, key of the attribute that defines the `goal-column'."
  :type '(choice (const :tag "none" nil)
                 (symbol :tag "key")))

(defcustom proced-auto-update-interval 5
  "Time interval in seconds for auto updating Proced buffers."
  :type 'integer)

(defcustom proced-auto-update-flag nil
  "Non-nil means auto update proced buffers.
Special value `visible' means only update proced buffers that are currently
displayed in a window.  Can be changed interactively via
`proced-toggle-auto-update'."
  :type '(radio (const :tag "Don't auto update" nil)
                (const :tag "Only update visible proced buffers" visible)
                (const :tag "Update all proced buffers" t))
  :local t)

(defcustom proced-tree-flag nil
  "Non-nil for display of Proced buffer as process tree."
  :type 'boolean
  :local t)

(defcustom proced-post-display-hook nil
  "Normal hook run after displaying or updating a Proced buffer.
May be used to adapt the window size via `fit-window-to-buffer'."
  :type 'hook
  :options '(fit-window-to-buffer))

(defcustom proced-after-send-signal-hook nil
  "Normal hook run after sending a signal to processes by `proced-send-signal'.
May be used to revert the process listing."
  :type 'hook
  :options '(proced-revert))

(defcustom proced-enable-color-flag nil
  "Non-nil means Proced should display some process attributes with color."
  :type 'boolean
  :version "29.1")

(defcustom proced-low-memory-usage-threshold 0.1
  "The upper bound for low relative memory usage display in Proced.

When `proced-enable-color-flag' is non-nil, RSS values denoting a
proportion of memory, relative to total memory, that is lower
than this value will be displayed using the `proced-memory-low-usage' face."
  :type 'float
  :version "29.1")

(defcustom proced-medium-memory-usage-threshold 0.5
  "The upper bound for medium relative memory usage display in Proced.

When `proced-enable-color-flag' is non-nil, RSS values denoting a
proportion of memory, relative to total memory, that is less than
this value, but greater than `proced-low-memory-usage-threshold',
will be displayed using the `proced-memory-medium-usage' face.
RSS values denoting a greater proportion than this value will be
displayed using the `proced-memory-high-usage' face."
  :type 'float
  :version "29.1")

;; Internal variables

(defvar proced-available t;(not (null (list-system-processes)))
  "Non-nil means Proced is known to work on this system.")

(defvar-local proced-process-alist nil
  "Alist of processes displayed by Proced.
The car of each element is the PID, and the cdr is a list of
cons pairs, see `proced-process-attributes'.")

(defvar proced-sort-internal nil
  "Sort scheme for listing (internal format).
It is a list of lists (KEY PREDICATE REVERSE).")

(defvar proced-marker-char ?*		; the answer is 42
  "In Proced, the current mark character.")

;; Faces and font-lock code taken from dired,
;; but face variables are deprecated for new code.
(defgroup proced-faces nil
  "Faces used by Proced."
  :group 'proced
  :group 'faces)

(defface proced-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for Proced marks.")

(defface proced-marked
  '((t (:inherit error)))
  "Face used for marked processes.")

(defface proced-sort-header
  '((t (:inherit font-lock-keyword-face)))
  "Face used for header of attribute used for sorting.")

(defface proced-run-status-code
  '((t (:foreground "green")))
  "Face used in Proced buffers for running or runnable status code character \"R\"."
  :version "29.1")

(defface proced-interruptible-sleep-status-code
  '((((class color) (min-colors 88)) (:foreground "DimGrey"))
    (t (:slant italic)))
  "Face used in Proced buffers for interruptible sleep status code character \"S\"."
  :version "29.1")

(defface proced-uninterruptible-sleep-status-code
  '((((class color)) (:foreground "red"))
    (t (:weight bold)))
  "Face used in Proced buffers for uninterruptible sleep status code character \"D\"."
  :version "29.1")

(defface proced-executable
  '((((class color) (min-colors 88) (background dark)) (:foreground "DeepSkyBlue"))
    (((class color) (background dark)) (:foreground "cyan"))
    (((class color) (background light)) (:foreground "blue"))
    (t (:weight bold)))
  "Face used in Proced buffers for executable names.
The first word in the process arguments attribute is assumed to
be the executable that runs in the process."
  :version "29.1")

(defface proced-memory-high-usage
  '((((class color) (min-colors 88) (background dark)) (:foreground "orange"))
    (((class color) (min-colors 88) (background light)) (:foreground "OrangeRed"))
    (((class color)) (:foreground "red"))
    (t (:underline t)))
  "Face used in Proced buffers for high memory usage."
  :version "29.1")

(defface proced-memory-medium-usage
  '((((class color) (min-colors 88) (background dark)) (:foreground "yellow3"))
    (((class color) (min-colors 88) (background light)) (:foreground "orange"))
    (((class color)) (:foreground "yellow")))
  "Face used in Proced buffers for medium memory usage."
  :version "29.1")

(defface proced-memory-low-usage
  '((((class color) (min-colors 88) (background dark)) (:foreground "#8bcd50"))
    (((class color)) (:foreground "green")))
  "Face used in Proced buffers for low memory usage."
  :version "29.1")

(defface proced-emacs-pid
  '((((class color) (min-colors 88)) (:foreground "purple"))
    (((class color)) (:foreground "magenta")))
  "Face used in Proced buffers for the process ID of the current Emacs process."
  :version "29.1")

(defface proced-pid
  '((((class color) (min-colors 88)) (:foreground "#5085ef"))
    (((class color)) (:foreground "blue")))
  "Face used in Proced buffers for process IDs."
  :version "29.1")

(defface proced-session-leader-pid
  '((((class color) (min-colors 88)) (:foreground "#5085ef" :underline t))
    (((class color)) (:foreground "blue" :underline t))
    (t (:underline t)))
  "Face used in Proced buffers for process IDs which are session leaders."
  :version "29.1")

(defface proced-ppid
  '((((class color) (min-colors 88)) (:foreground "#5085bf"))
    (((class color)) (:foreground "blue")))
  "Face used in Proced buffers for parent process IDs."
  :version "29.1")

(defface proced-pgrp
  '((((class color) (min-colors 88)) (:foreground "#4785bf"))
    (((class color)) (:foreground "blue")))
  "Face used in Proced buffers for process group IDs."
  :version "29.1")

(defface proced-sess
  '((((class color) (min-colors 88)) (:foreground "#41729f"))
    (((class color)) (:foreground "MidnightBlue")))
  "Face used in Proced buffers for process session IDs."
  :version "29.1")

(defface proced-cpu
  '((((class color) (min-colors 88)) (:foreground "#6d5cc3" :weight bold))
    (t (:weight bold)))
  "Face used in Proced buffers for process CPU utilization."
  :version "29.1")

(defface proced-mem
  '((((class color) (min-colors 88))
     (:foreground "#6d5cc3")))
  "Face used in Proced buffers for process memory utilization."
  :version "29.1")

(defface proced-user
  '((t (:weight bold)))
  "Face used in Proced buffers for the user owning the process."
  :version "29.1")

(defface proced-time-colon
  '((((class color) (min-colors 88)) (:foreground "DarkMagenta"))
    (t (:weight bold)))
  "Face used in Proced buffers for the colon in time strings."
  :version "29.1")

(defvar proced-re-mark "^[^ \n]"
  "Regexp matching a marked line.
Important: the match ends just after the marker.")

(defvar-local proced-header-line nil
  "Headers in Proced buffer as a string.")

(defvar proced-temp-alist nil
  "Temporary alist (internal variable).")

(defvar proced-process-tree nil
  "Proced process tree (internal variable).")

(defvar proced-tree-depth nil
  "Internal variable for depth of Proced process tree.")

(defvar proced-auto-update-timer nil
  "Stores if Proced auto update timer is already installed.")

(defvar proced-log-buffer "*Proced log*"
  "Name of Proced Log buffer.")

(defconst proced-help-string
  (concat "\\<proced-mode-map> "
          "\\[next-line] next, "
          "\\[previous-line] previous, "
          "\\[proced-mark] mark, "
          "\\[proced-unmark] unmark, "
          "\\[proced-send-signal] kill, "
          "\\[quit-window] quit "
          "(type \\[proced-help] for more help)")
  "Help string for Proced.")

(defconst proced-header-help-echo
  "mouse-1, mouse-2: sort by attribute %s%s (%s)"
  "Help string shown when mouse is over a sortable header.")

(defconst proced-field-help-echo
  "mouse-2, RET: refine by attribute %s %s"
  "Help string shown when mouse is over a refinable field.")

(defvar proced-font-lock-keywords
  `(;; (Any) proced marks.
    (,proced-re-mark . 'proced-mark)
    ;; Processes marked with `proced-marker-char'
    ;; Should we make sure that only certain attributes are font-locked?
    (,(concat "^[" (char-to-string proced-marker-char) "]")
     ".+" (proced-move-to-goal-column) nil (0 'proced-marked))))

(defvar-keymap proced-mode-map
  :doc "Keymap for Proced commands."
  ;; moving
  "SPC"       #'next-line
  "n"         #'next-line
  "p"         #'previous-line
  "C-n"       #'next-line
  "C-p"       #'previous-line
  "S-SPC"     #'previous-line
  "<down>"    #'next-line
  "<up>"      #'previous-line
  ;; marking
  "d"         #'proced-mark ; Dired compatibility ("delete")
  "m"         #'proced-mark
  "u"         #'proced-unmark
  "DEL"       #'proced-unmark-backward
  "M"         #'proced-mark-all
  "U"         #'proced-unmark-all
  "t"         #'proced-toggle-marks
  "C"         #'proced-mark-children
  "P"         #'proced-mark-parents
  ;; filtering
  "f"         #'proced-filter-interactive
  "<mouse-2>" #'proced-refine
  "RET"       #'proced-refine
  ;; sorting
  "s c"       #'proced-sort-pcpu
  "s m"       #'proced-sort-pmem
  "s p"       #'proced-sort-pid
  "s s"       #'proced-sort-start
  "s S"       #'proced-sort-interactive
  "s t"       #'proced-sort-time
  "s u"       #'proced-sort-user
  ;; similar to `Buffer-menu-sort-by-column'
  "<header-line> <mouse-1>"   #'proced-sort-header
  "<header-line> <mouse-2>"   #'proced-sort-header
  "T"         #'proced-toggle-tree
  ;; formatting
  "F"         #'proced-format-interactive
  ;; operate
  "o"         #'proced-omit-processes
  "x"         #'proced-send-signal ; Dired compatibility
  "k"         #'proced-send-signal ; kill processes
  "r"         #'proced-renice ; renice processes
  ;; misc
  "h"         #'describe-mode
  "?"         #'proced-help
  "<remap> <undo>"            #'proced-undo
  ;; Additional keybindings are inherited from `special-mode-map'
  )
(put 'proced-mark :advertised-binding "m")

(defvar-local proced-refinements nil
  "Information about the current buffer refinements.

It should be a list of elements of the form (REFINER PID KEY GRAMMAR), where
REFINER and GRAMMAR are as described in `proced-grammar-alist', PID is the
process ID of the process used to create the refinement, and KEY the attribute
of the process.  A value of nil indicates that there are no active refinements.")

(easy-menu-define proced-menu proced-mode-map
  "Proced Menu."
  `("Proced"
    ["Mark" proced-mark
     :help "Mark Current Process"]
    ["Unmark" proced-unmark
     :help "Unmark Current Process"]
    ["Mark All" proced-mark-all
     :help "Mark All Processes"]
    ["Unmark All" proced-unmark-all
     :help "Unmark All Process"]
    ["Toggle Marks" proced-toggle-marks
     :help "Marked Processes Become Unmarked, and Vice Versa"]
    ["Mark Children" proced-mark-children
     :help "Mark Current Process and its Children"]
    ["Mark Parents" proced-mark-parents
     :help "Mark Current Process and its Parents"]
    "--"
    ("Filters"
     :help "Select Filter for Process Listing"
     ,@(mapcar (lambda (el)
                 (let ((filter (car el)))
                   `[,(symbol-name filter)
                     (proced-filter-interactive ',filter)
                     :style radio
                     :selected (eq proced-filter ',filter)]))
               proced-filter-alist))
    ("Sorting"
     :help "Select Sort Scheme"
     ["Sort..." proced-sort-interactive
      :help "Sort Process List"]
     "--"
     ["Sort by %CPU" proced-sort-pcpu]
     ["Sort by %MEM" proced-sort-pmem]
     ["Sort by PID" proced-sort-pid]
     ["Sort by START" proced-sort-start]
     ["Sort by TIME" proced-sort-time]
     ["Sort by USER" proced-sort-user])
    ("Formats"
     :help "Select Format for Process Listing"
     ,@(mapcar (lambda (el)
                 (let ((format (car el)))
                   `[,(symbol-name format)
                     (proced-format-interactive ',format)
                     :style radio
                     :selected (eq proced-format ',format)]))
               proced-format-alist))
    ["Tree Display" proced-toggle-tree
     :style toggle
     :selected (eval proced-tree-flag)
     :help "Display Proced Buffer as Process Tree"]
    "--"
    ["Omit Marked Processes" proced-omit-processes
     :help "Omit Marked Processes in Process Listing."]
    "--"
    ["Revert" revert-buffer
     :help "Revert Process Listing"]
    ["Auto Update" proced-toggle-auto-update
     :style toggle
     :selected (eval proced-auto-update-flag)
     :help "Auto Update of Proced Buffer"]
    "--"
    ["Send signal" proced-send-signal
     :help "Send Signal to Marked Processes"]
    ["Renice" proced-renice
     :help "Renice Marked Processes"]))

;; helper functions
(defun proced-user-name (user)
  "Check the `user' attribute with user name `proced' is running for."
  (string-equal user (if (file-remote-p default-directory)
                         (file-remote-p default-directory 'user)
                       (user-real-login-name))))

(defun proced-marker-regexp ()
  "Return regexp matching `proced-marker-char'."
  ;; `proced-marker-char' must appear in column zero
  (concat "^" (regexp-quote (char-to-string proced-marker-char))))

(defun proced-success-message (action count)
  "Display success message for ACTION performed for COUNT processes."
  (message "%s %s process%s" action count (if (= 1 count) "" "es")))

;; Unlike dired, we do not define our own commands for vertical motion.
;; If `goal-column' is set, `next-line' and `previous-line' are fancy
;; commands to satisfy our modest needs.  If `proced-goal-attribute'
;; and/or `goal-column' are not set, `next-line' and `previous-line'
;; are really what we need to preserve the column of point.
;; We use `proced-move-to-goal-column' for "non-interactive" cases only
;; to get a well-defined position of point.

(defun proced-move-to-goal-column ()
  "Move to `goal-column' if non-nil.  Return position of point."
  (beginning-of-line)
  (unless (eobp)
    (if goal-column
        (forward-char goal-column)
      (forward-char 2)))
  (point))

(defun proced-header-line ()
  "Return header line for Proced buffer."
  (let ((base (line-number-display-width 'columns))
        (hl (if (<= (window-hscroll) (length proced-header-line))
                (substring proced-header-line (window-hscroll)))))
    (when hl
      ;; From buff-menu.el: Turn whitespace chars in the header into
      ;; stretch specs so they work regardless of the header-line face.
      (let ((pos 0))
	(while (string-match "[ \t\n]+" hl pos)
	  (setq pos (match-end 0))
	  (put-text-property (match-beginning 0) pos 'display
			     `(space :align-to (,(+ pos base) . width))
			     hl)))
      (setq hl (replace-regexp-in-string ;; preserve text properties
		"\\(%\\)" "\\1\\1"
		hl)))
    (list (propertize " " 'display `(space :align-to (,base . width)))
          hl)))

(defun proced-pid-at-point ()
  "Return pid of system process at point.
Return nil if point is not on a process line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^. .")
        (get-text-property (match-end 0) 'proced-pid))))

(defun proced--position-info (pos)
  "Return information of the process at POS.

The returned information will have the form `(PID KEY COLUMN)' where
PID is the process ID of the process at point, KEY is the value of the
proced-key text property at point, and COLUMN is the column for which the
current value of the proced-key text property starts, or 0 if KEY is nil."
  ;; If point is on a field, we try to return point to that field.
  ;; Otherwise we try to return to the same column
  (save-excursion
    (goto-char pos)
    (let ((pid (proced-pid-at-point))
          (key (get-text-property (point) 'proced-key)))
      (list pid key ; can both be nil
            (if key
                (if (get-text-property (1- (point)) 'proced-key)
                    (- (point) (previous-single-property-change
                                (point) 'proced-key))
                  0)
              (current-column))))))

(defun proced--determine-pos (key column)
  "Return position of point in the current line using KEY and COLUMN.

Attempt to find the first position on the current line where the
text property proced-key is equal to KEY.  If this is not possible, return
the position of point of column COLUMN on the current line."
  (save-excursion
    (let (new-pos)
      (if key
          (let ((limit (line-end-position)) pos)
            (while (and (not new-pos)
                        (setq pos (next-property-change (point) nil limit)))
              (goto-char pos)
              (when (eq key (get-text-property (point) 'proced-key))
                (forward-char (min column (- (next-property-change (point))
                                             (point))))
                (setq new-pos (point))))
            (unless new-pos
              ;; we found the process, but the field of point
              ;; is not listed anymore
              (setq new-pos (proced-move-to-goal-column))))
        (setq new-pos (min (+ (line-beginning-position) column)
                           (line-end-position))))
      new-pos)))

;; proced mode

(define-derived-mode proced-mode special-mode "Proced"
  "Mode for displaying system processes and sending signals to them.
Type \\[proced] to start a Proced session.  In a Proced buffer
type \\<proced-mode-map>\\[proced-mark] to mark a process for later commands.
Type \\[proced-send-signal] to send signals to marked processes.
Type \\[proced-renice] to renice marked processes.

The initial content of a listing is defined by the variable
`proced-filter' and the variable `proced-format'.

The variable `proced-filter' specifies which system processes are
displayed.

The variable `proced-format' specifies which attributes are
displayed for each process.

Type \\[proced-filter-interactive] and \\[proced-format-interactive] to \
change the values of `proced-filter' and
`proced-format'.  The current value of the variable
`proced-filter' is indicated in the mode line.

The sort order of Proced listings is defined by the variable `proced-sort'.
Type \\[proced-sort-interactive] or click on a header in the header \
line to change the sort scheme.
The current sort scheme is indicated in the mode line, using
\"+\" or \"-\" for ascending or descending sort order.

Type \\[proced-toggle-tree] to toggle whether the listing is displayed as process tree.

Type \\[proced-toggle-auto-update] to automatically update the
process list.  The time interval for updates can be configured
via `proced-auto-update-interval'.

An existing Proced listing can be refined by typing \\[proced-refine].
Refining an existing listing does not update the variable `proced-filter'.

The attribute-specific rules for formatting, filtering, sorting,
and refining are defined in `proced-grammar-alist'.

After displaying or updating a Proced buffer, Proced runs the
normal hook `proced-post-display-hook'.

\\{proced-mode-map}"
  :interactive nil
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (setq buffer-read-only t
        truncate-lines t
        header-line-format '(:eval (proced-header-line)))
  (add-hook 'post-command-hook #'force-mode-line-update nil t)  ;; FIXME: Why?
  (setq-local revert-buffer-function #'proced-revert)
  (setq-local font-lock-defaults
              '(proced-font-lock-keywords t nil nil beginning-of-line))
  (setq-local switch-to-buffer-preserve-window-point nil)
  ;; So that the heading scales together with the body of the table.
  (setq-local text-scale-remap-header-line t)
  (if (and (not proced-auto-update-timer) proced-auto-update-interval)
      (setq proced-auto-update-timer
            (run-at-time t proced-auto-update-interval
                         'proced-auto-update-timer))))

;;;###autoload
(defun proced (&optional arg)
  "Generate a listing of UNIX system processes.
\\<proced-mode-map>
If invoked with optional non-negative ARG, do not select the
window displaying the process information.

If `proced-show-remote-processes' is non-nil or the command is
invoked with a negative ARG `\\[universal-argument] \\[negative-argument]', \
and `default-directory'
points to a remote host, the system processes of that host are shown.

This function runs the normal hook `proced-post-display-hook'.

See `proced-mode' for a description of features available in
Proced buffers."
  (interactive "P")
  (unless proced-available
    (error "Proced is not available on this system"))
  (let ((buffer (get-buffer-create "*Proced*")) new)
    (set-buffer buffer)
    (when (and (file-remote-p default-directory)
               (not
                (or proced-show-remote-processes
                    (eq arg '-))))
      (setq default-directory temporary-file-directory))
    (setq new (zerop (buffer-size)))
    (when new
      (proced-mode)
      ;; `proced-update' runs `proced-post-display-hook' only if the
      ;; Proced buffer has been selected.  Yet the following call of
      ;; `proced-update' is for an empty Proced buffer that has not
      ;; yet been selected.  Therefore we need to call
      ;; `proced-post-display-hook' below.
      (proced-update t))
    (if arg
        (progn
          (display-buffer buffer)
          (with-current-buffer buffer
            (proced-update t)))
      (pop-to-buffer buffer)
      (proced-update t)
      (message
       (substitute-command-keys
        "Type \\<proced-mode-map>\\[quit-window] to quit, \\[proced-help] for help")))))

(defun proced-auto-update-timer ()
  "Auto-update Proced buffers using `run-at-time'.

If there are no proced buffers, cancel the timer."
  (if-let* ((buffers (match-buffers '(derived-mode . proced-mode))))
      (dolist (buf buffers)
        (when-let* ((flag (buffer-local-value 'proced-auto-update-flag buf))
                    ((or (not (eq flag 'visible))
                         (get-buffer-window buf 'visible))))
          (with-current-buffer buf
            (proced-update t t))))
    (cancel-timer proced-auto-update-timer)
    (setq proced-auto-update-timer nil)))

(defun proced-toggle-auto-update (arg)
  "Change whether this Proced buffer is updated automatically.
With prefix ARG, update this buffer automatically if ARG is positive,
update the buffer only when the buffer is displayed in a window if ARG is 0,
otherwise do not update.  Sets the variable `proced-auto-update-flag' by
cycling between nil, `visible' and t.  The time interval for updates is
specified via `proced-auto-update-interval'."
  (interactive (list (or current-prefix-arg 'toggle)) proced-mode)
  (setq proced-auto-update-flag
        (cond ((eq arg 'toggle)
               (cond ((not proced-auto-update-flag) 'visible)
                     ((eq proced-auto-update-flag 'visible) t)
                     (t nil)))
              (arg
               (setq arg (prefix-numeric-value arg))
               (message "%s" arg)
               (cond ((> arg 0) t)
                     ((eq arg 0) 'visible)
                     (t nil)))
              (t (not proced-auto-update-flag))))
  (message "Proced auto update %s"
           (cond ((eq proced-auto-update-flag 'visible) "enabled (only when buffer is visible)")
                 (proced-auto-update-flag "enabled (unconditionally)")
                 (t "disabled"))))

;;; Mark

(defun proced-mark (&optional count)
  "Mark the current (or next COUNT) processes."
  (interactive "p" proced-mode)
  (proced-do-mark t count))

(defun proced-unmark (&optional count)
  "Unmark the current (or next COUNT) processes."
  (interactive "p" proced-mode)
  (proced-do-mark nil count))

(defun proced-unmark-backward (&optional count)
  "Unmark the previous (or COUNT previous) processes."
  ;; Analogous to `dired-unmark-backward',
  ;; but `ibuffer-unmark-backward' behaves different.
  (interactive "p" proced-mode)
  (proced-do-mark nil (- (or count 1))))

(defun proced-do-mark (mark &optional count)
  "Mark the current (or next COUNT) processes using MARK."
  (or count (setq count 1))
  (let ((backward (< count 0))
	buffer-read-only)
    (setq count (1+ (if (<= 0 count) count
                      (min (1- (line-number-at-pos)) (abs count)))))
    (beginning-of-line)
    (while (not (or (zerop (setq count (1- count))) (eobp)))
      (proced-insert-mark mark backward))
    (proced-move-to-goal-column)))

(defun proced-toggle-marks ()
  "Toggle marks: marked processes become unmarked, and vice versa."
  (interactive nil proced-mode)
  (let ((mark-re (proced-marker-regexp))
        buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at mark-re)
               (proced-insert-mark nil))
              ((= (following-char) ?\s)
               (proced-insert-mark t))
              (t
               (forward-line 1)))))))

(defun proced-insert-mark (mark &optional backward)
  "If MARK is non-nil, insert `proced-marker-char'.
If BACKWARD is non-nil, move one line backwards before inserting the mark.
Otherwise move one line forward after inserting the mark."
  (if backward (forward-line -1))
  (insert (if mark proced-marker-char ?\s))
  (delete-char 1)
  (unless backward (forward-line)))

(defun proced-mark-all ()
  "Mark all processes.
If `transient-mark-mode' is turned on and the region is active,
mark the region."
  (interactive nil proced-mode)
  (proced-do-mark-all t))

(defun proced-unmark-all ()
  "Unmark all processes.
If `transient-mark-mode' is turned on and the region is active,
unmark the region."
  (interactive nil proced-mode)
  (proced-do-mark-all nil))

(defun proced-do-mark-all (mark)
  "Mark all processes using MARK.
If `transient-mark-mode' is turned on and the region is active,
mark the region."
  (let* ((count 0)
         (proced-marker-char (if mark proced-marker-char ?\s))
         (marker-re (proced-marker-regexp))
         end buffer-read-only)
    (save-excursion
      (if (use-region-p)
          ;; Operate even on those lines that are only partially a part
          ;; of region.  This appears most consistent with
          ;; `proced-move-to-goal-column'.
          (progn (setq end (save-excursion
                             (goto-char (region-end))
                             (unless (looking-at "^") (forward-line))
                             (point)))
                 (goto-char (region-beginning))
                 (unless (looking-at "^") (beginning-of-line)))
        (goto-char (point-min))
        (setq end (point-max)))
      (while (< (point) end)
        (unless (looking-at marker-re)
          (setq count (1+ count))
          (insert proced-marker-char)
          (delete-char 1))
        (forward-line))
      (proced-success-message (if mark "Marked" "Unmarked") count))))

(defun proced-mark-children (ppid &optional omit-ppid)
  "Mark child processes of process PPID.
Also mark process PPID unless prefix OMIT-PPID is non-nil."
  (interactive (list (proced-pid-at-point) current-prefix-arg) proced-mode)
  (proced-mark-process-alist
   (proced-filter-children proced-process-alist ppid omit-ppid)))

(defun proced-mark-parents (cpid &optional omit-cpid)
  "Mark parent processes of process CPID.
Also mark CPID unless prefix OMIT-CPID is non-nil."
  (interactive (list (proced-pid-at-point) current-prefix-arg) proced-mode)
  (proced-mark-process-alist
   (proced-filter-parents proced-process-alist cpid omit-cpid)))

(defun proced-mark-process-alist (process-alist &optional quiet)
  "Mark processes in PROCESS-ALIST.
If QUIET is non-nil suppress status message."
  (let ((count 0))
    (if process-alist
        (let (buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (assq (proced-pid-at-point) process-alist)
                (insert proced-marker-char)
                (delete-char 1)
                (setq count (1+ count)))
              (forward-line)))))
    (unless quiet
      (proced-success-message "Marked" count))))

;; Mostly analog of `dired-do-kill-lines'.
;; However, for negative args the target lines of `dired-do-kill-lines'
;; include the current line, whereas `dired-mark' for negative args operates
;; on the preceding lines.  Here we are consistent with `dired-mark'.
(defun proced-omit-processes (&optional arg quiet)
  "Omit marked processes.
With prefix ARG, omit that many lines starting with the current line.
\(A negative argument omits backward.)
If `transient-mark-mode' is turned on and the region is active,
omit the processes in region.
If QUIET is non-nil suppress status message.
Returns count of omitted lines."
  (interactive "P" proced-mode)
  (let ((mark-re (proced-marker-regexp))
        (count 0)
        buffer-read-only)
    (cond ((use-region-p) ;; Omit active region
           (let ((lines (count-lines (region-beginning) (region-end))))
             (save-excursion
               (goto-char (region-beginning))
               (while (< count lines)
                 (proced-omit-process)
                 (setq count (1+ count))))))
          ((not arg) ;; Omit marked lines
           (save-excursion
             (goto-char (point-min))
             (while (and (not (eobp))
                         (re-search-forward mark-re nil t))
               (proced-omit-process)
               (setq count (1+ count)))))
          ((< 0 arg) ;; Omit forward
           (while (and (not (eobp)) (< count arg))
             (proced-omit-process)
             (setq count (1+ count))))
          ((< arg 0) ;; Omit backward
           (while (and (not (bobp)) (< count (- arg)))
             (forward-line -1)
             (proced-omit-process)
             (setq count (1+ count)))))
    (unless (zerop count) (proced-move-to-goal-column))
    (unless quiet (proced-success-message "Omitted" count))
    count))

(defun proced-omit-process ()
  "Omit process from listing point is on.
Update `proced-process-alist' accordingly."
  (setq proced-process-alist
        (assq-delete-all (proced-pid-at-point) proced-process-alist))
  (delete-region (line-beginning-position)
                 (save-excursion (forward-line) (point))))

;;; Filtering

(defun proced-filter (process-alist filter-list)
  "Apply FILTER-LIST to PROCESS-ALIST.
Return the filtered process list."
  (if (symbolp filter-list)
      (setq filter-list (cdr (assq filter-list proced-filter-alist))))
  (dolist (filter filter-list)
    (let (new-alist)
      (cond ( ;; apply function to entire process list
             (eq (car filter) 'fun-all)
             (setq new-alist (funcall (cdr filter) process-alist)))
            ( ;; apply predicate to each list of attributes
             (eq (car filter) 'function)
             (dolist (process process-alist)
               (if (funcall (cdr filter) (cdr process))
                   (push process new-alist))))
            (t ;; apply predicate to specified attribute
             (let* ((cdrfilter (cdr filter))
                    (fun (if (stringp cdrfilter)
                            (lambda (val)
                              (string-match cdrfilter val))
                          cdrfilter))
                    value)
               (dolist (process process-alist)
                 (setq value (cdr (assq (car filter) (cdr process))))
                 (if (and value (funcall fun value))
                     (push process new-alist))))))
      (setq process-alist new-alist)))
  process-alist)

(defun proced-filter-interactive (scheme)
  "Filter Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no filtering.
Set variable `proced-filter' to SCHEME.  Revert listing."
  (interactive
   (let ((scheme (completing-read "Filter: "
                                  proced-filter-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme))))
   proced-mode)
  ;; only update if necessary
  (unless (eq proced-filter scheme)
    (setq proced-filter scheme)
    (proced-update t)))

(defun proced-filter-parents (process-alist pid &optional omit-pid)
  "For PROCESS-ALIST return list of parent processes of PID.
This list includes PID unless OMIT-PID is non-nil."
  (let ((parent-list (unless omit-pid (list (assq pid process-alist))))
        (process (assq pid process-alist))
        ppid)
    (while (and (setq ppid (cdr (assq 'ppid (cdr process))))
                ;; Ignore a PPID that equals PID.
                (/= ppid pid)
                ;; Accept only PPIDs that correspond to members in PROCESS-ALIST.
                (setq process (assq ppid process-alist)))
      (setq pid ppid)
      (push process parent-list))
    parent-list))

(defun proced-filter-children (process-alist ppid &optional omit-ppid)
  "For PROCESS-ALIST return list of child processes of PPID.
This list includes PPID unless OMIT-PPID is non-nil."
  (let ((proced-temp-alist (proced-children-alist process-alist))
        new-alist)
    (dolist (pid (proced-children-pids ppid))
      (push (assq pid process-alist) new-alist))
    (if omit-ppid
        (assq-delete-all ppid new-alist)
      new-alist)))

;;; Process tree

(defun proced-children-alist (process-alist)
  "Return children alist for PROCESS-ALIST.
The children alist has elements (PPID PID1 PID2 ...).
PPID is a parent PID.  PID1, PID2, ... are the child processes of PPID.
The children alist inherits the sorting order of PROCESS-ALIST.
The list of children does not include grandchildren."
  ;; The PPIDs inherit the sorting order of PROCESS-ALIST.
  (let ((process-tree (mapcar (lambda (a) (list (car a))) process-alist))
        ppid)
    (dolist (process process-alist)
      (setq ppid (cdr (assq 'ppid (cdr process))))
      (if (and ppid
               ;; Ignore a PPID that equals PID.
               (/= ppid (car process))
               ;; Accept only PPIDs that correspond to members in PROCESS-ALIST.
               (assq ppid process-alist))
          (let ((temp-alist process-tree) elt)
            (while (setq elt (pop temp-alist))
              (when (eq ppid (car elt))
                (setq temp-alist nil)
                (setcdr elt (cons (car process) (cdr elt))))))))
    ;; The child processes inherit the sorting order of PROCESS-ALIST.
    (setq process-tree
          (mapcar (lambda (a) (cons (car a) (nreverse (cdr a))))
                  process-tree))))

(defun proced-children-pids (ppid)
  "Return list of children PIDs of PPID (including PPID)."
  (let ((cpids (cdr (assq ppid proced-temp-alist))))
    (if cpids
        (cons ppid (apply #'append (mapcar #'proced-children-pids cpids)))
      (list ppid))))

(defun proced-process-tree (process-alist)
  "Return process tree for PROCESS-ALIST.
It is an alist of alists where the car of each alist is a parent process
and the cdr is a list of child processes according to the ppid attribute
of these processes.
The process tree inherits the sorting order of PROCESS-ALIST."
  (let ((proced-temp-alist (proced-children-alist process-alist))
        pid-alist proced-process-tree)
    (while (setq pid-alist (pop proced-temp-alist))
      (push (proced-process-tree-internal pid-alist) proced-process-tree))
    (nreverse proced-process-tree)))

(defun proced-process-tree-internal (pid-alist)
  "Helper function for `proced-process-tree'."
  (let ((cpid-list (cdr pid-alist)) cpid-alist cpid)
    (while (setq cpid (car cpid-list))
      (if (setq cpid-alist (assq cpid proced-temp-alist))
          ;; Unprocessed part of process tree that needs to be
          ;; analyzed recursively.
          (progn
            (setq proced-temp-alist
                  (assq-delete-all cpid proced-temp-alist))
            (setcar cpid-list (proced-process-tree-internal cpid-alist)))
        ;; We already processed this subtree and take it "as is".
        (setcar cpid-list (assq cpid proced-process-tree))
        (setq proced-process-tree
              (assq-delete-all cpid proced-process-tree)))
      (pop cpid-list)))
  pid-alist)

(defun proced-toggle-tree (arg)
  "Toggle the display of the process listing as process tree.
With prefix ARG, display as process tree if ARG is positive, otherwise
do not display as process tree.  Sets the variable `proced-tree-flag'.

The process tree is generated from the selected processes in the
Proced buffer (that is, the processes in `proced-process-alist').
All processes that do not have a parent process in this list
according to their ppid attribute become the root of a process tree.
Each parent process is followed by its child processes.
The process tree inherits the chosen sorting order of the process listing,
that is, child processes of the same parent process are sorted using
the selected sorting order."
  (interactive (list (or current-prefix-arg 'toggle)) proced-mode)
  (setq proced-tree-flag
        (cond ((eq arg 'toggle) (not proced-tree-flag))
              (arg (> (prefix-numeric-value arg) 0))
              (t (not proced-tree-flag))))
  (proced-update)
  (message "Proced process tree display %s"
           (if proced-tree-flag "enabled" "disabled")))

(defun proced-tree (process-alist)
  "Rearrange PROCESS-ALIST as process tree.
If `proced-tree-flag' is non-nil, rearrange PROCESS-ALIST such that
every processes is followed by its child processes.  Each process
gets a tree attribute that specifies the depth of the process in the tree.
A root process is a process with no parent within PROCESS-ALIST according
to its value of the ppid attribute.  It has depth 0.

If `proced-tree-flag' is nil, remove the tree attribute.
Return the rearranged process list."
  (if proced-tree-flag
      ;; add tree attribute
      (let ((process-tree (proced-process-tree process-alist))
            (proced-tree-depth 0)
            (proced-temp-alist process-alist)
            proced-process-tree pt)
        (while (setq pt (pop process-tree))
          (proced-tree-insert pt))
        (nreverse proced-process-tree))
    ;; remove tree attribute
    (let ((process-alist process-alist))
      (while process-alist
        (setcar process-alist
                (assq-delete-all 'tree (car process-alist)))
        (pop process-alist)))
    process-alist))

(defun proced-tree-insert (process-tree)
  "Helper function for `proced-tree'."
  (let ((pprocess (assq (car process-tree) proced-temp-alist)))
    (push (append (list (car pprocess))
                  (list (cons 'tree proced-tree-depth))
                  (cdr pprocess))
          proced-process-tree)
    (if (cdr process-tree)
        (let ((proced-tree-depth (1+ proced-tree-depth)))
          (mapc #'proced-tree-insert (cdr process-tree))))))

;; Refining

;; Filters are used to select the processes in a new listing.
;; Refiners are used to narrow down further (interactively) the processes
;; in an existing listing.

(defun proced-refine (&optional event)
  "Refine Proced listing by comparing with the attribute value at point.
Optional EVENT is the location of the Proced field.

Refinement is controlled by the REFINER defined for each attribute ATTR
in `proced-grammar-alist'.

If REFINER is a list of flags and point is on a process's value of ATTR,
this command compares the value of ATTR of every process with the value
of ATTR of the process at the position of point.

The predicate for the comparison of two ATTR values is defined
in `proced-grammar-alist'.  For each return value of the predicate
a refine flag is defined in `proced-grammar-alist'.  One can select
processes for which the value of ATTR is \"less than\", \"equal\",
and / or \"larger\" than ATTR of the process point is on.  A process
is included in the new listing if the refine flag for the corresponding
return value of the predicate is non-nil.
The help-echo string for `proced-refine' uses \"+\" or \"-\" to indicate
the current values of these refine flags.

If REFINER is a cons pair (FUNCTION . HELP-ECHO), FUNCTION is called
with one argument, the PID of the process at the position of point.
The function must return a list of PIDs that is used for the refined
listing.  HELP-ECHO is a string that is shown when mouse is over this field.

This command refines an already existing process listing generated initially
based on the value of the variable `proced-filter'.  It does not change
this variable.  It does not revert the listing.  If you frequently need
a certain refinement, consider defining a new filter in `proced-filter-alist'."
  (interactive (list last-input-event) proced-mode)
  (if event (posn-set-point (event-end event)))
  (let ((key (get-text-property (point) 'proced-key))
        (pid (get-text-property (point) 'proced-pid)))
    (if (and key pid)
        (let* ((grammar (assq key proced-grammar-alist))
               (refiner (nth 7 grammar)))
          (when refiner
            (add-to-list 'proced-refinements (list refiner pid key grammar) t)
            (proced-update)))
      (message "No refiner defined here."))))

;; Proced predicates for sorting and filtering are based on a three-valued
;; logic:
;; Predicates take two arguments P1 and P2, the corresponding attribute
;; values of two processes.  Predicates should return 'equal if P1 has
;; same rank like P2.  Any other non-nil value says that P1 is "less than" P2,
;; or nil if not.

(defun proced-< (num1 num2)
  "Return t if NUM1 less than NUM2.
Return `equal' if NUM1 equals NUM2.  Return nil if NUM1 greater than NUM2.
If either NUM1 or NUM2 is not a number, return nil."
  (when (and (numberp num1) (numberp num2))
    (if (= num1 num2)
        'equal
      (< num1 num2))))

(defun proced-string-lessp (s1 s2)
  "Return t if string S1 is less than S2 in lexicographic order.
Return `equal' if S1 and S2 have identical contents.
Return nil otherwise."
  (if (string= s1 s2)
      'equal
    (string-lessp s1 s2)))

(defun proced-time-lessp (t1 t2)
  "Return t if time value T1 is less than time value T2.
Return `equal' if T1 equals T2.  Return nil otherwise."
  (or (time-less-p t1 t2)
      (if (not (time-less-p t2 t1)) 'equal)))

;;; Sorting

(define-obsolete-function-alias 'proced-xor #'xor "27.1")

(defun proced-sort-p (p1 p2)
  "Predicate for sorting processes P1 and P2."
  (if (not (cdr proced-sort-internal))
      ;; only one predicate: fast scheme
      (let* ((sorter (car proced-sort-internal))
             (k1 (cdr (assq (car sorter) (cdr p1))))
             (k2 (cdr (assq (car sorter) (cdr p2)))))
        ;; if the attributes are undefined, we should really abort sorting
        (if (and k1 k2)
            (xor (funcall (nth 1 sorter) k1 k2)
                 (nth 2 sorter))))
    (let ((sort-list proced-sort-internal) sorter predicate k1 k2)
      (catch 'done
        (while (setq sorter (pop sort-list))
          (setq k1 (cdr (assq (car sorter) (cdr p1)))
                k2 (cdr (assq (car sorter) (cdr p2)))
                predicate
                (if (and k1 k2)
                    (funcall (nth 1 sorter) k1 k2)))
          (if (not (eq predicate 'equal))
              (throw 'done (xor predicate (nth 2 sorter)))))
        (eq t predicate)))))

(defun proced-sort (process-alist sorter descend)
  "Sort PROCESS-ALIST using scheme SORTER.
SORTER is a scheme like `proced-sort'.
DESCEND is non-nil if the first element of SORTER is sorted
in descending order.
Return the sorted process list."
  ;; translate SORTER into a list of lists (KEY PREDICATE REVERSE)
  (setq proced-sort-internal
        (mapcar (lambda (arg)
                  (let ((grammar (assq arg proced-grammar-alist)))
                    (unless (nth 4 grammar)
                      (error "Attribute %s not sortable" (car grammar)))
                    (list arg (nth 4 grammar) (nth 5 grammar))))
                (cond ((listp sorter) sorter)
                      ((and (symbolp sorter)
                            (nth 6 (assq sorter proced-grammar-alist))))
                      ((symbolp sorter) (list sorter))
                      (t (error "Sorter undefined %s" sorter)))))
  (if proced-sort-internal
      (progn
        ;; splice DESCEND into the list
        (setcar proced-sort-internal
                (list (caar proced-sort-internal)
                      (nth 1 (car proced-sort-internal)) descend))
        (sort process-alist 'proced-sort-p))
    process-alist))

(defun proced-sort-interactive (scheme &optional arg)
  "Sort Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no sorting.

Prefix ARG controls sort order:
- If prefix ARG is positive (negative), sort in ascending (descending) order.
- If ARG is nil or `no-arg' and SCHEME is equal to the previous sorting scheme,
  reverse the sorting order.
- If ARG is nil or `no-arg' and SCHEME differs from the previous sorting scheme,
  adopt the sorting order defined for SCHEME in `proced-grammar-alist'.

Set variable `proced-sort' to SCHEME.  The current sort scheme is displayed
in the mode line, using \"+\" or \"-\" for ascending or descending order."
  (interactive
   (let* (choices
          (scheme (completing-read "Sort attribute: "
                                   (dolist (grammar proced-grammar-alist choices)
                                     (if (nth 4 grammar)
                                         (push (list (car grammar)) choices)))
                                   nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           ;; like 'toggle in `define-derived-mode'
           (or current-prefix-arg 'no-arg)))
   proced-mode)

  (setq proced-descend
        ;; If `proced-sort-interactive' is called repeatedly for the same
        ;; sort key, the sort order is reversed.
        (cond ((and (eq arg 'no-arg) (equal proced-sort scheme))
               (not proced-descend))
              ((eq arg 'no-arg)
               (nth 5 (assq (if (consp scheme) (car scheme) scheme)
                            proced-grammar-alist)))
              (arg (< (prefix-numeric-value arg) 0))
              ((equal proced-sort scheme)
               (not proced-descend))
              (t (nth 5 (assq (if (consp scheme) (car scheme) scheme)
                                   proced-grammar-alist))))
        proced-sort scheme)
  (proced-update))

(defun proced-sort-pcpu (&optional arg)
  "Sort Proced buffer by percentage CPU time (%CPU).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)) proced-mode)
  (proced-sort-interactive 'pcpu arg))

(defun proced-sort-pmem (&optional arg)
  "Sort Proced buffer by percentage memory usage (%MEM).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)) proced-mode)
  (proced-sort-interactive 'pmem arg))

(defun proced-sort-pid (&optional arg)
  "Sort Proced buffer by PID.
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)) proced-mode)
  (proced-sort-interactive 'pid arg))

(defun proced-sort-start (&optional arg)
  "Sort Proced buffer by time the command started (START).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)) proced-mode)
  (proced-sort-interactive 'start arg))

(defun proced-sort-time (&optional arg)
  "Sort Proced buffer by CPU time (TIME).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)) proced-mode)
  (proced-sort-interactive 'time arg))

(defun proced-sort-user (&optional arg)
  "Sort Proced buffer by USER.
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)) proced-mode)
  (proced-sort-interactive 'user arg))

(defun proced-sort-header (event &optional arg)
  "Sort Proced listing based on an attribute.
EVENT is a mouse event with starting position in the header line.
It is converted to the corresponding attribute key.
This command updates the variable `proced-sort'.
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list last-input-event (or last-prefix-arg 'no-arg)) proced-mode)
  (let* ((start (event-start event))
         (obj (posn-object start))
         col key)
    (save-selected-window
      (select-window (posn-window start))
      (setq col (+ (if obj (cdr obj) (posn-point start))
                   (window-hscroll)))
      (when (and (<= 0 col) (< col (length proced-header-line)))
        (setq key (get-text-property col 'proced-key proced-header-line))
        (if key
            (proced-sort-interactive key arg)
          (message "No sorter defined here."))))))

;;; Formatting

(defun proced-format-time (time)
  "Format time interval TIME."
  (let* ((ftime (time-convert time 'integer))
         (days (truncate ftime 86400))
         (ftime (mod ftime 86400))
         (hours (truncate ftime 3600))
         (ftime (mod ftime 3600))
         (minutes (truncate ftime 60))
         (seconds (mod ftime 60))
         (colon (if proced-enable-color-flag
                    (propertize ":" 'font-lock-face 'proced-time-colon)
                  ":")))
    (cond ((< 0 days)
           (format "%d-%02d%s%02d%s%02d" days hours colon minutes colon seconds))
          ((< 0 hours)
           (format "%02d%s%02d%s%02d" hours colon minutes colon seconds))
          (t
           (format "%02d%s%02d" minutes colon seconds)))))

(defun proced-format-start (start)
  "Format time START."
  (let ((d-start (decode-time start))
        (d-current (decode-time))
        (colon (if proced-enable-color-flag
                   (propertize ":" 'font-lock-face 'proced-time-colon)
                 ":")))
    (cond (;; process started in previous years
           (< (decoded-time-year d-start) (decoded-time-year d-current))
           (format-time-string "  %Y" start))
          ;; process started today
          ((and (= (decoded-time-day d-start) (decoded-time-day d-current))
                (= (decoded-time-month d-start) (decoded-time-month d-current)))
           (string-replace ":" colon (format-time-string " %H:%M" start)))
          (t ;; process started this year
           (format-time-string "%b %e" start)))))

(defun proced-format-ttname (ttname)
  "Format attribute TTNAME, omitting path \"/dev/\"."
  ;; Does this work for all systems?
  (substring ttname (if (string-match "\\`/dev/" ttname)
                        (match-end 0) 0)))

(defun proced-format-tree (tree)
  "Format attribute TREE."
  (concat (make-string tree ?\s) (number-to-string tree)))

;; Proced assumes that every process occupies only one line in the listing.
(defun proced-format-args (args)
  "Format attribute ARGS.
Replace newline characters by \"^J\" (two characters)."
  (string-replace "\n" "^J"
                  (pcase-let* ((`(,exe . ,rest) (split-string args))
                               (exe-prop (if proced-enable-color-flag
                                             (propertize exe 'font-lock-face 'proced-executable)
                                           exe)))
                    (mapconcat #'identity (cons exe-prop rest) " "))))

(defun proced-format-memory (kilobytes)
  "Format KILOBYTES in a human readable format."
  (funcall byte-count-to-string-function (* 1024 kilobytes)))

(defun proced-format-rss (kilobytes)
  "Format RSS KILOBYTES in a human readable format."
  (let ((formatted (proced-format-memory kilobytes)))
    (if-let* ((proced-enable-color-flag)
              (total (car (memory-info)))
              (proportion (/ (float kilobytes) total)))
        (cond ((< proportion proced-low-memory-usage-threshold)
               (propertize formatted 'font-lock-face 'proced-memory-low-usage))
              ((< proportion proced-medium-memory-usage-threshold)
               (propertize formatted 'font-lock-face 'proced-memory-medium-usage))
              (t (propertize formatted 'font-lock-face 'proced-memory-high-usage)))
      formatted)))

(defun proced-format-state (state)
  "Format STATE."
  (cond ((and proced-enable-color-flag (string= state "R"))
         (propertize state 'font-lock-face 'proced-run-status-code))
        ((and proced-enable-color-flag (string= state "S"))
         (propertize state 'font-lock-face 'proced-interruptible-sleep-status-code))
        ((and proced-enable-color-flag (string= state "D"))
         (propertize state 'font-lock-face 'proced-uninterruptible-sleep-status-code))
        (t state)))

(defun proced-format-pid (pid)
  "Format PID."
  (let ((proc-info (process-attributes pid))
        (pid-s (number-to-string pid)))
    (cond ((and proced-enable-color-flag
                (not (file-remote-p default-directory))
                (equal pid (emacs-pid)))
           (propertize pid-s 'font-lock-face 'proced-emacs-pid))
          ((and proced-enable-color-flag (equal pid (alist-get 'sess proc-info)))
           (propertize pid-s 'font-lock-face 'proced-session-leader-pid))
          (proced-enable-color-flag
           (propertize pid-s 'font-lock-face 'proced-pid))
          (t pid-s))))

(defun proced-format-ppid (ppid)
  "Format PPID."
  (let ((ppid-s (number-to-string ppid)))
    (cond ((and proced-enable-color-flag
                (not (file-remote-p default-directory))
                (= ppid (emacs-pid)))
           (propertize ppid-s 'font-lock-face 'proced-emacs-pid))
          (proced-enable-color-flag
           (propertize ppid-s 'font-lock-face 'proced-ppid))
          (t ppid-s))))

(defun proced-format-pgrp (pgrp)
  "Format PGRP."
  (if proced-enable-color-flag
      (propertize (number-to-string pgrp) 'font-lock-face 'proced-pgrp)
    (number-to-string pgrp)))

(defun proced-format-sess (sess)
  "Format SESS."
  (if proced-enable-color-flag
      (propertize (number-to-string sess) 'font-lock-face 'proced-sess)
    (number-to-string sess)))

(defun proced-format-cpu (cpu)
  "Format CPU."
  (let ((formatted (format "%.1f" cpu)))
    (if proced-enable-color-flag
        (propertize formatted 'font-lock-face 'proced-cpu)
      formatted)))

(defun proced-format-mem (mem)
  "Format MEM."
  (let ((formatted (format "%.1f" mem)))
    (if proced-enable-color-flag
        (propertize formatted 'font-lock-face 'proced-mem)
      formatted)))

(defun proced-format-user (user)
  "Format USER."
  (if proced-enable-color-flag
      (propertize user 'font-lock-face 'proced-user)
    user))

(defun proced-format (process-alist format)
  "Display PROCESS-ALIST using FORMAT."
  (if (symbolp format)
      (setq format (cdr (assq format proced-format-alist))))

  ;; Not all systems give us all attributes.  We take `emacs-pid' as a
  ;; representative process PID.  If FORMAT contains a list of alternative
  ;; attributes, we take the first attribute that is non-nil for `emacs-pid'.
  ;; If none of the alternatives is non-nil, the attribute is ignored
  ;; in the listing.
  (let ((standard-attributes
         (car (proced-process-attributes (list-system-processes))))
        new-format fmi)
    (if (and proced-tree-flag
             (assq 'ppid standard-attributes))
        (push (cons 'tree 0) standard-attributes))
    (dolist (fmt format)
      (if (symbolp fmt)
          (if (assq fmt standard-attributes)
              (push fmt new-format))
        (while (setq fmi (pop fmt))
          (when (assq fmi standard-attributes)
            (push fmi new-format)
            (setq fmt nil)))))
    (setq format (nreverse new-format)))

  (insert (make-string (length process-alist) ?\n))
  (let ((whitespace " ") (unknown "?")
        (sort-key (if (consp proced-sort) (car proced-sort) proced-sort))
        header-list grammar)
    ;; Loop over all attributes
    (while (setq grammar (assq (pop format) proced-grammar-alist))
      (let* ((key (car grammar))
             (nth2grm (nth 2 grammar))
             (fun (cond ((stringp nth2grm)
                         (lambda (arg) (format nth2grm arg)))
                        ((not nth2grm) #'identity)
                        (t nth2grm)))
             (whitespace (if format whitespace ""))
             ;; Text properties:
             ;; We use the text property `proced-key' to store in each
             ;; field the corresponding key.
             ;; Of course, the sort predicate appearing in help-echo
             ;; is only part of the story.  But it gives the main idea.
             (hprops
              (if (nth 4 grammar)
                  (let ((descend (if (eq key sort-key) proced-descend (nth 5 grammar))))
                    `(proced-key ,key mouse-face header-line-highlight
                                 help-echo ,(format proced-header-help-echo
                                                    (if descend "-" "+")
                                                    (nth 1 grammar)
                                                    (if descend "descending" "ascending"))))))
             (refiner (nth 7 grammar))
             (fprops
              (cond ((functionp (car refiner))
                     `(proced-key ,key mouse-face highlight
                                  help-echo ,(format "mouse-2, RET: %s"
                                                     (nth 1 refiner))))
                    ((consp refiner)
                     `(proced-key ,key mouse-face highlight
                                  help-echo ,(format "mouse-2, RET: refine by attribute %s %s"
                                                     (nth 1 grammar)
                                                     (mapconcat (lambda (s)
                                                                  (if s "+" "-"))
                                                                refiner ""))))))
             value)

        ;; highlight the header of the sort column
        (if (eq key sort-key)
            (setq hprops (append '(face proced-sort-header) hprops)))
        (goto-char (point-min))
        (cond ( ;; fixed width of output field
               (numberp (nth 3 grammar))
               (dolist (process process-alist)
                 (end-of-line)
                 (setq value (cdr (assq key (cdr process))))
                 (insert (if value
                             (apply #'propertize (funcall fun value) fprops)
                           (format (concat "%" (number-to-string (nth 3 grammar)) "s")
                                   unknown))
                         whitespace)
                 (forward-line))
               (push (format (concat "%" (number-to-string (nth 3 grammar)) "s")
                             (apply #'propertize (nth 1 grammar) hprops))
                     header-list))

              ( ;; last field left-justified
               (and (not format) (eq 'left (nth 3 grammar)))
               (dolist (process process-alist)
                 (end-of-line)
                 (setq value (cdr (assq key (cdr process))))
                 (insert (if value (apply #'propertize (funcall fun value) fprops)
                           unknown))
                 (forward-line))
               (push (apply #'propertize (nth 1 grammar) hprops) header-list))

              (t ;; calculated field width
               (let ((width (length (nth 1 grammar)))
                     field-list value)
                 (dolist (process process-alist)
                   (setq value (cdr (assq key (cdr process))))
                   (if value
                       (setq value (apply #'propertize (funcall fun value) fprops)
                             width (max width (length value))
                             field-list (cons value field-list))
                     (push unknown field-list)
                     (setq width (max width (length unknown)))))
                 (let ((afmt (concat "%" (if (eq 'left (nth 3 grammar)) "-" "")
                                     (number-to-string width) "s")))
                   (push (format afmt (apply #'propertize (nth 1 grammar) hprops))
                         header-list)
                   (dolist (value (nreverse field-list))
                     (end-of-line)
                     (insert (format afmt value) whitespace)
                     (forward-line))))))))

    ;; final cleanup
    (goto-char (point-min))
    (dolist (process process-alist)
      ;; We use the text property `proced-pid' to store in each line
      ;; the corresponding pid
      (put-text-property (point) (line-end-position) 'proced-pid (car process))
      (forward-line))
    ;; Set header line
    (setq proced-header-line
          (mapconcat #'identity (nreverse header-list) whitespace))
    (if (string-match "[ \t]+$" proced-header-line)
        (setq proced-header-line (substring proced-header-line 0
                                            (match-beginning 0))))
    ;; (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun proced-format-interactive (scheme &optional revert)
  "Format Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no formatting.
Set variable `proced-format' to SCHEME.
With prefix REVERT non-nil revert listing."
  (interactive
   (let ((scheme (completing-read "Format: "
                                  proced-format-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           current-prefix-arg))
   proced-mode)
  ;; only update if necessary
  (when (or (not (eq proced-format scheme)) revert)
    (setq proced-format scheme)
    (proced-update revert)))

;; generate listing

(defun proced-process-attributes (&optional pid-list)
  "Return alist of attributes for each system process.
This alist can be customized via `proced-custom-attributes'.
Optional arg PID-LIST is a list of PIDs of system process that are analyzed.
If no attributes are known for a process (possibly because it already died)
the process is ignored."
  ;; Should we make it customizable whether processes with empty attribute
  ;; lists are ignored?  When would such processes be of interest?
  (let (process-alist attributes attr)
    (dolist (pid (or pid-list (list-system-processes)) process-alist)
      (when (setq attributes (process-attributes pid))
        (setq attributes (cons (cons 'pid pid) attributes))
        (dolist (fun proced-custom-attributes)
          (if (setq attr (funcall fun attributes))
              (push attr attributes)))
        (push (cons pid attributes) process-alist)))))

(defun proced-update (&optional revert quiet)
  "Update the Proced process information.  Preserves point and marks.
With prefix REVERT non-nil, revert listing.
Suppress status information if QUIET is nil.
After updating a displayed Proced buffer run the normal hook
`proced-post-display-hook'."
  ;; This is the main function that generates and updates the process listing.
  (interactive "P" proced-mode)
  (setq revert (or revert (not proced-process-alist)))
  (or quiet (message (if revert "Updating process information..."
                       "Updating process display...")))
  (if revert ;; evaluate all processes
      (setq proced-process-alist (proced-process-attributes)))
  ;; filtering
  (setq proced-process-alist (proced-filter proced-process-alist proced-filter))
  ;; refinements
  (pcase-dolist (`(,refiner ,pid ,key ,grammar) proced-refinements)
    ;; It's possible the process has exited since the refinement was made
    (when (assq pid proced-process-alist)
      (cond ((functionp (car refiner))
             (setq proced-process-alist (funcall (car refiner) pid)))
            ((consp refiner)
             (let ((predicate (nth 4 grammar))
                   (ref (cdr (assq key (cdr (assq pid proced-process-alist)))))
                   val new-alist)
               (dolist (process proced-process-alist)
                 (setq val (funcall predicate (cdr (assq key (cdr process))) ref))
                 (when (cond ((not val) (nth 2 refiner))
                             ((eq val 'equal) (nth 1 refiner))
                             (val (car refiner)))
                   (push process new-alist)))
               (setq proced-process-alist new-alist))))))

  ;; sorting
  (setq proced-process-alist
        (proced-sort proced-process-alist proced-sort proced-descend))

  ;; display as process tree?
  (setq proced-process-alist
        (proced-tree proced-process-alist))

  ;; It is useless to keep undo information if we revert, filter, or
  ;; refine the listing so that `proced-process-alist' has changed.
  ;; We could keep the undo information if we only re-sort the buffer.
  ;; Would that be useful?  Re-re-sorting is easy, too.
  (if (consp buffer-undo-list)
      (setq buffer-undo-list nil))
  (let ((buffer-undo-list t)
        (window-pos-infos
         (mapcar (lambda (w) `(,w . ,(proced--position-info (window-point w))))
                 (get-buffer-window-list (current-buffer) nil t)))
        (old-pos (proced--position-info (point)))
        buffer-read-only mp-list)
    ;; remember marked processes (whatever the mark was)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\S-\\)" nil t)
      (push (cons (save-match-data (proced-pid-at-point))
                  (match-string-no-properties 1)) mp-list))

    ;; generate listing
    (erase-buffer)
    (proced-format proced-process-alist proced-format)
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line))
    (setq proced-header-line (concat "  " proced-header-line))
    (if revert (set-buffer-modified-p nil))

    ;; set `goal-column'
    (let ((grammar (assq proced-goal-attribute proced-grammar-alist)))
      (setq goal-column ;; set to nil if no match
            (if (and grammar
                     (not (zerop (buffer-size)))
                     (string-match (regexp-quote (nth 1 grammar))
                                   proced-header-line))
                (if (nth 3 grammar)
                    (match-beginning 0)
                  (match-end 0)))))

    ;; Restore process marks and buffer position (if possible).
    ;; Sometimes this puts point in the middle of the proced buffer
    ;; where it is not interesting.  Is there a better / more flexible solution?
    (goto-char (point-min))

    (let (pid mark new-pos win-points)
      (if (or mp-list (car old-pos))
          (while (not (eobp))
            (setq pid (proced-pid-at-point))
            (when (setq mark (assq pid mp-list))
              (insert (cdr mark))
              (delete-char 1)
              (beginning-of-line))
            (when (eq (car old-pos) pid)
              (setq new-pos (proced--determine-pos (nth 1 old-pos)
                                                   (nth 2 old-pos))))
            (mapc (lambda (w-pos)
                    (when (eq (cadr w-pos) pid)
                      (push `(,(car w-pos) . ,(proced--determine-pos
                                               (nth 1 (cdr w-pos))
                                               (nth 2 (cdr w-pos))))
                            win-points)))
                  window-pos-infos)
            (forward-line)))
      (let ((fallback (save-excursion (goto-char (point-min))
                                      (proced-move-to-goal-column)
                                      (point))))
        (goto-char (or new-pos fallback))
        ;; Update window points
        (mapc (lambda (w-pos)
                (set-window-point (car w-pos)
                                  (alist-get (car w-pos) win-points fallback)))
              window-pos-infos)))
    ;; update mode line
    ;; Does the long `mode-name' clutter the mode line?  It would be nice
    ;; to have some other location for displaying the values of the various
    ;; flags that affect the behavior of proced (flags one might want
    ;; to change on the fly).  Where??
    (setq mode-name
          (concat "Proced"
                  (if proced-filter
                      (format ": %S" proced-filter)
                    "")
                  (if proced-sort
                      (let* ((key (if (consp proced-sort) (car proced-sort)
                                    proced-sort))
                             (grammar (assq key proced-grammar-alist)))
                        (concat " by " (if proced-descend "-" "+")
                                (nth 1 grammar)))
                    "")))
    (force-mode-line-update)
    ;; run `proced-post-display-hook' only for a displayed buffer.
    (if (get-buffer-window) (run-hooks 'proced-post-display-hook))
    ;; done
    (or quiet (input-pending-p)
        (message (if revert "Updating process information...done."
                   "Updating process display...done.")))))

(defun proced-revert (&rest _args)
  "Reevaluate the process listing based on the currently running processes.
Preserves point and marks, but not refinements (see `proced-refine' for
information on refinements)."
  (setq proced-refinements nil)
  (proced-update t))

(defun proced-marked-processes ()
  "Return marked processes as alist of PIDs.
If no process is marked return alist with the PID of the process point is on.
The cdrs of the alist are the text strings displayed by Proced for these
processes.  They are used for error messages."
  (let ((regexp (proced-marker-regexp))
        process-alist)
    ;; collect marked processes
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (proced-pid-at-point)
                    ;; How much info should we collect here?
                    (buffer-substring-no-properties
                     (+ 2 (line-beginning-position))
                     (line-end-position)))
              process-alist)))
    (if process-alist
        (nreverse process-alist)
      ;; take current process
      (let ((pid (proced-pid-at-point)))
        (if pid
            (list (cons pid
                        (buffer-substring-no-properties
                         (+ 2 (line-beginning-position))
                         (line-end-position)))))))))

(defmacro proced-with-processes-buffer (process-alist &rest body)
  "Execute the forms in BODY in a temporary buffer displaying PROCESS-ALIST.
PROCESS-ALIST is an alist of process PIDs as in `proced-process-alist'.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  ;; Use leading space in buffer name to make this buffer ephemeral
  `(let ((bufname  " *Marked Processes*")
         (header-line (substring-no-properties proced-header-line)))
     (with-current-buffer (get-buffer-create bufname)
       (setq truncate-lines t
             proced-header-line header-line ; inherit header line
             header-line-format '(:eval (proced-header-line)))
       (add-hook 'post-command-hook #'force-mode-line-update nil t) ;FIXME: Why?
       (let ((inhibit-read-only t))
         (erase-buffer)
         (buffer-disable-undo)
         (setq buffer-read-only t)
         (dolist (process ,process-alist)
           (insert "  " (cdr process) "\n"))
         (delete-char -1)
         (goto-char (point-min)))
       (save-window-excursion
         ;; Analogous to `dired-pop-to-buffer'
         ;; Don't split window horizontally.  (Bug#1806)
         ;; FIXME: `dired-pop-to-buffer' was removed and replaced with
         ;;        `dired-mark-pop-up'.  Should we just use
         ;;        `pop-to-buffer' here also?
         (display-buffer (current-buffer)
                         '(display-buffer-in-direction
                           (direction . bottom)
                           (window-height . fit-window-to-buffer)))
         ,@body))))

(defun proced--read-signal (count)
  "Read a SIGNAL via `completing-read' for COUNT processes."
  (completing-read
   (format-prompt "Send signal [%s]"
                  "TERM"
                  (if (= 1 count)
                      "1 process"
                    (format "%d processes" count)))
   (completion-table-with-metadata
    (completion-table-case-fold proced-signal-list)
    `((annotation-function
       . ,(lambda (s) (cdr (assoc s proced-signal-list))))))
   nil nil nil nil "TERM"))

(defun proced-send-signal (&optional signal process-alist)
  "Send a SIGNAL to processes in PROCESS-ALIST.
PROCESS-ALIST is an alist as returned by `proced-marked-processes'.
Interactively, PROCESS-ALIST contains the marked processes.
If no process is marked, it contains the process point is on,
SIGNAL may be a string (HUP, INT, TERM, etc.) or a number.
After sending SIGNAL to all processes in PROCESS-ALIST, this command
runs the normal hook `proced-after-send-signal-hook'.

For backward compatibility SIGNAL and PROCESS-ALIST may be nil.
Then PROCESS-ALIST contains the marked processes or the process point is on
and SIGNAL is queried interactively.  This noninteractive usage is still
supported but discouraged.  It will be removed in a future version of Emacs."
  (interactive
   (let ((process-alist (proced-marked-processes)))
     (proced-with-processes-buffer
         process-alist
       (list (proced--read-signal (length process-alist)) process-alist)))
   proced-mode)

  (unless (and signal process-alist)
    ;; Discouraged usage (supported for backward compatibility):
    ;; The new calling sequence separates more cleanly between the parts
    ;; of the code required for interactive and noninteractive calls so that
    ;; the command can be used more flexibly in noninteractive ways, too.
    (unless (get 'proced-send-signal 'proced-outdated)
       (put 'proced-send-signal 'proced-outdated t)
       (message "Outdated usage of `proced-send-signal'")
       (sit-for 2))
    (setq process-alist (proced-marked-processes))
    (unless signal
      (proced-with-processes-buffer
          process-alist
        (setq signal (proced--read-signal (length process-alist))))))

  (let (failures)
    ;; Why not always use `signal-process'?  See
    ;; https://lists.gnu.org/r/emacs-devel/2008-03/msg02955.html
    (if (functionp proced-signal-function)
        ;; use built-in `signal-process'
        (let ((signal (if (stringp signal)
                          (if (string-match "\\`[0-9]+\\'" signal)
                              (string-to-number signal)
                            (make-symbol signal))
                        signal)))   ; number
          (dolist (process process-alist)
            (condition-case err
                (unless (zerop (funcall
                                proced-signal-function (car process) signal
                                (file-remote-p default-directory)))
                  (proced-log "%s\n" (cdr process))
                  (push (cdr process) failures))
              (error ; catch errors from failed signals
               (proced-log "%s\n" err)
               (proced-log "%s\n" (cdr process))
               (push (cdr process) failures)))))
      ;; use external system call
      (let ((signal (format "-%s" signal)))
        (dolist (process process-alist)
          (with-temp-buffer
            (condition-case nil
                (unless (zerop (process-file
                                proced-signal-function nil t nil
                                signal (number-to-string (car process))))
                  (proced-log (current-buffer))
                  (proced-log "%s\n" (cdr process))
                  (push (cdr process) failures))
              (error ; catch errors from failed signals
               (proced-log (current-buffer))
               (proced-log "%s\n" (cdr process))
               (push (cdr process) failures)))))))
    (if failures
        ;; Proced error message are not always very precise.
        ;; Can we issue a useful one-line summary in the
        ;; message area (using FAILURES) if only one signal failed?
        (proced-log-summary
         (format "Signal %s" signal)
         (format "%d of %d signal%s failed"
                 (length failures) (length process-alist)
                 (if (= 1 (length process-alist)) "" "s")))
      (proced-success-message "Sent signal to" (length process-alist))))
  ;; final clean-up
  (run-hooks 'proced-after-send-signal-hook))

(defun proced-renice (priority process-alist)
  "Renice the processes in PROCESS-ALIST to PRIORITY.
PROCESS-ALIST is an alist as returned by `proced-marked-processes'.
Interactively, PROCESS-ALIST contains the marked processes.
If no process is marked, it contains the process point is on,
After renicing all processes in PROCESS-ALIST, this command runs
the normal hook `proced-after-send-signal-hook'."
  (interactive
   (let ((process-alist (proced-marked-processes)))
     (proced-with-processes-buffer process-alist
       (list (read-number "New priority: ")
             process-alist)))
   proced-mode)
  (if (numberp priority)
      (setq priority (number-to-string priority)))
  (let (failures)
    (dolist (process process-alist)
      (with-temp-buffer
        (condition-case nil
            (unless (zerop (process-file
                            proced-renice-command nil t nil
                            priority (number-to-string (car process))))
              (proced-log (current-buffer))
              (proced-log "%s\n" (cdr process))
              (push (cdr process) failures))
          (error ; catch errors from failed renice
           (proced-log (current-buffer))
           (proced-log "%s\n" (cdr process))
           (push (cdr process) failures)))))
    (if failures
        (proced-log-summary
         (format "Renice %s" priority)
         (format "%d of %d renice%s failed"
                 (length failures) (length process-alist)
                 (if (= 1 (length process-alist)) "" "s")))
      (proced-success-message "Reniced" (length process-alist))))
  ;; final clean-up
  (run-hooks 'proced-after-send-signal-hook))

;; similar to `dired-why'
(defun proced-why ()
  "Pop up a buffer with error log output from Proced.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive nil proced-mode)
  (if (get-buffer proced-log-buffer)
      (save-selected-window
        ;; move `proced-log-buffer' to the front of the buffer list
        (select-window (display-buffer (get-buffer proced-log-buffer)))
        (setq truncate-lines t)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (goto-char (point-max))
        (forward-line -1)
        (backward-page 1)
        (recenter 0))))

;; similar to `dired-log'
(defun proced-log (log &rest args)
  "Log a message or the contents of a buffer.
If LOG is a string and there are more args, it is formatted with
those ARGS.  Usually the LOG string ends with a \\n.
End each bunch of errors with (proced-log t signal):
this inserts the current time, buffer and signal at the start of the page,
and \\f (formfeed) at the end."
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create proced-log-buffer)
      (goto-char (point-max))
      (let (buffer-read-only)
	(cond ((stringp log)
	       (insert (if args
			   (apply #'format-message log args)
			 log)))
	      ((bufferp log)
	       (insert-buffer-substring log))
	      ((eq t log)
	       (backward-page 1)
	       (unless (bolp)
		 (insert "\n"))
	       (insert (current-time-string)
		       (format-message "\tBuffer `%s', signal `%s'\n"
				       (buffer-name obuf) (car args)))
	       (goto-char (point-max))
	       (insert "\f\n")))))))

;; similar to `dired-log-summary'
(defun proced-log-summary (signal string)
  "State a summary of SIGNAL's failures, in echo area and log buffer.
STRING is an overall summary of the failures."
  (message "Signal %s: %s--type ? for details" signal string)
  ;; Log a summary describing a bunch of errors.
  (proced-log (concat "\n" string "\n"))
  (proced-log t signal))

(defun proced-help ()
  "Provide help for the Proced user."
  (interactive nil proced-mode)
  (proced-why)
  (if (eq last-command 'proced-help)
      (describe-mode)
    (message (substitute-command-keys proced-help-string))))

(defun proced-undo ()
  "Undo in a Proced buffer.
This doesn't recover killed processes, it just undoes changes in the Proced
buffer.  You can use it to recover marks."
  (interactive nil proced-mode)
  (let (buffer-read-only)
    (undo))
  (message "Change in Proced buffer undone.
Killed processes cannot be recovered by Emacs."))

(provide 'proced)

;;; proced.el ends here
