;;; subr-x.el --- extra Lisp functions  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Free Software Foundation, Inc.

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

;; Less commonly used functions that complement basic APIs, often implemented in
;; C code (like hash-tables and strings), and are not eligible for inclusion
;; in subr.el.

;; Do not document these functions in the lispref.
;; https://lists.gnu.org/r/emacs-devel/2014-01/msg01006.html

;; NB If you want to use this library, it's almost always correct to use:
;; (eval-when-compile (require 'subr-x))

;;; Code:

(eval-when-compile (require 'cl-lib))


(defmacro internal--thread-argument (first? &rest forms)
  "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
  (pcase forms
    (`(,x (,f . ,args) . ,rest)
     `(internal--thread-argument
       ,first? ,(if first? `(,f ,x ,@args) `(,f ,@args ,x)) ,@rest))
    (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
    (_ (car forms))))

(defmacro thread-first (&rest forms)
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  (declare (indent 0)
           (debug (form &rest [&or symbolp (sexp &rest form)])))
  `(internal--thread-argument t ,@forms))

(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (declare (indent 0) (debug thread-first))
  `(internal--thread-argument nil ,@forms))

(defsubst hash-table-empty-p (hash-table)
  "Check whether HASH-TABLE is empty (has 0 elements)."
  (declare (side-effect-free t))
  (zerop (hash-table-count hash-table)))

(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (declare (side-effect-free t))
  (let ((keys nil))
    (maphash (lambda (k _) (push k keys)) hash-table)
    keys))

(defsubst hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  (declare (side-effect-free t))
  (let ((values nil))
    (maphash (lambda (_ v) (push v values)) hash-table)
    values))

;;;###autoload
(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR.
Optional argument SEPARATOR must be a string, a vector, or a list of
characters; nil stands for the empty string."
  (declare (pure t) (side-effect-free t))
  (mapconcat #'identity strings separator))

(define-obsolete-function-alias 'string-reverse 'reverse "25.1")

;;;###autoload
(defun string-truncate-left (string length)
  "If STRING is longer than LENGTH, return a truncated version.
When truncating, \"...\" is always prepended to the string, so
the resulting string may be longer than the original if LENGTH is
3 or smaller."
  (declare (pure t) (side-effect-free t))
  (let ((strlen (length string)))
    (if (<= strlen length)
	string
      (setq length (max 0 (- length 3)))
      (concat "..." (substring string (min (1- strlen)
                                           (max 0 (- strlen length))))))))

;;;###autoload
(defsubst string-blank-p (string)
  "Check whether STRING is either empty or only whitespace.
The following characters count as whitespace here: space, tab, newline and
carriage return."
  (declare (pure t) (side-effect-free t))
  (string-match-p "\\`[ \t\n\r]*\\'" string))

(defsubst string-remove-prefix (prefix string)
  "Remove PREFIX from STRING if present."
  (declare (pure t) (side-effect-free t))
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(defsubst string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if present."
  (declare (pure t) (side-effect-free t))
  (if (string-suffix-p suffix string)
      (substring string 0 (- (length string) (length suffix)))
    string))

;;;###autoload
(defun string-clean-whitespace (string)
  "Clean up whitespace in STRING.
All sequences of whitespaces in STRING are collapsed into a
single space character, and leading/trailing whitespace is
removed."
  (declare (important-return-value t))
  (let ((blank "[[:blank:]\r\n]+"))
    (string-trim (replace-regexp-in-string blank " " string t t)
                 blank blank)))

;;;###autoload
(defun string-fill (string width)
  "Try to word-wrap STRING so that it displays with lines no wider than WIDTH.
STRING is wrapped where there is whitespace in it.  If there are
individual words in STRING that are wider than WIDTH, the result
will have lines that are wider than WIDTH."
  (declare (important-return-value t))
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((fill-column width)
          (adaptive-fill-mode nil))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

;;;###autoload
(defun string-limit (string length &optional end coding-system)
  "Return a substring of STRING that is (up to) LENGTH characters long.
If STRING is shorter than or equal to LENGTH characters, return the
entire string unchanged.

If STRING is longer than LENGTH characters, return a substring
consisting of the first LENGTH characters of STRING.  If END is
non-nil, return the last LENGTH characters instead.

If CODING-SYSTEM is non-nil, STRING will be encoded before
limiting, and LENGTH is interpreted as the number of bytes to
limit the string to.  The result will be a unibyte string that is
shorter than LENGTH, but will not contain \"partial\"
characters (or glyphs), even if CODING-SYSTEM encodes characters
with several bytes per character.  If the coding system specifies
prefix like the byte order mark (aka \"BOM\") or a shift-in sequence,
their bytes will be normally counted as part of LENGTH.  This is
the case, for instance, with `utf-16'.  If this isn't desired, use a
coding system that doesn't specify a BOM, like `utf-16le' or `utf-16be'.

When shortening strings for display purposes,
`truncate-string-to-width' is almost always a better alternative
than this function."
  (declare (important-return-value t))
  (unless (natnump length)
    (signal 'wrong-type-argument (list 'natnump length)))
  (if coding-system
      ;; The previous implementation here tried to encode char by
      ;; char, and then adding up the length of the encoded octets,
      ;; but that's not reliably in the presence of BOM marks and
      ;; ISO-2022-CN which may add charset designations at the
      ;; start/end of each encoded char (which we don't want).  So
      ;; iterate (with a binary search) instead to find the desired
      ;; length.
      (let* ((glyphs (string-glyph-split string))
             (nglyphs (length glyphs))
             (too-long (1+ nglyphs))
             (stop (max (/ nglyphs 2) 1))
             (gap stop)
             candidate encoded found candidate-stop)
        ;; We're returning the end of the string.
        (when end
          (setq glyphs (nreverse glyphs)))
        (while (and (not found)
                    (< stop too-long))
          (setq encoded
                (encode-coding-string (string-join (seq-take glyphs stop))
                                      coding-system))
          (cond
           ((= (length encoded) length)
            (setq found encoded
                  candidate-stop stop))
           ;; Too long; try shortening.
           ((> (length encoded) length)
            (setq too-long stop
                  stop (max (- stop gap) 1)))
           ;; Too short; try lengthening.
           (t
            (setq candidate encoded
                  candidate-stop stop)
            (setq stop
                  (if (>= stop nglyphs)
                      too-long
                    (min (+ stop gap) nglyphs)))))
          (setq gap (max (/ gap 2) 1)))
        (cond
         ((not (or found candidate))
          "")
         ;; We're returning the end, so redo the encoding.
         (end
          (encode-coding-string
           (string-join (nreverse (seq-take glyphs candidate-stop)))
           coding-system))
         (t
          (or found candidate))))
    ;; Char-based version.
    (cond
     ((<= (length string) length) string)
     (end (substring string (- (length string) length)))
     (t (substring string 0 length)))))

;;;###autoload
(defun string-pad (string length &optional padding start)
  "Pad STRING to LENGTH using PADDING.
If PADDING is nil, the space character is used.  If not nil, it
should be a character.

If STRING is longer than the absolute value of LENGTH, no padding
is done.

If START is nil (or not present), the padding is done to the end
of the string, and if non-nil, padding is done to the start of
the string."
  (declare (pure t) (side-effect-free t))
  (unless (natnump length)
    (signal 'wrong-type-argument (list 'natnump length)))
  (let ((pad-length (- length (length string))))
    (cond ((<= pad-length 0) string)
          (start (concat (make-string pad-length (or padding ?\s)) string))
          (t (concat string (make-string pad-length (or padding ?\s)))))))

;;;###autoload
(defun string-chop-newline (string)
  "Remove the final newline (if any) from STRING."
  (declare (pure t) (side-effect-free t))
  (string-remove-suffix "\n" string))

;;;###autoload
(defmacro named-let (name bindings &rest body)
  "Looping construct taken from Scheme.
Like `let', bind variables in BINDINGS and then evaluate BODY,
but with the twist that BODY can evaluate itself recursively by
calling NAME, where the arguments passed to NAME are used
as the new values of the bound variables in the recursive invocation.

This construct can only be used with lexical binding."
  (declare (indent 2) (debug (symbolp (&rest (symbolp form)) body)))
  (require 'cl-lib)
  (unless lexical-binding
    (error "`named-let' requires lexical binding"))
  (let ((fargs (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
        (aargs (mapcar (lambda (b) (if (consp b) (cadr b))) bindings)))
    ;; According to the Scheme semantics of named let, `name' is not in scope
    ;; while evaluating the expressions in `bindings', and for this reason, the
    ;; "initial" function call below needs to be outside of the `cl-labels'.
    ;; When the "self-tco" eliminates all recursive calls, the `cl-labels'
    ;; expands to a lambda which the byte-compiler then combines with the
    ;; funcall to make a `let' so we end up with a plain `while' loop and no
    ;; remaining `lambda' at all.
    `(funcall
      (cl-labels ((,name ,fargs . ,body)) #',name)
      . ,aargs)))

(defvar work-buffer--list nil)
(defvar work-buffer-limit 10
  "Maximum number of reusable work buffers.
When this limit is exceeded, newly allocated work buffers are
automatically killed, which means that in a such case
`with-work-buffer' becomes equivalent to `with-temp-buffer'.")

(defsubst work-buffer--get ()
  "Get a work buffer."
  (let ((buffer (pop work-buffer--list)))
    (if (buffer-live-p buffer)
        buffer
      (generate-new-buffer " *work*" t))))

(defun work-buffer--release (buffer)
  "Release work BUFFER."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Flush BUFFER before making it available again, i.e. clear
        ;; its contents, remove all overlays and buffer-local
        ;; variables.  Is it enough to safely reuse the buffer?
        (let ((inhibit-read-only t)
              ;; Avoid deactivating the region as side effect.
              deactivate-mark)
          (erase-buffer))
        (delete-all-overlays)
        (let (change-major-mode-hook)
          (kill-all-local-variables t))
        ;; Make the buffer available again.
        (push buffer work-buffer--list)))
  ;; If the maximum number of reusable work buffers is exceeded, kill
  ;; work buffer in excess, taking into account that the limit could
  ;; have been let-bound to temporarily increase its value.
  (when (> (length work-buffer--list) work-buffer-limit)
    (mapc #'kill-buffer (nthcdr work-buffer-limit work-buffer--list))
    (setq work-buffer--list (ntake work-buffer-limit work-buffer--list))))

;;;###autoload
(defmacro with-work-buffer (&rest body)
  "Create a work buffer, and evaluate BODY there like `progn'.
Like `with-temp-buffer', but reuse an already created temporary
buffer when possible, instead of creating a new one on each call."
  (declare (indent 0) (debug t))
  (let ((work-buffer (make-symbol "work-buffer")))
    `(let ((,work-buffer (work-buffer--get)))
       (with-current-buffer ,work-buffer
         (unwind-protect
             (progn ,@body)
           (work-buffer--release ,work-buffer))))))

;;;###autoload
(defun string-pixel-width (string &optional buffer)
  "Return the width of STRING in pixels.
If BUFFER is non-nil, use the face remappings, alternative and default
properties from that buffer when determining the width.
If you call this function to measure pixel width of a string
with embedded newlines, it returns the width of the widest
substring that does not include newlines."
  (declare (important-return-value t))
  (if (zerop (length string))
      0
    ;; Keeping a work buffer around is more efficient than creating a
    ;; new temporary buffer.
    (with-work-buffer
      ;; Setup current buffer to correctly compute pixel width.
      (when buffer
        (dolist (v '(face-remapping-alist
                     char-property-alias-alist
                     default-text-properties))
          (if (local-variable-p v buffer)
              (set (make-local-variable v)
                   (buffer-local-value v buffer)))))
      ;; Avoid deactivating the region as side effect.
      (let (deactivate-mark)
        (insert string))
      ;; If `display-line-numbers' is enabled in internal
      ;; buffers (e.g. globally), it breaks width calculation
      ;; (bug#59311).  Disable `line-prefix' and `wrap-prefix',
      ;; for the same reason.
      (add-text-properties
       (point-min) (point-max)
       '(display-line-numbers-disable t line-prefix "" wrap-prefix ""))
      (car (buffer-text-pixel-size nil nil t)))))

;;;###autoload
(defun string-glyph-split (string)
  "Split STRING into a list of strings representing separate glyphs.
This takes into account combining characters and grapheme clusters:
if compositions are enabled, each sequence of characters composed
on display into a single grapheme cluster is treated as a single
indivisible unit."
  (declare (side-effect-free t))
  (let ((result nil)
        (start 0)
        comp)
    (while (< start (length string))
      (if (setq comp (find-composition-internal
                      start
                      ;; Don't search backward in the string for the
                      ;; start of the composition.
                      (min (length string) (1+ start))
                      string nil))
          (progn
            (push (substring string (car comp) (cadr comp)) result)
            (setq start (cadr comp)))
        (push (substring string start (1+ start)) result)
        (setq start (1+ start))))
    (nreverse result)))

(defun add-remove--display-text-property (start end spec value
                                                &optional object remove)
  (let ((sub-start start)
        (sub-end 0)
        (limit (if (stringp object)
                   (min (length object) end)
                 (min end (point-max))))
        disp)
    (while (< sub-end end)
      (setq sub-end (next-single-property-change sub-start 'display object
                                                 limit))
      (if (not (setq disp (get-text-property sub-start 'display object)))
          ;; No old properties in this range.
          (unless remove
            (put-text-property sub-start sub-end 'display (list spec value)
                               object))
        ;; We have old properties.
        (let ((changed nil)
              type)
          ;; Make disp into a list.
          (setq disp
                (cond
                 ((vectorp disp)
                  (setq type 'vector)
                  (seq-into disp 'list))
                 ((or (not (consp (car-safe disp)))
                      ;; If disp looks like ((margin ...) ...), that's
                      ;; still a single display specification.
                      (eq (caar disp) 'margin))
                  (setq type 'scalar)
                  (list disp))
                 (t
                  (setq type 'list)
                  disp)))
          ;; Remove any old instances.
          (when-let* ((old (assoc spec disp)))
            ;; If the property value was a list, don't modify the
            ;; original value in place; it could be used by other
            ;; regions of text.
            (setq disp (if (eq type 'list)
                           (remove old disp)
                         (delete old disp))
                  changed t))
          (unless remove
            (setq disp (cons (list spec value) disp)
                  changed t))
          (when changed
            (if (not disp)
                (remove-text-properties sub-start sub-end '(display nil) object)
              (when (eq type 'vector)
                (setq disp (seq-into disp 'vector)))
              ;; Finally update the range.
              (put-text-property sub-start sub-end 'display disp object)))))
      (setq sub-start sub-end))))

;;;###autoload
(defun add-display-text-property (start end spec value &optional object)
  "Add the display specification (SPEC VALUE) to the text from START to END.
If any text in the region has a non-nil `display' property, the existing
display specifications are retained.

OBJECT is either a string or a buffer to add the specification to.
If omitted, OBJECT defaults to the current buffer."
  (add-remove--display-text-property start end spec value object))

;;;###autoload
(defun remove-display-text-property (start end spec &optional object)
  "Remove the display specification SPEC from the text from START to END.
SPEC is the car of the display specification to remove, e.g. `height'.
If any text in the region has other display specifications, those specs
are retained.

OBJECT is either a string or a buffer to remove the specification from.
If omitted, OBJECT defaults to the current buffer."
  (add-remove--display-text-property start end spec nil object 'remove))

;;;###autoload
(defun read-process-name (prompt)
  "Query the user for a process and return the process object."
  ;; Currently supports only the PROCESS argument.
  ;; Must either return a list containing a process, or signal an error.
  ;; (Returning nil would mean the current buffer's process.)
  (unless (fboundp 'process-list)
    (error "Asynchronous subprocesses are not supported on this system"))
  ;; Local function to return cons of a complete-able name, and the
  ;; associated process object, for use with `completing-read'.
  (cl-flet ((procitem
             (p) (when (process-live-p p)
                   (let ((pid (process-id p))
                         (procname (process-name p))
                         (procbuf (process-buffer p)))
                     (and (eq (process-type p) 'real)
                          (cons (if procbuf
                                    (format "%s (%s) in buffer %s"
                                            procname pid
                                            (buffer-name procbuf))
                                  (format "%s (%s)" procname pid))
                                p))))))
    ;; Perform `completing-read' for a process.
    (let* ((currproc (get-buffer-process (current-buffer)))
           (proclist (or (process-list)
                         (error "No processes found")))
           (collection (delq nil (mapcar #'procitem proclist)))
           (selection (completing-read
                       (format-prompt prompt
                                      (and currproc
                                           (eq (process-type currproc) 'real)
                                           (procitem currproc)))
                       collection nil :require-match nil nil
                       (car (seq-find (lambda (proc)
                                        (eq currproc (cdr proc)))
                                      collection))))
           (process (and selection
                         (cdr (assoc selection collection)))))
      (unless process
        (error "No process selected"))
      process)))

(defmacro with-buffer-unmodified-if-unchanged (&rest body)
  "Like `progn', but change buffer-modified status only if buffer text changes.
If the buffer was unmodified before execution of BODY, and
buffer text after execution of BODY is identical to what it was
before, ensure that buffer is still marked unmodified afterwards.
For example, the following won't change the buffer's modification
status:

  (with-buffer-unmodified-if-unchanged
    (insert \"a\")
    (delete-char -1))

Note that only changes in the raw byte sequence of the buffer text,
as stored in the internal representation, are monitored for the
purpose of detecting the lack of changes in buffer text.  Any other
changes that are normally perceived as \"buffer modifications\", such
as changes in text properties, `buffer-file-coding-system', buffer
multibyteness, etc. -- will not be noticed, and the buffer will still
be marked unmodified, effectively ignoring those changes."
  (declare (debug t) (indent 0))
  (cl-with-gensyms (hash buffer)
    `(let ((,hash (and (not (buffer-modified-p))
                       (buffer-hash)))
           (,buffer (current-buffer)))
       (prog1
           (progn
             ,@body)
         ;; If we didn't change anything in the buffer (and the buffer
         ;; was previously unmodified), then flip the modification status
         ;; back to "unchanged".
         (when (and ,hash (buffer-live-p ,buffer))
           (with-current-buffer ,buffer
             (when (and (buffer-modified-p)
                        (equal ,hash (buffer-hash)))
               (restore-buffer-modified-p nil))))))))

(defun emacs-etc--hide-local-variables ()
  "Hide local variables.
Used by `emacs-authors-mode' and `emacs-news-mode'."
  (narrow-to-region (point-min)
                    (save-excursion
                      (goto-char (point-max))
                      ;; Obfuscate to avoid this being interpreted
                      ;; as a local variable section itself.
                      (if (re-search-backward "^Local\sVariables:$" nil t)
                          (progn (forward-line -1) (point))
                        (point-max)))))

(provide 'subr-x)

;;; subr-x.el ends here
