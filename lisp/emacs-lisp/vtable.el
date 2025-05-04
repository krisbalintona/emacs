;;; vtable.el --- Displaying data in tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'text-property-search)
(require 'mule-util)

(defface vtable
  '((t :inherit variable-pitch))
  "Face used (by default) for vtable bodies."
  :version "29.1"
  :group 'faces)

(defface vtable-header
  '((t :inherit (header-line vtable)))
  "Face used (by default) for vtable headers."
  :version "31.1"
  :group 'faces)

(defface vtable-marked
  '((t :inherit (region vtable)))
  "Face used (by default) for marked vtable objects."
  :version "31.1"
  :group 'faces)

(defface vtable-sort-indicator-ascend
  '((t :inherit vtable-header))
  "Face used (by default) for vtable ascend sort indicator."
  :version "31.1"
  :group 'faces)

(defface vtable-sort-indicator-descend
  '((t :inherit vtable-header))
  "Face used (by default) for vtable descend sort indicator."
  :version "31.1"
  :group 'faces)

(defvar vtable-sort-indicator-default '((?▼ ?v)
                                        (?▲ ?^))
  "Default descending and ascending sort indicators.
The form is a list of two conses of two characters.  The first set indicates
sorting descending, the second ascending.  The first character in each cons
is for fonts that can display symbols, and the second is plain text.")

(cl-defstruct vtable-column
  "A vtable column."
  name
  width
  infer-width ; nil or 'data uses data, 'data+name includes column name
  min-width
  max-width
  truncate-guess-tolerance
  primary
  align
  getter
  formatter
  displayer
  comparator
  -numerical
  -aligned)

(defclass vtable ()
  ((columns :initarg :columns :accessor vtable-columns)
   (objects :initarg :objects :accessor vtable-objects)
   (objects-function :initarg :objects-function
                     :accessor vtable-objects-function)
   (getter :initarg :getter :accessor vtable-getter)
   (formatter :initarg :formatter :accessor vtable-formatter)
   (displayer :initarg :displayer :accessor vtable-displayer)
   (use-header-line :initarg :use-header-line
                    :accessor vtable-use-header-line)
   (text-scale :initarg :text-scale :accessor vtable-text-scale)
   (text-scale-header :initarg :text-scale-header
                      :accessor vtable-text-scale-header)
   (object-equal :initarg :object-equal
                 :accessor vtable-object-equal :initform #'eq)
   (face :initarg :face :accessor vtable-face)
   (header-face :initarg :header-face :accessor vtable-header-face)
   (marked-face :initarg :marked-face :accessor vtable-marked-face)
   (actions :initarg :actions :accessor vtable-actions)
   (keymap :initarg :keymap :accessor vtable-keymap)
   (separator-width :initarg :separator-width :accessor vtable-separator-width)
   (divider :initarg :divider :accessor vtable-divider :initform nil)
   (divider-on-header :initarg :divider-on-header
                      :accessor vtable-divider-on-header :initform nil)
   (sort-by :initarg :sort-by :accessor vtable-sort-by)
   (sort-indicator :initarg :sort-indicator :accessor vtable-sort-indicator)
   (sort-indicator-face-ascend :initarg :sort-indicator-face-ascend
                               :accessor vtable-sort-indicator-face-ascend)
   (sort-indicator-face-descend :initarg :sort-indicator-face-descend
                                :accessor vtable-sort-indicator-face-descend)
   (ellipsis :initarg :ellipsis :accessor vtable-ellipsis)
   (row-properties :initarg :row-properties :accessor vtable-row-properties)
   (column-colors :initarg :column-colors :accessor vtable-column-colors)
   (row-colors :initarg :row-colors :accessor vtable-row-colors)
   (column-color-function :initarg :column-color-function :accessor vtable-column-color-function)
   (row-color-function :initarg :row-color-function :accessor vtable-row-color-function)
   (close-action :initarg :close-action :accessor vtable-close-action)
   (extra-data :initarg :extra-data :accessor vtable-extra-data)
   (-marked-objects :initform nil)
   (-orig-sort-by :initform nil)
   (-cached-colors :initform nil)
   (-cache :initform (make-hash-table :test #'equal))
   (-cached-keymap :initform nil)
   (-has-column-spec :initform nil))
  "An object to hold the data for a table.")

(defvar-keymap vtable-map
  "S" #'vtable-sort-by-current-column
  "U" #'vtable-unsort
  "{" #'vtable-narrow-current-column
  "}" #'vtable-widen-current-column
  "g" #'vtable-revert-command
  "M-<left>" #'vtable-previous-column
  "M-<right>" #'vtable-next-column)

(defvar-keymap vtable-header-line-map
  :parent vtable-map
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'vtable-header-line-sort)

(defvar-keymap vtable-navigation-map
  "n"       #'vtable-next-line
  "C-n"     #'vtable-next-line
  "<down>"  #'vtable-next-line
  "<tab>"   #'vtable-next-line
  "p"       #'vtable-prev-line
  "C-p"     #'vtable-prev-line
  "<up>"    #'vtable-prev-line
  "S-<tab>" #'vtable-prev-line
  "q"       #'vtable-close)

;; Define inline functions early.

(defsubst vtable--text-scale-pixels (pixels)
  ;; Adjust pixels for text-scaled buffers
  (ceiling (* pixels (/ (float (default-font-width)) (frame-char-width)))))

;; NOTE: The alternative to `vtable--text-scale-pixels' would be to
;; record the vtable buffer and use that as a reference buffer for
;; `string-pixel-width'.
(defsubst vtable--string-pixel-width (str)
  ;; Adjust pixel-width for text-scaled buffers
  (vtable--text-scale-pixels (string-pixel-width str)))

(defsubst vtable--char-width (table)
  (vtable--string-pixel-width
   (propertize "x" 'face (vtable-face table))))

(cl-defun make-vtable (&key columns objects objects-function
                            getter
                            formatter
                            displayer
                            (use-header-line t)
                            (object-equal #'eq)
                            (face 'vtable)
                            (header-face 'vtable-header)
                            (marked-face 'vtable-marked)
                            actions keymap
                            (use-navigation-keymap nil)
                            (separator-width 1)
                            divider
                            divider-width
                            (divider-on-header t)
                            divider-intangible
                            sort-by
                            (sort-indicator vtable-sort-indicator-default)
                            (sort-indicator-face-ascend 'vtable-sort-indicator-ascend)
                            (sort-indicator-face-descend 'vtable-sort-indicator-descend)
                            (ellipsis (truncate-string-ellipsis))
                            (insert t)
                            row-properties
                            column-colors
                            row-colors
                            column-color-function
                            row-color-function
                            text-scale
                            text-scale-header
                            close-action
                            extra-data)
  "Create and insert a vtable at point.
The vtable object is returned.  If INSERT is nil, the table won't
be inserted.

See info node `(vtable)Top' for vtable documentation."
  (when objects-function
    (setq objects (funcall objects-function)))
  ;; We'll be altering the list, so create a copy.
  (setq objects (copy-sequence objects))
  (let ((table
         (make-instance
          'vtable
          :objects objects
          :objects-function objects-function
          :getter getter
          :formatter formatter
          :displayer displayer
          :use-header-line use-header-line
          :text-scale text-scale
          :text-scale-header text-scale-header
          :object-equal object-equal
          :face face
          :header-face header-face
          :marked-face marked-face
          :actions actions
          :keymap keymap
          :separator-width separator-width
          :divider-on-header divider-on-header
          :sort-by sort-by
          :sort-indicator sort-indicator
          :sort-indicator-face-ascend sort-indicator-face-ascend
          :sort-indicator-face-descend sort-indicator-face-descend
          :row-properties row-properties
          :row-colors row-colors
          :column-colors column-colors
          :column-color-function column-color-function
          :row-color-function row-color-function
          :ellipsis ellipsis
          :close-action close-action
          :extra-data extra-data)))
    ;; Store whether the user has specified columns or not.
    (setf (slot-value table '-has-column-spec) (not (not columns)))
    ;; Auto-generate the columns.
    (unless columns
      (unless objects
        (error "Can't auto-generate columns; no objects"))
      (setq columns (make-list (length (car objects)) "")))
    (setf (vtable-columns table)
          (mapcar (lambda (column)
                    (let ((new-col
                           (cond
                            ;; We just have the name (as a string).
                            ((stringp column)
                             (make-vtable-column :name column))
                            ;; A plist of keywords/values.
                            ((listp column)
                             (apply #'make-vtable-column column))
                            ;; A full `vtable-column' object.
                            (t
                             column))))
                      (let ((truncate-guess-tolerance
                             (vtable-column-truncate-guess-tolerance new-col)))
                        (unless (or (and (integerp truncate-guess-tolerance)
                                         (> truncate-guess-tolerance -1))
                                    (null truncate-guess-tolerance))
                          (error "column `%s' truncate-guess-tolerance must be nil or >= 0"
                                 (vtable-column-name new-col))))
                      new-col))
                  columns))
    ;; Compute missing column data.
    (setf (vtable-columns table) (vtable--compute-columns table))
    ;; Compute the colors.
    (when (or row-colors column-colors)
      (setf (slot-value table '-cached-colors)
            (vtable--compute-colors row-colors column-colors)))
    ;; Compute the divider.
    (when (or divider divider-width)
      (let ((div
             (propertize
              (or (copy-sequence divider)
                  (propertize
                   " " 'display
                   (list 'space :width
                         (list (vtable--compute-width table divider-width)))))
              'mouse-face 'highlight
              'keymap
              (define-keymap
                "<drag-mouse-1>" #'vtable--drag-resize-column
                "<down-mouse-1>" #'ignore))))
        (when divider-intangible
          (add-text-properties
           0 (length div)
           (list 'field t 'rear-nonsticky t 'front-sticky t
                 'intangible-text t 'cursor-intangible t)
           div))
        (setf (vtable-divider table) div)
        (when divider-intangible
          (cursor-intangible-mode))))
    ;; Compute the keymap.
    (setf (slot-value table '-cached-keymap) (vtable--make-keymap table use-navigation-keymap))
    (if sort-by
        (setf (slot-value table '-orig-sort-by) sort-by)
      (seq-do-indexed (lambda (column index)
                        (when (vtable-column-primary column)
                          (push (cons index (vtable-column-primary column))
                                (vtable-sort-by table))))
                      (vtable-columns table)))
    (when text-scale
      (add-hook 'text-scale-mode-hook
                (lambda ()
                  (when-let* ((table (vtable-current-table)))
                    (when (vtable-text-scale table)
                      (vtable-revert-command))))
                nil 'local))
    (when insert
      (vtable-insert table))
    table))

(defun vtable--compute-colors (row-colors column-colors)
  (cond
   ((null column-colors)
    (mapcar #'vtable--make-color-face row-colors))
   ((null row-colors)
    (mapcar #'vtable--make-color-face column-colors))
   (t
    (cl-loop for row in row-colors
             collect (cl-loop for column in column-colors
                              collect (vtable--face-blend
                                       (vtable--make-color-face row)
                                       (vtable--make-color-face column)))))))

(defun vtable--make-color-face (object)
  (if (stringp object)
      (list :background object)
    object))

(defun vtable--face-blend (face1 face2)
  (let ((foreground (vtable--face-color face1 face2 #'face-foreground
                                        :foreground))
        (background (vtable--face-color face1 face2 #'face-background
                                        :background)))
    `(,@(and foreground (list :foreground foreground))
      ,@(and background (list :background background)))))

(defun vtable--face-color (face1 face2 accessor slot)
  (let ((col1 (if (facep face1)
                  (funcall accessor face1)
                (plist-get face1 slot)))
        (col2 (if (facep face2)
                  (funcall accessor face2)
                (plist-get face2 slot))))
    (if (and col1 col2)
        (apply #'color-rgb-to-hex
               `(,@(color-blend (color-name-to-rgb col1)
                                (color-name-to-rgb col2))
                 2))
      (or col1 col2))))

;;; Interface utility functions.

(defun vtable-current-table ()
  "Return the table under point."
  (get-text-property (point) 'vtable))

(defun vtable-current-object ()
  "Return the object under point."
  (get-text-property (point) 'vtable-object))

(defun vtable-current-column ()
  "Return the index of the column under point."
  (get-text-property (point) 'vtable-column))

(defun vtable-beginning-of-table ()
  "Go to the start of the current table."
  (if (or (text-property-search-backward 'vtable (vtable-current-table) #'eq)
          (get-text-property (point) 'vtable))
      (point)
    (goto-char (point-min))))

(defun vtable-end-of-table ()
  "Go to the end of the current table."
  (if (text-property-search-forward 'vtable (vtable-current-table) #'eq)
      (point)
    (goto-char (point-max))))

(defun vtable-goto-object (object)
  "Go to OBJECT in the current table.
Return the position of the object if found, and nil if not."
  (let ((start (point)))
    (vtable-beginning-of-table)
    (let ((predicate (vtable-object-equal (vtable-current-table))))
      (save-restriction
        (narrow-to-region (point) (save-excursion (vtable-end-of-table)))
        (if (text-property-search-forward 'vtable-object object predicate)
            (progn
              (forward-line -1)
              (point))
          (goto-char start)
          nil)))))

(defun vtable-goto-table (table)
  "Go to TABLE in the current buffer.
If TABLE is found, return the position of the start of the table.
If it can't be found, return nil and don't move point."
  (let ((start (point)))
    (goto-char (point-min))
    (if-let* ((match (text-property-search-forward 'vtable table t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      nil)))

(defun vtable-goto-column (column)
  "Go to COLUMN on the current line."
  (beginning-of-line)
  (if-let* ((match (text-property-search-forward 'vtable-column column t)))
      (goto-char (prop-match-beginning match))
    (end-of-line)))

(defun vtable-beginning-of-table-line-number ()
  "Absolute buffer line number of the start of the current table."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos (vtable-beginning-of-table)))))

(defun vtable-end-of-table-line-number ()
  "Absolute buffer line number of the end of the current table."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos (vtable-end-of-table)))))

(defun vtable-object-line-number (object)
  "Absolute buffer line number of OBJECT or nil, if it is not in the table."
  (save-excursion
    (save-restriction
      (widen)
      (when (vtable-goto-object object)
        (line-number-at-pos)))))

(defun vtable-object-line-index (object)
  "Line number of OBJECT or nil, if it is not in the table.
The index is OBJECT's line number relative to the start of the table.
It is 0-based."
  (save-excursion
    (save-restriction
      (widen)
      (when (vtable-goto-object object)
        (- (line-number-at-pos)
           (vtable-beginning-of-table-line-number))))))

(defun vtable-update-object (table object &optional old-object sort-after)
  "Update OBJECT's representation in TABLE.
If OLD-OBJECT is non-nil, replace OLD-OBJECT with OBJECT and display it.
In either case, if the existing object is not found in the table, signal
an error.

If SORT-AFTER is non-nil, sort and redisplay the table after the object
is updated.

Note a limitation: if TABLE's buffer is not in a visible window, or if its
window has changed width since it was updated, updating the TABLE is not
possible, and an error is signaled."
  (unless old-object
    (setq old-object object))
  (let* ((objects (vtable-objects table))
         (inhibit-read-only t))
    ;; First replace the object in the object storage.
    (if (funcall (vtable-object-equal table) old-object (car objects))
        ;; It's at the head, so replace it there.
        (setf (vtable-objects table)
              (cons object (cdr objects)))
      ;; Otherwise splice into the list.
      (while (and (cdr objects)
                  (not (funcall (vtable-object-equal table) (cadr objects) old-object)))
        (setq objects (cdr objects)))
      (unless objects
        (error "Can't find the old object"))
      (setcar (cdr objects) object))
    ;; Then update the cache...
    ;; FIXME: If the table's buffer has no visible window, or if its
    ;; width has changed since the table was updated, the cache key will
    ;; not match and the object can't be updated.  (Bug #69837).
    (if-let* ((line-number (seq-position (vtable-objects table) old-object))
              (line (elt (car (vtable--ensure-cache table)) line-number)))
        (progn
          (setcar line object)
          (setcdr line (vtable--compute-cached-line table object))
          ;; ... and redisplay the line in question.
          (save-excursion
            (vtable-goto-object old-object)
            (let ((keymap (get-text-property (point) 'keymap))
                  (start (point)))
              (delete-line)
              (vtable--insert-line table line line-number
                                   (nth 1 (vtable--cache table))
                                   (vtable--spacer table))
              (add-text-properties start (point) (list 'keymap keymap
                                                       'vtable table))))
          ;; We may have inserted a non-numerical value into a previously
          ;; all-numerical table, so recompute.
          (vtable--recompute-numerical table (cdr line))
          (when sort-after
            (vtable--sort table)
            (vtable-revert)))
      (error "Can't find cached object in vtable"))))

(defun vtable-remove-object (table object &optional retain-rows)
  "Remove OBJECT from TABLE.
This will also remove the displayed line, and the object will be unmarked.

If the object is not found in the table, signal and error.

If RETAIN-ROWS is nil, all rows can be removed.  If it is an integer,
retain that many rows.  If it is t, prevent removing rows."
  (when (and (not (eq retain-rows t))
             (or (not retain-rows)
                 (and (integerp retain-rows)
                      (> (length (vtable-objects table)) retain-rows))))
    (let ((cache (vtable--ensure-cache table))
          (inhibit-read-only t))
      (unless (seq-contains-p (vtable-objects table)
                              object
                              (vtable-object-equal table))
        (error "Can't find the object to remove"))
      ;; First remove from the objects.
      (setf (vtable-objects table) (seq-remove
                                    (lambda (elt)
                                      (funcall (vtable-object-equal table) elt object))
                                    (vtable-objects table)))
      ;; Then unmark the object.
      (vtable--unmark-object table object)
      ;; Then adjust the cache and display.
      (setcar cache (delq (assoc object (car cache) (vtable-object-equal table)) (car cache)))
      (save-excursion
        (vtable-goto-table table)
        (when (vtable-goto-object object)
          (let ((line-index (- (line-number-at-pos (point))
                               (vtable-beginning-of-table-line-number))))
            (delete-line)
            ;; Now redisplay the lines below if there are row/column colors.
            (when (or (vtable-column-colors table)
                      (vtable-row-colors table)
                      (vtable-column-color-function table)
                      (vtable-row-color-function table))
              (vtable--redisplay-range table line-index))))))
    ;; Keep point within table bounds.
    (unless (vtable-current-object)
      (vtable-prev-line))))

;; FIXME: The fact that the `location' argument of
;; `vtable-insert-object' can be an integer and is then interpreted as
;; an index precludes the use of integers as objects.  This seems a very
;; unlikely use-case, so let's just accept this limitation.

(defun vtable-insert-object (table object &optional location before sort-after)
  "Insert OBJECT into TABLE at LOCATION.
LOCATION is an object in TABLE.  OBJECT is inserted after LOCATION,
unless BEFORE is non-nil, in which case it is inserted before LOCATION.

If LOCATION is nil, or does not exist in the table, OBJECT is inserted
at the end of the table, or at the beginning if BEFORE is non-nil.

LOCATION can also be an integer, a (zero-based) index into the table.
OBJECT is inserted at this location.  If the index is out of range,
OBJECT is inserted at the beginning (if the index is less than 0) or
end (if the index is too large) of the table.  BEFORE is ignored in this
case.

If SORT-AFTER is non-nil, sort the table after the object is inserted.
In either case, update the displayed table."
  ;; If the vtable is empty, just add the object and regenerate the
  ;; table.
  (if (null (vtable-objects table))
      (progn
        (setf (vtable-objects table) (list object))
        (vtable--recompute-numerical table (vtable--compute-cached-line table object))
        (vtable-goto-table table)
        (vtable-revert-command))
    ;; Ensure the cache is warm with the current objects before we
    ;; manually insert rows into either the object or cache lists.  This
    ;; ensures the object/line count is consistent.
    (let ((cache (vtable--ensure-cache table)))
      ;; First insert into the objects.
      (let ((pos (if location
                     (if (integerp location)
                         (prog1
                             (nthcdr location (vtable-objects table))
                           ;; Do not prepend if index is too large:
                           (setq before nil))
                       ;; First find the object by specified equality,
                       ;; then memq once we have the object.
                       (or (memq
                            (seq-find (lambda (elt)
                                        (funcall (vtable-object-equal table)
                                                 elt location))
                                      (vtable-objects table))
                            (vtable-objects table))
                           ;; Prepend if `location' is not found and
                           ;; `before' is non-nil:
                           (and before (vtable-objects table))))
                   ;; If `location' is nil and `before' is non-nil, we
                   ;; prepend the new object.
                   (if before (vtable-objects table)))))
        (if (or before  ; If `before' is non-nil, `pos' should be, as well.
                (and pos (integerp location)))
            ;; Add the new object before.
            (let ((old-object (car pos)))
              (setcar pos object)
              (setcdr pos (cons old-object (cdr pos))))
          ;; Otherwise, add the object after.
          (if pos
              ;; Splice the object into the list.
              (setcdr pos (cons object (cdr pos)))
            ;; Otherwise, append the object.
            (nconc (vtable-objects table) (list object)))))
      ;; Then adjust the cache and display.
      (save-excursion
        (vtable-goto-table table)
        (let* ((inhibit-read-only t)
               (keymap (get-text-property (point) 'keymap))
               (ellipsis (vtable-ellipsis table))
               (elem (if location  ; This binding mirrors the binding of `pos' above.
                         (if (integerp location)
                             (nth location (car cache))
                           (or (assoc location (car cache) (vtable-object-equal table))
                               (and before (caar cache))))
                       (if before (caar cache))))
               (pos (memq elem (car cache)))
               (line (cons object (vtable--compute-cached-line table object))))
          (if (or before
                  (and pos (integerp location)))
              ;; Add the new object before:.
              (let ((old-line (car pos)))
                (setcar pos line)
                (setcdr pos (cons old-line (cdr pos)))
                (unless (vtable-goto-object (car elem))
                  (vtable-beginning-of-table)))
            ;; Otherwise, add the object after.
            (if pos
                ;; Splice the object into the list.
                (progn
                  (setcdr pos (cons line (cdr pos)))
                  (if (vtable-goto-object location)
                      (forward-line 1)  ; Insert *after*.
                    (vtable-end-of-table)))
              ;; Otherwise, append the object.
              (setcar cache (nconc (car cache) (list line)))
              (vtable-end-of-table)))
          (let* ((start (point))
                 (line-index (- (line-number-at-pos start)
                                (vtable-beginning-of-table-line-number))))
            (vtable--insert-line table line line-index
                                 (nth 1 cache) (vtable--spacer table)
                                 ellipsis)
            (add-text-properties start (point) (list 'keymap keymap
                                                     'vtable table))
            ;; We may have inserted a non-numerical value into a previously
            ;; all-numerical table, so recompute.
            (vtable--recompute-numerical table (cdr line))
            (if sort-after
                (progn
                  ;; Revert does redisplay.
                  (vtable--sort table)
                  (vtable-revert))
              ;; Now redisplay the lines below if there are row/column colors.
              (when (or (vtable-column-colors table)
                        (vtable-row-colors table)
                        (vtable-column-color-function table)
                        (vtable-row-color-function table))
                (vtable--redisplay-range table (1+ line-index))))))))))

(defun vtable--redisplay-range (table &optional from-line to-line)
  "Update row/column colors for TABLE.
If FROM-LINE is nil, start at 0, the first line.
If TO-LINE is nil, end at the last line, the number of objects."
  (let ((line-index (or from-line 0))
        (inhibit-read-only t))
    (setq to-line (or to-line (1- (length (vtable-objects table)))))
    (when (<= from-line to-line)
      (save-excursion
        (while (<= line-index to-line)
          (let ((line (elt (car (vtable--cache table)) line-index)))
            (let ((keymap (get-text-property (point) 'keymap))
                  (start (point)))
              (delete-line)
              (vtable--insert-line table line line-index
                                   (nth 1 (vtable--cache table))
                                   (vtable--spacer table))
              (add-text-properties start (point) (list 'keymap keymap
                                                       'vtable table))))
          (cl-incf line-index))))))

(defun vtable-column (table index)
  "Return the name of the INDEXth column in TABLE."
  (vtable-column-name (elt (vtable-columns table) index)))

;;; Generating the table.

(defun vtable--get-value (object index column table)
  "Compute a cell value."
  (cond
   ((vtable-column-getter column)
    (funcall (vtable-column-getter column)
             object table))
   ((vtable-getter table)
    (funcall (vtable-getter table)
             object index table))
   ;; No getter functions; standard getters.
   ((stringp object)
    object)
   (t
    (elt object index))))

(defun vtable--compute-columns (table &optional recompute)
  "Compute column specs for TABLE.
Set the `align', `-aligned' and `-numerical' properties of each column.
If the column contains only numerical data, set `-numerical' to t,
otherwise to nil.  `-aligned' indicates whether the column has an
`align' property set by the user.  If it does, `align' is not touched,
otherwise it is set to `right' for numeric columns and to `left' for
non-numeric columns.

If RECOMPUTE is non-nil, do not set `-aligned'.  This can be used to
recompute the column specs when the table data has changed."
  (let ((numerical (make-vector (length (vtable-columns table)) t))
        (columns (vtable-columns table)))
    ;; First determine whether there are any all-numerical columns.
    (dolist (object (vtable-objects table))
      (seq-do-indexed
       (lambda (_elem index)
         (unless (numberp (vtable--get-value object index (elt columns index)
                                             table))
           (setf (elt numerical index) nil)))
       (vtable-columns table)))
    ;; Check if any columns have an explicit `align' property.
    (unless recompute
      (dolist (column (vtable-columns table))
        (when (vtable-column-align column)
          (setf (vtable-column--aligned column) t))))
    ;; Then fill in defaults.
    (seq-map-indexed
     (lambda (column index)
       ;; This is used when displaying.
       (unless (vtable-column--aligned column)
         (setf (vtable-column-align column)
               (if (elt numerical index)
                   'right
                 'left)))
       ;; This is used for sorting.
       (setf (vtable-column--numerical column)
             (elt numerical index))
       column)
     (vtable-columns table))))

(defun vtable--spacer (table)
  (vtable--compute-width table (vtable-separator-width table)))

(defun vtable--recompute-cache (table)
  (let* ((data (vtable--compute-cache table))
         (widths (vtable--compute-widths table data)))
    (setf (gethash (vtable--cache-key) (slot-value table '-cache))
          (list data widths))))

(defun vtable--ensure-cache (table)
  (or (vtable--cache table)
      (vtable--recompute-cache table)))

(defun vtable-insert (table)
  (let* ((spacer (vtable--spacer table))
         (start (point))
         (ellipsis (vtable-ellipsis table))
         ;; We maintain a cache per screen/window width, so that we render
         ;; correctly if Emacs is open on two different screens (or the
         ;; user resizes the frame).
         (widths (vtable--widths table)))
    ;; Don't insert any header or header line if the user hasn't
    ;; specified the columns.
    (when (slot-value table '-has-column-spec)
      (if (vtable-use-header-line table)
          (vtable--set-header-line table widths spacer)
        ;; Insert the header line directly into the buffer, and put a
        ;; keymap to be able to sort the columns there (by clicking on
        ;; them).
        (vtable--insert-header-line table widths spacer)
        (add-text-properties start (point)
                             (list 'keymap vtable-header-line-map
                                   'rear-nonsticky t
                                   'vtable table))
        (setq start (point))))
    (vtable--sort table)
    ;; Insert the data.
    (let ((line-number 0))
      (dolist (line (car (vtable--ensure-cache table)))
        (vtable--insert-line table line line-number widths spacer
                             ellipsis)
        (setq line-number (1+ line-number))))
    (add-text-properties start (point)
                         (list 'rear-nonsticky t
                               'vtable table))
    (goto-char start)))

(defun vtable--insert-line (table line line-number widths spacer &optional ellipsis)
  (let ((start (point))
        (columns (vtable-columns table))
        (column-color-function (vtable-column-color-function table))
        (row-color-function (vtable-row-color-function table))
        (column-colors
         (and (vtable-column-colors table)
              (if (vtable-row-colors table)
                  (elt (slot-value table '-cached-colors)
                       (mod line-number (length (vtable-row-colors table))))
                (slot-value table '-cached-colors))))
        (divider (vtable-divider table))
        (keymap (slot-value table '-cached-keymap)))
    (seq-do-indexed
     (lambda (elem index)
       (let* ((value (nth 0 elem))
              (column (elt columns index))
              ;; Cached widths should be text-scale adjusted.
              (column-width (elt widths index))
              (pre-computed (nth 2 elem)))
         ;; See if we have any formatters here.
         (cond
          ((vtable-column-formatter column)
           (setq value (funcall (vtable-column-formatter column) value)
                 pre-computed nil))
          ((vtable-formatter table)
           (setq value (funcall (vtable-formatter table)
                                value index table)
                 pre-computed nil)))
         (let* ((ellipsis+ (copy-sequence ellipsis))
                ;; Make ellipsis properties match the final character of value.
                (x (length (or pre-computed value)))
                (_ (when (> x 0)
                     (add-text-properties
                      0 (length ellipsis+)
                      (text-properties-at (1- x)
                                          (or pre-computed value))
                      ellipsis+)))
                (ellipsis-width (vtable--string-pixel-width ellipsis+))
                (displayed
                 ;; Allow any displayers to have their say.
                 (cond
                  ((vtable-column-displayer column)
                   (funcall (vtable-column-displayer column)
                            value column-width table))
                  ((vtable-displayer table)
                   (funcall (vtable-displayer table)
                            value index column-width table))
                  (pre-computed
                   ;; If we don't have a displayer, use the pre-made
                   ;; (cached) string value.
                   (if (> (nth 1 elem) column-width)
                       (concat
                        (vtable--limit-string
                         pre-computed (- column-width
                                         (or ellipsis-width 0))
                         (vtable-column-truncate-guess-tolerance column))
                        ellipsis+)
                     pre-computed))
                  ;; Recompute widths.
                  (t
                   (if (> (vtable--string-pixel-width value) column-width)
                       (concat
                        (vtable--limit-string
                         value (- column-width
                                  (or ellipsis-width 0))
                         (vtable-column-truncate-guess-tolerance column))
                        ellipsis+)
                     value))))
                (start (point))
                ;; Don't insert the separator or divider after the final column.
                (last (= index (- (length line) 2)))
                ;; On the last column, leave one char width in header harmony.
                (spacer (if last (vtable--char-width table) spacer)))
           (if (eq (vtable-column-align column) 'left)
               (progn
                 (insert displayed)
                 (insert (propertize
                          " " 'display
                          (list 'space
                                :width (list
                                        (+ (- column-width
                                              (vtable--string-pixel-width displayed))
                                           spacer))))))
             ;; Align to the right.
             (insert (propertize " " 'display
                                 (list 'space
                                       :width (list (- column-width
                                                       (vtable--string-pixel-width
                                                        displayed)))))
                     displayed)
             (unless last
               (insert (propertize " " 'display
                                   (list 'space
                                         :width (list spacer))))))
           (put-text-property start (point) 'vtable-column index)
           (put-text-property start (point) 'keymap keymap)
           (when (or column-color-function column-colors)
             (add-face-text-property
              start (point)
              (if column-color-function
                  (funcall column-color-function line-number index value (car line) column-colors)
                (elt column-colors (mod index (length column-colors))))))
           (when (and divider (not last))
             (insert divider)
             (setq start (point))))))
     (cdr line))
    (insert "\n")
    (put-text-property start (point) 'vtable-object (car line))
    (when (vtable-object-marked-p table (car line))
      (add-face-text-property start (point) (vtable-marked-face table) (car line)))
    (let ((row-colors (slot-value table '-cached-colors)))
      (cond
       (row-color-function
        (add-face-text-property
         start (point)
         (funcall row-color-function line-number (car line) row-colors)))
       ((and row-colors
             (null column-colors))
        (add-face-text-property
         start (point)
         (elt row-colors (mod line-number (length row-colors)))))))
    (when (vtable-row-properties table)
      (save-excursion
        (forward-line -1)
        (add-text-properties (pos-bol) (pos-eol) (vtable-row-properties table))))))

(defun vtable--cache-key ()
  (cons (frame-terminal) (window-body-width nil 'remap)))

(defun vtable--cache (table)
  (gethash (vtable--cache-key) (slot-value table '-cache)))

(defun vtable--clear-cache (table)
  (setf (gethash (vtable--cache-key) (slot-value table '-cache)) nil))

(defun vtable--sort (table)
  (pcase-dolist (`(,index . ,direction) (vtable-sort-by table))
    (let* ((cache (vtable--ensure-cache table))
           (column (elt (vtable-columns table) index))
           (numerical (vtable-column--numerical column))
           (numcomp (if (eq direction 'descend)
                        #'> #'<))
           (stringcomp (if (eq direction 'descend)
                           #'string> #'string<))
           (comparator (vtable-column-comparator column))
           (comparator-func (when comparator
                              (if (eq direction 'descend)
                                  (lambda (v1 v2)
                                    (funcall comparator v2 v1))
                                comparator))))
      (setcar cache
              (sort (car cache)
                    (lambda (e1 e2)
                      (let ((c1 (elt e1 (1+ index)))
                            (c2 (elt e2 (1+ index))))
                        (if comparator-func
                            (funcall comparator-func (car c1) (car c2))
                          (if numerical
                              (funcall numcomp (car c1) (car c2))
                            (funcall
                             stringcomp
                             (if (stringp (car c1))
                                 (car c1)
                               (format "%s" (car c1)))
                             (if (stringp (car c2))
                                 (car c2)
                               (format "%s" (car c2)))))))))))))

(defun vtable--indicator (table index)
  (let ((order (car (last (vtable-sort-by table)))))
    (if (eq index (car order))
        (let* ((dir (cdr order))
               (n (if (eq dir 'ascend) 1 0)))
          ;; We're sorting by this column last, so return an indicator.
          (catch 'found
            (dolist (candidate (nth n (vtable-sort-indicator table)))
              (when (char-displayable-p candidate)
                (throw 'found (cons (string candidate) dir))))
            (cons "" nil))))))

(defun vtable--insert-header-line (table widths spacer)
  ;; Insert the header directly into the buffer.
  (let* ((start (point))
         (divider (vtable-divider table))
         (divider-pixels (vtable--string-pixel-width divider))
         (divider-on-header (vtable-divider-on-header table))
         (cmap (define-keymap
                 "<header-line> <drag-mouse-1>" #'vtable--drag-resize-column
                 "<header-line> <down-mouse-1>" #'ignore))
         (dmap (define-keymap
                 "<header-line> <drag-mouse-1>"
                 (lambda (e)
                   (interactive "e")
                   (vtable--drag-resize-column e t))
                 "<header-line> <down-mouse-1>" #'ignore)))
    (seq-do-indexed
     (lambda (column index)
       (let* ((name (vtable-column-name column))
              (name (propertize name
                                'mouse-face 'header-line-highlight
                                'keymap cmap))
              (start (point))
              ;; Cached widths should be text-scale adjusted.
              (column-width (elt widths index))
              (column-align (vtable-column-align column))
              ;; Pad the indicator to avoid abutting its neighbors.
              (indicator+dir (vtable--indicator table index))
              (indicator (propertize
                          (concat " " (car indicator+dir) " ")
                          'face (if (eq (cdr indicator+dir) 'ascend)
                                    (vtable-sort-indicator-face-ascend table)
                                  (vtable-sort-indicator-face-descend table))
                          'display '(space-width 0.5)))
              (indicator-width (vtable--string-pixel-width indicator))
              ;; Don't insert the separator or divider after the final column.
              (last (= index (1- (length (vtable-columns table)))))
              ;; On the last column, leave one char to stave off name truncation.
              ;; We use the body face here but we could use the header face.
              (spacer (if last (vtable--char-width table) spacer)))
         (let* ((displayed
                 (if (> (vtable--string-pixel-width name)
                        (- (+ column-width spacer) indicator-width))
                     (vtable--limit-string
                      name (- (+ column-width spacer) indicator-width)
                      (vtable-column-truncate-guess-tolerance column))
                   name))
                (fill-width
                 (+ (- column-width
                       (vtable--string-pixel-width displayed)
                       indicator-width)
                    spacer)))
           (if (or (not last)
                   (zerop indicator-width)
                   (< (seq-reduce #'+ widths 0) (window-body-width nil t)))
               ;; Normal case.
               (if (eq column-align 'left)
                   (insert
                    displayed
                    (propertize " " 'display
                                (list 'space :width (list fill-width)))
                    indicator)
                 (insert
                  (propertize " " 'display
                              (list 'space :width (list fill-width)))
                  displayed
                  indicator))
             ;; This is the final column, and we have a sorting
             ;; indicator, and the table is too wide for the window.
             (let* ((pre-indicator (vtable--string-pixel-width
                                    (buffer-substring (point-min) (point))))
                    (pre-fill
                     (- (window-body-width nil t)
                        pre-indicator
                        (vtable--string-pixel-width displayed))))
               (if (eq column-align 'left)
                   (insert
                    displayed
                    (propertize " " 'display
                                (list 'space :width (list pre-fill)))
                    indicator
                    (propertize " " 'display
                                (list 'space :width
                                      (list (- fill-width pre-fill)))))
                 (insert
                  (propertize " " 'display
                              (list 'space :width (list pre-fill)))
                  displayed
                  indicator
                  (propertize " " 'display
                              (list 'space :width
                                    (list (- fill-width pre-fill)))))))))
         (when (and divider (not last))
           (if divider-on-header
               (insert (propertize divider 'keymap dmap))
             (insert (propertize " " 'display
                                 (list 'space :width (list divider-pixels))))))
         (put-text-property start (point) 'vtable-column index)))
     (vtable-columns table))
    (insert "\n")
    (add-face-text-property start (point) (vtable-header-face table) t)))

(defun vtable--drag-resize-column (e &optional next)
  "Resize the column by dragging.
If NEXT, do the next column."
  (interactive "e")
  (let* ((pos-start (event-start e))
	 (obj (posn-object pos-start)))
    (with-current-buffer (window-buffer (posn-window pos-start))
      (let ((column
             ;; In the header line we have a text property on the
             ;; divider.
             (or (get-text-property (if obj (cdr obj)
                                      (posn-point pos-start))
			            'vtable-column
			            (car obj))
                 ;; For reasons of efficiency, we don't have that in
                 ;; the buffer itself, so find the column.
                 (save-excursion
                   (goto-char (posn-point pos-start))
                   (1+
                    (get-text-property
                     (prop-match-beginning
                      (text-property-search-backward 'vtable-column))
                     'vtable-column)))))
            (start-x (car (posn-x-y pos-start)))
            (end-x (car (posn-x-y (event-end e)))))
        (when (or (> column 0) next)
          (vtable--alter-column-width (vtable-current-table)
                                      (if next
                                          column
                                        (1- column))
                                      (- end-x start-x)))))))

(defun vtable--recompute-numerical (table line)
  "Recompute numericalness of columns if necessary."
  (let ((columns (vtable-columns table))
        (recompute nil))
    (seq-do-indexed
     (lambda (elem index)
       (when (and (vtable-column--numerical (elt columns index))
                  (not (numberp (car elem))))
         (setq recompute t)))
     line)
    (when recompute
      (vtable--compute-columns table t))))

(defvar text-scale-remap-header-line)

(defun vtable--set-header-line (table widths spacer)
  (let ((reference-buffer (current-buffer)))
    (setq header-line-format
          (concat
           (propertize " " 'display
                       (list 'space :align-to
                             (list (line-number-display-width t))))
           (string-replace
            "%" "%%"
            (with-temp-buffer
              ;; Cribbed from string-pixel-width to normalize the temp
              ;; buffer to the originating buffer and window.
              (dolist (v '(face-remapping-alist
                           char-property-alias-alist
                           default-text-properties))
                (if (local-variable-p v reference-buffer)
                    (set (make-local-variable v)
                         (buffer-local-value v reference-buffer))))
              (vtable--insert-header-line table widths spacer)
              (buffer-substring (point-min) (1- (point-max))))))))
  (when (vtable-text-scale-header table)
    (setq text-scale-remap-header-line t))
  (vtable-header-mode))

(defun vtable--limit-string (string pixels &optional truncate-guess-tolerance)
  "Truncate STRING to fit into width PIXELS.
This function tries to guess STRING's truncated length, in characters,
based the average pixel width of its characters, including its text
properties, relative to PIXELS.

If TRUNCATE-GUESS-TOLERANCE is nil, then no guessing is done.

If TRUNCATE-GUESS-TOLERANCE is an integer, it is the number of additional
characters to add to the guess.  Start with 0 and increase the tolerance
if you find that the guess is too small."
  (let ((string-len (length string))
        (string-pixels))
    (when (and (> string-len 0)
               (integerp truncate-guess-tolerance))
      (setq string-pixels (vtable--string-pixel-width string))
      (when (> string-pixels pixels)
        ;; Use average pixels/character from STRING to seed the guess.
        (let ((guess (+ truncate-guess-tolerance
                        (ceiling (/ pixels (/ string-pixels string-len))))))
          (when (< guess string-len)
            (setq string (substring string 0 guess))))))
    (while (and (length> string 0)
                ;; Reuse the initial string-pixels for the fast path.
                (if string-pixels (prog1
                                      (> string-pixels pixels)
                                    (setq string-pixels nil))
                  (> (vtable--string-pixel-width string) pixels)))
      (setq string (substring string 0 (1- (length string)))))
    string))

(defun vtable--compute-width (table spec)
  (cond
   ((numberp spec)
    (* spec (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)ex" spec)
    (* (string-to-number (match-string 1 spec)) (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)px" spec)
    (string-to-number (match-string 1 spec)))
   ((string-match "\\([0-9.]+\\)%" spec)
    (/ (* (string-to-number (match-string 1 spec)) (window-body-width nil t))
       100))
   (t
    (error "Invalid spec: %s" spec))))

(defun vtable--compute-widths (table cache)
  "Compute the display widths for TABLE.
CACHE is TABLE's cache data as returned by `vtable--compute-cache'."
  (let* ((n-0cols 0) ; Count the number of zero-width columns.
         (widths (seq-map-indexed
                  (lambda (column index)
                    (let ((width
                           (or
                            ;; Explicit widths.
                            (and (vtable-column-width column)
                                 (vtable--compute-width table (vtable-column-width column)))
                            ;; If the vtable is empty and no explicit width is given,
                            ;; set its width to 0 and deal with it below.
                            (when (null cache)
                              (setq n-0cols (1+ n-0cols))
                              0)
                            ;; Otherwise, compute based on the displayed widths of the
                            ;; data.
                            (max
                             (seq-max (seq-map (lambda (elem)
                                                 (nth 1 (elt (cdr elem) index)))
                                               cache))
                             (if (eq (vtable-column-infer-width column) 'data+name)
                                 (let ((name (vtable-column-name column)))
                                   (add-face-text-property
                                    0 (length name)
                                    (vtable-header-face table) t name)
                                   (vtable--string-pixel-width name))
                               0)))))
                      ;; Let min-width/max-width specs have their say.
                      (when-let* ((min-width (and (vtable-column-min-width column)
                                                  (vtable--compute-width
                                                   table (vtable-column-min-width column)))))
                        (setq width (max width min-width)))
                      (when-let* ((max-width (and (vtable-column-max-width column)
                                                  (vtable--compute-width
                                                   table (vtable-column-max-width column)))))
                        (setq width (min width max-width)))
                      width))
                  (vtable-columns table))))
    ;; If there are any zero-width columns, divide the remaining window
    ;; width evenly over them.
    (when (> n-0cols 0)
      (let* ((combined-width (apply #'+ widths))
             (default-width (/ (- (window-body-width nil t) combined-width) n-0cols)))
        (setq widths (mapcar (lambda (width)
                               (if (zerop width)
                                   default-width
                                 width))
                             widths))))
    (seq-into widths 'vector)))

(defun vtable--compute-cache (table)
  (seq-map
   (lambda (object)
     (cons object (vtable--compute-cached-line table object)))
   (vtable-objects table)))

(defun vtable--compute-cached-line (table object)
  (seq-map-indexed
   (lambda (column index)
     (let* ((value (vtable--get-value object index column table))
            (string
             (cond
              ((vtable-column-formatter column)
               (funcall (vtable-column-formatter column) value))
              ((vtable-formatter table)
               (funcall (vtable-formatter table) value index table))
              (t
               (if (stringp value)
                   (copy-sequence value)
                 (format "%s" value))))))
       (add-face-text-property 0 (length string)
                               (vtable-face table)
                               t string)
       ;; We stash the computed width and string here -- if there are
       ;; no formatters/displayers, we'll be using the string, and
       ;; then won't have to recreate it.
       (list value (vtable--string-pixel-width string) string)))
   (vtable-columns table)))

(defun vtable--make-keymap (table use-navigation-keymap)
  (let ((map (if (or (vtable-actions table)
                     (vtable-keymap table))
                 (copy-keymap vtable-map)
               vtable-map)))
    (when-let* ((actions (vtable-actions table)))
      (while actions
        (funcall (lambda (key binding)
                   (keymap-set map key
                               (lambda (object)
                                 (interactive (list (vtable-current-object)))
                                 (funcall binding object))))
                 (car actions) (cadr actions))
        (setq actions (cddr actions))))

    (when use-navigation-keymap
      (setq map (make-composed-keymap (list map vtable-navigation-map))))

    (if (vtable-keymap table)
        (progn
          (setf (vtable-keymap table)
                (copy-keymap (vtable-keymap table)))
          ;; Respect any previously set parent keymaps.
          (set-keymap-parent (vtable-keymap table)
                             (if (keymap-parent (vtable-keymap table))
                                 (append (ensure-list
                                          (vtable-keymap table))
                                         (list map))
                               map))
          (vtable-keymap table))
      map)))

(defun vtable-revert ()
  "Regenerate the table under point."
  (let ((table (vtable-current-table))
        (object (vtable-current-object))
        (column (vtable-current-column))
        (inhibit-read-only t))
    (unless table
      (user-error "No table under point"))
    (delete-region (vtable-beginning-of-table) (vtable-end-of-table))
    (vtable-insert table)
    (when object
      (vtable-goto-object object))
    (when column
      (vtable-goto-column column))))

(defun vtable--widths (table)
  (nth 1 (vtable--ensure-cache table)))

;;; Commands.

(defvar-keymap vtable-header-mode-map
  "<header-line> <mouse-1>" 'vtable-header-line-sort
  "<header-line> <mouse-2>" 'vtable-header-line-sort)

(defvar-local vtable--display-line-numbers nil)

(defun vtable--post-command ()
  (unless (eq vtable--display-line-numbers display-line-numbers)
    (setq vtable--display-line-numbers display-line-numbers)
    (vtable-revert-command)))

(define-minor-mode vtable-header-mode
  "Minor mode for buffers with vtables with headers."
  :keymap vtable-header-mode-map
  (if vtable-header-mode
      (add-hook 'post-command-hook #'vtable--post-command nil 'local)
    (remove-hook 'post-command-hook #'vtable--post-command 'local)))

(defun vtable-narrow-current-column (&optional n)
  "Narrow the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (let* ((table (vtable-current-table))
         (column (vtable-current-column)))
    (unless column
      (user-error "No column under point"))
    (vtable--alter-column-width table column
                                (- (* (vtable--char-width table) (or n 1))))))

(defun vtable--alter-column-width (table column delta)
  (let ((widths (vtable--widths table))
        (char-width (vtable--char-width table))
        (min-width (vtable-column-min-width
                    (elt (vtable-columns table) column))))
    (setf (aref widths column)
          (max (* (or min-width 2)
                  char-width)
               (+ (aref widths column) delta)))
    ;; Store the width so it'll be respected on a revert.
    (setf (vtable-column-width (elt (vtable-columns table) column))
          (format "%dpx" (aref widths column)))
    (vtable-revert)))

(defun vtable-widen-current-column (&optional n)
  "Widen the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (vtable-narrow-current-column (- n)))

(defun vtable-previous-column ()
  "Go to the previous column."
  (interactive)
  (vtable-goto-column
   (max 0 (1- (or (vtable-current-column)
                  (length (vtable--widths (vtable-current-table))))))))

(defun vtable-next-column ()
  "Go to the next column."
  (interactive)
  (when (vtable-current-column)
    (vtable-goto-column
     (min (1- (length (vtable--widths (vtable-current-table))))
          (1+ (vtable-current-column))))))

(defun vtable-revert-command ()
  "Re-query data and regenerate the table under point."
  (interactive)
  (let ((table (vtable-current-table)))
    (when (vtable-objects-function table)
      (setf (vtable-objects table) (funcall (vtable-objects-function table))))
    (vtable--clear-cache table))
  (vtable-revert))

(defun vtable-sort-by-current-column ()
  "Sort the table under point by the column under point."
  (interactive)
  (unless (vtable-current-column)
    (user-error "No current column"))
  (let* ((table (vtable-current-table))
         (last (car (last (vtable-sort-by table))))
         (index (vtable-current-column)))
    ;; First prune any previous appearance of this column.
    (setf (vtable-sort-by table)
          (delq (assq index (vtable-sort-by table))
                (vtable-sort-by table)))
    ;; Then insert this as the last sort key.
    (setf (vtable-sort-by table)
          (append (vtable-sort-by table)
                  (list (cons index
                              (if (eq (car last) index)
                                  (if (eq (cdr last) 'ascend)
                                      'descend
                                    'ascend)
                                'ascend))))))
  (vtable-revert))

(defun vtable-header-line-sort (e)
  "Sort a vtable from the header line."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (goto-char (point-min))
      (vtable-goto-column
       (get-text-property (if obj (cdr obj) (posn-point pos))
			  'vtable-column
			  (car obj)))
      (vtable-sort-by-current-column))))

(defun vtable-unsort ()
  "Disable vtable sort, or toggle disabled and the original `:sort-by'.
The default order is determined by the table's objects or
`:objects-function'."
  (interactive)
  (let ((table (vtable-current-table)))
    (unless table
      (user-error "No table under point"))
    (cond
     ((null (vtable-sort-by table))
      (when (slot-value table '-orig-sort-by)
        (message "Original sort order")
        (setf (vtable-sort-by table) (slot-value table '-orig-sort-by))))
     (t
      (message "Sort disabled")
      (setf (vtable-sort-by table) nil)))
    (vtable-revert)))

(defun vtable-next-line (&optional n)
  "Like `forward-line', but keeps point within table body bounds.
N has the same meaning as in `forward-line', which see."
  (interactive "p")
  (when (save-excursion
          (forward-line (or n 1))
          (vtable-current-object))
    (forward-line (or n 1))))

(defun vtable-prev-line (&optional n)
  "Like `forward-line', but keeps point within table body bounds.
N has the same meaning as a negative argument in `forward-line', which see."
  (interactive "p")
  (vtable-next-line (* -1 (or n 1))))

(defun vtable-close ()
  (interactive)
  (when-let* ((table (vtable-current-table)))
    (pcase (vtable-close-action table)
      ((pred (lambda (x) (when (functionp x) (funcall x) t))) t)
      ('quit-window (quit-window))
      ('quit-window-kill (quit-window 'kill))
      (_ (bury-buffer)))))

;; Object marking functions.

(defun vtable-marked-objects (table)
  (slot-value table '-marked-objects))

(defun vtable-object-marked-p (table object)
  (seq-contains-p (slot-value table '-marked-objects)
                  object
                  (vtable-object-equal table)))

(defun vtable--mark-object (table object)
  (unless (vtable-object-marked-p table object)
    (push object (slot-value table '-marked-objects))
    (vtable-update-object table object)))

(defun vtable-mark-object (object &optional inhibit-next-line)
  (vtable--mark-object (vtable-current-table) object)
  (unless inhibit-next-line
    (vtable-next-line 1)))

(defun vtable--unmark-object (table object)
  (let ((removed-seq (seq-remove (lambda (elt)
                                   (funcall (vtable-object-equal table)
                                            elt object))
                                 (slot-value table '-marked-objects))))
    (unless (eq (length removed-seq)
                (length (slot-value table '-marked-objects)))
      (setf (slot-value table '-marked-objects) removed-seq)
      (vtable-update-object table object))))

(defun vtable-unmark-object (object &optional inhibit-next-line)
  (vtable--unmark-object (vtable-current-table) object)
  (unless inhibit-next-line
    (vtable-next-line 1)))

(defun vtable-toggle-marked-object (object)
  (let ((table (vtable-current-table)))
    (if (vtable-object-marked-p table object)
        (vtable--unmark-object table object)
      (vtable--mark-object table object))))

(defun vtable-mark-objects (table predicate)
  (dolist (line (car (vtable--ensure-cache table)))
    (let ((object (car line)))
      (when (funcall predicate object)
        (vtable--mark-object table object)))))

(defun vtable-mark-all-objects (&rest _)
  (vtable-mark-objects (vtable-current-table) #'identity))

(defun vtable-unmark-objects (table predicate)
  (dolist (line (car (vtable--ensure-cache table)))
    (let ((object (car line)))
      (when (funcall predicate object)
        (vtable--unmark-object table object)))))

(defun vtable-unmark-all-objects (&rest _)
  (vtable-unmark-objects (vtable-current-table) #'identity))

;; Extra data convenience function.

(defun vtable-set-extra-data (table extra-data)
  (setf (vtable-extra-data table) extra-data))

(provide 'vtable)

;;; vtable.el ends here
