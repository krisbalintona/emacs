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

(defvar vtable-sort-indicator-pad-space-width 0.3
  "The width of white space padding around the sort indicator.
In units of character-width.")

(cl-defstruct vtable-column
  "A vtable column."
  name
  width
  infer-width ; nil or 'data uses data, 'data+name includes column name
  min-width
  max-width
  (truncate-guess 0)
  inhibit-text-scaling
  primary
  (numeric 'infer)
  align
  header-align
  getter
  formatter
  displayer
  comparator
  extra-data
  -numerical
  -aligned)

(defclass vtable ()
  ((columns :initarg :columns :accessor vtable-columns)
   (objects :initarg :objects :accessor vtable-objects)
   (objects-function :initarg :objects-function
                     :accessor vtable-objects-function)
   (name :initarg :name :accessor vtable-name :initform "*unnamed*")
   (duplicate-objects :initarg :duplicate-objects
                      :accessor vtable-duplicate-objects
                      :initform 'allow)
   (getter :initarg :getter :accessor vtable-getter)
   (formatter :initarg :formatter :accessor vtable-formatter)
   (displayer :initarg :displayer :accessor vtable-displayer)
   (use-header-line :initarg :use-header-line
                    :accessor vtable-use-header-line)
   (header-intangible :initarg :header-intangible
                      :accessor vtable-header-intangible)
   (decor-intangible :initarg :decor-intangible
                     :accessor vtable-decor-intangible :initform nil)
   (text-scale-redraw :initarg :text-scale-redraw
                      :accessor vtable-text-scale-redraw)
   (text-scale-header-line :initarg :text-scale-header-line
                           :accessor vtable-text-scale-header-line)
   (auto-resize-delay :initarg :auto-resize-delay
                      :accessor vtable-auto-resize-delay)
   (object-equal :initarg :object-equal
                 :accessor vtable-object-equal :initform #'eq)
   (face :initarg :face :accessor vtable-face)
   (header-face :initarg :header-face :accessor vtable-header-face)
   (marked-face :initarg :marked-face :accessor vtable-marked-face)
   (actions :initarg :actions :accessor vtable-actions)
   (keymap :initarg :keymap :accessor vtable-keymap)
   (separator-width :initarg :separator-width
                    :accessor vtable-separator-width)
   (divider :initarg :divider :accessor vtable-divider :initform nil)
   (divider-width :initarg :divider-width
                  :accessor vtable-divider-width :initform nil)
   (divider-on-header :initarg :divider-on-header
                      :accessor vtable-divider-on-header :initform nil)
   (sort-by :initarg :sort-by :accessor vtable-sort-by)
   (sort-indicator :initarg :sort-indicator :accessor vtable-sort-indicator)
   (sort-indicator-face-ascend :initarg :sort-indicator-face-ascend
                               :accessor vtable-sort-indicator-face-ascend)
   (sort-indicator-face-descend :initarg :sort-indicator-face-descend
                                :accessor vtable-sort-indicator-face-descend)
   (ellipsis :initarg :ellipsis :accessor vtable-ellipsis)
   (row-text-properties :initarg :row-text-properties
                        :accessor vtable-row-text-properties)
   (column-colors :initarg :column-colors :accessor vtable-column-colors)
   (row-colors :initarg :row-colors :accessor vtable-row-colors)
   (column-color-function :initarg :column-color-function
                          :accessor vtable-column-color-function)
   (row-color-function :initarg :row-color-function
                       :accessor vtable-row-color-function)
   (close-action :initarg :close-action :accessor vtable-close-action)
   (extra-data :initarg :extra-data :accessor vtable-extra-data
               :initform nil)
   (pulse :initarg :pulse :accessor vtable-pulse :initform nil)
   (-objects-tick :initform 0)
   (-marked-objects :initform nil)
   (-orig-sort-by :initform nil)
   (-cached-colors :initform nil)
   (-buffer :initform nil)
   (-indicator-pad :initform nil)
   (-cache :initform (make-hash-table :test #'equal))
   (-cached-keymap :initform nil)
   (-cached-drag-keymap :initform nil)
   (-has-column-spec :initform nil))
  "An object to hold the data for a table.")

(defvar-keymap vtable-map
  "S"         #'vtable-sort-by-current-column
  "U"         #'vtable-unsort
  "{"         #'vtable-narrow-current-column
  "}"         #'vtable-widen-current-column
  "g"         #'vtable-revert-command
  "M-<left>"  #'vtable-previous-column
  "M-<right>" #'vtable-next-column)

(defvar-keymap vtable-header-line-map
  :parent vtable-map
  "<follow-link>" 'mouse-face
  "<header-line> <mouse-1>" #'vtable--header-line-sort
  "<header-line> <mouse-2>" #'vtable--header-line-sort
  "<mouse-1>"               #'vtable--header-line-sort
  "<mouse-2>"               #'vtable--header-line-sort
  "q"                       #'vtable-close
  "<tab>"                   #'vtable-goto-beginning-of-table)

(defvar-keymap vtable-drag-resize-column-map
  "<header-line> <drag-mouse-1>" #'vtable--drag-resize-column
  "<header-line> <down-mouse-1>" #'ignore
  "<drag-mouse-1>"               #'vtable--drag-resize-column
  "<down-mouse-1>"               #'ignore)

(defvar-keymap vtable-header-drag-resize-column-map
  :parent (make-composed-keymap
           vtable-header-line-map
           vtable-drag-resize-column-map))

(defvar-keymap vtable-navigation-map
  "C-a"    (lambda () (interactive)
             (beginning-of-line)
             (scroll-right most-positive-fixnum))
  "n"      #'vtable-next-line
  "<down>" #'vtable-next-line
  "p"      #'vtable-previous-line
  "<up>"   #'vtable-previous-line
  "<home>" #'vtable-goto-beginning-of-table
  "<end>"  #'vtable-goto-end-of-table
  "<remap> <forward-paragraph>"  #'vtable-goto-next-table
  "<remap> <backward-paragraph>" #'vtable-goto-previous-table
  "q"      #'vtable-close)

(cl-defun make-vtable (&key columns objects objects-function
                            (name "*unnamed*")
                            (duplicate-objects 'allow)
                            getter
                            formatter
                            displayer
                            (use-header-line t)
                            header-intangible
                            decor-intangible
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
                            sort-by
                            (sort-indicator vtable-sort-indicator-default)
                            (sort-indicator-face-ascend 'vtable-sort-indicator-ascend)
                            (sort-indicator-face-descend 'vtable-sort-indicator-descend)
                            (ellipsis (truncate-string-ellipsis))
                            (insert t)
                            row-text-properties
                            column-colors
                            row-colors
                            column-color-function
                            row-color-function
                            (text-scale-redraw t)
                            text-scale-header-line
                            ;; Interval in seconds to delay a table resize post window size change.
                            (auto-resize-delay 0.15)
                            close-action
                            extra-data
                            pulse)
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
          :name name
          :duplicate-objects duplicate-objects
          :getter getter
          :formatter formatter
          :displayer displayer
          :use-header-line use-header-line
          :header-intangible header-intangible
          :decor-intangible decor-intangible
          :text-scale-redraw text-scale-redraw
          :text-scale-header-line text-scale-header-line
          :auto-resize-delay auto-resize-delay
          :object-equal object-equal
          :face face
          :header-face header-face
          :marked-face marked-face
          :actions actions
          :keymap keymap
          :separator-width separator-width
          :divider-width divider-width
          :divider-on-header divider-on-header
          :sort-by sort-by
          :sort-indicator sort-indicator
          :sort-indicator-face-ascend sort-indicator-face-ascend
          :sort-indicator-face-descend sort-indicator-face-descend
          :row-text-properties row-text-properties
          :row-colors row-colors
          :column-colors column-colors
          :column-color-function column-color-function
          :row-color-function row-color-function
          :ellipsis ellipsis
          :close-action close-action
          :extra-data extra-data
          :pulse pulse)))
    ;; Store whether the user has specified columns or not.
    (setf (slot-value table '-has-column-spec) (not (not columns)))
    ;; Auto-generate the columns.
    (unless columns
      (unless objects
        (error "Can't auto-generate columns; no objects (vtable `%s')"
               (vtable-name table)))
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
                      (let ((truncate-guess
                             (vtable-column-truncate-guess new-col)))
                        (unless (or (and (integerp truncate-guess)
                                         (> truncate-guess -1))
                                    (null truncate-guess))
                          (error
                           "column `%s' truncate-guess must be nil or >= 0 (vtable `%s')"
                           (vtable-column-name new-col)
                           (vtable-name table))))
                      new-col))
                  columns))
    ;; Compute the balance of column data.
    (vtable--initialize-columns table)
    ;; Compute the colors.
    (when (or row-colors column-colors)
      (setf (slot-value table '-cached-colors)
            (vtable--compute-colors row-colors column-colors)))
    ;; Compute the divider. For alignment, use the same face in the body
    ;; and on the header.
    (when divider
      (let ((div (propertize (copy-sequence divider)
                             'mouse-face 'highlight)))
        (add-face-text-property 0 (length div) face 'append div)
        (when (vtable-decor-intangible table)
          (add-text-properties 0 (length div)
                               (list 'cursor-intangible t
                                     'front-sticky t
                                     'rear-nonsticky t)
                               div))
        (setf (vtable-divider table) div)))
    ;; To achieve pixel-level alignment, we need introduce the same
    ;; header sort-indicator pixel-precision error in the body and
    ;; column-width computation.
    ;; NOTE: Keep this in sync with the indicator in
    ;; `vtable--insert-header-line'.
    (let ((indicator-pad (propertize
                          (make-string 2 ?\s)
                          'display
                          (list 'space-width vtable-sort-indicator-pad-space-width))))
      (when decor-intangible
        (add-text-properties 0 (length indicator-pad)
                             (list
                              'cursor-intangible t
                              'front-sticky t
                              'rear-nonsticky t)
                             indicator-pad))
      (setf (slot-value table '-indicator-pad) indicator-pad))
    ;; Compute the keymaps.
    (let ((keymap (vtable--make-keymap table use-navigation-keymap)))
      (setf (slot-value table '-cached-keymap) keymap)
      (setf (slot-value table '-cached-drag-keymap)
            (make-composed-keymap keymap
                                  vtable-drag-resize-column-map)))
    (progn
      (unless sort-by
        (seq-do-indexed (lambda (column index)
                          (when (vtable-column-primary column)
                            (push (cons index (vtable-column-primary column))
                                  (vtable-sort-by table))))
                        (vtable-columns table)))
      (when (vtable-sort-by table)
        (setf (slot-value table '-orig-sort-by) (vtable-sort-by table))))
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

(defun vtable-buffer (&optional table)
  "Return the buffer associated with TABLE.
If TABLE is nil, use the table under point.  Return nil if the table has
not yet been inserted into a buffer."
  (slot-value (or table (vtable-current-table))
              '-buffer))

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

(defun vtable-goto-beginning-of-table ()
  "Move point to the first row of the current table.
If the table is empty and if `use-header-line' is nil, point is moved to
the header.

If no table is found, point is moved to the start of the buffer."
  (interactive)
  (if (or (text-property-search-backward 'vtable (vtable-current-table) #'eq)
          (get-text-property (point) 'vtable))
      ;; Accommodate header in buffer vs. header-line.  Attempt to move
      ;; point into the first object's line.
      (progn
        (when (get-text-property (point) 'vtable-header)
          (forward-line)
          (unless (vtable-current-object)
            (forward-line -1)))
        (point))
    (goto-char (point-min))))

(defun vtable-end-of-table ()
  "Move point to the end of the current table.
Point will be moved to the character following the table, not the last
row.  Use `vtable-goto-end-of-table' to keep point within table bounds.

If no table is found, point is moved to the end of the buffer."
  (if (text-property-search-forward 'vtable (vtable-current-table) #'eq)
      (point)
    (goto-char (point-max))))

(defun vtable-goto-end-of-table ()
  "Go to the end of the current table, keeping point within table bounds."
  (interactive)
  (vtable-end-of-table)
  ;; Keep point within table bounds.
  (vtable-previous-line)
  (vtable-next-line))

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
If TABLE is found, return the position of the start of the table.  If it
can't be found, return nil and don't move point.

If `:use-header-line' is nil, the header line is part of the buffer and
point will be moved to the header.  Use `vtable-goto-beginning-of-table'
to move point to the first data row of the table, if data exist."
  (let ((start (point)))
    (goto-char (point-min))
    (if-let* ((match (text-property-search-forward 'vtable table t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      nil)))

(defun vtable--buffer-tables ()
  "Return a list of vtables in the current buffer.
The list is returned in the order tables were found.
If no tables are found, return nil."
  (let (vtables)
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward 'vtable)))
        (push (prop-match-value match) vtables)))
    (nreverse vtables)))

(defun vtable--maybe-set-window-point (&optional pos)
  (setq pos (or pos (point)))
  (unless (eq (selected-window) (get-buffer-window (current-buffer)))
    (set-window-point (get-buffer-window (current-buffer)) pos))
  pos)

(defun vtable-goto-column (column &optional maybe-set-window-point)
  "Go to COLUMN on the current line."
  (beginning-of-line)
  (if-let* ((match (text-property-search-forward 'vtable-column column t)))
      (goto-char (prop-match-beginning match))
    (end-of-line))
  (when maybe-set-window-point
    (vtable--maybe-set-window-point))
  (point))

(defun vtable-beginning-of-table-line-number ()
  "Absolute buffer line number of the start of the current table."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos (vtable-goto-beginning-of-table)))))

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

(defun vtable-update-object (table object
                                   &optional
                                   old-object
                                   sort-after)
  "Update OBJECT's representation in TABLE.
If OLD-OBJECT is non-nil, replace OLD-OBJECT with OBJECT and display it.
In either case, if the existing object is not found in the table, signal
an error.

If SORT-AFTER is non-nil, sort and redisplay the table after the object
is updated.

Note a limitation: if TABLE's buffer is not in a visible window, or if its
window has changed width since it was updated, updating the TABLE is not
possible, and an error is signaled."
  (with-current-buffer (vtable-buffer table)
    (unless old-object
      (setq old-object object))
    (unless (and (not (funcall (vtable-object-equal table) old-object object))
                 (vtable--handle-duplicate-object table object))
      (let ((objects (vtable-objects table))
            (cache (vtable--ensure-cache table))
            (inhibit-read-only t))
        ;; First replace the object in the object storage.
        (if (funcall (vtable-object-equal table) old-object (car objects))
            ;; It's at the head, so replace it there.
            (setf (vtable-objects table)
                  (cons object (cdr objects)))
          ;; Otherwise splice into the list.
          (while (and (cdr objects)
                      (not (funcall (vtable-object-equal table)
                                    (cadr objects) old-object)))
            (setq objects (cdr objects)))
          (unless (and objects
                       (funcall (vtable-object-equal table)
                                (cadr objects) old-object))
            (error "Can't find the old object (vtable `%s')"
                   (vtable-name table)))
          (setcar (cdr objects) object))
        ;; Then update the cache...
        (if-let* ((line-number (seq-position
                                (car cache)
                                (assoc old-object
                                       (car cache)
                                       (vtable-object-equal table))))
                  (line (elt (car cache) line-number)))
            (progn
              (setcar line object)
              (setcdr line (vtable--compute-cached-line table object))
              ;; ...and redisplay the line in question.
              ;;
              ;; Keep point stable if table is the current table and the
              ;; object being updated is the current object; i.e., where
              ;; point is.
              (let ((orig-column
                     (when (and (eq table (vtable-current-table))
                                (eq old-object (vtable-current-object)))
                       (vtable-current-column))))
                (save-excursion
                  ;; If point is not already in the table.
                  (unless orig-column
                    (vtable-goto-table table))
                  (unless (vtable-goto-object old-object)
                    (error "Can't find old-object (vtable `%s')"
                           (vtable-name table)))
                  (delete-line)
                  (vtable--insert-line table line line-number
                                       (nth 1 cache)))
                (when orig-column
                  (vtable-goto-object object)
                  (vtable-goto-column orig-column 'maybe-set-window-point)))
              ;; We may have inserted a non-numerical value into a previously
              ;; all-numerical table, so recompute.
              (vtable--maybe-recompute-numerical-from-cache-line table (cdr line))
              ;; Cache coherence.
              (vtable--tick-objects table)
              (vtable--cache-tick-sync table cache)
              (when sort-after
                ;; Revert does redisplay, and we call it only if sort
                ;; actually sorted.
                (when (vtable--sort table cache)
                  (vtable-revert table)))
              (when (memq 'update (vtable-pulse table))
                (save-excursion
                  (unless (eq table (vtable-current-table))
                    (vtable-goto-table table))
                  (vtable-goto-object object)
                  (pulse-momentary-highlight-one-line))))
          ;; At this point, the object was updated in objects, but not the
          ;; cache, which will be considered stale.
          (error "Can't find cached object (vtable `%s')"
                 (vtable-name table)))))))

(defun vtable-remove-object (table object
                                   &optional
                                   retain-rows
                                   inhibit-row-redisplay)
  "Remove OBJECT from TABLE.
This will also remove the displayed line, and the object will be unmarked.

If the object is not found in the table, signal and error.

Rows below the removed object are redisplayed to update row colors, if
present.  If INHIBIT-ROW-REDISPLAY is non-nil, this redisplay is
inhibited.  This is useful for batch updates.  Call `vtable-revert' or
`vtable-redisplay-range' at the end of a batch to update row colors.

If RETAIN-ROWS is nil, all rows can be removed.  If it is an integer,
retain that many rows.  If it is t, prevent removing rows."
  (when (and (not (eq retain-rows t))
             (or (not retain-rows)
                 (and (integerp retain-rows)
                      (length> (vtable-objects table) retain-rows))))
    (with-current-buffer (vtable-buffer table)
      (let ((cache (vtable--ensure-cache table))
            (inhibit-read-only t))
        (unless (seq-contains-p (vtable-objects table)
                                object
                                (vtable-object-equal table))
          (error "Can't find the object to remove (vtable `%s')"
                 (vtable-name table)))
        ;; First remove from the objects.
        (setf (vtable-objects table) (seq-remove
                                      (lambda (elt)
                                        (funcall (vtable-object-equal table)
                                                 elt object))
                                      (vtable-objects table)))
        ;; Then unmark the object.
        (vtable--unmark-object table object 'inhibit-update)
        ;; Then adjust the cache and display.
        (if-let* ((old-line (assoc object
                                   (car cache)
                                   (vtable-object-equal table))))
            (progn
              ;; We save the current column if table is the current table
              ;; and the object being removed is the current object; i.e.,
              ;; where point is.  delete-region, if point is in the
              ;; deleted region, will force point to point-min if the
              ;; region point is in is deleted.
              (let ((orig-column
                     (when (and (eq table (vtable-current-table))
                                (eq object (vtable-current-object)))
                       (vtable-current-column)))
                    line-index)
                (setcar cache (delq old-line (car cache)))
                (save-excursion
                  ;; If point is not already in the table.
                  (unless orig-column
                    (vtable-goto-table table))
                  (when (vtable-goto-object object)
                    (setq line-index (- (line-number-at-pos (point))
                                        (vtable-beginning-of-table-line-number)))
                    (delete-line)
                    (when (memq 'remove (vtable-pulse table))
                      (when (save-excursion
                              (forward-line)
                              (if (eq table (vtable-current-table))
                                  t
                                (forward-line -1)
                                (eq table (vtable-current-table))))
                        (forward-line))
                      (pulse-momentary-highlight-one-line))))
                ;; Cache coherence.
                (vtable--tick-objects table)
                (vtable--cache-tick-sync table cache)
                ;; Now redisplay the lines below if there are row/column colors.
                (unless inhibit-row-redisplay
                  (vtable--maybe-redisplay-range table line-index nil cache
                                                 'inhibit-restore-point))
                ;; We may have removed a non-numerical value from a table that is
                ;; now all-numerical, so recompute.
                (vtable--maybe-recompute-numerical-from-cache-line table (cdr old-line))
                ;; orig-point is non-nil if this table was
                ;; vtable-current-table.
                (when orig-column
                  ;; Keep point within table bounds in case point moved
                  ;; beyond the current table removing the final row.
                  (unless (eq table (vtable-current-table))
                    (vtable-goto-table table)
                    (vtable-goto-end-of-table))
                  (unless (vtable-current-object)
                    (vtable-previous-line))
                  (vtable-goto-column orig-column 'maybe-set-window-point))))
          ;; At this point, the object was removed from objects, but not
          ;; the cache, which will be considered stale.
          (error "Can't find cached object (vtable `%s')"
                 (vtable-name table)))))))

;; FIXME: The fact that the `location' argument of
;; `vtable-insert-object' can be an integer and is then interpreted as
;; an index precludes the use of integers as objects.  This seems a very
;; unlikely use-case, so let's just accept this limitation.

(defun vtable-insert-object (table object
                                   &optional
                                   location
                                   before
                                   select-after
                                   sort-after
                                   inhibit-row-redisplay)
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

Rows below the inserted object are redisplayed to update row colors, if
present.  If INHIBIT-ROW-REDISPLAY is non-nil, this redisplay is
inhibited.  This can be useful for batch updates.  Call `vtable-revert'
or `vtable-redisplay-range' at the end of a batch to update row colors.

Note: INHIBIT-ROW-REDISPLAY has no effect if SORT-AFTER is non-nil.

If SELECT-AFTER is non-nil, the new object is selected.  Otherwise,
whatever object is selected remains selected.

If SORT-AFTER is non-nil, sort the table after the object is inserted
and update the display, if necessary.

Consult the table's `:duplicate-objects' property."
  (with-current-buffer (vtable-buffer table)
    ;; If the vtable is empty, just add the object and regenerate the
    ;; table.
    (if (null (vtable-objects table))
        (progn
          (setf (vtable-objects table) (list object))
          ;; No need to tick the cache, it will be refreshed.
          (vtable--tick-objects table)
          (vtable--maybe-recompute-numerical-from-cache-line
           table
           (vtable--compute-cached-line table object))
          (save-excursion
            (vtable-goto-table table)
            (vtable-revert-command table)))
      (unless (vtable--handle-duplicate-object table object)
        (let ((cache (vtable--ensure-cache table)))
          ;; First insert into the objects.
          (let ((pos (if location
                         (if (integerp location)
                             (if (vtable--cache-sorted-p cache)
                                 (error "Unsort the vtable to insert by integer location (vtable `%s')"
                                        (vtable-name table))
                               (prog1
                                   (nthcdr location (vtable-objects table))
                                 ;; Do not prepend if index is too large:
                                 (setq before nil)))
                           (or
                            (let ((loc (vtable-objects table)))
                              (while (and (cdr loc)
                                          (not (funcall (vtable-object-equal table)
                                                        (car loc) location)))
                                (setq loc (cdr loc)))
                              (if (funcall (vtable-object-equal table)
                                           (car loc) location)
                                  loc
                                nil))
                            ;; Prepend if `location' is not found and
                            ;; `before' is non-nil:
                            (and before (vtable-objects table))))
                       ;; If `location' is nil and `before' is non-nil, we
                       ;; prepend the new object.
                       (if before (vtable-objects table)))))
            (if (or before ; If `before' is non-nil, `pos' should be, as well.
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
          ;; Cache coherence.  There is a non-local exit, via error, below;
          ;; cache will be stale on error as the ticks will not match up.
          (vtable--tick-objects table)
          ;; Then adjust the cache and display.
          (let (orig-object
                orig-column
                line
                line-index)
            (when (eq table (vtable-current-table))
              (setq orig-object (vtable-current-object)
                    orig-column (vtable-current-column)))
            (save-excursion
              ;; If point is not already in the table.
              (unless orig-object
                (vtable-goto-table table))
              (let* ((inhibit-read-only t)
                     (ellipsis (vtable-ellipsis table))
                     (elem (if location ; location mirrors `pos', above.
                               (if (integerp location)
                                   (nth location (car cache))
                                 (or (assoc
                                      location
                                      (car cache)
                                      (vtable-object-equal table))
                                     (and before (caar cache))))
                             (if before (caar cache))))
                     (pos (memq elem (car cache))))
                (setq line
                      (cons object (vtable--compute-cached-line table object)))
                (if (or before
                        (and pos (integerp location)))
                    ;; Add the new object before.
                    (let ((old-line (car pos)))
                      (setcar pos line)
                      (setcdr pos (cons old-line (cdr pos)))
                      (unless (vtable-goto-object (car elem))
                        (vtable-goto-beginning-of-table)))
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
                (setq line-index (- (line-number-at-pos (point))
                                    (vtable-beginning-of-table-line-number)))
                (forward-line 0)
                (vtable--insert-line table line line-index
                                     (nth 1 cache)
                                     ellipsis)))
            ;; We may have inserted a non-numerical value into a previously
            ;; all-numerical table, so recompute.
            (vtable--maybe-recompute-numerical-from-cache-line table (cdr line))
            ;; Cache coherence.
            (vtable--cache-tick-sync table cache)
            (if sort-after
                ;; Revert does redisplay, and we call it only if sort
                ;; actually sorted, otherwise it is safe to call
                ;; vtable--redisplay-range.
                (when (vtable--sort table cache)
                  (vtable-revert table))
              ;; Now redisplay the lines below if there are row/column
              ;; colors, if necessary.
              (unless inhibit-row-redisplay
                (vtable--maybe-redisplay-range table (1+ line-index) nil cache
                                               'inhibit-restore-point)))
            (when (memq 'insert (vtable-pulse table))
              (save-excursion
                (vtable-goto-object object)
                (pulse-momentary-highlight-one-line)))
            ;; orig-object is non-nil if this table is
            ;; vtable-current-table and is not empty.
            (when orig-object
              (if select-after
                  (vtable-goto-object object)
                (vtable-goto-object orig-object))
              (vtable-goto-column orig-column 'maybe-set-window-point))))))))

(defun vtable--redisplay-range (table &optional
                                      from-line to-line
                                      cache
                                      inhibit-restore-point)
  "Update row/column colors for TABLE.
If FROM-LINE is nil, start at 0, the first line.
If TO-LINE is nil, end at the last line, the number of objects.
If CACHE is non-nil, use that copy, or retrieve the cache.
If INHIBIT-RESTORE-POINT is non-nil, assume the caller handles point."
  (with-current-buffer (vtable-buffer table)
    (let ((line-index (or from-line 0))
          (num-objects (length (vtable-objects table)))
          (inhibit-read-only t))
      (setq to-line (or to-line (1- num-objects)))
      (when (< to-line line-index)
        (setq line-index (prog1 to-line
                           (setq to-line line-index))))
      (when (< line-index num-objects)
        ;; We save the location if table is the current table, as
        ;; delete-region, via delete-line, will move point to point-min when
        ;; the line point is on gets deleted.
        (let (orig-point
              orig-column)
          (when (eq table (vtable-current-table))
            (setq orig-point (point)
                  orig-column (vtable-current-column)))
          (save-excursion
            ;; If point is not already in the table.
            (unless orig-point
              (vtable-goto-table table))
            (setq cache (or cache (vtable--ensure-cache table)))
            (vtable-goto-object (car (elt (car cache) line-index)))
            (while (<= line-index to-line)
              (let ((line (elt (car cache) line-index)))
                (delete-line)
                (vtable--insert-line table line line-index
                                     (nth 1 cache)))
              (cl-incf line-index)))
          (when (and (not inhibit-restore-point)
                     orig-point)
            (goto-char orig-point)
            (vtable-goto-column orig-column 'maybe-set-window-point)))))))

(defun vtable--maybe-redisplay-range (table &optional
                                            from-line to-line
                                            cache
                                            inhibit-restore-point)
  "Update row/column colors for TABLE if there are row/column colors.
If FROM-LINE is nil, start at 0, the first line.
If TO-LINE is nil, end at the last line, the number of objects.
If CACHE is non-nil, use that copy.
If INHIBIT-RESTORE-POINT is non-nil, assume the caller handles point."
  (when (or (vtable-column-colors table)
            (vtable-row-colors table)
            (vtable-column-color-function table)
            (vtable-row-color-function table))
    (vtable--redisplay-range table
                             from-line to-line
                             cache
                             inhibit-restore-point)))

(defun vtable-maybe-redisplay-range (table &optional from-line to-line)
  "Update row/column colors for TABLE if there are row/column colors.
If FROM-LINE is nil, start at 0, the first line.
If TO-LINE is nil, end at the last line, the number of objects."
  (vtable--maybe-redisplay-range table from-line to-line))

(defun vtable-redisplay-range (table &optional from-line to-line)
  "Update row/column colors for TABLE.
If FROM-LINE is nil, start at 0, the first line.
If TO-LINE is nil, end at the last line, the number of objects."
  (vtable--redisplay-range table from-line to-line))

(defun vtable-column (table index)
  "Return the name of the 0-based INDEXth column in TABLE."
  (vtable-column-name (elt (vtable-columns table) index)))

;;; Generating the table.

(defun vtable--some-objects (table object)
  (seq-some (lambda (elt)
              (funcall (vtable-object-equal table) elt object))
            (vtable-objects table)))

(defun vtable--handle-duplicate-object (table new-object)
  "Return nil if NEW-OBJECT is acceptable in TABLE, t if not.
Return nil if duplicates are allowed.
Return t if duplicate found and should be ignored.
Signal an error if duplicate found and disallowed."
  (pcase-exhaustive (vtable-duplicate-objects table)
    ('allow
     nil)
    ('ignore
     (vtable--some-objects table new-object))
    ('ignore-warn
     (if (vtable--some-objects table new-object)
         (progn
           (message "Warning: duplicate object ignored (vtable `%s')"
                    (vtable-name table))
           t)
       nil))
    ('error
     (when (vtable--some-objects table new-object)
       (error "Duplicate objects not allowed (vtable `%s')"
              (vtable-name table))))))

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

(defun vtable--initialize-columns (table)
  "Compute column specs for TABLE.
Set the `align', `-aligned' and `-numerical' properties of each column.

`-aligned' indicates whether the column has an `align' property set by
the user.  If it does, `align' is not touched, otherwise it is set to
`right' for numeric columns and to `left' for non-numeric columns.

Scan the initially provided objects for numericalness on columns with
their `numeric' property as \\='infer, and set the column's alignment
and numeric flags, if needed.

Columns with `numeric' set to nil are never scanned and are assumed to
be non-numeric, or have their own comparator.  Columns with `numeric'
set to t are never scanned and assumed to be numeric."
  (dolist (column (vtable-columns table))
    ;; Check if any columns have an explicit `align' property.
    (if (vtable-column-align column)
        (setf (vtable-column--aligned column) t)
      ;; Default to 'right until data appear.
      (setf (vtable-column-align column) 'right))
    ;; Set the default numeric or non-numeric sort flags.
    (if (eq (vtable-column-numeric column) t)
        ;; Explicitly numeric.
        (setf (vtable-column--numerical column) t)
      ;; Default to non-numeric.  'infer columns will be computed later.
      (setf (vtable-column--numerical column) nil)))
  (vtable--recompute-numerical-from-objects table))

(defun vtable--recompute-numerical-from-objects (table)
  "Recompute numericalness of columns for TABLE from its objects."
  (let ((numerical (make-vector (length (vtable-columns table)) t))
        (columns (vtable-columns table))
        post-process)
    (dolist (object (vtable-objects table))
      (seq-do-indexed
       (lambda (column index)
         (pcase-exhaustive (vtable-column-numeric column)
           ;; Explicitly non-numeric.
           ((pred null)
            (setf (elt numerical index) nil))
           ;; Explicitly numeric.
           ((pred (eq t))
            ;; no-op, default is t.
            )
           ('infer
            (unless (numberp (vtable--get-value object index
                                                (elt columns index)
                                                table))
              (setf (elt numerical index) nil))))
         (setq post-process t))
       columns))
    ;; Compute alignment for inferred numeric columns without explicit
    ;; alignment, and set numerical sort flags.
    (when post-process
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
               (elt numerical index)))
       (vtable-columns table)))))

(defun vtable--maybe-recompute-numerical-from-cache-line (table line)
  "Recompute numericalness of columns for TABLE from cache LINE."
  (let ((columns (vtable-columns table))
        (recompute nil))
    (seq-do-indexed
     (lambda (elem index)
       (let ((column (elt columns index)))
         (when (and (eq (vtable-column-numeric column) 'infer)
                    (vtable-column--numerical column)
                    (not (numberp (car elem))))
           (setq recompute t))))
     line)
    (when recompute
      (vtable--recompute-numerical-from-objects table))))

(defun vtable--spacer (table)
  (vtable--text-scale-pixels
   (vtable--compute-width table (vtable-separator-width table))))

(defun vtable--divider (table)
  (or (vtable-divider table)
      (when (vtable-divider-width table)
        (propertize " "
                    'display (list 'space :width
                                   (list
                                    (vtable--text-scale-pixels
                                     (vtable--compute-width
                                      table (vtable-divider-width table)))))))))

(defun vtable--recompute-cache (table)
  (let* ((data (vtable--compute-cache table))
         (widths (vtable--compute-widths table data))
         (cache (list
                 data
                 widths
                 (vtable--objects-tick table)
                 ;; Cache sorted flag.
                 nil)))
    (vtable--sort table cache)
    (setf (gethash (vtable--cache-key table)
                   (slot-value table '-cache))
          cache)))

(defun vtable--cache-tick-sync (table cache)
  (setf (nth 2 cache)
        (vtable--objects-tick table)))

(defun vtable--cache-tick-valid-p (table cache)
  (eq (nth 2 cache)
      (vtable--objects-tick table)))

(defun vtable--cache-set-sorted (cache sorted)
  (setf (nth 3 cache) sorted))

(defun vtable--cache-sorted-p (cache)
  (nth 3 cache))

(defun vtable--ensure-cache (table)
  (or (vtable--cache table)
      (vtable--recompute-cache table)))

(defvar-local vtable--post-command-hooked nil)
(defvar-local vtable--display-line-numbers nil)
(defvar text-scale-remap-header-line) ; this is buffer local

(defun vtable-insert (table)
  "Insert TABLE into the current buffer.
The current buffer will be set as TABLE's buffer.  If this is done more
than once, or if the table is attempted to be inserted more than once
into the same buffer, signal an error."
  (if-let* ((table-buffer (vtable-buffer table)))
      (if (eq table-buffer (current-buffer))
          (error "A vtable cannot be inserted more than once into a buffer (vtable `%s')"
                 (vtable-name table))
        (error "A vtable cannot be inserted into more than one buffer (vtable `%s')"
               (vtable-name table))))
  (setf (slot-value table '-buffer) (current-buffer))
  (vtable--insert table)
  (when (and (vtable-use-header-line table)
             (vtable-text-scale-header-line table))
    (setq text-scale-remap-header-line t))
  (unless vtable--post-command-hooked
    (add-hook 'post-command-hook
              (lambda ()
                (unless (eq vtable--display-line-numbers display-line-numbers)
                  (setq vtable--display-line-numbers display-line-numbers)
                  (vtable-revert table)))
              nil 'local)
    (setq-local vtable--post-command-hooked table)))

(defun vtable--insert (table)
  (let* ((start (point))
         (ellipsis (vtable-ellipsis table))
         ;; We maintain a cache per screen/window width, so that we render
         ;; correctly if Emacs is open on two different screens (or the
         ;; user resizes the frame).
         (widths (vtable--widths table)))
    ;; Don't insert any header or header line if the user hasn't
    ;; specified the columns.
    (when (slot-value table '-has-column-spec)
      (if (vtable-use-header-line table)
          (vtable--set-header-line table widths)
        ;; Insert the header line directly into the buffer, and put a
        ;; keymap to be able to sort the columns there (by clicking on
        ;; them).
        (vtable--insert-header-line table widths)
        (add-text-properties start (point)
                             (list 'rear-nonsticky t
                                   'vtable-header t
                                   'vtable table))
        (when (vtable-header-intangible table)
          (add-text-properties start (point)
                               (list 'cursor-intangible t
                                     'front-sticky t
                                     'rear-nonsticky t))
          (cursor-intangible-mode))
        (setq start (point))))
    ;; The cache will be sorted if there are sort criteria.
    (let ((cache (vtable--ensure-cache table)))
      ;; Insert the data.
      (let ((line-number 0))
        (dolist (line (car cache))
          (vtable--insert-line table line line-number widths ellipsis)
          (setq line-number (1+ line-number))))
      (add-text-properties start (point)
                           (list 'rear-nonsticky t
                                 'vtable table))
      (goto-char start))
    (when (vtable-decor-intangible table)
      (cursor-intangible-mode))
    ;; The 0-delay idle timer avoids resizing immediately after the next
    ;; redisplay cycle ends.
    (run-with-idle-timer
     0 nil
     (lambda ()
       ;; window-size-change-functions get called when text-scale-mode
       ;; causes a window size change, so we debounce both.
       (when (vtable-text-scale-redraw table)
         (add-hook 'text-scale-mode-hook
                   (lambda () (vtable--resize-tables-debouncer table))
                   nil 'local))
       (add-hook 'window-size-change-functions
                 (lambda (window) (vtable--resize-tables-debouncer table window))
                 nil 'local)
       (add-hook 'window-selection-change-functions
                 (lambda (window) (vtable--refresh-window-cache table window))
                 nil 'local)))))

(defun vtable--refresh-window-cache (table window)
  ;; If a table's buffer window is selected, refresh the table's
  ;; window-width cache, if necessary.  This is called from a
  ;; buffer-local hook, so current-buffer is the table's buffer.
  (when (and (eq window (selected-window))
             (buffer-live-p (window-buffer window))
             (eq (window-buffer window) (current-buffer)))
    (unless (vtable--cache table)
      (vtable-revert table))))

(defvar-local vtable--resize-tables-timer nil)

(defun vtable--resize-tables-debouncer (table &optional window)
  (when (timerp vtable--resize-tables-timer)
    (cancel-timer vtable--resize-tables-timer))
  ;; Ensure the window is associated with the table.  This is called
  ;; from a buffer-local hook, so current-buffer is the table's buffer.
  (when (eq (window-buffer (or window (selected-window)))
            (current-buffer))
    (setq vtable--resize-tables-timer
          (run-with-timer (vtable-auto-resize-delay table)
                          nil
                          (lambda ()
                            (when (timerp vtable--resize-tables-timer)
                              (cancel-timer vtable--resize-tables-timer))
                            (setq vtable--resize-tables-timer nil)
                            (vtable--resize-tables))))))

(defun vtable--resize-table (table)
  ;; Clear the cache to refresh scaled widths.
  (vtable--clear-cache table)
  (vtable-revert table))

(defun vtable--resize-tables ()
  (dolist (table (vtable--buffer-tables))
    (vtable--resize-table table)))

(defun vtable--insert-line (table line line-number widths
                                  &optional ellipsis)
  (let* ((start (point))
         (buffer (vtable-buffer table))
         (body-face (vtable-face table))
         (columns (vtable-columns table))
         (column-color-function (vtable-column-color-function table))
         (row-color-function (vtable-row-color-function table))
         (column-colors
          (and (vtable-column-colors table)
               (if (vtable-row-colors table)
                   (elt (slot-value table '-cached-colors)
                        (mod line-number (length (vtable-row-colors table))))
                 (slot-value table '-cached-colors))))
         ;; spacer and divider, when in pixels, are text-scale adjusted
         (spacer (vtable--spacer table))
         (divider (vtable--divider table))
         ;; column-width is adjusted by indicator-pad-width for
         ;; pixel-alignment with the header line.
         (indicator-pad-width (string-pixel-width
                               (slot-value table '-indicator-pad) buffer))
         (keymap (slot-value table '-cached-keymap))
         (drag-keymap (slot-value table '-cached-drag-keymap)))
    (seq-do-indexed
     (lambda (elem index)
       (let* ((value (nth 0 elem))
              (column (elt columns index))
              ;; Cached widths will be text-scale adjusted, except for
              ;; widths specified in pixels or percent of window width,
              ;; which are absolute and pixelwise.
              (column-width (- (elt widths index) indicator-pad-width))
              (pre-computed (nth 2 elem)))
         ;; pre-computed already has the body face.
         ;; We add the body face to the formatted value as needed.
         ;; See if we have any formatters here.
         (cond
          ((vtable-column-formatter column)
           (setq value
                 (funcall (vtable-column-formatter column) value)
                 pre-computed nil)
           (add-face-text-property 0 (length value) body-face 'append value))
          ((vtable-formatter table)
           (setq value (funcall (vtable-formatter table)
                                value index table)
                 pre-computed nil)
           (add-face-text-property 0 (length value) body-face 'append value)))
         (let* ((column-start (point))
                (value-length (length (or pre-computed value)))
                ;; ellipsis+ text properties are those of the final value character.
                (ellipsis+ (copy-sequence ellipsis))
                (_ (when (> value-length 0)
                     (add-text-properties
                      0 (length ellipsis+)
                      (text-properties-at (1- value-length)
                                          (or pre-computed value))
                      ellipsis+)))
                (ellipsis-width (string-pixel-width ellipsis+ buffer))
                (clipped-value-width (- column-width ellipsis-width))
                (displayed
                 ;; Allow any displayers to have their say.
                 (cond
                  ((vtable-column-displayer column)
                   (funcall (vtable-column-displayer column)
                            value clipped-value-width table))
                  ((vtable-displayer table)
                   (funcall (vtable-displayer table)
                            value index clipped-value-width table))
                  (pre-computed
                   ;; If we don't have a displayer, use the pre-made
                   ;; (cached) string value.
                   (if (> (nth 1 elem) column-width)
                       (concat
                        (vtable--limit-string
                         pre-computed
                         clipped-value-width
                         buffer
                         (vtable-column-truncate-guess column))
                        ellipsis+)
                     pre-computed))
                  ;; Recompute widths.
                  (t
                   (if (> (string-pixel-width value buffer) column-width)
                       (concat
                        (vtable--limit-string
                         value
                         clipped-value-width
                         buffer
                         (vtable-column-truncate-guess column))
                        ellipsis+)
                     value))))
                (displayed-width
                 ;; Do not text-scale adjust images, they should be
                 ;; scaled via `create-image' :scale set to 1.0.
                 (if (or (vtable-column-inhibit-text-scaling column)
                         (get-display-property 0 'image displayed))
                     (string-pixel-width displayed)
                   (string-pixel-width displayed buffer)))
                ;; Don't insert the separator or divider after the final column.
                (last (= index (- (length line) 2)))
                ;; On the last column, leave one char width.
                (spacer (if last
                            (vtable--char-width table)
                          spacer))
                (spacer-str (propertize " "
                                        ;; 'face (list :box (list :line-width (cons -1 -1) :color "darkgray")) ; Keep for debugging.
                                        'display
                                        (list 'space :width (list spacer))))
                (fill-width (- column-width displayed-width))
                (align (vtable-column-align column))
                fill-str
                fill-right-str)
           (pcase-exhaustive align
             ((or 'left 'right)
              (setq fill-str
                    (propertize
                     " "
                     ;; 'face (list :box (list :line-width (cons -1 -1) :color "white")) ; Keep for debugging.
                     'display
                     (list 'space :width (list fill-width)))))
             ('center
              (let ((half-fill (/ fill-width 2)))
                (setq fill-str
                      (propertize
                       " "
                       ;; 'face (list :box (list :line-width (cons -1 -1) :color "white")) ; Keep for debugging.
                       'display
                       (list 'space :width (list half-fill))))
                (setq fill-right-str
                      (propertize
                       " "
                       ;; 'face (list :box (list :line-width (cons -1 -1) :color "white")) ; Keep for debugging.
                       'display
                       (list 'space :width (list (- fill-width half-fill))))))))
           (when (vtable-decor-intangible table)
             ;; Note: each are different, so take some care.
             (add-text-properties 0 (length spacer-str)
                                  (list 'cursor-intangible t
                                        'front-sticky t
                                        'rear-nonsticky nil)
                                  spacer-str)
             (pcase-exhaustive align
               ('left
                (add-text-properties 0 (length fill-str)
                                     (list 'cursor-intangible t
                                           'front-sticky nil
                                           'rear-nonsticky t)
                                     fill-str))
               ('right
                (add-text-properties 0 (length fill-str)
                                     (list 'cursor-intangible t
                                           'front-sticky nil
                                           'rear-nonsticky nil)
                                     fill-str))
               ('center
                (add-text-properties 0 (length fill-str)
                                     (list 'cursor-intangible t
                                           'front-sticky nil
                                           'rear-nonsticky t)
                                     fill-str)
                (add-text-properties 0 (length fill-right-str)
                                     (list 'cursor-intangible t
                                           'front-sticky nil
                                           'rear-nonsticky nil)
                                     fill-right-str))))
           (pcase-exhaustive align
             ('left
              (insert
               displayed
               fill-str
               (slot-value table '-indicator-pad)
               spacer-str))
             ('right
              (insert
               fill-str
               (slot-value table '-indicator-pad)
               displayed
               spacer-str))
             ('center
              (insert
               fill-str
               displayed
               fill-right-str
               (slot-value table '-indicator-pad)
               spacer-str)))
           (when (or column-color-function column-colors)
             (add-face-text-property
              column-start (point)
              (if column-color-function
                  (funcall column-color-function
                           line-number index value (car line) column-colors)
                (elt column-colors (mod index (length column-colors))))))
           ;; The column header keymap includes sorting.
           (put-text-property column-start (point)
                              'keymap keymap)
           ;; The divider keymap adds drag to resize.
           (when (and divider (not last))
             (insert
              (propertize divider
                          'keymap drag-keymap)))
           (put-text-property column-start (point) 'vtable-column index))))
     (cdr line))
    (insert (propertize "\n" 'keymap keymap))
    (add-text-properties start (point)
                         (list 'vtable-object (car line)
                               'vtable table))
    (when (vtable-object-marked-p table (car line))
      (add-face-text-property start (point)
                              (vtable-marked-face table)))
    (let ((row-colors (slot-value table '-cached-colors)))
      (cond
       (row-color-function
        (add-face-text-property
         start (point)
         (funcall row-color-function
                  line-number (car line) row-colors)))
       ((and row-colors
             (null column-colors))
        (add-face-text-property
         start (point)
         (elt row-colors (mod line-number (length row-colors)))))))
    (when (vtable-row-text-properties table)
      (save-excursion
        (forward-line -1)
        (add-text-properties (pos-bol) (pos-eol)
                             (vtable-row-text-properties table))))))

(defvar vtable--inhibit-objects-tick nil
  "If non-nil, objects tick will not be increased.  This is bound by
object marking functions which do not alter object state.")

(defun vtable--objects-tick (table)
  (slot-value table '-objects-tick))

(defun vtable--tick-objects (table)
  (unless vtable--inhibit-objects-tick
    (setf (slot-value table '-objects-tick)
          (1+ (slot-value table '-objects-tick)))))

(defun vtable--cache-key (table)
  (let ((window (selected-window)))
    (if (and (window-live-p window)
             (eq (window-buffer window) (vtable-buffer table)))
        (cons (frame-terminal) (window-body-width window 'remap))
      ;; Default key for an active table not currently displayed in a
      ;; window, or if the `selected-window' is a non-table window.
      (cons t t))))

(defun vtable--cache (table)
  (let ((cache (gethash (vtable--cache-key table) (slot-value table '-cache))))
    (if (length= (car cache) 0)
        ;; Force an empty cache to be populated; this can occur at
        ;; vtable initialization.
        (progn
          (vtable--clear-cache table)
          nil)
      (if (vtable--cache-tick-valid-p table cache)
          cache
        ;; Force a stale cache to be repopulated.
        (vtable--clear-cache table)
        nil))))

(defun vtable--clear-cache (table)
  (setf (gethash (vtable--cache-key table) (slot-value table '-cache)) nil))

(defun vtable--clear-all-caches (table)
  (clrhash (slot-value table '-cache)))

(defun vtable--sort (table cache)
  "Sort the TABLE CACHE (not its objects) if there are sort criteria.
Return non-nil if sorted, nil otherwise."
  (let (sorted)
    (pcase-dolist (`(,index . ,direction) (vtable-sort-by table))
      (let* ((column (elt (vtable-columns table) index))
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
                                 (format "%s" (car c2))))))))))
        (setq sorted t)
        (vtable--cache-set-sorted cache t)))
    sorted))

(defun vtable--indicator (table index)
  (let ((order (car (last (vtable-sort-by table)))))
    (when (eq index (car order))
      (let* ((dir (cdr order))
             (n (if (eq dir 'ascend) 1 0)))
        ;; We're sorting by this column last, so return an indicator.
        (catch 'found
          (dolist (candidate (nth n (vtable-sort-indicator table)))
            (when (char-displayable-p candidate)
              (throw 'found (cons (string candidate) dir))))
          (cons nil nil))))))

(defun vtable--insert-header-line (table widths)
  ;; Insert the header directly into the buffer.
  (let* ((start (point))
         (buffer (vtable-buffer table))
         (header-face (vtable-header-face table))
         ;; spacer and divider, when in pixels, are text-scale adjusted
         (spacer (vtable--spacer table))
         (divider (vtable--divider table))
         (divider-pixels (string-pixel-width divider buffer))
         (divider-on-header (vtable-divider-on-header table))
         (drag-keymap (slot-value table '-cached-drag-keymap)))
    (seq-do-indexed
     (lambda (column index)
       (let* ((name (vtable-column-name column))
              (_ (add-face-text-property 0 (length name)
                                         header-face 'append name))
              (_ (add-text-properties 0 (length name)
                                      (list 'mouse-face 'header-line-highlight)
                                      name))
              (start (point))
              (column-width (elt widths index))
              (align (or (vtable-column-header-align column)
                         (vtable-column-align column)))
              (indicator+dir (vtable--indicator table index))
              ;; Pad the indicator to avoid abutting its neighbors.
              (indicator (if (car indicator+dir)
                             (propertize
                              (concat " " (car indicator+dir) " ") ; two single spaces
                              'face header-face
                              ;; xdisp.c
                              ;; it->pixel_width *= XFLOATINT (it->space_width);
                              ;; introduces single-pixel precision errors in the
                              ;; display engine.
                              ;;
                              ;; NOTE: Keep this in sync with the
                              ;; pre-computed indicator-pad used in
                              ;; `vtable--insert-line'.
                              'display
                              (list 'space-width
                                    vtable-sort-indicator-pad-space-width))
                           ""))
              (_ (add-face-text-property 0 (length indicator)
                                         (if (eq (cdr indicator+dir) 'ascend)
                                             (vtable-sort-indicator-face-ascend table)
                                           (vtable-sort-indicator-face-descend table))
                                         nil indicator))
              (indicator-width (string-pixel-width indicator buffer))
              ;; Don't insert the separator or divider after the final column.
              (last (= index (1- (length (vtable-columns table)))))
              (spacer (if last
                          0
                        spacer))
              (spacer-str (if last ""
                            (propertize " "
                                        ;; 'face (list :box (list :line-width (cons -1 -1) :color "darkgray")) ; Keep for debugging.
                                        'display
                                        (list 'space :width (list spacer))))))
         (let* ((max-name-width (- column-width
                                   indicator-width))
                (displayed (vtable--limit-string
                            name
                            max-name-width
                            buffer
                            (vtable-column-truncate-guess column)))
                (displayed-width (string-pixel-width displayed buffer))
                (fill-width
                 ;; Adjust for very small column widths; e.g., 1 character wide.
                 (max 0 (- column-width
                           displayed-width
                           indicator-width)))
                fill-str
                fill-right-str)
           (pcase-exhaustive align
             ((or 'left 'right)
              (setq fill-str
                    (propertize " "
                                ;; 'face (list :box (list :line-width (cons -1 -1) :color "white")) ; Keep for debugging.
                                'display
                                (list 'space :width (list fill-width)))))
             ('center
              (let ((half-fill (/ (+ fill-width indicator-width) 2)))
                (setq fill-str
                      (propertize " "
                                  ;; 'face (list :box (list :line-width (cons -1 -1) :color "white")) ; Keep for debugging.
                                  'display
                                  (list 'space :width (list half-fill))))
                (setq fill-right-str
                      (propertize " "
                                  ;; 'face (list :box (list :line-width (cons -1 -1) :color "white")) ; Keep for debugging.
                                  'display
                                  (list 'space :width (list (- fill-width half-fill))))))))
           ;; If the table is already wider than the window, take the
           ;; normal path for the final column.
           ;;
           ;; If the table would be wider than the window including
           ;; the final column, take the abnormal path to left align
           ;; the last column's name and sort indicator so it remains
           ;; visible.
           (let ((normal-path t))
             (when last
               (let* ((pixels-until-last
                       (string-pixel-width
                        (buffer-substring (line-beginning-position) (point))
                        buffer))
                      (pixels-until-last+last
                       (+ pixels-until-last
                          column-width))
                      (window-body-width (window-body-width nil 'pixelwise)))
                 (when (and (< pixels-until-last window-body-width)
                            (>= pixels-until-last+last window-body-width))
                   (setq normal-path nil))))
             (if normal-path
                 ;; Normal case.
                 (pcase-exhaustive align
                   ('left
                    (insert
                     displayed
                     fill-str
                     indicator
                     spacer-str))
                   ('right
                    (insert
                     indicator
                     fill-str
                     displayed
                     spacer-str))
                   ('center
                    (insert
                     fill-str
                     displayed
                     fill-right-str
                     indicator
                     spacer-str)))
               ;; Abnormal case.
               (let* ((pre-indicator (string-pixel-width
                                      (buffer-substring (point-min) (point))
                                      buffer))
                      (pre-fill
                       (- (window-body-width nil 'pixelwise)
                          pre-indicator
                          displayed-width)))
                 (setq fill-str
                       (propertize " "
                                   'display (list 'space :width (list pre-fill))))
                 (setq fill-right-str
                       (propertize " "
                                   'display (list 'space :width
                                                  (list (- fill-width pre-fill)))))
                 (insert
                  indicator
                  displayed
                  fill-str
                  fill-right-str)))))
         (when (and divider (not last))
           (if divider-on-header
               (insert divider)
             (insert (propertize " "
                                 'display (list 'space
                                                :width
                                                (list divider-pixels))))))
         (add-text-properties start (point)
                              (list 'vtable-column index
                                    'keymap
                                    drag-keymap))))
     (vtable-columns table))
    (insert (propertize "\n" 'keymap drag-keymap))
    (add-face-text-property start (point) header-face t)))

(defun vtable--drag-resize-column (e &optional next)
  "Resize the column by dragging.
If NEXT, adjust the next column.  If the next column exceeds the number
of columns, resize the clicked column."
  (interactive "e")
  (let* ((pos-start (event-start e))
         (obj (posn-object pos-start))
         (start-x (car (posn-x-y pos-start)))
         (end-x (car (posn-x-y (event-end e))))
         (table (get-text-property
                 (if obj (cdr obj) (posn-point pos-start))
		 'vtable
		 (car obj)))
         (column (get-text-property
                  (if obj (cdr obj) (posn-point pos-start))
		  'vtable-column
		  (car obj))))
    (when (and table column)
      (setq column
            (if (and next
                     (< column (1- (length (vtable-columns table)))))
                (1+ column)
              column))
      (with-current-buffer (window-buffer (posn-window pos-start))
        (if (eq table (vtable-current-table))
            (vtable--alter-column-width table column
                                        (- end-x start-x))
          (save-excursion
            (vtable-goto-table table)
            (vtable--alter-column-width table column
                                        (- end-x start-x))))))))
(put 'vtable--drag-resize-column 'completion-predicate #'ignore)

(defun vtable--set-header-line (table widths)
  (let ((reference-buffer (vtable-buffer table)))
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
              (vtable--insert-header-line table widths)
              (add-text-properties (point-min)
                                   (point)
                                   (list 'vtable-header t
                                         'vtable table))
              (buffer-substring (point-min) (1- (point-max)))))))))

(defun vtable--text-scale-pixels (pixels)
  "Adjust PIXELS for text-scaled buffers."
  (ceiling (* pixels (/ (float (default-font-width)) (frame-char-width)))))

(defun vtable--limit-string (string pixels buffer &optional truncate-guess)
  "Truncate STRING to fit into width PIXELS.
This function tries to guess STRING's truncated length, in characters,
based the average pixel width of its characters, including its text
properties, relative to PIXELS.

Use BUFFER to derive text-scale adjustments.

If TRUNCATE-GUESS is nil, no guessing is done.

If TRUNCATE-GUESS is an integer, it is the number of additional
characters added to the guess.  Start with 0 and increase the tolerance
if you find that the guess is too small for the column's values."
  (let ((string-len (length string))
        (string-pixels))
    (when (and (> string-len 0)
               (integerp truncate-guess))
      (setq string-pixels (string-pixel-width string buffer))
      (when (> string-pixels pixels)
        ;; Use average pixels/character from STRING to seed the guess.
        (let ((guess (+ 1
                        truncate-guess
                        (ceiling (/ pixels (/ string-pixels string-len))))))
          (when (< guess string-len)
            (setq string (substring string 0 guess))))))
    (while (and (length> string 0)
                ;; Reuse the initial string-pixels for the fast path.
                (if string-pixels (prog1
                                      (> string-pixels pixels)
                                    (setq string-pixels nil))
                  (> (string-pixel-width string buffer) pixels)))
      (setq string (substring string 0 (1- (length string)))))
    string))

(defun vtable--char-width (table)
  (string-pixel-width
   (propertize "x" 'face (vtable-face table))
   (vtable-buffer table)))

(defun vtable--compute-width (table spec)
  (cond
   ((numberp spec)
    (* spec (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)ex" spec)
    (* (string-to-number (match-string 1 spec)) (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)px" spec)
    (string-to-number (match-string 1 spec)))
   ((string-match "\\([0-9.]+\\)%" spec)
    (/ (* (string-to-number (match-string 1 spec)) (window-body-width nil 'pixelwise))
       100))
   (t
    (error "Invalid spec: `%s' (vtable `%s')" spec
           (vtable-name table)))))

(defun vtable--compute-widths (table cache)
  "Compute the display widths for TABLE.
CACHE is TABLE's cache data as returned by `vtable--compute-cache'."
  (let* ((n-0cols 0) ; Count the number of zero-width columns.
         ;; column-width is adjusted by indicator-pad-width for
         ;; pixel-alignment with the header line.
         (buffer (vtable-buffer table))
         (indicator-pad-width (string-pixel-width
                               (slot-value table '-indicator-pad)
                               buffer))
         (widths (seq-map-indexed
                  (lambda (column index)
                    (let ((width
                           (or
                            ;; Explicit widths.
                            (and (vtable-column-width column)
                                 (vtable--compute-width
                                  table
                                  (vtable-column-width column)))
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
                                 (let ((name (copy-sequence
                                              (vtable-column-name column))))
                                   (add-face-text-property
                                    0 (length name)
                                    (vtable-header-face table) t name)
                                   (string-pixel-width name buffer))
                               0)))))
                      ;; Let min-width/max-width specs have their say.
                      (when-let* ((min-width (and (vtable-column-min-width column)
                                                  (vtable--compute-width
                                                   table
                                                   (vtable-column-min-width column)))))
                        (setq width (max width min-width)))
                      (when-let* ((max-width (and (vtable-column-max-width column)
                                                  (vtable--compute-width
                                                   table
                                                   (vtable-column-max-width column)))))
                        (setq width (min width max-width)))
                      (+ width indicator-pad-width)))
                  (vtable-columns table))))
    ;; If there are any zero-width columns, divide the remaining window
    ;; width evenly over them.
    (when (> n-0cols 0)
      (let* ((combined-width (apply #'+ widths))
             (default-width (/ (- (window-body-width nil 'pixelwise)
                                  combined-width) n-0cols)))
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
       (list value
             (string-pixel-width string (vtable-buffer table))
             string)))
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

(defun vtable-revert (&optional table)
  "Regenerate TABLE in its buffer.
If TABLE is nil, use the table under point."
  (setq table (or table (vtable-current-table)))
  (unless table
    (user-error "No table found"))
  (with-current-buffer (vtable-buffer table)
    ;; Below handles reverting tables that are not the "current table."
    ;; This handles the cases where point is not on the table and
    ;; `vtable-revert' is called by code, and the case where multiple
    ;; vtables share a buffer and revert is called via hooks to handle
    ;; `text-scale-mode' or changes to `display-line-numbers'.
    (let (orig-object
          orig-column
          (inhibit-read-only t))
      (when (eq table (vtable-current-table))
        (setq orig-object (vtable-current-object)
              orig-column (vtable-current-column)))
      (save-excursion
        (vtable-goto-table table)
        (delete-region (point) (vtable-end-of-table))
        (vtable--insert table))
      (when orig-object
        ;; Need goto-table, if point is within the table's region, to
        ;; accommodate delete-region moving point away.
        (vtable-goto-table table)
        (vtable-goto-object orig-object)
        (vtable-goto-column orig-column 'maybe-set-window-point)))))

(defun vtable--widths (table)
  (nth 1 (vtable--ensure-cache table)))

;;; Commands.

(defun vtable-goto-next-table ()
  "Go to the next table in the buffer.
Do nothing if no next table."
  (interactive)
  (let (match)
    (save-excursion
      ;; Find the next non-nil 'vtable, skipping the current table, if
      ;; present.
      (setq match (text-property-search-forward 'vtable
                                                nil nil
                                                (vtable-current-table))))
    (when match
      (goto-char (prop-match-beginning match))
      (vtable-goto-beginning-of-table))))

(defun vtable-goto-previous-table ()
  "Go to the previous table in the buffer.
Do nothing if no previous table."
  (interactive)
  (let (match)
    (save-excursion
      ;; Find the previous non-nil 'vtable, skipping the current table,
      ;; if present.
      (setq match (text-property-search-backward 'vtable
                                                 nil nil
                                                 (vtable-current-table))))
    (when match
      (goto-char (prop-match-beginning match))
      (vtable-goto-beginning-of-table))))

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

(defun vtable--alter-column-width (table column-index delta)
  (let* ((widths (vtable--widths table))
         (char-width (vtable--char-width table))
         (column (elt (vtable-columns table) column-index))
         (curr-width (aref widths column-index))
         (adj-width (+ curr-width delta))
         ;; Adj for min-width.
         (min-width (or
                     (and (vtable-column-min-width column)
                          (vtable--compute-width
                           table (vtable-column-min-width column)))
                     (* 2 char-width)))
         (new-width (max min-width adj-width))
         ;; Adj for max-width.
         (max-width (and (vtable-column-max-width column)
                         (vtable--compute-width
                          table (vtable-column-max-width column))))
         (new-width (if max-width
                        (min max-width new-width)
                      new-width)))
    (setf (aref widths column-index) new-width)
    ;; Store the width so it'll be respected on a revert.
    (setf (vtable-column-width column)
          (format "%dpx" (aref widths column-index)))
    (cond
     ((eq new-width min-width)
      (message "Column is at its minimum width"))
     ((eq new-width max-width)
      (message "Column is at its maximum width")))
    (vtable-revert table)))

(defun vtable-widen-current-column (&optional n)
  "Widen the current column by N characters.
If N isn't given, N defaults to 1.
Interactively, N is the prefix argument."
  (interactive "p")
  (vtable-narrow-current-column (- n)))

(defun vtable-previous-column ()
  "Move point to the previous column of the current table."
  (interactive)
  (vtable-goto-column
   (max 0 (1- (or (vtable-current-column)
                  (length (vtable--widths (vtable-current-table))))))))

(defun vtable-next-column ()
  "Move point to the next column of the current table."
  (interactive)
  (when (vtable-current-column)
    (vtable-goto-column
     (min (1- (length (vtable--widths (vtable-current-table))))
          (1+ (vtable-current-column))))))

(defun vtable-revert-command (&optional table)
  "Re-query data and regenerate TABLE in its buffer.
If TABLE is nil, use the current table at point."
  (interactive)
  (setq table (or table (vtable-current-table)))
  (unless table
    (user-error "No table found"))
  (vtable--unmark-all-objects table)
  (when (vtable-objects-function table)
    (setf (vtable-objects table) (funcall (vtable-objects-function table))))
  (vtable--clear-all-caches table)
  (vtable-revert table))

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
                                'ascend)))))
    ;; Clear the cache to force resort.
    (vtable--clear-cache table)
    (vtable-revert table)))

(defun vtable--header-line-sort (e)
  "Sort the current table from the header line, triggered by event E."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos))
         (table (get-text-property
                 (if obj (cdr obj) (posn-point pos))
		 'vtable
		 (car obj)))
         (column (get-text-property
                  (if obj (cdr obj) (posn-point pos))
		  'vtable-column
		  (car obj))))
    (with-current-buffer (window-buffer (posn-window pos))
      (if (eq table (vtable-current-table))
          (progn
            (vtable-goto-column column)
            (vtable-sort-by-current-column)
            ;; Keep point within table bounds.
            (vtable-goto-beginning-of-table))
        (save-excursion
          (vtable-goto-table table)
          (vtable-goto-column column)
          (vtable-sort-by-current-column))))))
(put 'vtable--header-line-sort 'completion-predicate #'ignore)

(defun vtable-unsort (&optional table)
  "Toggle TABLE sort between unsorted and its original `:sort-by'.
The default order is determined by the table's objects or its
`:objects-function'."
  (interactive)
  (setq table (or table (vtable-current-table)))
  (unless table
    (user-error "No table found"))
  (cond
   ((null (vtable-sort-by table))
    (if (slot-value table '-orig-sort-by)
        (progn
          (message "Original sort order")
          (setf (vtable-sort-by table) (slot-value table '-orig-sort-by)))
      (message "Sort disabled")))
   (t
    (message "Sort disabled")
    (setf (vtable-sort-by table) nil)))
  ;; Clear the cache to force resort or restore object order.
  (vtable--clear-cache table)
  (vtable-revert table))

(defun vtable-next-line (&optional n)
  "Like `forward-line', keeping point within table body bounds.
N has the same meaning as in `forward-line', which see."
  (interactive "p")
  (with-no-warnings ; Inhibit next-line warning.
    (when (save-excursion
            (next-line (or n 1))
            (vtable-current-object))
      (next-line (or n 1)))))

(defun vtable-previous-line (&optional n)
  "Like `forward-line', keeping point within table body bounds.
N has the same meaning as a negative argument in `forward-line', which
see."
  (interactive "p")
  (vtable-next-line (* -1 (or n 1))))

(defun vtable-close ()
  "Close the current table.
The close action is determined by the table's `:close-action'."
  (interactive)
  (when-let* ((table (vtable-current-table)))
    (pcase (vtable-close-action table)
      ((pred (lambda (x) (when (functionp x) (funcall x) t))) t)
      ('quit-window (quit-window))
      ('quit-window-kill (quit-window 'kill))
      (_ (bury-buffer)))))

;; Object marking functions.

(defun vtable-marked-objects (table)
  "Return a list of marked objects in TABLE.
Note, the order of this list is undefined."
  (slot-value table '-marked-objects))

(defun vtable-object-marked-p (table object)
  "Return non-nil if OBJECT is marked in TABLE."
  (seq-contains-p (slot-value table '-marked-objects)
                  object
                  (vtable-object-equal table)))

(defun vtable--mark-object (table object)
  (unless (vtable-object-marked-p table object)
    (push object (slot-value table '-marked-objects))
    (let ((vtable--inhibit-objects-tick t))
      (vtable-update-object table object))))

(defun vtable-mark-object (object &optional inhibit-next-line)
  "Mark OBJECT in the current table.
If INHIBIT-NEXT-LINE is non-nil, do not move point to the next line."
  (vtable--mark-object (vtable-current-table) object)
  (unless inhibit-next-line
    (vtable-next-line 1)))

(defun vtable--unmark-object (table object &optional inhibit-update)
  (let ((removed-seq (seq-remove (lambda (elt)
                                   (funcall (vtable-object-equal table)
                                            elt object))
                                 (slot-value table '-marked-objects))))
    (unless (length= removed-seq
                     (length (slot-value table '-marked-objects)))
      (setf (slot-value table '-marked-objects) removed-seq)
      (unless inhibit-update
        (let ((vtable--inhibit-objects-tick t))
          (vtable-update-object table object))))))

(defun vtable-unmark-object (object &optional inhibit-next-line)
  "Unmark OBJECT in the current table.
If INHIBIT-NEXT-LINE is non-nil, do not move point to the next line."
  (vtable--unmark-object (vtable-current-table) object)
  (unless inhibit-next-line
    (vtable-next-line 1)))

(defun vtable-toggle-marked-object (object)
  "Toggle the mark on OBJECT in the current table."
  (let ((table (vtable-current-table)))
    (if (vtable-object-marked-p table object)
        (vtable--unmark-object table object)
      (vtable--mark-object table object))))

(defun vtable-mark-objects (table predicate)
  "Mark objects in TABLE for which PREDICATE, when called, is non-nil.
PREDICATE is called with one argument, the object."
  (dolist (line (car (vtable--ensure-cache table)))
    (let ((object (car line)))
      (when (funcall predicate object)
        (vtable--mark-object table object)))))

(defun vtable-mark-all-objects (&rest _)
  "Mark all objects in the current table."
  (vtable-mark-objects (vtable-current-table) #'identity))

(defun vtable-unmark-objects (table predicate)
  "Unmark objects in TABLE for which PREDICATE, when called, is non-nil.
PREDICATE is called with one argument, the object."
  (dolist (line (car (vtable--ensure-cache table)))
    (let ((object (car line)))
      (when (funcall predicate object)
        (vtable--unmark-object table object)))))

(defun vtable--unmark-all-objects (table)
  (vtable-unmark-objects table #'identity))

(defun vtable-unmark-all-objects (&rest _)
  "Unmark all objects in the current table."
  (vtable--unmark-all-objects (vtable-current-table)))

;; Extra data convenience functions.

(defun vtable-set-extra-data (table extra-data)
  "Return EXTRA-DATA for TABLE."
  (setf (vtable-extra-data table) extra-data))

(defun vtable-column-set-extra-data (table column-or-index extra-data)
  "Set EXTRA-DATA for TABLE's COLUMN-OR-INDEX.
If COLUMN-OR-INDEX is an integer, amend the column associated with that
integer index, otherwise, it should be a column object."
  (when (integerp column-or-index)
    (setq column-or-index (elt (vtable-columns table) column-or-index)))
  (setf (vtable-column-extra-data column-or-index) extra-data))

(provide 'vtable)

;;; vtable.el ends here
