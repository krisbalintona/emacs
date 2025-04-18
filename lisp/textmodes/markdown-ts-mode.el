;;; markdown-ts-mode.el --- tree sitter support for Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author     : Rahul Martim Juliato <rahul.juliato@gmail.com>
;; Maintainer : Rahul Martim Juliato <rahul.juliato@gmail.com>
;; Created    : April 2024
;; Keywords   : markdown md languages tree-sitter

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

(require 'treesit)
(require 'subr-x)

(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")

;;; Helper functions

(defvar markdown-ts--code-block-language-map
  '(("c++" . cpp) ("c#" . c-sharp))
  "Alist mapping code block language names to tree-sitter languages.

Keys should be strings, and values should be language symbols.

For example, \"c++\" in

    ```c++
    int main() {
        return 0;
    }
    ```

maps to tree-sitter language `cpp'.")

(defvar markdown-ts-code-block-source-mode-map
  '((javascript . js-ts-mode))
  "An alist of supported code block languages and their major mode.")

;;; Faces

(defgroup markdown-ts-faces nil
  "Faces used in Markdown TS Mode."
  :group 'markdown-ts-faces
  :group 'faces)

(defface markdown-ts-delimiter '((t (:inherit shadow)))
  "Face for the # before headings.")

(defface markdown-ts-heading-1 '((t (:inherit outline-1)))
  "Face for first level headings.")

(defface markdown-ts-setext-heading '((t (:inherit markdown-ts-heading-1)))
  "Face for setext headings (headings underlined by === or ---).")

(defface markdown-ts-heading-2 '((t (:inherit outline-2)))
  "Face for second level headings.")

(defface markdown-ts-heading-3 '((t (:inherit outline-3)))
  "Face for third level headings.")

(defface markdown-ts-heading-4 '((t (:inherit outline-4)))
  "Face for fourth level headings.")

(defface markdown-ts-heading-5 '((t (:inherit outline-5)))
  "Face for fifth level headings.")

(defface markdown-ts-heading-6 '((t (:inherit outline-6)))
  "Face for sixth level headings.")

(defface markdown-ts-list-marker '((t (:inherit font-lock-keyword-face)))
  "Face for list markers like - and *.")

(defface markdown-ts-block-quote '((t (:inherit font-lock-string-face)))
  "Face for block quotes.")

(defface markdown-ts-language-keyword '((t (:inherit font-lock-keyword-face)))
  "Face for the language keyword for code blocks.")

;;; Font-lock

(defvar markdown-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'markdown
   :override t
   :feature 'delimiter
   '([ "[" "]" "(" ")" ] @shadow)

   :language 'markdown
   :feature 'heading
   '((atx_heading (atx_h1_marker)) @markdown-ts-heading-1
     (atx_heading (atx_h2_marker)) @markdown-ts-heading-2
     (atx_heading (atx_h3_marker)) @markdown-ts-heading-3
     (atx_heading (atx_h4_marker)) @markdown-ts-heading-4
     (atx_heading (atx_h5_marker)) @markdown-ts-heading-5
     (atx_heading (atx_h6_marker)) @markdown-ts-heading-6
     (setext_heading) @markdown-ts-setext-heading)

   :language 'markdown
   :feature 'heading
   :override 'prepend
   '((atx_h1_marker) @markdown-ts-delimiter
     (atx_h2_marker) @markdown-ts-delimiter
     (atx_h3_marker) @markdown-ts-delimiter
     (atx_h4_marker) @markdown-ts-delimiter
     (atx_h5_marker) @markdown-ts-delimiter
     (atx_h6_marker) @markdown-ts-delimiter)

   :language 'markdown
   :feature 'paragraph
   '(((thematic_break) @markdown-ts-delimiter)
     ((indented_code_block) @font-lock-string-face)
     (list_item (list_marker_star) @markdown-ts-list-marker)
     (list_item (list_marker_plus) @markdown-ts-list-marker)
     (list_item (list_marker_minus) @markdown-ts-list-marker)
     (list_item (list_marker_dot) @markdown-ts-list-marker)

     (block_quote) @markdown-ts-block-quote)

   :language 'markdown
   :feature 'paragraph
   :override 'prepend
   '((block_quote_marker) @markdown-ts-delimiter
     (fenced_code_block_delimiter) @markdown-ts-delimiter
     (fenced_code_block
      (info_string (language) @markdown-ts-language-keyword))
     (block_quote
      (block_quote_marker) @markdown-ts-delimiter
      (paragraph (inline (block_continuation) @markdown-ts-delimiter))))

   :language 'markdown-inline
   :feature 'paragraph-inline
   '(((image_description) @link)
     ((link_destination) @font-lock-string-face)
     ((code_span) @font-lock-string-face)
     ((emphasis) @italic)
     ((strong_emphasis) @bold)
     (inline_link (link_text) @link)
     (inline_link (link_destination) @font-lock-string-face)
     (shortcut_link (link_text) @link))

   :language 'markdown-inline
   :feature 'paragraph-inline
   :override 'append
   '((emphasis_delimiter) @markdown-ts-delimiter)
   ))

;;; Imenu

(defun markdown-ts-imenu-node-p (node)
  "Check if NODE is a valid entry to imenu."
  (equal (treesit-node-type (treesit-node-parent node))
         "atx_heading"))

(defun markdown-ts-imenu-name-function (node)
  "Return an imenu entry if NODE is a valid header."
  (let ((name (treesit-node-text node)))
    (if (markdown-ts-imenu-node-p node)
	(thread-first (treesit-node-parent node) (treesit-node-text))
      name)))

;;; Code blocks

(defvar-local markdown-ts--configured-languages nil
  "A list of languages that have been setup in this buffer.

When a code block of a language appear, `markdown-ts-mode' loads
language setups like font-lock and indentation for that language, and
add that language to this list.")

(defun markdown-ts--harvest-treesit-configs (mode)
  "Harvest tree-sitter configs from MODE.
Return a plist with the following keys and value:

    :font-lock (from `treesit-font-lock-settings')
    :simple-indent (from `treesit-simple-indent-rules')
    :range (from `treesit-range-settings')"
  (with-temp-buffer
    (funcall mode)
    (list :font-lock treesit-font-lock-settings
          :simple-indent treesit-simple-indent-rules
          :range treesit-range-settings)))

(defun markdown-ts--add-config-for-mode (language mode)
  "Add configurations for LANGUAGE from MODE to current buffer.

Configuration includes font-lock and indent.  For font-lock rules, use
the same features enabled in MODE."
  (let ((configs (markdown-ts--harvest-treesit-configs mode)))
    (ignore language) ; We might make use of this later.
    (setq treesit-font-lock-settings
          (append treesit-font-lock-settings
                  ;; Get all the font-lock settings, including ones that
                  ;; don't pertain to LANGUAGE.  This way we get jsdoc
                  ;; from js-ts-mode, for example.
                  (plist-get configs :font-lock)))
    (setq treesit-simple-indent-rules
          (append treesit-simple-indent-rules
                  ;; Similarly, get all indent rules.
                  (plist-get configs :simple-indent)))
    (setq treesit-range-settings
          (append treesit-range-settings
                  (plist-get configs :range)))
    (setq-local indent-line-function #'treesit-indent)
    (setq-local indent-region-function #'treesit-indent-region)))

(defun markdown-ts--convert-code-block-language (node)
  "Convert NODE to a language for the code block."
  (let* ((lang-string (alist-get (treesit-node-text node)
                                 markdown-ts--code-block-language-map
                                 (treesit-node-text node) nil #'equal))
         (lang (intern (downcase lang-string))))
    ;; FIXME: Kind of a hack here: we use this function as a hook for
    ;; loading up configs for the language for the code block on-demand.
    (unless (memq lang markdown-ts--configured-languages)
      (let ((mode (alist-get lang markdown-ts-code-block-source-mode-map)))
        (when mode
          (markdown-ts--add-config-for-mode lang mode)
          (push lang markdown-ts--configured-languages))))
    lang))

(defun markdown-ts--range-settings ()
  "Return range settings for `markdown-ts-mode'."
  (treesit-range-rules
   :embed 'markdown-inline
   :host 'markdown
   :range-fn #'treesit-range-fn-exclude-children
   '((inline) @markdown-inline)

   :embed 'html
   :host 'markdown
   '((html_block) @html)

   :embed #'markdown-ts--convert-code-block-language
   :host 'markdown
   :local t
   '((fenced_code_block (info_string (language) @language)
                        (code_fence_content) @content))))

;;; Major mode

(defun markdown-ts-setup ()
  "Setup treesit for `markdown-ts-mode'."
  (setq-local treesit-font-lock-settings markdown-ts--treesit-settings)
  (setq-local treesit-range-settings (markdown-ts--range-settings))
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode markdown-ts-mode text-mode "Markdown"
  "Major mode for editing Markdown using tree-sitter grammar."
  (setq-local font-lock-defaults nil
	      treesit-font-lock-feature-list '((delimiter heading)
					       (paragraph)
					       (paragraph-inline)))

  (setq-local treesit-simple-imenu-settings
              `(("Headings" markdown-ts-imenu-node-p nil markdown-ts-imenu-name-function)))
  (setq-local treesit-outline-predicate "section")

  (when (treesit-ready-p 'markdown)
    (treesit-parser-create 'markdown-inline)
    (treesit-parser-create 'markdown)
    (markdown-ts-setup)))

(derived-mode-add-parents 'markdown-ts-mode '(markdown-mode))

(if (treesit-ready-p 'markdown)
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode)))

(provide 'markdown-ts-mode)
;;; markdown-ts-mode.el ends here
