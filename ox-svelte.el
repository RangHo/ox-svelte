;;; ox-svelte.el --- Svelte Backend for Org Export Engine  -*- lexical-binding: t -*-

;; Copyright (C) 2024 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Svelte backend for Org mode generic exporter.
;; It is based on the HTML backend, and generates a HTML file per Org file.

;;; Code:

(require 'json)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'ox-html)

(defgroup org-export-svelte nil
  "Options for exporting Org mode files to Svelte."
  :tag "Org Export Svelte"
  :group 'org-export)

(defcustom org-svelte-component-import-alist
  nil
  "Alist of components to import in the generated Svelte file.

The `car' of each element is the component name, and the `cdr' is the path to
the component file relative to the generated Svelte file.  This list is provided
to import shared components that are used to render certain Org elements (e.g.
using a custom component to render LaTeX fragments).

If this list is nil, no components will be imported.

For example, if you want to import a component named `LaTeX' from the path
`./components/LaTeX.svelte', you can set this variable as follows:

  (setq org-svelte-component-import-alist
        \\'((\"LaTeX\" . \"./components/LaTeX.svelte\")))

And the generated Svelte file will contain the following import statement:

  import LaTeX from './components/LaTeX.svelte';"
  :group 'org-export-svelte
  :type '(repeat (cons (string "Component name")
                       (string "Component path"))))

(defconst org-svelte--component-import-format
  "import %s from '%s';"
  "Format string that will be used to generate the import statement.")

(defconst org-svelte-metadata-format
  "export const metadata = %s;"
  "Format string that will be used to generate the metadata.")

(defconst org-svelte--module-context-script-regexp
  "<script context=\"module\">"
  "Regexp that matches the opening tag of the module-context script.")

(defcustom org-svelte-latex-environment-format
  "{@html %s}"
  "Format string that will be used to generate the LaTeX environment.

The format string should contain a single `%s' specifier, which will be replaced
with the LaTeX environment's source code as a JavaScript raw string.

By default, the source code will be printed as a raw HTML string."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-latex-fragment-format
  "{@html %s}"
  "Format string that will be used to generate the LaTeX fragment.

The format string should contain a single `%s' specifier, which will be replaced
with the LaTeX fragment's source code as a JavaScript raw string.

By default, the source code will be printed as-is."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-src-block-format
  "<pre><code class=\"language-%s\">{@html %s.replaceAll('<', '&lt;').replaceAll('>', '&gt;')}</code></pre>"
  "Format string that will be used to generate the source block.

The format string should contain two `%s' specifiers.  The first specifier will
be replaced with the language of the source block, and the second specifier will
be replaced with the source code of the source block as a JavaScript raw string.

By default, the source code will be encosed in a <code> tag with the language
class inside a <pre> tag."
  :group 'org-export-svelte
  :type 'string)

(org-export-define-derived-backend 'svelte 'html
  :menu-entry '(?S "Export to Svelte"
                   ((?S "As Svelte buffer"
                        (lambda (a s v _b)
                          (org-svelte-export-as-svelte a s v)))
                    (?s "As Svelte file"
                        (lambda (a s v _b)
                          (org-svelte-export-to-svelte a s v)))))
  :translate-alist '((export-snippet . org-svelte-export-snippet)
                     (inner-template . org-svelte-inner-template)
                     (latex-environment . org-svelte-latex-environment)
                     (latex-fragment . org-svelte-latex-fragment)
                     (src-block . org-svelte-src-block)
                     (template . org-svelte-template))
  :options-alist '( ; Overriding HTML options
                   (:html-doctype "HTML_DOCTYPE" nil "html5")
                   (:html-html5-fancy nil nil t)))

(defun org-svelte--convert-to-raw-string (string)
  "Convert STRING to a JavaScript raw string.

This function converts the given string to a JavaScript raw template string.
The result of this function, therefore, can be inserted where a JavaScript
value is appropriate.

This function expects a standalone string that does not depend on any string
interpolation.  Every instance of backticks and substitution markers will be
escaped accordingly.

For example, one could use this function to insert a string into a JavaScript:
  (format \"const str = %s;\"
          (org-svelte--convert-to-raw-string \"Hello, World!\")

Where the result would be:
  const str = String.raw`Hello, World!`;"
  (let* ((escaped-string (replace-regexp-in-string "`"
                                                   "\\`"
                                                   string))
         (escaped-string (replace-regexp-in-string "\\${"
                                                   "\\\\${"
                                                   escaped-string)))
    (format "String.raw`%s`" escaped-string)))

(defun org-svelte-export-snippet (export-snippet _contents _info)
  "Transcode an EXPORT-SNIPPET element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (or (eq (org-export-snippet-backend export-snippet) 'svelte)
            (eq (org-export-snippet-backend export-snippet) 'html))
    (org-element-property :value export-snippet)))

(defun org-svelte-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((frag (org-remove-indentation
                 (org-element-property :value latex-environment)))
          (label (org-html--reference latex-environment info t)))
      (format org-svelte-latex-environment-format
              (org-svelte--convert-to-raw-string
               (if (org-string-nw-p label)
                   (replace-regexp-in-string "\\`.*"
                                             (format "\\&\n\\\\label{%s}" label)
                                             frag)
                 frag))))))

(defun org-svelte-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (format org-svelte-latex-fragment-format
                (org-svelte--convert-to-raw-string (substring frag 2 -2))))
       ((string-match-p "^\\\\\\[" frag)
        (format org-svelte-latex-environment-format
                (org-svelte--convert-to-raw-string (substring frag 2 -2))))))))

(defun org-svelte-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info)))
    (format org-svelte-src-block-format
            lang
            (org-svelte--convert-to-raw-string code))))

(defun org-svelte--generate-imports (info)
  "Generate import statements for components.
INFO is a plist holding contextual information."
  (let ((imports (mapcar (lambda (component)
                           (format org-svelte--component-import-format
                                   (car component)
                                   (cdr component)))
                         org-svelte-component-import-alist)))
    (string-join imports "\n")))

(defun org-svelte--generate-metadata-export (info)
  "Return a JSON string containing the metadata of the current Org file.
INFO is a plist holding contextual information."
  (let ((title (org-export-data (plist-get info :title) info))
        (subtitle (org-export-data (plist-get info :subtitle) info))
        (author (org-export-data (plist-get info :author) info))
        (date (org-export-data (org-export-get-date info "%Y-%m-%d") info))
        (description (plist-get info :description))
        (keywords (plist-get info :keywords))
        (language (plist-get info :language))
        (creator (plist-get info :creator)))
    (format org-svelte-metadata-format
            (json-encode `((title . ,title)
                           (subtitle . ,subtitle)
                           (author . ,author)
                           (date . ,date)
                           (description . ,description)
                           (keywords . ,keywords)
                           (language . ,language)
                           (creator . ,creator))))))

(defun org-svelte--module-context-script (info)
  "Generate the module-context script that imports assets and exports metadata.
INFO is a plist holding contextual information."
  (let ((imports (org-svelte--generate-imports info))
        (metadata (org-svelte--generate-metadata-export info)))
    (format "<script context=\"module\">\n%s\n%s\n</script>\n"
            imports
            metadata)))

(defun org-svelte-inner-template (contents _info)
  "Return body of document after converting it to Svelte.
CONTENTS is the transcoded contents string.  INFO is a plist holding export
options."
  contents)

(defun org-svelte-template (contents info)
  "Return complete document string after Svelte conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used as a
communication channel."
  (concat (org-svelte--module-context-script info)
          contents))

;;;###autoload
(defun org-svelte-export-as-svelte
    (&optional async subtreep visible-only)
  "Export current buffer to a Svelte component buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements."
  (interactive)
  (org-export-to-buffer 'svelte "*Org Svelte Export*"
    async subtreep visible-only nil nil
    (lambda ()
      (if (fboundp 'web-mode)
          (web-mode)
        (html-mode)))))

;;;###autoload
(defun org-svelte-export-to-svelte
    (&optional async subtreep visible-only)
  "Export current buffer to a Svelte component file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements."
  (interactive)
  (org-export-to-file 'svelte (org-export-output-file-name ".svelte" subtreep)
    async subtreep visible-only))

(provide 'ox-svelte)

;;; ox-svelte.el ends here
