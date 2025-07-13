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

;; ---------------------------------------------------------------------
;; Constants and Variables
;; ---------------------------------------------------------------------

(defconst org-svelte--component-import-format
  "import %s from '%s';"
  "Format string that will be used to generate the import statement.")

(defconst org-svelte--metadata-export-format
  "export const metadata = %s;"
  "Format string that will be used to generate the metadata.")

(defconst org-svelte--script-format
  "<script>\n%s\n</script>"
  "Format string that will be used to generate the script.")

(defconst org-svelte--module-script-format
  "<script module>\n%s\n</script>"
  "Format string that will be used to generate the module script.")

(defgroup org-export-svelte nil
  "Options for exporting Org mode files to Svelte."
  :tag "Org Export Svelte"
  :group 'org-export)

(defcustom org-svelte-anchor-format
  "<a id=\"%s\" href=\"%s\">%s</a>"
  "Format string that will be used to generate the anchor link.

The format should contain three \"%s\" specifiers.  The first specifier will be
replaced with the ID of the anchor, the second specifier will be replaced with
the URL of the anchor, and the third specifier will be replaced with the text
of the anchor.

By default, the anchor will be rendered using the \"a\" tag."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-component-import-alist
  nil
  "Alist of components to import in the generated Svelte file.

The `car' of each element is the binding name(s) of the imported component, and
the `cdr' is the module name.  This list is provided to import shared components
that are used to render certain Org elements (e.g. using a custom component to
render LaTeX fragments).

The `car' can be either a string or a list of strings.  If it is a string, the
default export of the specified module will be imported with the given name.  If
it is a list of strings, the named exports of the specified module will be
imported.  This module will not perform any validation on the given names, and
it is up to the user to ensure that the names are valid.

If this list is nil, no components will be imported.

For example, if you want to import a component named \"LaTeX\" from the path
\"./components/LaTeX.svelte\", you can set this variable as follows:

    (setq org-svelte-component-import-alist
          \\'((\"LaTeX\" . \"./components/LaTeX.svelte\")))

And the generated Svelte file will contain the following import statement:

    import LaTeX from \"./components/LaTeX.svelte\";

If you want to import multiple components from the same module, you can put a
list of strings as the `car' of the element:

    (setq org-svelte-component-import-alist
          \\'(((\"LaTeX\" \"CodeBlock\") . \"my-ui-library\")))

And the generated Svelte file will contain the following import statements:

    import { LaTeX, CodeBlock } from \"my-ui-library\";"
  :group 'org-export-svelte
  :type '(repeat (cons (choice (string :tag "Binding name")
                               (repeat :tag "Binding names" string))
                       (string "Module"))))

(defcustom org-svelte-metadata-export-list
  '(:title :subtitle :description :date :language)
  "List of keywords or properties to export as metadata.

These opotions will be retrieved from the contextual information property list
during exports.  While most options will be transcoded as plain strings, some
keywords have specific transcode functions that represent the data better.  For
more information, see `org-svelte--extract-metadata-as-json' and \"index.d.ts\"
type declaration file."
  :group 'org-export-svelte
  :type '(repeat symbol))

(defcustom org-svelte-image-format
  "<img id=\"%s\" src=\"%s\" alt=\"%s\" />"
  "Format string that will be used to generate the image link.

The format should contain three \"%s\" specifiers.  The first specifier will be
replaced with the ID of the image, the second specifier will be replaced with
the URL of the image, and the third specifier will be replaced with the alt text
of the image.

By default, the image will be rendered using the \"img\" tag."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-latex-environment-format
  "{@html %s}"
  "Format string that will be used to generate the LaTeX environment.

The format string should contain a single \"%s\" specifier, which will be
replaced with the LaTeX environment's source code as a JavaScript raw string.

By default, the source code will be printed as a raw HTML string."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-latex-fragment-format
  "{@html %s}"
  "Format string that will be used to generate the LaTeX fragment.

The format string should contain a single \"%s\" specifier, which will be
replaced with the LaTeX fragment's source code as a JavaScript raw string.

By default, the source code will be printed as a raw HTML string."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-link-org-files-as-svelte
  nil
  "Non-nil means make file links to \"file.org\" point to \"file.svelte\".

When Org mode is exporting an Org file to Svelte, the URL of the file links will
be placed in the \"href\" attribute of the anchor tag.  However, should other
Org files be converted to Svelte component without being managed by a
preprocessor, it is better to convert the file links to Org files to Svelte.

Default is nil, assuming that the user will manage the file links manually."
  :group 'org-export-svelte
  :type 'boolean)

(defcustom org-svelte-raw-script-content
  ""
  "JavaScript code that will be included in the module context script verbatim.

The content of this variable will be inserted right after the generated import
statements.  It must contain a valid JavaScript code, as this Emacs module will
perform no validation.

In the similar sense, it is highly recommended that you always use semicolons
at least for this section.  Your script, combined with automatically generated
import statements and metadata storage, may be parsed unexpectedly if you rely
on JavaScript semicolon correction mechanism."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-src-block-format
  "<pre><code class=\"language-%s\">{@html %s.replaceAll('<', '&lt;').replaceAll('>', '&gt;')}</code></pre>"
  "Format string that will be used to generate the source block.

The format string should contain two \"%s\" specifiers.  The first specifier
will be replaced with the language of the source block, and the second specifier
will be replaced with the source code of the source block as a JavaScript raw
string.

By default, the source code will be encosed in a \"code\" tag with the language
class inside a \"pre\" tag."
  :group 'org-export-svelte
  :type 'string)

(defcustom org-svelte-stable-reference
  nil
  "Non-nil means use stable references instead of the default randomized ones."
  :group 'org-export-svelte
  :type 'boolean)

(defcustom org-svelte-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<code>%s</code>")
    (italic . "<i>%s</i>")
    (strike-through . "<s>%s</s>")
    (underline . "<u>%s</u>")
    (verbatim . "<kbd>%s</kbd>"))
  "Alist of HTML expressions to convert text markup."
  :group 'org-export-svelte
  :type '(alist :key-type (symbol :tag "Markup type")
                :value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

(defcustom org-svelte-verbose
  nil
  "Non-nil means be more verbose during export."
  :group 'org-export-svelte
  :type 'boolean)

;; ---------------------------------------------------------------------
;; Utility Functions
;; ---------------------------------------------------------------------

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
  (let* ((escaped-string
          (replace-regexp-in-string "`" "\\`" string))
         (escaped-string
          (replace-regexp-in-string "\\${" "\\\\${" escaped-string)))
    (format "String.raw`%s`" escaped-string)))

(defun org-svelte--extract-metadata-as-json (info)
  "Extract metadata from INFO and return it as a JSON string."
  (let* ((info-alist
          (mapcar (lambda (key)
                    (cons key (plist-get info key)))
                  org-svelte-metadata-export-list))
         (info-alist-sanitized
          (mapcar (lambda (pair)
                    (cons (substring (symbol-name (car pair)) 1) ; remove the leading colon
                          (cl-case (car pair)
                            ((:title :subtitle :description)
                             (org-export-data (cdr pair) info))
                            (:date
                             (org-export-get-date info "%FT%T%z")) ; ISO 8601
                            (otherwise
                             (cdr pair)))))
                  info-alist)))
    (json-encode info-alist-sanitized)))

(defun org-svelte--extract-reference (datum)
  "Extract the reference from DATUM and return it as a string."
  (or (org-element-property :CUSTOM_ID datum)
      (let ((raw-value (org-element-property :raw-value datum)))
        (if (stringp raw-value)
            (org-svelte--to-kebab-case raw-value)
          ""))))

(defun org-svelte--format-anchor (id href inner-text)
  "Return an anchor element with the given ID, HREF, and INNER-TEXT."
  (format org-svelte-anchor-format id href inner-text))

(defun org-svelte--format-image (id src alt)
  "Return an image element with the given ID, SRC, and ALT."
  (format org-svelte-image-format id src alt))

(defun org-svelte--format-module-context-script (info)
  "Generate the module-context script that imports assets and exports metadata.
INFO is a plist holding contextual information."
  (let* ((imports-list
          (mapcar (lambda (component)
                    (format org-svelte--component-import-format
                            (if (listp (car component))
                                ;; If the `car' is a list, import the named exports.
                                (concat "{ " (string-join (car component) ", ") " }")
                              ;; Otherwise, import the default export.
                              (car component))
                            (cdr component)))
                  org-svelte-component-import-alist))
         (imports-string
          (string-join imports-list "\n"))
         (metadata-json
          (org-svelte--extract-metadata-as-json info))
         (metadata-string
          (format org-svelte--metadata-export-format metadata-json)))
    (format "%s\n%s\n"
            (format org-svelte--module-script-format metadata-string)
            (format org-svelte--script-format imports-string))))

(defun org-svelte--message (&rest args)
  "Display a message with ARGS if `org-svelte-verbose' is non-nil."
  (when org-svelte-verbose
    (apply #'message args)))

(defun org-svelte--reference (orig-fun datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the current export
state, as a plist.

When DATUM is a headline and `org-svelte-stable-reference' is non-nil, return
the stablized version of the reference based on the content of the headline.

When NAMED-ONLY is non-nil and DATUM nas no NAME keyword, return nil.  This
doesn't apply to headlines, inline tasks, radio targets, and targets.

This function is an advice around `org-html--reference'.  ORIG-FUN is the
original function."
  (if (or (not org-svelte-stable-reference)
          (not (org-element-type-p datum 'headline))
          named-only)
      (funcall orig-fun datum info named-only)
    (org-svelte--message "[org-svelte--reference] using stable reference")
    (org-svelte--extract-reference datum)))

(defun org-svelte--to-kebab-case (string)
  "Convert STRING to kebab-case."
  (string-trim
   (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase string))
   "-+"
   "-+"))

;; ---------------------------------------------------------------------
;; Backend Definition
;; ---------------------------------------------------------------------

(org-export-define-derived-backend 'svelte 'html
  :menu-entry '(?S "Export to Svelte"
                   ((?S "As Svelte buffer"
                        (lambda (a s v _b)
                          (org-svelte-export-as-svelte a s v)))
                    (?s "As Svelte file"
                        (lambda (a s v _b)
                          (org-svelte-export-to-svelte a s v)))))
  :translate-alist '((export-block . org-svelte-export-block)
                     (export-snippet . org-svelte-export-snippet)
                     (inner-template . org-svelte-inner-template)
                     (keyword . org-svelte-keyword)
                     (latex-environment . org-svelte-latex-environment)
                     (latex-fragment . org-svelte-latex-fragment)
                     (link . org-svelte-link)
                     (src-block . org-svelte-src-block)
                     (template . org-svelte-template))
  :options-alist '( ; Export options
                   (:title "TITLE" nil nil space)
                   (:subtitle "SUBTITLE" nil nil space)
                   (:description "DESCRIPTION" nil nil space)
                   (:author "AUTHOR" nil user-full-name parse)
                   (:email "EMAIL" nil user-mail-address parse)
                   (:date "DATE" nil nil parse)
                   (:creator "CREATOR" nil org-export-creator-string)
                   (:category "CATEGORY" nil nil parse)
                   (:tags "TAGS" nil nil parse)
                   ;; HTML option overrides
                   (:html-doctype nil nil "html5")
                   (:html-html5-fancy nil nil t)
                   (:html-text-markup-alist nil nil org-svelte-text-markup-alist)))

;; ---------------------------------------------------------------------
;; Transcoders
;; ---------------------------------------------------------------------

(defun org-svelte-export-block (export-block _contents _info)
  "Transcode an EXPORT-BLOCK element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (or (string= (org-element-property :type export-block) "HTML")
            (string= (org-element-property :type export-block) "SVELTE"))
    (org-remove-indentation (org-element-property :value export-block))))

(defun org-svelte-export-snippet (export-snippet _contents _info)
  "Transcode an EXPORT-SNIPPET element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (or (eq (org-export-snippet-backend export-snippet) 'svelte)
            (eq (org-export-snippet-backend export-snippet) 'html))
    (org-element-property :value export-snippet)))

(defun org-svelte-keyword (keyword _contents _info)
  "Transcode a KEYWORD element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (or (string= (org-element-property :key keyword) "HTML")
            (string= (org-element-property :key keyword) "SVELTE"))
    (org-element-property :value keyword)))

(defun org-svelte-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((frag (org-remove-indentation
                 (org-element-property :value latex-environment)))
          (label (org-html--reference latex-environment info t)))
      (org-svelte--message "[org-svelte-latex-environment] processing a LaTeX environment: %s" frag)
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
      (if (string-match-p "^\$" frag)
          ;; Legacy usage of LaTeX fragment.
          (cond
           ((string-match-p "^\$[^$]")
            (format org-svelte-latex-fragment-format
                    (org-svelte--convert-to-raw-string (substring frag 1 -1))))
           ((string-match-p "^\$\$[^$]" frag)
            (format org-svelte-latex-fragment-format
                    (org-svelte--convert-to-raw-string (substring frag 2 -2)))))
        ;; Newer, "proper" usage of LaTeX fragment.
        (cond
         ((string-match-p "^\\\\(" frag)
          (format org-svelte-latex-fragment-format
                  (org-svelte--convert-to-raw-string (substring frag 2 -2))))
         ((string-match-p "^\\\\\\[" frag)
          (org-svelte--message "[org-svelte-latex-fragment] processing a display math fragment: %s" frag)
          (format org-svelte-latex-environment-format
                  (org-svelte--convert-to-raw-string (substring frag 2 -2)))))))))

(defun org-svelte-link (link desc info)
  "Transcode a LINK element from Org to Svelte.
DESC is the description part of the link, or the empty string.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (let* ((raw-link (org-element-property :raw-link link))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (link-is-url (member type '("http" "https" "ftp" "mailto" "news"))))
    (org-svelte--message "[org-svelte-link] type: %s" type)
    (org-svelte--message "[org-svelte-link] path: %s" path)
    (org-svelte--message "[org-svelte-link] desc: %s" desc)
    (org-svelte--message "[org-svelte-link] link-is-url: %s" (if link-is-url "yes" "no"))
    (cond
     ;; Link type is handled by a special function
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; ID or fuzzy links
     ((member type '("fuzzy" "id" "custom-id"))
      (let ((destination
	         (if (string= type "fuzzy")
		         (org-export-resolve-fuzzy-link link info)
	           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          ;; External file
          ('plain-text
           (org-svelte--message "[org-svelte-link] processing a plain text: %s" raw-link)
           (org-svelte--format-anchor
            (org-html--reference link info)
            destination desc))
          ;; Headline
          ('headline
           (org-svelte--message "[org-svelte-link] processing a headline: %s" raw-link)
           (org-svelte--format-anchor
            (org-html--reference link info)
            (concat "#"
                    (or (org-element-property :CUSTOM_ID destination)
                        (org-html--reference destination info)))
            desc))
          (_
           (org-svelte--message "[org-svelte-link] processing unknown type: %s" type)
           (org-svelte--message "[org-svelte-link] destination: %s" destination)))))
     ;; Inline image
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (org-svelte--message "[org-svelte-link] processing an image: %s" raw-link)
      (org-svelte--format-image
       (org-html--reference link info)
       raw-link
       ""))
     ;; External link
     (path
      (org-svelte--message "[org-svelte-link] processing an external link: %s" raw-link)
      (org-svelte--format-anchor
       (org-html--reference link info)
       raw-link
       (or desc "")))
     ;; Edge case (e.g. a link without path)
     (t
      (org-svelte--message "[org-svelte-link] processing a weird link")
      (format "<i>%s</i>" desc)))))

(defun org-svelte-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to Svelte.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info)))
    (format org-svelte-src-block-format
            lang
            (org-svelte--convert-to-raw-string code))))

;; ---------------------------------------------------------------------
;; Document Body Aggregators
;; ---------------------------------------------------------------------

(defun org-svelte-inner-template (contents _info)
  "Return body of document after converting it to Svelte.
CONTENTS is the transcoded contents string.  INFO is a plist holding export
options."
  contents)

(defun org-svelte-template (contents info)
  "Return complete document string after Svelte conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used as a
communication channel."
  (concat (org-svelte--format-module-context-script info)
          contents))

;; ---------------------------------------------------------------------
;; Autoloads
;; ---------------------------------------------------------------------

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
  (advice-add 'org-html--reference :around #'org-svelte--reference)
  (org-export-to-buffer 'svelte "*Org Svelte Export*"
    async subtreep visible-only nil nil
    (lambda ()
      (if (fboundp 'web-mode)
          (web-mode)
        (html-mode))))
  (advice-remove 'org-html--reference #'org-svelte--reference))

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
  (advice-add 'org-html--reference :around #'org-svelte--reference)
  (org-export-to-file 'svelte (org-export-output-file-name ".svelte" subtreep)
    async subtreep visible-only)
  (advice-remove 'org-html--reference #'org-svelte--reference))

(provide 'ox-svelte)

;;; ox-svelte.el ends here
