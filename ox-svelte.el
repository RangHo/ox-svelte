;;; ox-svelte.el --- Svelte Backend for Org Export Engine

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

(require 'ox)
(require 'ox-html)

(defgroup org-export-svelte nil
  "Options for exporting Org mode files to Svelte."
  :tag "Org Export Svelte"
  :group 'org-export)

(org-export-define-derived-backend 'svelte 'html)

(provide 'ox-svelte)

;;; ox-svelte.el ends here
