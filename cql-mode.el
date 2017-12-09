;;; cql-mode.el --- major mode for work with CQL files

;; Copyright (C) Alex Ott
;;
;; Author: Alex Ott <alexott@gmail.com>
;; Keywords: cql, cassandra
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/alexott/cql-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst cql-mode-font-lock-keywords
  (list
   (cons (concat "\\<\\("
		 "create[[:blank:]]+\\(table\\|keyspace\\)\\(?:[[:blank:]]+if[[:blank:]]+not[[:blank:]]+exists\\)?"
		 "\\|drop[[:blank:]]+\\(table\\|keyspace\\)\\(?:[[:blank:]]+if[[:blank:]]+exists\\)?"
		 "\\|alter[[:blank:]]+table"
		 "\\|\\(?:create\\|drop\\)[[:blank:]]+type"
		 "\\|with[[:blank:]]clustering[[:blank:]]order[[:blank:]]by"
		 "\\|insert[[:blank:]]into"
		 "\\|order[[:blank:]]by\\|primary[[:blank:]]key"
		 "\\|with\\|and\\|asc\\|desc\\|use\\|values\\|select\\|from\\|where\\|copy\\|frozen\\|source\\|add"
		 "\\|truncate\\(?:[[:blank:]]+table\\)?"
		 "\\)\\>")
	 'font-lock-keyword-face)
   (cons (regexp-opt '("ascii" "bigint" "blob" "boolean" "counter" "decimal" "double"
		      "float" "inet" "int" "list" "map" "set" "text" "timestamp"
		      "uuid" "timeuuid" "varchar" "varint") 'words)
	 'font-lock-type-face)
   (cons (regexp-opt '("true" "false") 'words)
	 'font-lock-constant-face)
   (list (concat "\\<\\(?:create[[:blank:]]+\\(?:table\\|keyspace\\)\\(?:[[:blank:]]+if[[:blank:]]+not[[:blank:]]+exists\\)?"
		 "\\|drop[[:blank:]]+\\(?:table\\|keyspace\\)\\(?:[[:blank:]]+if[[:blank:]]+exists\\)?"
		 "\\|alter[[:blank:]]+\\(?:table\\|keyspace\\)"
		 "\\|truncate\\(?:[[:blank:]]+table\\)?"
		 "\\|copy"
		 "\\|use"
		 "\\)[[:blank:]]+"
		 "\\(\\w+\\(?:\\s-*[.]\\s-*\\w+\\)*\\)"
		 "\\>")
	 1 'font-lock-function-name-face)
   )
  "Highlighting definition for CQL mode")

;; partially copied from sql-mode
(defvar cql-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; double-dash starts comments
    (modify-syntax-entry ?- ". 12b" st)
    ;; newline and formfeed end comments
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\f "> b" st)
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\" "." st)
    st)
  "Syntax table for CQL mode")

;;;###autoload
(define-derived-mode cql-mode prog-mode "CQL"
  "CQL major mode"

  (set (make-local-variable 'comment-start) "--")
  
  (set-syntax-table cql-mode-syntax-table)
  (setq font-lock-defaults '(cql-mode-font-lock-keywords nil t))
  
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cql\\'" . cql-mode))

(provide 'cql-mode)

;;; cql-mode.el ends here
