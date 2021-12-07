;;; nickel-mode.el --- description -*- lexical-binding: t; -*-
;; Copyright (C) 2021  Russell Sim
;; Author: Guangtao Zhang
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
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
;;; Code


(require 'smie)
(require 'cl-extra)


(defgroup nickel '()
  "Major mode for editing Nickel files."
  :group 'languages)

(defcustom nickel-typecheck-command
  '("nickel" "typecheck")
  "Nickel Typecheck Command."
  :type '(repeat string)
  :group 'nickel)

(defcustom nickel-library-search-directories
  nil "Sequence of Nickel package search directories."
  :type '(repeat directory)
  :group 'nickel)

(defcustom nickel-indent-level
  2
  "Number of spaces to indent with."
  :type '(number)
  :group 'nickel)

(defvar nickel--identifier-regexp
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regular expression matching a Nickel identifier.")

(defvar nickel-font-lock-keywords
  (let ((builtin-regex (regexp-opt '("switch" "import" "if" "else" "then" "let" "in") 'words))
        (constant-regex (regexp-opt '("false" "null" "true") 'words))
        (type-regex (regexp-opt '("List" "Str") 'words))
        (standard-functions-regex (regexp-opt '("fun" "doc" "default") 'words)))
    (list
     `(,builtin-regex . font-lock-builtin-face)
     `(,constant-regex . font-lock-constant-face)
     `(,type-regex . font-lock-constant-face)
     `(,standard-functions-regex . font-lock-function-name-face)
     ;; identifiers starting with a # or _ are reserved for definitions
     ;; and hidden fields
     `(,(concat "_?[^\"]#" nickel--identifier-regexp "+:?") . font-lock-type-face)
     ))
  "Minimal highlighting for ‘nickel-mode’.")

(defun nickel-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (cond ((or (nth 4 ppss)             ;Inside a comment
               (and string-start
                    ;; Inside of a string quoted with different triple quotes.
                    (not (eql (char-after string-start)
                              (char-after quote-starting-pos)))))
           ;; Do nothing.
           nil)
          ((nth 5 ppss)
           ;; The first quote is escaped, so it's not part of a triple quote!
           (goto-char (1+ quote-starting-pos)))
          ((null string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          (t
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))


(defvar nickel-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (expr (arg ":" expr)
             ("if" expr "then" expr "else")
             ("let" decls "in" expr)
             ("with" expr "nonsep-;" expr)
             ("assert" expr "nonsep-;" expr)
             (attrset)
             (id))
       (attrset ("{" decls "}"))
       (decls (decls "," decls)
              (id "=" expr))
       (arg (id) ("{" args "}"))
       (args (args "," args) (id "arg-?" expr)))
     '((assoc ","))
     '((assoc "|"))
     )
    (smie-precs->prec2
     '((right "=")
       (left "||" "|" )
       (left "&&" "&")
       (nonassoc "=~" "!~" "!=" "==" "<=" ">=" "<" ">" "->" "@")
       (left "+" "-")
       (left "++" "%"))))))

;; Operators
;; +     &&    ==    <     =     (     ) =>
;; -     ||    !=    >     :     {     }
;; *     &     =~    <=    ?     [     ]     ,
;; /     |     !~    >=    !     _|_   ...   .
;; _|_ bottom

(defvar nickel-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "\"\"\"" "'''"))
    (0 (ignore (nickel-syntax-stringify))))))

(defconst nickel-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments. Nickel supports /* */ and // as comment delimiters
    (modify-syntax-entry ?/ ". 124" table)
    ;; Additionally, Nickel supports # as a comment delimiter
    (modify-syntax-entry ?\n ">" table)
    ;; ", ', ,""" and ''' are quotations in Nickel.
    ;; both """ and ''' are handled by nickel--syntax-propertize-function
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `nickel-mode'.")


(defun nickel-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) smie-indent-basic)
    (`(,_ . ",") (nickel-smie--indent-nested))
    (`(,_ . "}") (nickel-smie--indent-closing))
    (`(,_ . "]") (smie-rule-parent (- 0 nickel-indent-level)))
    (`(,_ . ")") (smie-rule-parent (- 0 nickel-indent-level)))
    ))

(defun nickel-smie--in-object-p ()
  "Return t if the current block we are in is wrapped in {}."
  (let ((ppss (syntax-ppss)))
    (or (null (nth 1 ppss))
        (and (nth 1 ppss)
             (or
              (eq ?{ (char-after (nth 1 ppss)))
              (eq ?\( (char-after (nth 1 ppss))))))))


(defun nickel-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (not (eq (char-before) ?\,)) ;Coalesce ";" and "\n".
           (> pos (line-end-position))
           (nickel-smie--in-object-p))
      (skip-chars-forward " \t")
      ;; Why bother distinguishing \n and ,?
      ",") ;;"\n"
     (t
      (buffer-substring-no-properties
       (point)
       (progn (if (zerop (skip-syntax-backward "."))
                  (skip-syntax-backward "w_'"))
              (point)))))))


(defun nickel-smie-forward-token ()
  (skip-chars-forward " \t")
  (cond
   ((and (looking-at "[\n]")
         (or (save-excursion (skip-chars-backward " \t")
                             ;; Only add implicit , when needed.
                             (or (bolp) (eq (char-before) ?\,)))
             (nickel-smie--in-object-p)))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ;; Why bother distinguishing \n and ;?
    ",") ;;"\n"
   ((progn (forward-comment (point-max)) nil))
   (t
    (buffer-substring-no-properties
     (point)
     (progn (if (zerop (skip-syntax-forward "."))
                (skip-syntax-forward "w_'"))
            (point))))))

(defun nickel-smie--indent-nested ()
  (let ((ppss (syntax-ppss)))
    (if (nth 1 ppss)
        (let ((parent-indentation (save-excursion
                                    (goto-char (nth 1 ppss))
                                    (back-to-indentation)
                                    (current-column))))
          (cons 'column (+ parent-indentation nickel-indent-level))))))

(defun nickel-smie--indent-closing ()
  (let ((ppss (syntax-ppss)))
    (if (nth 1 ppss)
        (let ((parent-indentation (save-excursion
                                    (goto-char (nth 1 ppss))
                                    (back-to-indentation)
                                    (current-column))))
          (cons 'column parent-indentation)))))

(defvar nickel-smie-verbose-p nil
  "Emit context information about the current syntax state.")

(defmacro nickel-smie-debug (message &rest format-args)
  `(progn
     (when nickel-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))
;; all identifiers starting with __(double underscores) as keywords

(defun verbose-nickel-smie-rules (kind token)
  (let ((value (nickel-smie-rules kind token)))
    (nickel-smie-debug "%s '%s'; sibling-p:%s prev-is-OP:%s hanging:%s == %s" kind token
                       (ignore-errors (smie-rule-sibling-p))
                       (ignore-errors (smie-rule-prev-p "OP"))
                       (ignore-errors (smie-rule-hanging-p))
                       value)
    value))

;;;###autoload
(define-derived-mode nickel-mode prog-mode "Nickel Lang Mode"
  :syntax-table nickel-mode-syntax-table
  (setq-local font-lock-defaults '(nickel-font-lock-keywords ;; keywords
                                   nil  ;; keywords-only
                                   nil  ;; case-fold
                                   nil  ;; syntax-alist
                                   nil  ;; syntax-begin
                                   ))

  (setq-local syntax-propertize-function
              nickel-syntax-propertize-function
              )

  ;; nickel lang uses tabs for indent by default
  (setq-local indent-tabs-mode t)
  (setq-local tab-width nickel-indent-level)

  (smie-setup nickel-smie-grammar 'verbose-nickel-smie-rules
              :forward-token  #'nickel-smie-forward-token
              :backward-token #'nickel-smie-backward-token
              )
  (setq-local smie-indent-basic nickel-indent-level)
  (setq-local smie-indent-functions '(smie-indent-fixindent
                                      smie-indent-bob
                                      smie-indent-comment
                                      smie-indent-comment-continue
                                      smie-indent-comment-close
                                      smie-indent-comment-inside
                                      smie-indent-keyword
                                      smie-indent-after-keyword
                                      smie-indent-empty-line
                                      smie-indent-exps))

  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+[\t ]*")
  (setq-local comment-end "")
  )

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ncl\\'" 'nickel-mode))


;;; Code:
(provide 'nickel-mode)
;;; nickel-mode.el ends here
