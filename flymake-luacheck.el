;;; flymake-luacheck.el --- Flymake for Lua using luacheck -*- lexical-binding: t -*-

;; Copyright (C) 2020 David M. Cooke <david.m.cooke@gmail.com>

;; Author: David M. Cooke <david.m.cooke@gmail.com>
;; Version: 0.2
;; Created: Oct. 14, 2020
;; Package-Requires: (flymake)
;; SPDX-License-Identifier: MIT
;; Keywords: languages, lua

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package adds a flymake backend to check Lua files using luacheck.
;; If luacheck is not available, luac will be used to check for syntax
;; errors.
;;
;; Luacheck is a static analyzer and a linter for Lua. It is available
;; from <https://github.com/mpeterv/luacheck>.
;;
;; The function `flymake-luacheck-setup' should be run in the buffer to
;; be checked before enabling `flymake-mode'.  For convenience, the
;; function `flymake-luacheck-activate' will add the backend, and
;; turn on `flymake-mode'.
;;
;;   (require 'flymake-luacheck)
;;   (add-hook 'lua-mode #'flymake-luacheck-activate)
;;
;; or, using `use-package':
;;
;;   (use-package flymake-luacheck
;;     :hook (lua-mode . flymake-luacheck-activate))
;;

(require 'rx)
(require 'pcase)
(require 'flymake)

;;; Code:

(defgroup flymake-luacheck nil
  "Flymake-luacheck group"
  :group 'flymake
  :prefix 'flymake-luacheck)

(defcustom flymake-luacheck-path
  (executable-find "luacheck")
  "Path to the 'luacheck' executable."
  :group 'flymake-luacheck
  :type '(string)
  :risky t)

(defcustom flymake-luacheck-standard-globals '(+ max)
  "List of symbols setting what luacheck considers to be the standard globals.
The symbols in the list are concatenated with a \"+\" separator,
and passed to luacheck with the \"--std\" option.  The accepted
symbols can be found in the luacheck documentation at
URL `https://luacheck.readthedocs.io/en/stable/cli.html#command-line-options'.

If the list starts with `+', then the sets of globals will be added to the
current set (from a config file, for instance), rather than replacing it.

The default `(+ max)' is equivalent to `(+ lua51c lua52c lua53c luajit)'.

If the standard globals are set in a \".luacheckrc\" file, this should be
set to `nil', which will inhibit adding a \"--std\" option to the luacheck
command line. (Options on the command line overwrite those in config files.)
"
  :group 'flymake-luacheck
  :type '(set symbol)
  :local t)

(defcustom flymake-luacheck-extra-arguments nil
  "Extra arguments to pass to luacheck.
The full set of arguments is constructed from
`flymake-luacheck-standard-globals', this variable, and
\"--formatter=plain --codes --ranges -\"."
  :group 'flymake-luacheck
  :type '(set string)
  :local t)

(defcustom flymake-luacheck-dialect 'lua51
  "The Lua dialect used when checking this buffer.

This is only used by the fallback parser to determine which command for
luac should be used."
  :group 'flymake-luacheck
  :type '(symbol)
  :local t)

(defcustom flymake-luacheck-fallback-lua-parser-command
  nil
  "Function that returns the command line for checking stdin.
The function should take one argument, a symbol denoting the Lua dialect.
It should return a list of strings, which should be a command line that
parses the standard input, and prints out syntax errors in the same
way as \"luac -p -\" would. Alternatively, the first element should
be either the symbol 'lua or 'luac, and the appropiate arguments will
be appended to the end of the cdr.

If this variable is nil, then a executable is determined from
`flymake-luacheck-dialect' and `flymake-luacheck-fallback-lua-parser-alist'."
  :group 'flymake-luacheck
  :type '(choice (const :tag "No function" nil)
                 (function)))

(defcustom flymake-luacheck-fallback-lua-parser-alist
  '((lua51 luac "luac5.1")
    (lua52 luac "luac5.2")
    (lua53 luac "luac5.3")
    (luajit lua "luajit")
    (nil luac "luac"))
  "Fallback syntax checkers using lua or luac.
This is a list with elements (DIALECT-SYMBOL METHOD COMMAND).
The METHOD is one of 'lua or 'luac; if 'lua, a script is used that
reads the file with 'loadfile()', otherwise, for 'luac, the '-p'
argument is used.  The COMMAND should be the executable to run."
  :group 'flymake-luacheck
  :type '(repeat :tag "Alist by dialect"
                 (list :tag "Dialect parser"
                       (symbol :tag "Dialect symbol")
                       (choice (const :tag "Parse with interpreter" lua)
                               (const :tag "Parse with compiler (-p)" luac))
                       (string :tag "Command"))))


(defun flymake-luacheck-standard-globals-args ()
  "Argument list for the globals in `flymake-luacheck-standard-globals'.
This is added to the luacheck command line."
  (if flymake-luacheck-standard-globals
      (cons "--std"
            (let* ((add-plus (eq (car flymake-luacheck-standard-globals) '+))
                   (std (mapconcat
                         #'symbol-name
                         (if add-plus
                             (cdr flymake-luacheck-standard-globals)
                           flymake-luacheck-standard-globals)
                         "+")))
              (if add-plus
                  (concat "+" std)
                std)))
    nil))

(defconst flymake-luacheck--loadfile-script
  "local ok, err = loadfile(); if not ok then print(err) end"
  "Lua script to read stdin and print the first syntax error.")

(defun flymake-luacheck--append-command-args (parser-type cmd)
  "Append the appropiate arguments to CMD for the PARSER-TYPE."
  (pcase parser-type
    ('lua (append cmd (list "-e" flymake-luacheck--loadfile-script)))
    ('luac (append cmd (list "-p" "-")))
    (_ (error "Unknown Lua parser type %S" parser-type))))

(defun flymake-luacheck-fallback-command (dialect)
  "Return a command that will parse standard input.
The DIALECT is looked up in `flymake-luacheck-fallback-lua-parser-alist',
and the command is built from that."
  (let* ((dinfo (or (assq dialect flymake-luacheck-fallback-lua-parser-alist)
                    (assq nil flymake-luacheck-fallback-lua-parser-alist)
                    '(luac "luac")))
         (parser-type (cadr dinfo))
         (cmd (cddr dinfo))
         (prog (executable-find (car cmd))))
    (when prog
      (flymake-luacheck--append-command-args parser-type
                                             (cons prog (cdr cmd))))))

(defun flymake-luacheck-command ()
  "Build the luacheck command line, or nil if luacheck or luac isn't found."
  (if-let ((prog (executable-find flymake-luacheck-path)))
      (list 'luacheck
            #'flymake-luacheck--match-errors
            (append (list prog)
                    (flymake-luacheck-standard-globals-args)
                    flymake-luacheck-extra-arguments
                    '("--formatter=plain" "--codes" "--ranges"
                      "--no-color" "-")))
    (if-let ((cmd (if flymake-luacheck-fallback-lua-parser-command
                      (let ((cmd-info
                             (funcall
                              flymake-luacheck-fallback-lua-parser-command
                              flymake-luacheck-dialect)))
                        (if (symbolp (car cmd-info))
                            (flymake-luacheck--append-command-args
                             (car cmd-info) (cdr cmd-info))
                          cmd-info))
                    (flymake-luacheck-fallback-command
                     flymake-luacheck-dialect))))
        (list 'luac
              #'flymake-luacheck--match-errors-luac
              cmd)
      nil)))

;;

(defconst flymake-luacheck-err-re
  (rx bol (group (* (not ":")))         ; file name
      ":" (group (+ (any digit)))       ; line number
      ":" (group (+ (any digit)))       ; start column
      (? "-" (group (+ (any digit))))   ; end column
      (: ":" (* space)
         (group
          (? (group "(" (group (any "W" "E")) (+ digit) ")")) ; error code
          (+ space) (* nonl)) eol))                           ; message
  "Regular expression that matches luacheck's output.")

(defun flymake-luacheck-line-column-region (source line start-col end-col)
  "Get the region in SOURCE on line number LINE between START-COL and END-COL.
The numbers are those returned by luacheck (1-based), and the columns
include the end points.  The returned cons is half-open."
  (with-current-buffer source
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (when (> start-col 0)
        (move-to-column (1- start-col)))
      (let ((start (point)))
        (if (>= end-col start-col)
            (progn
              ;; end-col: 1- for 1-based, then 1+ for exclusive end point
              (move-to-column end-col)
              (cons start (point)))
          (cons start start))))))

(defun flymake-luacheck--match-errors (source)
  "Parse the error and warning messages from luacheck output.
Argument SOURCE Source code buffer."
  (let (diags)
    (while (search-forward-regexp flymake-luacheck-err-re nil t)
      (let* ((lineno (string-to-number (match-string 2)))
             (start-col (string-to-number (match-string 3)))
             (end-col (string-to-number (match-string 4)))
             (msg (match-string 5))
             (type (if (null (match-string 6))
                       :error ; more like fatal error
                     (pcase (match-string 7)
                       ("E" :error)
                       ("W" :warning)
                       (_ :error))))
             (region (flymake-luacheck-line-column-region
                      source lineno start-col end-col)))
        (push (flymake-make-diagnostic source (car region)
                                       (cdr region) type msg)
              diags)))
    (nreverse diags)))

(defconst flymake-luacheck-err-re-luac
  (rx bol (* nonl) "luac" (* (any digit ?.)) (? ".exe") ; process name
      ":" (* space) (group (* nonl))                    ; file name
      ":" (group (+ (any digit)))                       ; line number
      ":" (+ space) (group (* nonl)) eol)               ; message
  "Regular expression that matches the (error) output of \"luac -p\".")

(defun flymake-luacheck--match-errors-luac (source)
  "Parse the error message from \"luac -p\" output.
Argument SOURCE Source code buffer."
  (let (diags)
    (while (search-forward-regexp flymake-luacheck-err-re-luac nil t)
      (let* ((lineno (string-to-number (match-string 2)))
             (region (flymake-diag-region source lineno)))
        (push (flymake-make-diagnostic source (car region)
                                       (cdr region)
                                       :error
                                       (match-string 3))
              diags)))
    (nreverse diags)))

(defvar-local flymake-luacheck--proc nil
  "(Internal) The luacheck process for the current buffer.")

(defun flymake-luacheck--sentinel (match-errors source report-fn)
  "Return a process sentinel for the luacheck process.
Argument MATCH-ERRORS function to match error messages in the output buffer.
Argument SOURCE Source code buffer.
Argument REPORT-FN Flymake callback."
  #'(lambda (proc _event)
      (when (eq 'exit (process-status proc))
        (unwind-protect
            (if (with-current-buffer source (eq proc flymake-luacheck--proc))
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-min))
                  (let ((diags (funcall match-errors source)))
                    (funcall report-fn diags)))
              (flymake-log :warning "Cancelling obsolete check %s" proc))
          (kill-buffer (process-buffer proc))))))

(defun flymake-luacheck-backend (report-fn &rest _args)
  "Backend implementation for flymake adding luacheck support.
Argument REPORT-FN Flymake callback."
  (pcase (flymake-luacheck-command)
    ('nil (error "Could not find luacheck executable"))
    (`(,_ ,match-errors ,cmdline)
     (let ((source (current-buffer)))
       (when (process-live-p flymake-luacheck--proc)
         (kill-process flymake-luacheck--proc))
       (save-restriction
         (widen)
         (setq flymake-luacheck--proc
               (make-process
                :name "luacheck-flymake" :noquery t
                :connection-type 'pipe
                :buffer (generate-new-buffer "*luacheck-flymake*")
                :command cmdline
                :sentinel (flymake-luacheck--sentinel
                           match-errors source report-fn)))
         (process-send-region flymake-luacheck--proc (point-min) (point-max))
         (process-send-eof flymake-luacheck--proc))))))

;;;###autoload
(defun flymake-luacheck-setup ()
  "Add the luacheck backend to Flymake's list of diagnostic functions.
This should be called in the buffer that should be checked."
  (add-hook 'flymake-diagnostic-functions #'flymake-luacheck-backend 50 t))

;;;###autoload
(defun flymake-luacheck-activate ()
  "Enable Flymake mode, after adding the luacheck backend for this buffer."
  (interactive)
  (flymake-luacheck-setup)
  (flymake-mode 1))

(provide 'flymake-luacheck)

;;; flymake-luacheck.el ends here
