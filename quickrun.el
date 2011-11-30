;;; quickrun.el --- Run commands quickly

;; Copyright (C) 2011  by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package respects `quickrun.vim' developed by thinca
;;   - https://github.com/thinca/vim-quickrun
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'quickrun)
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar quickrun/timeout-seconds 10
  "Timeout seconds for running too long process")

(defconst quickrun/buffer-name "*quickrun*")

;;
;; Compat
;;

(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
    (lexical-let ((fun fun) (args1 args))
      (lambda (&rest args2) (apply fun (append args1 args2))))))

;;
;; language command parameters
;;

(defvar quickrun/language-alist
  '(("c/gcc" . ((:command . "gcc")
                (:compile . "%c %o -o %n %s")
                (:exec    . "%n %a")
                (:remove . ("%n"))))

    ("c/clang" . ((:command . "clang")
                  (:compile . "%c %o -o %n %s")
                  (:exec    . "%n %a")
                  (:remove  . ("%n"))))

    ("c/cl" . ((:command . "cl")
               (:compile . "%c %o %s /nologo /Fo%n.obj /Fe%n.exe")
               (:exec    . "%n %a")
               (:remove  . "%n.obj %n.exe")))

    ("c++/g++" . ((:command . "g++")
                  (:compile . "%c %o -o %n %s")
                  (:exec    . "%n %a")
                  (:remove . ("%n"))))

    ("c++/clang" . ((:command . "clang")
                    (:compile . "%c %o -o %n %s")
                    (:exec    . "%n %a")
                    (:remove  . ("%n"))))

    ("c++/cl" . ((:command . "cl")
                 (:compile . "%c %o %s /nologo /Fo%n.obj /Fe%n.exe")
                 (:exec    . "%n %a")
                 (:remove  . "%n.obj %n.exe")))

    ("objc" . ((:command . "gcc")
               (:compile . "%c %o -o %n %s -lobjc")
               (:exec    . "%n %a")
               (:remove  . ("%n"))))

    ("d" . ((:command . "dmd")
            (:compile . "%c %o %s")
            (:exec    . "%n %a")
            (:remove  . ("%n" "%n.o"))))

    ("java" . ((:command . "java")
               (:compile . "javac %o %s")
               (:exec    . "%c %N %a")
               (:remove  . ("%n.class"))))

    ("perl" . ((:command . "perl")))
    ("ruby" . ((:command . "ruby")))
    ("python" . ((:command . "python")))
    ("php" . ((:command . "php")))

    ("emacs" . ((:command . "emacs")
                (:exec    . "%c -Q --script %s")))
    ("lisp" . ((:command . "clisp")))
    ("scheme/gosh" . ((:command . "gosh")))

    ("clojure/jark"        . ((:command . "jark")))
    ("clojure/clj-env-dir" . ((:command . "clj-env-dir")))

    ("javascript/node" . ((:command . "node")))
    ("javascript/v8" . ((:command . "v8")))
    ("javascript/js" . ((:command . "js")))
    ("javascript/jrunscript" . ((:command . "jrunscript")))
    ("javascript/phantomjs" . ((:command . "phantomjs")))
    ("javascript/cscript" . ((:command . "cscript")
                             (:exec . "%c //e:jscript %o %s %a")
                             (:cmdopt . "//Nologo")))

    ("coffee" . ((:command . "coffee")))

    ("markdown/Markdown.pl" . ((:command . "Markdown.pl")))
    ("markdown/bluecloth"   . ((:command . "bluecloth")
                               (:cmdopt  . "-f")))
    ("markdown/kramdown"    . ((:command . "kramdown")))
    ("markdown/pandoc"      . ((:command . "pandoc")
                               (:exec . "%c --from=markdown --to=html %o %s %a")))
    ("markdown/redcarpet"   . ((:command . "redcarpet")))

    ("haskell" . ((:command . "runghc")))

    ("go/8g"  .  ((:command . "8g")
                  (:compile . "%c %o -o %n.8 %s")
                  (:link    . "8l -o %e %n.8")
                  (:exec    . "%e %a")
                  (:remove  . ("%e" "%n.8"))))
    ("go/6g"  .  ((:command . "6g")
                  (:compile . "%c %o -o %n.6 %s")
                  (:link    . "6l -o %e %n.6")
                  (:exec    . "%e %a")
                  (:remove  . ("%e" "%n.6"))))
    ("go/5g"  .  ((:command . "5g")
                  (:compile . "%c %o -o %n.5 %s")
                  (:link    . "5l -o %e %n.5")
                  (:exec    . "%e %a")
                  (:remove  . ("%e" "%n.5"))))

    ("io" . ((:command . "io")))
    ("groovy" . ((:command . "groovy")))
    ("scala" . ((:command . "scala")))
    ("sass" . ((:command . "sass")
               (:exec    . "%c %o --no-cache %s")))
    ("less" . ((:command . "lessc")))

    ("erlang" . ((:command . "escript")))
    ("ocaml" . ((:command . "ocamlc")
                (:compile . "%c %o -o %n %s")
                (:exec    . "%n %a")
                (:remove  . ("%n" "%n.cmi" "%n.cmo"))))

    ("shellscript" . ((:command . (lambda () (symbol-name sh-shell)))))
    ("awk" . ((:command . "awk")
              (:exec    . "%c %o -f %s -a")))
    )
  "List of each programming languages information.
Parameter form is (\"language\" . parameter-alist). parameter-alist has
5 keys and those values , :command, :compile, :link, :exec, :remove.
:command pair is mandatory, other pairs are optional. Associated value
should be string or a function which returns a string object.

Assosiated values are
:command = Program name which is used compiled or executed source code.
:compile = Compile command template
:link    = Link command template
:exec    = Exec command template. If you omit this parameter, quickrun
           use default parameter \"%c %o %s %a\".
:remove  = Remove files or directories templates.
           Compiler or executor generates temporary files,
           you should specified this parameter.
           If value is List, quickrun removes each element.
Every pair should be dot-pair.

See explanation of quickrun/template-place-holders
if you set your own language configuration.
")

(defvar quickrun/major-mode-alist
  '((c-mode . "c")
    (c++-mode . "c++")
    (objc-mode . "objc")
    ((perl-mode cperl-mode) . "perl")
    (ruby-mode . "ruby")
    (python-mode . "python")
    (php-mode . "php")
    (emacs-lisp-mode . "emacs")
    (lisp-mode . "lisp")
    (scheme-mode . "scheme")
    ((javascript-mode js-mode js2-mode) . "javascript")
    (clojure-mode . "clojure")
    (erlang-mode . "erlang")
    ((ocaml-mode tuareg-mode) . "ocaml")
    (go-mode . "go")
    (io-mode . "io")
    (haskell-mode . "haskell")
    (java-mode . "java")
    (d-mode . "d")
    (markdown-mode . "markdown")
    (coffee-mode . "coffee")
    (scala-mode . "scala")
    (groove-mode . "groovy")
    (sass-mode . "sass")
    ((less-mode less-css-mode) . "less")
    (sh-mode . "shellscript")
    (awk-mode . "awk"))
  "Alist of major-mode and langkey")

(defconst quickrun/extension-same-as-lang
  '("c" "php" "go" "d" "java" "scala" "coffee" "sass" "less" "groovy" "awk")
  "Extension of file is same as language key")

(defvar quickrun/extension-alist
  '((("cpp" "C") . "c++")
    ("m"    . "objc")
    (("pl" "pm")   . "perl")
    ("rb" . "ruby")
    ("py" . "python")
    (("el" "elisp") . "emacs")
    (("lisp" "lsp") . "lisp")
    ("scm" . "scheme")
    ("js"  . "javascript")
    ("clj" . "clojure")
    ("erl" . "erlang")
    ("ml" . "ocaml")
    ("hs"  . "haskell")
    ("io" . "io")
    (("md" "markdown" "mdown" "mkdn")  . "markdown")
    (("sh" "bash" "zsh")  . "shellscript"))
  "Alist of file extensions and langkey")

(defun quickrun/decide-file-type (filename)
  (let ((extension (file-name-extension filename)))
    (cond ((not (eq major-mode 'fundamental-mode))
           (quickrun/find-lang-from-alist quickrun/major-mode-alist major-mode))
          ((member extension quickrun/extension-same-as-lang) extension)
          (t (quickrun/find-lang-from-alist quickrun/extension-alist
                                            extension)))))

(defun quickrun/find-lang-from-alist (alist param)
  (loop for pair in alist
        for lang = (car pair)
        when (if (listp lang)
                 (member param lang)
               (string= param lang))
        return (cdr pair)))

(defun quickrun/extension-from-lang (lang)
  (let ((pair (rassoc lang quickrun/extension-alist)))
    (if pair
        (let ((extensions (car pair)))
          (cond ((listp extensions) (car extensions))
                (t extensions))))))

(defun quickrun/get-lang-info (lang)
  (let ((pair (assoc lang quickrun/language-alist)))
    (or (and pair (cdr pair))
        (error "not found [%s] language information" lang))))

;;
;; Compile
;;

(defun quickrun/compile-and-link (compile link)
  (dolist (cmd (list compile link))
    (when cmd
      (message "exec: %s" cmd)
      (quickrun/command-synchronous cmd))))

(defun quickrun/command-synchronous (cmd)
  (let* ((cmd-list (split-string cmd))
         (program  (car cmd-list))
         (args     (cdr cmd-list))
         (buf      (get-buffer-create quickrun/buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min)))
    (let ((compile-func (apply-partially 'call-process program nil buf t)))
      (when (not (= (apply compile-func args) 0))
        (pop-to-buffer buf)
        (throw 'compile 'compile-error)))))

;;
;; Execute
;;
(defvar quickrun/timeout-timer nil)
(make-variable-buffer-local 'quickrun-timeout-timer)

(defun quickrun/run (cmd)
  (let* ((buf (get-buffer-create quickrun/buffer-name))
         (process-name (format "quickrun-process-%s" (buffer-name)))
         (cmd-list (split-string cmd))
         (program (car cmd-list))
         (args (cdr cmd-list))
         (run-func (apply-partially 'start-process process-name buf program)))
    (message "exec: %s" cmd)
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min)))
    (lexical-let ((process (apply run-func args)))
      (if quickrun/timeout-seconds
          (setq quickrun/timeout-timer
                (run-at-time quickrun/timeout-seconds nil
                             #'quickrun/kill-process process)))
      process)))

(defun quickrun/kill-process (process)
  (when (eq (process-status process) 'run)
    (kill-process process)
    (let ((buf (get-buffer-create quickrun/buffer-name)))
      (with-current-buffer buf
        (erase-buffer)
        (insert (message "Time out(running over %d second)"
                         quickrun/timeout-seconds)))
      (quickrun/remove-temp-files)
      (pop-to-buffer buf))))

;;
;; Composing command
;;
(defconst quickrun/template-place-holders
  '("%c" "%o" "%s" "%a" "%n" "%N" "%e" "%E")
  "A list of place holders of each language parameter.
Place holders are beginning with '%' and replaced by:
%c: :command parameter
%o: command options
%s: source code
%a: program argument
%n: abosolute path of source code without extension
%N: source code name without extension
%e: abosolute path of source code with exeutable extension(.exe, .out, .class)
%E: source code name with executable extension
")

(defun quickrun/executable-suffix (command)
  (cond ((string= command "java") ".class")
        ((quickrun/windows-p) ".exe")
        (t ".out")))

(defun* quickrun/place-holder-info (&key command
                                         command-option
                                         source
                                         argument)
  (let* ((without-extension (file-name-sans-extension source))
         (executable-suffix (quickrun/executable-suffix command))
         (executable-name (concat without-extension executable-suffix)))
    `(("%c" . ,command)
      ("%o" . ,command-option)
      ("%s" . ,source)
      ("%n" . ,(expand-file-name without-extension))
      ("%N" . ,without-extension)
      ("%e" . ,(expand-file-name executable-name))
      ("%E" . ,executable-name)
      ("%a" . ,argument))))

(defun quickrun/get-lang-info-param (key lang-info)
  (let ((pair (assoc key lang-info)))
    (when pair
      (let ((tmpl (cdr pair)))
        (cond ((functionp tmpl)
               (let ((ret (funcall tmpl)))
                 (or (and (stringp ret) ret)
                     (error "%s's param should return string" key))))
              (t tmpl))))))

(defconst quickrun/default-tmpl-alist
  '((:exec . "%c %o %s %a")))

(defun quickrun/check-has-command (cmd)
  (unless (executable-find cmd)
    (quickrun/remove-temp-files)
    (error "Command not found: %s" cmd)))

(defun quickrun/fill-templates (lang src &optional argument)
  (let* ((lang-info (quickrun/get-lang-info lang))
         (cmd       (or (quickrun/get-lang-info-param :command lang-info)
                        (error "not specified command parameter")))
         (cmd-opt   (or (quickrun/get-lang-info-param :cmdopt lang-info) ""))
         (arg       (or argument
                        (quickrun/get-lang-info-param :argument lang-info)
                        ""))
         (tmpl-arg (quickrun/place-holder-info :command cmd
                                               :command-option cmd-opt
                                               :source src
                                               :argument arg))
         (info (make-hash-table)))
    (quickrun/check-has-command cmd)
    (dolist (key `(:compile :link :exec))
      (let ((tmpl (or (quickrun/get-lang-info-param key lang-info)
                      (and (assoc key quickrun/default-tmpl-alist)
                           (cdr (assoc key quickrun/default-tmpl-alist))))))
        (if tmpl
            (puthash key (quickrun/fill-template tmpl tmpl-arg) info))))
    (let ((remove-tmpl  (quickrun/get-lang-info-param :remove lang-info)))
      (if remove-tmpl
          (puthash :remove (mapcar (lambda (x)
                                     (quickrun/fill-template x tmpl-arg))
                                   remove-tmpl) info)))
    info))

(defun quickrun/fill-template (tmpl info)
  (let ((place-holders quickrun/template-place-holders)
        (str tmpl))
    (dolist (holder place-holders str)
      (let ((rep (cdr (assoc holder info)))
            (case-fold-search nil)
            (case-replace nil))
        (setq str (replace-regexp-in-string holder rep str nil))))))

;;
;; initialize
;;
(defun quickrun/windows-p ()
  (or (string= system-type "ms-dos")
      (string= system-type "windows-nt")))

(defvar quickrun/lang-key
  (make-hash-table :test #'equal))

(defun quickrun/find-executable (lst)
  (or (find-if (lambda (cmd) (executable-find cmd)) lst) ""))

(defun quickrun/set-lang-key (lang candidates)
  (puthash lang
           (concat lang "/" (quickrun/find-executable candidates))
           quickrun/lang-key))

(defun quickrun/add-command-if-windows (cmd lst)
  (if (quickrun/windows-p)
      (append cmd lst)
    lst))

(defun quickrun/init-lang-key ()
  "Decide command for programing language which has multiple candidates"
  (let ((c-candidates          '("gcc" "clang"))
        (c++-candidates        '("g++" "clang"))
        (javascript-candidates '("node" "v8" "js"
                                 "jrunscript" "cscript"))
        (scheme-candidates     '("gosh"))
        (markdown-candidates   '("Markdown.pl" "kramdown"
                                 "bluecloth" "redcarpet" "pandoc"))
        (clojure-candidates    '("jark" "clj-env-dir"))
        (go-candidates         '("8g" "6g" "5g")))
    (quickrun/set-lang-key "c" (quickrun/add-command-if-windows
                                "cl" c-candidates))
    (quickrun/set-lang-key "c++" (quickrun/add-command-if-windows
                                  "cl" c++-candidates))
    (quickrun/set-lang-key "javascript" javascript-candidates)
    (quickrun/set-lang-key "scheme" scheme-candidates)
    (quickrun/set-lang-key "markdown" markdown-candidates)
    (quickrun/set-lang-key "clojure" clojure-candidates)
    (quickrun/set-lang-key "go" go-candidates)))

(quickrun/init-lang-key)

;;
;; main
;;
(defun quickrun ()
  "Run commands quickly for current buffer"
  (interactive)
  (quickrun-common))

(defun quickrun-with-arg (arg)
  "Run commands quickly for current buffer with arguments"
  (interactive
   (list (read-string "QuickRun Arg: ")))
  (quickrun-common :argument arg))

(defun quickrun-lang (lang)
  "Run specified commands quickly for current buffer"
  (interactive
   (list (completing-read "QuickRun Lang: " quickrun/language-alist)))
  (quickrun-common :language lang))

(defvar quickrun/remove-files nil)

(defun quickrun/get-lang-key (lang)
  (or (gethash lang quickrun/lang-key) lang
      (error "Can't found language setting")))

(defun quickrun/temp-name (extension)
  (concat (make-temp-name "qr_") "." extension))

(defun quickrun/add-remove-files (files)
  (if (listp files)
      (setq quickrun/remove-files (append files quickrun/remove-files))
    (push files quickrun/remove-files)))

(defun* quickrun-common (&key argument language)
  (let* ((orig-src (file-name-nondirectory (buffer-file-name)))
         (lang (quickrun/decide-file-type orig-src))
         (lang-key (or language (quickrun/get-lang-key lang)))
         (extension (or (and lang (quickrun/extension-from-lang lang))
                        (file-name-extension orig-src)))
         (src (quickrun/temp-name extension)))
    (cond ((string= lang-key "java") (setq src orig-src))
          (t
           (copy-file orig-src src)
           (quickrun/add-remove-files src)))
    (let* ((cmd-info-hash (quickrun/fill-templates lang-key src argument))
           (compile-state
            (let ((compile-cmd (gethash :compile cmd-info-hash))
                  (link-cmd    (gethash :link    cmd-info-hash)))
              (catch 'compile
                (and compile-cmd
                     (quickrun/compile-and-link compile-cmd link-cmd))))))
      (cond ((eq compile-state 'compile-error)
             (unless (string= orig-src src)
               (delete-file src)))
            (t
             (let* ((exec-cmd (gethash :exec cmd-info-hash))
                    (process (quickrun/run exec-cmd)))
               (quickrun/add-remove-files (gethash :remove cmd-info-hash))
               (set-process-sentinel process #'quickrun/sentinel)))))))

(defun quickrun/remove-temp-files ()
  (dolist (file quickrun/remove-files)
    (cond
     ((file-directory-p file) (delete-directory file t))
     ((file-exists-p file) (delete-file file)))))

(defun quickrun/sentinel (process state)
  (let ((status (process-status process)))
    (cond ((eq status 'exit)
           (pop-to-buffer (process-buffer process))
           (quickrun/remove-temp-files)
           (delete-process process)
           (cancel-timer quickrun/timeout-timer))
          (t nil))))

(provide 'quickrun)
