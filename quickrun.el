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

    ("c++/clang++" . ((:command . "clang++")
                      (:compile . "%c %o -o %n %s")
                      (:exec    . "%n %a")
                      (:remove  . ("%n"))))

    ("c++/cl" . ((:command . "cl")
                 (:compile . "%c %o %s /nologo /Fo%n.obj /Fe%n.exe")
                 (:exec    . "%n %a")
                 (:remove  . "%n.obj %n.exe")))

    ("objc" . ((:command . "gcc")
               (:compile . (lambda ()
                             (cond ((string= system-type "darwin")
                                    ("%c %o -o %n %s -framework foundation"))
                                   (t "%c %o -o %n %s -lobjc"))) )
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

    ("perl" . ((:command . "perl") (:compile-only . "%c -wc %s")))
    ("ruby" . ((:command . "ruby") (:compile-only . "%c -wc %s")))
    ("python" . ((:command . "python") (:compile-only . "pyflakes %s")))
    ("php" . ((:command . "php") (:compile-only . "%c -l %s")))

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
    ("lua" . ((:command . "lua")))
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
    (lua-mode . "lua")
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

(defvar quickrun/extension-alist
  '((("cpp" "C" "cxx" "cc") . "c++")
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
  (let* ((extension (file-name-extension filename))
         (from-major-mode
          (quickrun/find-lang-from-alist quickrun/major-mode-alist major-mode))
         (from-extension
          (quickrun/find-lang-from-alist quickrun/extension-alist extension)))
    (or from-major-mode from-extension
        (find extension quickrun/support-languages :test #'equal))))

(defun quickrun/find-lang-from-alist (alist param)
  (loop for pair in alist
        for lang = (car pair)
        when (if (listp lang)
                 (member param lang)
               (string= param lang))
        return (cdr pair)))

(defun quickrun/extension-from-lang (lang)
  (let* ((pair (and (rassoc lang quickrun/extension-alist)))
         (extensions (and pair (car pair))))
    (if extensions
        (cond ((listp extensions) (car extensions))
              (t extensions)))))

(defun quickrun/get-lang-info (lang)
  (or (assoc-default lang quickrun/language-alist)
      (error "not found [%s] language information" lang)))

;;
;; Compile
;;

(defun quickrun/compile-and-link (compile link)
  (unless compile
    (error "Compile command is null"))
  (message "QuickRun Compile: %s" compile)
  (if quickrun/compile-only-flag
      (quickrun/compilation-start compile)
    (quickrun/command-synchronous compile))
  (when link
    (message "QuickRun Link: %s" link)
    (quickrun/command-synchronous link)))

(defun quickrun/compilation-start (cmd)
  (let ((program (car (split-string cmd))))
    (quickrun/check-has-command program
                                #'(lambda (command)
                                    (message "%s not found" command)
                                    (throw 'compile 'command-not-found)))
    (setf compilation-finish-functions #'quickrun/compilation-finish-func)
    (compilation-start cmd t (lambda (x) quickrun/buffer-name))))

(defun quickrun/compilation-finish-func (buffer str)
  (quickrun/remove-temp-files))

(defun quickrun/command-synchronous (cmd)
  (destructuring-bind (program . args) (split-string cmd)
    (let* ((buf (get-buffer-create quickrun/buffer-name))
           (compile-func (apply-partially 'call-process program nil buf t)))
      (with-current-buffer buf
        (erase-buffer))
      (let ((command-status (apply compile-func args)))
        (unless (= command-status 0)
          (pop-to-buffer buf)
          (throw 'compile 'compile-error))))))

;;
;; Execute
;;
(defvar quickrun/timeout-timer nil)
(make-variable-buffer-local 'quickrun-timeout-timer)

(defun quickrun/run (cmd)
  (destructuring-bind (program . args) (split-string cmd)
    (let* ((buf (get-buffer-create quickrun/buffer-name))
           (proc-name (format "quickrun-process-%s" (buffer-name)))
           (run-func (apply-partially 'start-process proc-name buf program)))
      (message "Quickrun Execute: %s" cmd)
      (with-current-buffer buf
        (erase-buffer))
      (lexical-let ((process (apply run-func args)))
        (if quickrun/timeout-seconds
            (setq quickrun/timeout-timer
                  (run-at-time quickrun/timeout-seconds nil
                               #'quickrun/kill-process process)))
        process))))

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
  (let ((tmpl (assoc-default key lang-info)))
    (when tmpl
      (cond ((functionp tmpl)
             (let ((ret (funcall tmpl)))
               (or (and (stringp ret) ret)
                   (error "%s's param should return string" key))))
            (t tmpl)))))

(defconst quickrun/default-tmpl-alist
  '((:exec . "%c %o %s %a")))

(defun quickrun/check-has-command (cmd &optional cleanup)
  (unless (executable-find cmd)
    (and cleanup (funcall cleanup cmd))))

(defun quickrun/fill-templates (lang src &optional argument)
  (let* ((lang-info (quickrun/get-lang-info lang))
         (cmd       (or (quickrun/get-lang-info-param :command lang-info)
                        (error "not specified command parameter in %s") lang))
         (cmd-opt   (or quickrun-command-option
                        (quickrun/get-lang-info-param :cmdopt lang-info) ""))
         (arg       (or argument quickrun-command-argument
                        (quickrun/get-lang-info-param :argument lang-info)
                        ""))
         (tmpl-arg (quickrun/place-holder-info :command cmd
                                               :command-option cmd-opt
                                               :source src
                                               :argument arg))
         (info (make-hash-table)))
    (quickrun/check-has-command cmd #'(lambda (cmd)
                                        (quickrun/remove-temp-files)
                                        (error "Command not found: %s" cmd)))
    (dolist (key `(:compile :compile-only :link :exec))
      (let ((tmpl (or (quickrun/get-lang-info-param key lang-info)
                      (assoc-default key quickrun/default-tmpl-alist))))
        (if tmpl
            (puthash key (quickrun/fill-template tmpl tmpl-arg) info))))
    (let ((remove-tmpl (quickrun/get-lang-info-param :remove lang-info)))
      (if remove-tmpl
          (puthash :remove (mapcar (lambda (x)
                                     (quickrun/fill-template x tmpl-arg))
                                   remove-tmpl) info)))
    info))

(defun quickrun/fill-template (tmpl info)
  (let ((place-holders quickrun/template-place-holders)
        (str tmpl)
        (case-fold-search nil))
    (dolist (holder place-holders str)
      (let ((rep (assoc-default holder info)))
        (setq str (replace-regexp-in-string holder rep str t))))))

;;
;; initialize
;;
(defun quickrun/windows-p ()
  (or (string= system-type "ms-dos")
      (string= system-type "windows-nt")))

(defconst quickrun/support-languages
  '("c" "c++" "objc" "perl" "ruby" "python" "php" "emacs" "lisp" "scheme"
    "javascript" "clojure" "erlang" "ocaml" "go" "io" "haskell" "java" "d"
    "markdown" "coffee" "scala" "groovy" "sass" "less" "shellscript" "awk" "lua")
  "Programming languages supported by quickrun.el")

(defvar quickrun/lang-key
  (make-hash-table :test #'equal))

(defun* quickrun-add-parameter (key alist &key default extension mode)
  (cond ((not key) (error "undefined 1st argument 'key'"))
        ((not alist) (error "undefined 2nd argument 'language alist'"))
        ((not (assoc :command alist))
         (error "not found :command parameter in language alist")))
  (push (cons key alist) quickrun/language-alist)
  (let ((lang-key (or default key)))
    (when default
      (unless (member default quickrun/support-languages)
        (error "(:default parameter) %s is not support language" default))
      (puthash lang-key key quickrun/lang-key))
    (if extension
        (push (cons extension lang-key) quickrun/extension-alist))
    (if mode
        (push (cons mode lang-key) quickrun/major-mode-alist))))

(defun quickrun/find-executable (lst)
  (or (find-if (lambda (cmd) (executable-find cmd)) lst) ""))

(defun quickrun/set-lang-key (lang candidates)
  (let ((lang-key (concat lang "/" (quickrun/find-executable candidates))))
    (if lang-key
        (puthash lang lang-key quickrun/lang-key))))

(defun quickrun/add-command-if-windows (cmd lst)
  (if (quickrun/windows-p)
      (append cmd lst)
    lst))

(defconst quicklang/lang-candidates
  `(("c" . ,(quickrun/add-command-if-windows "cl" '("gcc" "clang")))
    ("c++" . ,(quickrun/add-command-if-windows "cl" '("g++" "clang++")))
    ("javascript" . ("node" "v8" "js" "jrunscript" "cscript"))
    ("scheme" . ("gosh"))
    ("markdown" . ("Markdown.pl" "kramdown" "bluecloth" "redcarpet" "pandoc"))
    ("clojure" . ("jark" "clj-env-dir"))
    ("go" . ("8g" "6g" "5g")))
  "Candidates of language which has some compilers or interpreters")

(defun quickrun/init-lang-key ()
  "Decide command for programing language which has multiple candidates"
  (dolist (lang quickrun/support-languages)
    (puthash lang lang quickrun/lang-key))
  (loop for (lang . candidates) in quicklang/lang-candidates
        do
        (quickrun/set-lang-key lang candidates)))

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

(defvar quickrun-last-lang nil)
(make-local-variable 'quickrun-last-lang)

(defun quickrun-lang (lang)
  "Run specified commands quickly for current buffer"
  (interactive
   (list (completing-read
          (format "QuickRun Lang%s: "
                  (and quickrun-last-lang
                       (format "[Default: %s]" quickrun-last-lang)))
          quickrun/language-alist
          nil nil nil nil quickrun-last-lang)))
  (quickrun-common :language lang))

(defvar quickrun/compile-only-flag nil)
(make-local-variable 'quickrun/compile-only-flag)

(defun quickrun-compile-only ()
  (interactive)
  (let ((quickrun/compile-only-flag t))
    (quickrun-common)))

(defvar quickrun/remove-files nil)
(make-local-variable 'quickrun/remove-files)

(defun quickrun/get-lang-key (lang)
  (or (gethash lang quickrun/lang-key) lang
      (error "Can't found language setting")))

(defun quickrun/add-remove-files (files)
  (if (listp files)
      (setq quickrun/remove-files (append files quickrun/remove-files))
    (push files quickrun/remove-files)))

(defun quickrun/temp-name (src lang)
  (let ((extension (or (and lang (quickrun/extension-from-lang lang))
                       (file-name-extension src))))
   (concat (make-temp-name "qr_") "." extension)))

(defun quickrun/compile-command (cmd-info)
  (or (gethash :compile cmd-info)
      (and quickrun/compile-only-flag (gethash :compile-only cmd-info))))

(defun quickrun/link-command (cmd-info)
  (or (and quickrun/compile-only-flag nil)
      (gethash :link cmd-info)))

(defun* quickrun-common (&key argument language)
  (let* ((orig-src (file-name-nondirectory (buffer-file-name)))
         (lang (quickrun/decide-file-type orig-src))
         (lang-key (or language (quickrun/get-lang-key lang)))
         (src (quickrun/temp-name orig-src lang)))
    (setq quickrun-last-lang lang-key)
    (cond ((string= lang-key "java") (setq src orig-src))
          (t
           (copy-file orig-src src)
           (quickrun/add-remove-files src)))
    (let* ((cmd-info-hash (quickrun/fill-templates lang-key src argument))
           (compile-status
            (let ((compile-cmd (quickrun/compile-command cmd-info-hash))
                  (link-cmd (quickrun/link-command cmd-info-hash)))
              (catch 'compile
                (when (and quickrun/compile-only-flag (null compile-cmd))
                  (message "[%s] compilation command not found" lang-key)
                  (throw 'compile 'command-not-found))
                (and compile-cmd
                     (quickrun/compile-and-link compile-cmd link-cmd))))))
      (cond ((member compile-status '(compile-error command-not-found))
             (unless (string= orig-src src)
               (delete-file src)))
            (t
             (quickrun/add-remove-files (gethash :remove cmd-info-hash))
             (unless quickrun/compile-only-flag
               (let ((process (quickrun/run (gethash :exec cmd-info-hash))))
                 (set-process-sentinel process #'quickrun/sentinel))))))))

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

;;
;; file local variable
;; Based on shadow.el. https://raw.github.com/mooz/shadow.el/master/shadow.el
;;
(defmacro quickrun/defvar (name &optional value safep doc)
  "Define buffer-local and safe-local variable."
  (declare (indent defun))
  `(progn
     (defvar ,name ,value ,doc)
     (make-variable-buffer-local (quote ,name))
     ;; Suppress file local variable warning
     ,(when safep
        `(put (quote ,name) 'safe-local-variable (quote ,safep)))))

(quickrun/defvar quickrun-command-option
                 nil stringp
                 "Specify command option directly as as file local variable")

(quickrun/defvar quickrun-command-argument
                 nil stringp
                 "Specify command argument directly as as file local variable")

(provide 'quickrun)
;;; quickrun.el ends here
