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

(defvar quickrun/timeout 10
  "Timeout seconds for running too long process")

(defvar quickrun/process nil)
(make-variable-buffer-local 'quickrun/process)

(defvar quickrun/buffer-name "*quickrun*")

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

    ("c/clang" . ((:command . "gcc")
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

    ("c++/clang++" . ((:command . "g++")
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
               (:exec    . "%c %j %a")
               (:remove  . ("%n.class"))))

    ("perl" . ((:command . "perl")))
    ("ruby" . ((:command . "ruby")))
    ("python" . ((:command . "python")))
    ("php" . ((:command . "php")))

    ("emacs" . ((:command . "emacs")
                (:exec    . "%c -Q --script %s")))
    ("lisp" . ((:command . "clisp")))
    ("scheme/gosh" . ((:command . "gosh")))

    ("clojure/clj-env-dir" . ((:command . "clj-env-dir")))

    ("javascript/node" . ((:command . "node")))
    ("javascript/d8" . ((:command . "d8")))
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
    ("markdown/kranmdown"   . ((:command . "kranmdown")))
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

    ("groovy" . ((:command . "groovy")))
    ("scala" . ((:command . "scala")))
    ("sass" . ((:command . "sass")
               (:exec    . "%c %o --no-cache %s")))

    ("shellscript" . ((:command . "sh")))
    ))

;;
;; decide file type
;;
(defun* quickrun/decide-file-type (filename)
  (if (eq major-mode 'fundamental-mode)
      (quickrun/decide-file-type-by-extension filename)
    (quickrun/decide-file-type-by-mode major-mode)))

(defun quickrun/decide-file-type-by-mode (mode)
  (case mode
    ('c-mode "c")
    ('c++-mode "c++")
    ('objc-mode "objc")
    ('perl-mode "perl") ('cperl-mode "perl")
    ('ruby-mode "ruby")
    ('python-mode "python")
    ('php-mode    "php")
    ('emacs-lisp-mode "emacs")
    ('lisp-mode "lisp")
    ('scheme-mode "scheme")
    ('javascript-mode "javascript") ('js-mode "javascript") ('js2-mode "javascript")
    ('clojure-mode "clojure")
    ('erlang-mode "erlang")
    ('go-mode "go")
    ('haskell-mode "haskell")
    ('java-mode "java")
    ('d-mode "d")
    ('markdown-mode "markdown")
    ('coffee-mode "coffee")
    ('scala-mode "scala")
    ('groove-mode "groovy")
    ('sass-mode "sass")
    ('sh-mode "shellscript")
    (t (error (format "cannot decide file type by mode[%s]" mode)))))

(defconst quickrun/extension-same-as-lang
  '("c" "php" "go" "d" "java" "scala" "coffee" "sass" "groovy")
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
    ("hs"  . "haskell")
    (("md" "markdown" "mdown" "mkdn")  . "markdown")
    ("sh"  . "shellscript")))

(defun quickrun/find-extension-alist (extension)
  (loop for pair in quickrun/extension-alist
        for lang = (car pair)
        when (if (listp lang)
                 (member extension lang)
               (string= extension lang))
        return (cdr pair)))

(defun quickrun/decide-file-type-by-extension (filename)
  (let ((extension (file-name-extension filename)))
    (if (member extension quickrun/extension-same-as-lang)
        extension
      (quickrun/find-extension-alist extension))))

(defun quickrun/extension-from-lang (lang)
  (let ((pair (rassoc lang quickrun/extension-alist)))
    (if pair
        (let ((extensions (car pair)))
          (cond ((listp extensions) (car extensions))
                (t extensions))))))

(defun quickrun/get-lang-info (lang)
  (let ((lang-info (assoc lang quickrun/language-alist)))
    (if (null lang-info)
        (error (format "not found [%s] language information" lang))
      lang-info)))

;;
;; Compile
;;

(defun quickrun/compile-and-link (compile link)
  (dolist (cmd (list compile link))
    (if cmd
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

(defun quickrun/run (cmd)
  (let* ((buf (get-buffer-create quickrun/buffer-name))
         (process-name (format "quickrun-process-%s" (buffer-name)))
         (cmd-list (split-string cmd))
         (program (car cmd-list))
         (args (cdr cmd-list))
         (run-func (apply-partially 'start-process process-name buf program)))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min)))
    (setf quickrun/process (apply run-func args))
    (setf quickrun/timeout-timer
          (run-at-time quickrun/timeout nil #'quickrun/kill-process))
    quickrun/process))

(defun quickrun/sentinel (process state)
  (let ((status (process-status process)))
    (cond
     ((eq status 'exit)
      (progn
        (funcall quickrun/epilogue)
        (pop-to-buffer (process-buffer process))))
     (t nil))))

(defun quickrun/kill-process ()
  (when (eq (process-status quickrun/process) 'run)
    (kill-process quickrun/process)
    (let ((buf (get-buffer-create quickrun/buffer-name)))
      (with-current-buffer buf
        (erase-buffer)
        (insert (message "Time out(running over %d second)"
                         quickrun/timeout)))
      (quickrun/remove-temp-files)
      (pop-to-buffer buf))))

;;
;; Composing command
;;
(defvar quickrun/template-place-holders '("%c" "%o" "%s" "%a" "%n" "%e" "%j"))

(defun quickrun/executable-suffix (command)
  (if (string= command "java")
      ".class"
    (cond
     ((quickrun/windows-p) ".exe")
     (t ".out"))))

(defun* quickrun/place-holder-info (&key command
                                         command-option
                                         source
                                         argument)
  `(("%c" . ,command)
    ("%o" . ,command-option)
    ("%s" . ,source)
    ("%j" . ,(file-name-sans-extension source))
    ("%n" . ,(expand-file-name (file-name-sans-extension source)))
    ("%e" . ,(expand-file-name (concat (file-name-sans-extension source)
                                       (quickrun/executable-suffix command))))
    ("%a" . ,argument)))

(defun quickrun/get-lang-info-param (key lang-info)
  (let ((tmpl (assoc key lang-info)))
    (if tmpl
        (cdr tmpl))))

(defconst quickrun/default-tmpl-alist
  '((:exec . "%c %o %s %a")))

(defun quickrun/fill-templates (lang src &optional argument)
  (let* ((lang-info (quickrun/get-lang-info lang))
         (cmd       (quickrun/get-lang-info-param :command lang-info))
         (cmd-opt   (or (quickrun/get-lang-info-param :cmdopt lang-info) ""))
         (arg       (or argument
                        (quickrun/get-lang-info-param :argument lang-info)
                        ""))
         (tmpl-arg (quickrun/place-holder-info :command cmd
                                               :command-option cmd-opt
                                               :source src
                                               :argument arg))
         (info (make-hash-table)))
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
        (setf str (replace-regexp-in-string holder rep str nil))))))

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

(defun quickrun/init-lang-key ()
  (let ((c-candidates          '("gcc" "clang"))
        (c++-candidates        '("g++" "clang++"))
        (javascript-candidates '("node" "d8" "js"
                                 "phantomjs" "jrunscript" "cscript"))
        (scheme-candidates     '("gosh" "mzscheme"))
        (markdown-candidates   '("Markdown.pl" "krandown"
                                 "bluecloth" "redcarpet" "pandoc"))
        (clojure-candidates    '("jark" "clj-env-dir"))
        (go-candidates         '("8g" "6g" "5g")))
   (progn
     (quickrun/set-lang-key "c" (if (quickrun/windows-p)
                                    (append "cl" c-candidates)
                                  c-candidates))
     (quickrun/set-lang-key "c++" (if (quickrun/windows-p)
                                      (append "cl" c++-candidates)
                                    c++-candidates))
     (quickrun/set-lang-key "javascript" javascript-candidates)
     (quickrun/set-lang-key "scheme" scheme-candidates)
     (quickrun/set-lang-key "markdown" markdown-candidates)
     (quickrun/set-lang-key "clojure" clojure-candidates)
     (quickrun/set-lang-key "go" go-candidates))))

(quickrun/init-lang-key)

;;
;; main
;;
(defun quickrun ()
  (interactive)
  (quickrun-common))

(defun quickrun-with-arg (arg)
  (interactive
   (list (read-string "QuickRun Arg: ")))
  (quickrun-common :argument arg))

(defun quickrun-lang (lang)
  (interactive
   (list (completing-read "QuickRun Lang: " quickrun/language-alist)))
  (quickrun-common :language lang))

(defvar quickrun/remove-files nil)

(defun quickrun/check-has-command (lang)
  (let* ((lang-info (quickrun/get-lang-info lang))
         (cmd (cdr (assoc :command lang-info))))
    (if cmd
        (if (executable-find cmd)
            cmd
          (error "Command not found: %s" cmd))
      (error "Internal error: ':command' parameter not found in %s" lang))))

(defun* quickrun-common (&key argument language)
  (let* ((orig-src (file-name-nondirectory (buffer-file-name)))
         (lang (quickrun/decide-file-type orig-src))
         (lang-key (or language (gethash lang quickrun/lang-key) lang))
         (src (concat (make-temp-name "qr_")
                      "." (or (and lang (quickrun/extension-from-lang lang))
                              (file-name-extension orig-src)))))
    (quickrun/check-has-command lang-key)
    (if (string= lang "java")
        (setf src orig-src)
      (copy-file orig-src src))
    (let* ((cmd-info-hash (quickrun/fill-templates lang-key src argument))
           (compile-cmd   (gethash :compile cmd-info-hash))
           (link-cmd      (gethash :link    cmd-info-hash))
           (exec-cmd      (gethash :exec    cmd-info-hash))
           (compile-state
            (catch 'compile
              (if compile-cmd
                  (quickrun/compile-and-link compile-cmd link-cmd)))))
      (cond ((eq compile-state 'compile-error)
             (if (not (string= orig-src src))
                 (delete-file src)))
            (t
             (let ((process (quickrun/run exec-cmd))
                   (remove-files (gethash :remove cmd-info-hash)))
               (setf quickrun/remove-files
                     (if (string= orig-src src)
                         remove-files
                       (cons src remove-files)))
               (set-process-sentinel quickrun/process #'quickrun/sentinel)))))))

(defun quickrun/remove-temp-files ()
  (dolist (file quickrun/remove-files)
    (if (file-exists-p file)
        (cond
         ((file-directory-p file) (delete-directory file t))
         (t (delete-file file))))))

(defun quickrun/sentinel (process state)
  (let ((status (process-status process)))
    (cond
     ((eq status 'exit)
      (progn
        (message "Finish %s" (process-command process))
        (quickrun/remove-temp-files)
        (pop-to-buffer (process-buffer process))))
     (t nil))
    (delete-process process)
    (cancel-timer quickrun/timeout-timer)))

(provide 'quickrun)
