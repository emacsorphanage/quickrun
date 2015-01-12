;;;
;;; Sample configuration of `quickrun.el'
;;;

;; require quickrun.el
(require 'quickrun)

;; You should assign key binding, if you often use `quickrun' commands.
(global-set-key (kbd "<f5>") 'quickrun)
(global-set-key (kbd "M-<f5>") 'quickrun-compile-only)

;; I recommend you set popwin for quickrun.el
;; See also http://www.emacswiki.org/emacs/PopWin
(push '("*quickrun*") popwin:special-display-config)

;; Add C++ command for C11 and set it default in C++ file.
(quickrun-add-command "c++/c11"
                      '((:command . "g++")
                        (:exec    . ("%c -std=c++11 %o -o %e %s"
                                     "%e %a"))
                        (:remove  . ("%e")))
                      :default "c++")

;; Override existed command
(quickrun-add-command "c/gcc"
                      '((:exec . ("%c -std=c++11 %e -o %e %s"
                                  "%e %a")))
                      :override t)

;; Add pod command and set to use when extension of file is '.pod'
;; or major-mode of file is pod-mode.
(quickrun-add-command "pod"
                      '((:command . "perldoc")
                        (:exec    . "%c -T -F %s"))
                      :mode 'pod-mode)

;; File suffix is '.pod', then `quickrun' use "pod" command-key.
(add-to-list 'quickrun-file-alist '("\\.pod$" . "pod"))

;; If you have gcc and clang, quickrun set `gcc' as default,
;; `quickrun-set-default' change default command(2nd argument)
;; in language(1st argument).
;; Following, quickrun uses clang in C file.
(quickrun-set-default "c" "c/clang")
