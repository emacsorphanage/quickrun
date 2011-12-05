;;;
;;; Sample configuration of `quickrun.el'
;;;

;; require quickrun.el
(require 'quickrun)

;; Add C++ command for C11 and set it default in C++ file.
(quickrun-add-parameter "c++/c11"
                        '((:command . "g++")
                          (:compile . "%c -std=c++0x %o -o %n %s")
                          (:exec    . "%n %a")
                          (:remove  . ("%n")))
                        :default "c++")

;; Add pod command and set to use when extension of file is '.pod'
;; or major-mode of file is pod-mode.
(quickrun-add-parameter "pod"
                        '((:command . "perldoc")
                          (:exec    . "%c -T -F %s"))
                        :extension "pod" :mode 'pod-mode)

;; If you have gcc and clang, quickrun set `gcc' as default,
;; `quickrun-set-default' change default command(2nd argument)
;; in language(1st argument).
;; Following, quickrun uses clang in C file.
(quickrun-set-default "c" "c/clang")
