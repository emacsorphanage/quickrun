;; -*- mode: eask; lexical-binding: t -*-

(package "quickrun"
         "2.3.1"
         "Run commands quickly")

(website-url "https://github.com/emacsorphanage/quickrun")
(keywords "tools")

(package-file "quickrun.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "26.1")
(depends-on "ht")

(development
 (depends-on "helm")
 (depends-on "ert-runner"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
