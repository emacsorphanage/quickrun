;;; test-quickrun.el --- test for quickrun

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'quickrun)

(ert-deftest quickrun:exec-quickrun ()
  "Exec `quickrun'"
  (let ((buf (find-file-noselect "sample/sample.py")))
    (with-current-buffer buf
      (quickrun))
    ;; quickrun is async function
    (sleep-for 1)
    (with-current-buffer "*quickrun*"
      (let ((str (buffer-substring-no-properties (point-min) (point-max))))
        (should (string= "Hello Python quickrun.el\n" str))))
    (sleep-for 1)))

(ert-deftest quickrun:add-command ()
  "Add new command"
  (quickrun-add-command "-test"
                        '((:command . "test foo")
                          (:description . "test description")))
  (let ((params (assoc-default "-test" quickrun--language-alist)))
    (should params)
    (let ((command (assoc-default :command params))
          (desc (assoc-default :description params)))
      (should (string= command "test foo"))
      (should (string= desc "test description")))))

(ert-deftest quickrun:override-configuration ()
  "Override registerd command"
  (quickrun-add-command "c/gcc"
                        '((:command . "clang")
                          (:description . "Compile clang"))
                        :override t)
  (let* ((params (assoc-default "c/gcc" quickrun--language-alist))
         (command (assoc-default :command params)))
    (should (string= command "clang"))))

(ert-deftest quickrun:use-tempfile-p ()
  "Whether use temporary file or not."
  (quickrun-add-command "tempfile0" '((:command . "tempfile0") (:tempfile . t)))
  (let ((use-tempfile (quickrun--use-tempfile-p "tempfile0")))
    (should use-tempfile))

  (quickrun-add-command "tempfile1" '((:command . "tempfile1") (:tempfile . nil)))
  (let ((use-tempfile (quickrun--use-tempfile-p "tempfile1")))
    (should-not use-tempfile))

  ;; use temporary file if :tempfile paramter is not specified
  (quickrun-add-command "tempfile2" '((:command . "tempfile2")))
  (let ((use-tempfile (quickrun--use-tempfile-p "tempfile2")))
    (should use-tempfile))

  (let* ((quickrun--compile-only-flag t)
         (use-tempfile (quickrun--use-tempfile-p "hoge")))
    (should-not use-tempfile)))

(ert-deftest quickrun:user-manually-terminates-program ()
  "Users are able to manually terminate the program with no errors."
  (with-current-buffer (find-file-noselect "sample/sample_endless_loop.bash")
    (quickrun))
  (sleep-for 1)
  (condition-case err
      (let ((buf (get-buffer "*quickrun*"))
            (inhibit-message t))
        (set-process-query-on-exit-flag (get-buffer-process buf) nil)
        (kill-buffer buf)
	    (sleep-for 1)) ; Wait for the sentinel.
    (wrong-type-argument
     (quickrun--remove-temp-files)
     (signal 'error "Cannot access a deleted buffer"))))

;;; test-quickrun.el ends here
