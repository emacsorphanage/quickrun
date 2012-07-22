#!perl
use strict;
use warnings;

print catfile("/home", "user", "tmp", "perl"), "\n";
print getcwd(), "\n";

__END__

(defun exec-perl ()
  (interactive)
  (quickrun :source '((:command . "perl")
                      (:default-directory . "/home/syohei/tmp")
                      (:exec . ("perl -MCwd -MFile::Spec::Functions %s")))))
