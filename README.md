quickrun.el
==================

Introduction
------------
quickrun.el is emacs version of [quickrun.vim](https://github.com/thinca/vim-quickrun).


quickrun is a extension to execute editing buffer.
quickrun is similar to executable-interpret, but quickrun provides more convenient
commands. quickrun execute not only script languages, but also compiling languages.

Requirements
------------
Emacs 22.1 or higher.


Installation
------------

You have installed auto-install
<pre>
  (install-elisp "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")
</pre>

You have not installed auto-install
<pre>
  $ cd load-path-dir
  $ wget https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el
</pre>

After Installation
<pre>
  (require 'quickrun)
</pre>


Basic Usage
-----------
> M-x quickrun

Run command, compiling, linking, executing.

> M-x quickrun-with-arg

Run command with arguments.

> M-x quickrun-lang

Run command by specified language

Support Programming Languages
-----------------------------
* C(requir gcc or clang or Visual C++)
* C++(require g++ or clang or Visual C++)
* Objective-C(require gobjc)
* D Language(require dmd)
* Java(require JDK)
* Perl
* Ruby
* Python
* PHP
* Emacs Lisp
* Scheme(require gosh in Gauche)
* Common Lisp(require clisp)
* Clojure(require jark or clj-env-dir)
* Javascript(require node.js or v8 or js or jrunscript or cscript)
* Coffee Script(require node.js and coffee)
* Markdown(require Markdown.pl or bluecloth or kramdown or pandoc or redcarpet)
* Haskell(require runghc)
* Go Language(require 8g or 6g or 5g)
* Io
* Groovy
* Scala
* SASS
* LESS(require lessc)
* Erlang(require escript)
* Ocaml(require ocamlc)
* ShellScript
* AWK
