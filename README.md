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
**Programming Language(commands used)**

* C(gcc or clang or Visual C++)
* C++(g++ or clang++ or Visual C++)
* Objective-C(gobjc)
* D Language(dmd)
* Java(JDK javac and java)
* Perl(perl)
* Ruby(ruby)
* Python(python)
* PHP(php)
* Emacs Lisp(emacs)
* Scheme(gosh in Gauche)
* Common Lisp(clisp)
* Clojure(jark or clj-env-dir)
* Javascript(node or v8 or js or jrunscript or cscript)
* Coffee Script(coffee)
* Markdown(Markdown.pl or bluecloth or kramdown or pandoc or redcarpet)
* Haskell(runghc)
* Go Language(8g or 6g or 5g)
* Io(io)
* Lua(lua)
* Groovy(groovy)
* Scala(scala)
* SASS(sass)
* LESS(lessc)
* Erlang(escript)
* Ocaml(ocamlc)
* ShellScript(sheban's shell)
* AWK(awk)
