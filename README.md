quickrun.el
==================

Introduction
------------
**quickrun.el** is emacs version of [quickrun.vim](https://github.com/thinca/vim-quickrun).


quickrun is a extension to execute editing buffer.
quickrun is similar to executable-interpret, but quickrun provides more convenient
commands. quickrun execute not only script languages(Perl, Ruby, Python etc), but also
compiling languages(C, C++, Go, Java etc).


**This software is under development. Any API may change**

Requirements
------------
Emacs 22.1 or higher.


I test quickrun on Ubuntu 11.10(Emacs23.3) and MacOSX(Emacs 23.3, 21.1),
I don't test on Windows.


Installation
------------

You have installed auto-install

    (install-elisp "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")

You have not installed auto-install

    $ cd load-path-dir
    $ wget https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el

After Installation

    (require 'quickrun)


Basic Usage
-----------

Run command, compiling, linking, executing.

    M-x quickrun

Run command with arguments.

    M-x quickrun-with-arg

Run command by specified language

    M-x quickrun-lang

Compile only with compile.el framework.

    M-x quickrun-compile-only

Support Programming Languages
-----------------------------
**Programming Language(commands used)**

* C(gcc or clang or Visual C++)
* C++(g++ or clang++ or Visual C++)
* Objective-C(gcc -objc)
* D Language(dmd)
* Java(javac and java)
* Perl(perl)
* Ruby(ruby)
* Python(python)
* PHP(php)
* Emacs Lisp(emacs)
* Scheme(gosh)
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


See also `quickrun/support-languages` global variable.


User Defined Command
--------------------
`quickrun-add-parameter` define new command.

    (quickrun-add-parameter "c++/c11"
                            '((:command . "g++")
                              (:compile . "%c -std=c++0x %o -o %n %s")
                              (:exec    . "%n %a")
                              (:remove  . ("%n")))
                            :default "c++")

    (quickrun-add-parameter "pod"
                            '((:command . "perldoc")
                              (:exec    . "%c -T -F %s"))
                            :extension "pod" :mode 'pod-mode)

quickrun-add-parameter has key parameters, ':default', ':extension', ':mode'.

* `:default "c++"` means that quickrun uses this command to C++ files as default.
* `:extension "pod"` means that quickrun uses this comand to '.pod' files.
* `:mode 'pod-mode` means that quickrun uses this command when major-mode is pod-mode.


Change Default Command
----------------------
`quickrun-set-default` changes default command in some language.

    (quickrun-set-default "c" "c/clang")


This means that quickrun uses "c/clang" command in C files.


User Defined Command by file
----------------------------
quickrun.el let you define your own command by file.
You can do it to use *'quickrun-command'* file local variable.

For example, C11 C++ program file.

    #include <iostream>
    #include <vector>
    #include <string>

    int main (int argc, char *argv[])
    {
        std::vector <std::string> lst = { "a", "b", "c", "d" };

        for (auto x : lst){
            std::cout << "[" << x << "]" << std::endl;
        }

        for (auto i = 1; i < argc; i++){
            std::cout << "[" << argv[i] << "]" << std::endl;
        }

        return 0;
    }

    /*
      Local Variables:
      quickrun-command: ((:command . "g++")
                         (:compile . "%c -std=c++0x -o %n %s")
                         (:exec    . "%n apple orange melon")
                         (:remove  . ("%n")))
      End:
    */

In this case, quickrun compiles this file with following command
(source file is /home/bob/sample/sample.cpp)

    g++ -std=c++0x -o /home/bob/sample/sample /home/bob/sample/sample.cpp

And quickrun execute with command.

    /home/bob/sample/sample apple orange melon
