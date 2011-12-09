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

Execute current buffer. If `quickrun.el` does not find command-key,
then `quickrun.el` asks you command-key.

    M-x quickrun

Execute current buffer with specified command.
`quickrun.el` asks you command-key.

    C-u M-x quickrun

Execute region. (Java is not supported)

    M-x quickrun-region

Execute current buffer with arguments.
`quickrun.el` asks you command line argument

    M-x quickrun-with-arg

Compile current buffer with compile.el framework, not execute.

    M-x quickrun-compile-only

Support Programming Languages
-----------------------------
`quickrun.el` supports following programming languages and markup languages
as default. But you can register your own command and apply other languages.

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
`quickrun-add-command` define new command.

    (quickrun-add-command "c++/c11"
                           '((:command . "g++")
                             (:compile . "%c -std=c++0x %o -o %n %s")
                             (:exec    . "%n %a")
                             (:remove  . ("%n")))
                           :default "c++")

    (quickrun-add-command "pod"
                           '((:command . "perldoc")
                             (:exec    . "%c -T -F %s"))
                           :mode 'pod-mode)

quickrun-add-command has key parameters, ':default', ':extension', ':mode'.

* `:default "c++"` means that quickrun uses this command to C++ files as default.
* `:mode 'pod-mode` means that quickrun uses this command when major-mode is pod-mode.

Add new Language setting
------------------------
Alist of **filename patterns** vs corresponding **command-key**.

    (quickrun-add-command "prove" '((:command "prove") (:exec "%c -bv %s")))
    (add-to-list 'quickrun-file-alist '("\\.t$" . "prove"))


If file name is matched to regexp "\\.t$", then quickrun.el uses "prove"
command set for that file.


quickrun-file-alist is similar to `auto-mode-alist`, car of list is
regexp, cdr of list is "command-key".


Change Default Command
----------------------
`quickrun-set-default` changes default command in some language.

    (quickrun-set-default "c" "c/clang")


This means that quickrun uses "c/clang" command in C files.


File Local Variables
--------------------
File local variables is priority to other parameter.

    quickrun-option-cmd-alist

Command alist.

    quickrun-option-command

String expanded to %c.

    quickrun-option-cmdkey

Command key

    quickrun-option-cmdopt

String expanded to %o

    quickrun-option-args

String expanded to %a.

    quickrun-option-shebang

If this value is not 0, and first line of source file is stated "#!",
the following string is treated as ":command".


User Defined Command with file local variables
----------------------------
`quickrun.el` has some file local variable.

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
      quickrun-option-cmd: ((:command . "g++")
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


Command-Alist
-------------
Command alist has ':command', ':compile', ':link', ':exec', ':remove'
parameters.

:command

`:command` paramter is mandatory parameter.

:compile

Compile command template.
Set this parameter when compile command is different from execute command
(Called compile language, like  C, C++, Java etc).

:link

Link command template.
Set this parameter when compile command is different from link command.
(like Go Language).

:execute

Execute command temple.
If this parameter is omitted, `quickrun.el` use default execute
command template "%c %o %s %a".

:remove

Remove files after executing.
If command create some intermediate files, you should set this
parameter. :remove value is atom or list.


Format of Command-Alist
-----------------------
You can use following placeholders in command-alist.

| Placeholder | Result                                        |
|:-----------:|:----------------------------------------------|
|  %c         |  Command                                      |
|  %o         |  Command line option                          |
|  %s         |  Source(absolute path)                        |
|  %a         |  Script's argumetns                           |
|  %n         |  Source without extension(absolute path)      |
|  %N         |  Source without extension(nondirectory)       |
|  %e         |  Source with executable suffix(absolute path) |
|  %E         |  Source with executable suffix(nondirectory)  |


`quickrun.el` copys source file to temporary file firstly,
so name of source file is at random.
