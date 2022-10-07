[![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

# quickrun.el

## Introduction

**quickrun.el** is Emacs port of [quickrun.vim](https://github.com/thinca/vim-quickrun).

[![CI](https://github.com/emacsorphanage/quickrun/actions/workflows/test.yml/badge.svg)](https://github.com/emacsorphanage/quickrun/actions/workflows/test.yml)
[![travis badge][travis-badge]][travis-link]

`quickrun.el` is a extension to execute editing buffer.
`quickrun.el` is similar to executable-interpret, but `quickrun.el` provides more convenient
commands. `quickrun.el` execute not only script languages(Perl, Ruby, Python etc), but also
compiling languages(C, C++, Go, Java etc) and markup language.

## Requirements

* Emacs 24.3 or higher.

## Installation

You can install `quickrun.el` from [MELPA](https://melpa.org/) with package.el.

Or install directly:

```
$ cd load-path-dir
$ wget https://raw.githubusercontent.com/syohex/emacs-quickrun/master/quickrun.el
```

After Installation add following to your configuration file(~/.emacs.d/init.el, ~/.emacs etc)

```lisp
(require 'quickrun)
```

## Support Programming Languages

`quickrun.el` supports following programming languages and markup languages
as default. But you can register your own command and apply other languages.

**Programming Language(commands used)**

* C(gcc or clang or Visual C++)
* C++(g++ or clang++ or Visual C++)
* C#(dotnet or mono)
* Objective-C(gcc -objc)
* D Language(dmd)
* Fortran(gfortran)
* Java(javac and java)
* Perl(perl)
* Perl6(perl6)
* Ruby(ruby or mruby)
* Python(python)
* PHP(php)
* Emacs Lisp(emacs)
* Scheme(gosh)
* Smalltalk (gst)
* Racket(racket)
* Common Lisp(clisp or sbcl or ccl)
* Clojure(jark or clj-env-dir)
* Javascript(node or v8 or js or jrunscript or cscript)
* Coffee Script(coffee)
* JSX(jsx)
* Markdown(Markdown.pl or bluecloth or kramdown or pandoc or redcarpet)
* Haskell(runghc)
* Go Language(go or gccgo)
* Io(io)
* Lua(lua)
* Groovy(groovy)
* Scala(scala) **Please use UTF-8 encoding**
* HAML(haml)
* SASS(sass)
* LESS(lessc)
* Erlang(escript)
* OCaml(ocamlc)
* F#(fsharpc)
* ShellScript(shebang's shell)
* AWK(awk)
* Rust(rustc)
* Dart(dart)
* Elixir(elixir)
* TypeScript(tsc)
* Tcl(tclsh)
* Swift(swift, xcrun)
* ATS2(patscc)
* R(Rscript)
* Nim/NimScript(nim)
* Julia(julia)
* Gnuplot(gnuplot)
* Kotlin(kotlin)
* Crystal (crystal)
* V(v)
* Applescript(osascript)

See also `quickrun--support-languages` global variable.

## Basic Usage

#### `quickrun`

Execute current buffer. If `quickrun.el` does not find command-key,
then `quickrun.el` asks you command-key(You always input command
if you use `C-u` prefix key)

#### `quickrun-region`

Execute region. (Java is not supported)

#### `quickrun-with-arg`

Execute current buffer with arguments.
`quickrun.el` asks you command line argument

#### `quickrun-shell`

Execute current buffer in eshell for interactive command such as program
which reads input from STDIN.

#### `quickrun-compile-only`

Compile current buffer with compile.el framework, not execute.
quickrun with `C-u C-u` prefix behaves same as quickrun-compile-only.

#### `quickrun-replace-region`

Replace region of code with its output.

### `quickrun-autorun-mode`

Minor mode which executes `quickrun` after saving buffer.

#### `helm-quickrun`

`M-x quickrun` with helm interface

#### `anything-quickrun`

`M-x quickrun` with anything interface

## Note

If quickrun returns `command not found`, please check `(executable-find "THE_COMMAND_NAME")` [for example `(executable-find "gnuplot")`] .
If this returns `nil`, I strongly recommend you use https://github.com/purcell/exec-path-from-shell

## Send File to STDIN

If `executed_file.qrinput`(like `foo.c.qrinput`) is existed in directory same as executed
buffer file, `quickrun.el` sends its content to stdin of executed program. Please set
`quickrun-input-file-extension` to `nil` If you want to disable this feature.

## Customize

### `quickrun-focus-p`(Default: `t`)

If this value is `nil`, quickrun.el does not move focus to output buffer.

## User Defined Command

You can add your own command or override existsing command  by `quickrun-add-command` as below.

```lisp
;; Use this parameter as C++ default
(quickrun-add-command "c++/c1z"
  '((:command . "g++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

;; Use this parameter in pod-mode
(quickrun-add-command "pod"
  '((:command . "perldoc")
    (:exec    . "%c -T -F %s"))
  :mode 'pod-mode)

;; You can override existing command
(quickrun-add-command "c/gcc"
  '((:exec . ("%c -std=c++1z %o -o %e %s"
	      "%e %a")))
  :override t)
```

First argument of `quickrun-add-command` is command key. Second argument of it is
command parameter, which is described laster. `quickrun-add-command` also takes
key parameters, `:default`, `:mode`, `:override`.

| Argument         | Description                                                 |
|:-----------------|:------------------------------------------------------------|
| `:default` lang  | Use this command parameter as default in specified language |
| `:mode` mode     | this command parameter in specified mode                    |
| `:override` bool | Override existing parameter with specified parameter        |

### Command Parameter

Command alist has following parameters,

#### `:command`(mandatory parameter)

Command name. `%c` is expanded into this value.

#### `:cmdopt`(optional)

Command(`:command`) option. `%o` is expanded into this value.

#### `:exec`

Executed commands. You can also set command list parameter.
If you set list parameter, `quickrun.el` executes command
list in order.

If this parameter is omitted, `quickrun.el` use default execute
command template "%c %o %s %a".

#### `:timeout`(optional)

Timeout in seconds for the process spawn by the command. This value
takes precedence over the `quickrun-timeout-seconds` custom variable.

#### `:compile-only`

Command exected by `quickrun-compile-only`. This option is used for
syntax check or converting another language(e.g. CoffeeScript => JavaScript).

### `:compile-conf`

Configuration of `quickrun-compile-only`. This parameter must be alist.

#### `:remove`

Remove files after executing.
If command create some intermediate files, you should set this
parameter. :remove value is atom or list.

#### `:outputter`

Please see Outputter section.

#### `:default-directory`

Directory where commands are executed.

#### `:tempfile`

Use temporary file or not. `quickrun.el` uses temporary file
if you omit this parameter.

NOTE: If you set this parameter, you cannot use `quickrun-region`.

#### `:description`

Description of this command. This parameter is used in
`helm-quickrun` or `anything-quickrun`

### Placeholders

You can use following placeholders in command parameter

| Placeholder | Expanded                                      |
|:-----------:|:----------------------------------------------|
|  `%c`       |  Command                                      |
|  `%o`       |  Command line option                          |
|  `%s`       |  Source(absolute path)                        |
|  `%a`       |  Script's arguments                           |
|  `%n`       |  Source without extension(absolute path)      |
|  `%N`       |  Source without extension(nondirectory)       |
|  `%d`       |  Directory name of Source(absolute path)      |
|  `%e`       |  Source with executable suffix(absolute path) |
|  `%E`       |  Source with executable suffix(nondirectory)  |

Source file name(`%s`, `%n` etc) is not original file name except
Java language. Because `quickrun.el` copys source file to temporary
file firstly.

## Change Default Command

`quickrun-set-default` changes default command in language that is registerd
multiple command parameters(like c, c++,Javascript).

```lisp
(quickrun-set-default "c" "c/clang")
```

This means that quickrun uses "c/clang" for C files.

## Timeout Seconds

`quickrun.el` kills process if program run over 10 seconds as default.
This avoids infinite loop program or endless program by some mistakes.
You control timeout second to set `quickrun-timeout-seconds`.
This feature is disabled if `quickrun-timeout-seconds` is `nil`. The
timeout can also be set per command with the `:timeout` parameter.
(You can also kill process by `C-c C-c` in quickrun buffer)

## Key bindings in quickrun buffer

| Key       | Command                |
|:---------:|:-----------------------|
| `q`       | Close quickrun window  |
| `C-c C-c` | Kill quickrun process  |

## Buffer Local Variables

Buffer local variables is priority to default parameters.

#### `quickrun-option-cmd-alist`

Command alist.

#### `quickrun-option-command`

Command key(Expanded to %c)

#### `quickrun-option-cmdkey`

Command key of command parameter.

#### `quickrun-option-cmdopt`

Command option(Expanded to %o)

#### `quickrun-option-args`

Program argument(Expanded to %a.)

#### `quickrun-option-shebang`

If this value is `non-nil` and first line of source file is started "#!",
the following string is treated as ":command".

#### `quickrun-option-outputter`

Outputter function. See *Outputter* section

### Example of buffer local variable

Setting C++11.

```c++
#include <iostream>
#include <vector>
#include <string>

int main (int argc, char *argv[])
{
    std::vector <std::string> lst = { "a", "b", "c", "d" };

    for (auto x : lst) {
        std::cout << "[" << x << "]" << std::endl;
    }

    for (auto i = 1; i < argc; i++) {
        std::cout << "[" << argv[i] << "]" << std::endl;
    }

    return 0;
}

/*
  Local Variables:
  quickrun-option-cmd-alist: ((:command . "g++")
                              (:exec    . ("%c -std=c++0x -o %n %s"
                                           "%n apple orange melon"))
                              (:remove  . ("%n")))
  End:
*/
```

## Hooks

#### `quickrun-after-run-hook`

Run hooks after execute all commands.

## Outputter

Outputter is a function for processing command output. Default outputter is
to output to \*quickrun\* buffer and processing ANSI Color sequence.

`quickrun.el` defines following functions as default.

#### `buffer:buffername`

Output to buffer. [outputter *buffer* sample](sample/sample_outputter_buffer.pl)

#### `file:filename`

Output to file. [outputter *file* sample](sample/sample_outputter_file.pl)

#### `variable:varname`

Output to variable. [outputter *variable* sample](sample/sample_outputter_variable.pl)

#### `browser`

Output to Web browser(using function *browse-url*) [outputter *browser* sample](sample/sample_outputter_browser.pl)

#### `message`

Output to \*Message\* buffer(using function *message*) [outputter *message* sample](sample/sample_outputter_message.pl)

#### `multi`

Use multiple outputters. [outputter *multi* sample](sample/sample_outputter_multi.pl)

#### `null`

No output. [outputter *null* sample](sample/sample_outputter_null.pl)

## Using quickrun as function from other functions

`quickrun` can be used as function from other functions.
You can pass configuration by `:source` argument.
Sample is following:

```lisp
(defun test-perl ()
  (interactive)
  (let* ((cmd "git rev-parse --show-toplevel")
         (topdir (with-temp-buffer
                   (call-process-shell-command cmd nil t nil)
                   (goto-char (point-min))
                   (if (re-search-forward "^\\(.+\\)$" nil t)
                       (match-string 1)))))
    (quickrun :source `((:command . "prove")
                        (:default-directory . ,topdir)
                        (:exec . ("%c -bv --color %s"))))))
```

[travis-badge]: https://travis-ci.org/emacsorphanage/quickrun.svg
[travis-link]: https://travis-ci.org/github/emacsorphanage/quickrun/builds/
[melpa-link]: https://melpa.org/#/quickrun
[melpa-stable-link]: https://stable.melpa.org/#/quickrun
[melpa-badge]: https://melpa.org/packages/quickrun-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/quickrun-badge.svg
