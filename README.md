Advent of Code, Emacs Lisp Edition
==================================

These are some solutions to advent of code 2020, but using Emacs Lisp. I don't
expect to have time to solve them all, but I thought it would be a good exercise
to help me learn the Emacs Lisp programming language.

Emacs can run things as a script without loading the editor. Command line
argument support isn't particularly good, so I've hard-coded input files. Simply
CD into the directory, and then do something like this:

    emacs --batch -l solution.el

You can also open the files in Emacs and use `M-x eval-buffer`.
