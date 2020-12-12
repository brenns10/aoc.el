;;; solution.el --- Advent of Code Day 1 -*- lexical-binding: t; -*-
;;
;; Author: Stephen Brennan <http://github.com/brenns10>
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Advent of Code Day 1.
;;  Run me in the same directory as input.txt with:
;;    $ emacs --batch -l solution.el
;;  Or if you open this file within Emacs in the git repo, M-x eval-buffer
;;  should do the trick.
;;
;;; Code:

(defun aoc2020/day01/load-ints (fn)
  "Load a list of newline delimited integers from FN."
  (with-temp-buffer
    (insert-file-contents fn t)
    (goto-char (point-min))
    (let ((numbers '()))
      (while (looking-at "\\([0-9]+\\)$")
        (push (string-to-number (match-string 1)) numbers)
        (forward-line)
        (beginning-of-line)
        )
      numbers
      )
    )
  )

(defun aoc2020/day01/main (fn)
  "Run solution with FN as input."
  (let* ((numbers (aoc2020/day01/load-ints fn))
         (numvec (vconcat (sort numbers #'<) []))
         (start 0)
         (end (- (length numvec) 1)))
    (while (<= start end)
      (let* ((lhs (aref numvec start))
             (rhs (aref numvec end))
             (sum (+ lhs rhs)))
        (cond ((= sum 2020) (progn (message "found! %d * %d = %d" lhs rhs (* lhs rhs))
                                   (setq start (+ start 1))
                                   (setq end (- end 1))))
              ((< sum 2020) (setq start (+ start 1)))
              (t (setq end (- end 1)))
        ))
      )
    ))

(aoc2020/day01/main "input.txt")

(provide 'aoc2020-day01-solution)
;;; solution.el ends here
