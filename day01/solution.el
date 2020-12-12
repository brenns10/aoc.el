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

(defun aoc2020/day01/find2-sumto (numvec target start end)
  "Find two elements of NUMVEC summing to TARGET.

Starting at index START and ending at index END, try to find two elements of the
sorted vector NUMVEC, which sum to TARGET."
  (let* ((lhs (aref numvec start))
         (rhs (aref numvec end))
         (sum (+ lhs rhs)))
    (while (and (< start end) (not (= sum target)))
        (cond ((< sum target) (setq start (+ start 1)))
              ((> sum target) (setq end (- end 1)))
        )
        (setq lhs (aref numvec start))
        (setq rhs (aref numvec end))
        (setq sum (+ lhs rhs))
      )
    (if (= sum target) (list lhs rhs (* lhs rhs)) nil)
    )
  )

(defun aoc2020/day01/find3-sumto (numvec target)
  "Find three elements of NUMVEC summing to TARGET."
  (let* ((i 0)
         (end (- (length numvec) 3))
         (cur (aref numvec i))
         (res nil))
    (while (and (< i end) (not res))
      (setq res (aoc2020/day01/find2-sumto numvec (- target cur) (+ i 0) (- (length numvec) 1)))
      (if (not res)
          (progn (setq i (+ i 1))
                 (setq cur (aref numvec i)))
        )
      )
    (if res
        (list cur (elt res 0) (elt res 1) (* cur (elt res 0) (elt res 1)))
        nil)
    )
  )

(defun aoc2020/day01/main (fn)
  "Run solution with FN as input."
  (let* ((numbers (aoc2020/day01/load-ints fn))
         ;; This is a weird way to create a vector from a list, why do I need to
         ;; concat here?
         (numvec (vconcat (sort numbers #'<) []))
         (twores (aoc2020/day01/find2-sumto numvec 2020 0 (- (length numvec) 1)))
         (threeres (aoc2020/day01/find3-sumto numvec 2020)))
    (if twores
        (message "Found 2: %d * %d = %d" (elt twores 0) (elt twores 1) (elt twores 2)))
    (if threeres
        (message "Found 3: %d * %d * %d = %d"
                 (elt threeres 0) (elt threeres 1) (elt threeres 2) (elt threeres 3)))
    ))

(aoc2020/day01/main "input.txt")

(provide 'aoc2020-day01-solution)
;;; solution.el ends here
