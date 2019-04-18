#|

This file is a part of DSAMA project.
Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

DSAMA is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

DSAMA is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
DSAMA.  If not, see <http://www.gnu.org/licenses/>.

|#

#|

Code for converting a random forest into logical conditions

|#

(in-package :dsama)

(defun expand-forest (forest)
  (let ((vars (mapcar (lambda (tree) (as-axiom (expand-dtree tree) 'tree))
                      (cl-random-forest::forest-dtree-list forest))))
    (cl-sat:simplify-nnf
     (cl-sat:to-nnf
      (majority-formula-bitonic-sort vars)))))

(defun expand-dtree (dtree)
  "Returns a condition that the decision tree returns the class 1 (true)."
  (match (expand-node (cl-random-forest::dtree-root dtree))
    (t `(and))
    (nil `(or))
    (it it)))

(defun expand-node (node)
  "For the leaf nodes, returns T if constantly true; NIL if false.
For the non-leaf nodes, it normally returns a condition tree (cons),
but if it is known that the condition is never satisfied, then may return T or NIL."
  (ematch node
    ((clrf::node (clrf::test-attribute attr)
                 (clrf::test-threshold th)
                 (clrf::left-node l)
                 (clrf::right-node r))
     (if attr
         (flet ((left ()
                  (let ((result (expand-node l)))
                    (match result
                      (t   (z attr))
                      (nil nil)
                      (_   `(and ,(z attr) ,result)))))
                (right ()
                  (let ((result (expand-node r)))
                    (match result
                      (t   `(not ,(z attr)))
                      (nil nil)
                      (_   `(and (not ,(z attr)) ,result))))))
           (cond
             ((<= th 0) (left))           ;always true
             ((<= 1 th) (right))          ;alwyas false
             (t                           ; 0<th<1; either way is possible
              (match* ((left) (right))
                ((nil nil) nil)
                ((nil r) r)
                ((l nil) l)
                ;; ((t t) t)
                ;; ((t r) t)
                ;; ((l t) t)
                ((l r) `(or ,l ,r))))))
         (let ((distribution (clrf::node-class-distribution node)))
           (<= 0.5 (/ (cl:aref distribution 1) *pu-coefficient*)))))))

;; https://en.wikipedia.org/wiki/Bitonic_sorter#Example_code

(defun %len/2 (inputs) (floor (length inputs) 2))

(defun majority-formula-bitonic-sort (inputs)
  (elt (%bitonic-sort t inputs)
       (%len/2 inputs)))

(defun %bitonic-sort (up inputs)
  (if (= 1 (length inputs))
      inputs
      (let ((half1 (%bitonic-sort t   (subseq inputs 0 (%len/2 inputs))))
            (half2 (%bitonic-sort nil (subseq inputs (%len/2 inputs)))))
        (%bitonic-merge up (append half1 half2)))))

(defun %bitonic-merge (up inputs)
  (if (= 1 (length inputs))
      inputs
      (progn
        (setf inputs (%bitonic-compare up inputs))
        (let ((half1 (%bitonic-merge up (subseq inputs 0 (%len/2 inputs))))
              (half2 (%bitonic-merge up (subseq inputs (%len/2 inputs)))))
          (append half1 half2)))))

(defun %bitonic-compare (up inputs)
  (let ((dist (%len/2 inputs)))
    (if up
        (iter (for i below dist)
              (for x = (elt inputs i))
              (for y = (elt inputs (+ i dist)))
              ;;  0 0 -> 0 0
              ;;  0 1 -> 0 1
              ;;  1 0 -> 0 1
              ;;  1 1 -> 1 1
              (for newx = `(and ,x ,y))
              (for newy = `(or  ,x ,y))
              (collect newx into half1)
              (collect newy into half2)
              (finally
               (return (append half1 half2))))
        (iter (for i below dist)
              (for x = (elt inputs i))
              (for y = (elt inputs (+ i dist)))
              ;;  0 0 -> 0 0
              ;;  0 1 -> 1 0
              ;;  1 0 -> 1 0
              ;;  1 1 -> 1 1
              (for newx = `(or  ,x ,y))
              (for newy = `(and ,x ,y))
              (collect newx into half1)
              (collect newy into half2)
              (finally
               (return (append half1 half2)))))))

;; test

#+(or)
(progn
  (terpri)
  (pprint-linear
   *standard-output*
   (let (*axioms*)
     (list (majority-formula-bitonic-sort '(a b c d e))
           *axioms*))))
