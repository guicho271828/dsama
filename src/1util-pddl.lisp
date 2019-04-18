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

;; pddl utilities

(in-package :dsama)

(defmethod print-object ((o (eql nil)) s)
  (write-char #\( s)
  (write-char #\) s))

(defun sym (&rest args)
  (intern (format nil "~{~a~}" args)))

(defvar *predicates*)
(declaim (hash-table *predicates*))

(defun substitute-vars (tree)
  "Replace the propositional variables in the logical tree into zero-ary predicates"
  (etypecase tree
    (list
     (mapcar #'substitute-vars tree))
    (atom
     (ematch tree
       ((symbol)
        (if (gethash tree *predicates*)
            (list tree)
            tree))
       (_
        tree)))))

(defvar *action-max*)
(defvar *action-current*)
(defvar *feature-max*)
(defvar *feature-current*)

(defparameter *dim-larger-than-feature-hook* 'z-normal-case)

(defun z-normal-case (dim)
  (let ((r (sym 'z dim)))
    (setf (gethash r *predicates*) t)
    r))

(defun z (dim)
  "(z 0) returns a symbol Z0 (for predicates)"
  (let ((r (if (< dim *feature-max*)
               (z-normal-case dim)
               (funcall *dim-larger-than-feature-hook* dim))))
    (assert r)
    r))

(defun a (n)
  "(a 0) returns a symbol A0"
  (sym 'a n))

(defvar *axiom-formula*)
(defvar *formula-axiom*)
(declaim (hash-table *axiom-formula* *formula-axiom*))

(defun as-axiom (formula &optional (prefix 'x) (count (hash-table-count *formula-axiom*)) force)
  "Store the logical formula as an axiom and returns the axiom."
  (let ((formula (cl-sat:simplify-nnf (cl-sat:to-nnf formula))))
    (flet ((intern-axiom (formula)
             ;; still does not allow duplicated formula??
             (or (gethash formula *formula-axiom*)
                 (let ((name (sym prefix count)))
                   (setf (gethash name    *predicates*)    t
                         (gethash name    *axiom-formula*) formula
                         (gethash formula *formula-axiom*) name)
                   name))))
      (if force
          (intern-axiom formula)
          (match formula
            ;; don't make axioms for these simple clauses
            ((list 'and) formula)
            ((list 'or)  formula)
            ((list 'not _) formula)           ; nnf, therefore NOT means a leaf
            ((symbol) formula)
            (_ (intern-axiom formula)))))))

(defun dump-axioms ()
  (iter (for (name formula) in-hashtable *axiom-formula*)
    (collect
        `(:derived ,name ,formula))))

(defgeneric to-precondition (model))
(defgeneric to-effect       (model i))

(defun to-effects (models)
  (iter
    (for i from 0)
    (for m in-sequence models)
    (for effect = (to-effect m i))
    (when (first-iteration-p)
      (collect 'and))
    (when effect
      (collect effect))))
