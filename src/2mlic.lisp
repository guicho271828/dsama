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

MLIC --- not used.

|#


(in-package :dsama)

(defclass cnf-classifier (remlic:cnf classifier) ())
(defclass binary-cnf-classifier (cnf-classifier binary-classifier) ())
(defclass diff-cnf-classifier   (diff-classifier binary-cnf-classifier) ())
(defclass extrinsic-cnf-classifier   (extrinsic-classifier binary-cnf-classifier) ())


(defclass cnf+-classifier (remlic:cnf+ classifier) ())
(defclass binary-cnf+-classifier (cnf+-classifier binary-classifier) ())
(defclass diff-cnf+-classifier   (diff-classifier binary-cnf+-classifier) ())
(defclass extrinsic-cnf+-classifier   (extrinsic-classifier binary-cnf+-classifier) ())


(defclass eqv-classifier (remlic:eqv classifier) ())
(defclass binary-eqv-classifier (eqv-classifier binary-classifier) ())
(defclass diff-eqv-classifier   (diff-classifier binary-eqv-classifier) ())
(defclass extrinsic-eqv-classifier   (extrinsic-classifier binary-eqv-classifier) ())


(defclass eqv+-classifier (remlic:eqv+ classifier) ())
(defclass binary-eqv+-classifier (eqv+-classifier binary-classifier) ())
(defclass diff-eqv+-classifier   (diff-classifier binary-eqv+-classifier) ())
(defclass extrinsic-eqv+-classifier   (extrinsic-classifier binary-eqv+-classifier) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(or)
(defmethod train-precondition ((type (eql :mlic)) params pres fake)
  (destructuring-bind (rules &key (pu t) (train-ratio *train-ratio*) &allow-other-keys) params
    (let* ((true (-<> pres
                   add-true-labels
                   (asarray <> :type 'bit)))
           (fake (-<> fake
                   add-false-labels
                   (asarray <> :type 'bit)))
           (dataset (shuffle (concatenate (list true fake))))
           (in        (aref dataset t '(0 -2)))
           (out       (round (aref dataset t -1)))
           (train-num (floor (* *train-ratio* (length pres))))
           (train-in  (aref pres `(0 ,train-num)))
           (train-out (aref answers `(0 ,train-num)))
           (val-in  (aref pres `(,train-num t)))
           (val-out (aref answers `(,train-num t)))
           (model (apply #'mlic-iterative
                         train-in
                         train-out
                         rules
                         :val-input val-in
                         :val-output val-out
                         (rest params))))
      (with-slots (train-accuracy val-accuracy) model
        (setf train-accuracy (evaluate model train-in train-out)
              val-accuracy   (evaluate model val-in val-out))
        model))))

(defun z* (n)
  (if (minusp n)
      `(not ,(z (lognot n)))
      (z n)))

(defmethod to-preconditon ((m remlic:rule))
  (cl-sat:simplify-nnf
   (first (logical-form m :map-fn #'z*))))

(defmethod to-effect ((m remlic:rule) i)
  (match (cl-sat:simplify-nnf (logical-form m :map-fn #'z*))
    ((list 'or)            ;constantly false -- always delete
     (collect `(not ,(z i))))
    ((list 'and)           ;constantly true -- always add
     (collect (z i)))
    ((eq (z i))            ; e.g. (when z0 z0) (when (not z0) (not z0)) --- no change
     nil)
    (effect-condition
     (collect `(when ,effect-condition
                 ,(z i)))
     (collect `(when ,(cl-sat:to-nnf `(not ,effect-condition))
                 (not ,(z i)))))))
