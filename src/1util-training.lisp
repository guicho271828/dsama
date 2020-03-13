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

;; logging utilities

(in-package :dsama)

;; true = 1, false = 0

(defun add-true-labels (pres)
  (concatenate (list pres
                     (ones (list (length pres) 1)))
               :axis -1))

(defun add-false-labels (pres)
  (concatenate (list pres
                     (zeros (list (length pres) 1)))
               :axis -1))

(defvar *train-ratio* 0.9)

(defvar *pu-coefficient*)

(defgeneric %train-precondition (model params valid fake valid2 invalid))

(defgeneric %train-effect       (model params pres sucs feature))

(defmethod %train-precondition ((model binary-classifier) params valid fake valid2 invalid)
  (destructuring-bind (&key (train-ratio *train-ratio*) &allow-other-keys) params
    (let* ((dataset (shuffle (concatenate (list (add-true-labels valid)
                                                (add-false-labels fake)))))
           (in        (aref dataset t '(0 -1)))
           (out       (aref dataset t -1))
           (len (length dataset))
           (train-num (floor (* train-ratio len)))
           ;; ensure the training set has at least 1 element
           (train-in  (aref in  `(0 ,(max 1 train-num))))
           (train-out (aref out `(0 ,(max 1 train-num))))
           ;; ensure the validation set has at least 1 element
           (val-in  (aref in  `(,(min train-num (1- len)))))
           (val-out (aref out `(,(min train-num (1- len))))))
      (train model
             train-in train-out
             :params params
             :val-input val-in :val-output val-out)

      (let* ((ppred (predict model valid2))
             (npred (predict model invalid))
             (pos  (length valid2))
             (neg  (length invalid))
             (len  (+ pos neg))
             (tp   (sum ppred))
             (tn   (- neg (sum npred)))
             (tpr  (ignore-errors (/ tp pos)))
             (tnr  (ignore-errors (/ tn neg))))

        ;; note: these ratio values are not used in the final evaluations anyways
        (setf (getf *logs* :test)
              `(:object :accuracy         ,(ignore-errors (/ (+ tp tn) len))
                        :recall           ,tpr
                        :specificity      ,tnr
                        :f                ,(ignore-errors (/ (* 2 tpr tnr) (+ tpr tnr)))
                        :true-positive    ,tp
                        :true-negative    ,tn
                        :positive-samples ,pos
                        :negative-samples ,neg
                        :samples          ,len)))
      model)))

(defmethod %train-effect ((model binary-classifier) params pres sucs feature)
  (destructuring-bind (&key (train-ratio *train-ratio*) &allow-other-keys) params
    (let* ((len (length pres))
           (train-num (floor (* train-ratio len)))
           (train-in  (aref pres `(0 ,(max 1 train-num))))
           (train-out (aref sucs `(0 ,(max 1 train-num))))
           ;; ensure the validation set has at least 1 element
           (val-in  (aref pres `(,(min train-num (1- len)) t)))
           (val-out (aref sucs `(,(min train-num (1- len)) t))))
      (assert (plusp len))
      
      (train model
             train-in train-out
             :params params
             :val-input val-in :val-output val-out)

      (flet ((fn (tag in out)
               (let* ((pred (predict model in))
                      (len  (length out))
                      (pos  (sum out))
                      (neg  (- len pos))
                      (tp   (sum (min out pred)))
                      (tn   (- len (sum (max out pred))))
                      (tpr  (ignore-errors (/ tp pos)))
                      (tnr  (ignore-errors (/ tn neg))))
                 (setf (getf *logs* tag)
                       `(:object :accuracy         ,(ignore-errors (/ (+ tp tn) len))
                                 :recall           ,tpr
                                 :specificity      ,tnr
                                 :f                ,(ignore-errors (/ (* 2 tpr tnr) (+ tpr tnr)))
                                 :true-positive    ,tp
                                 :true-negative    ,tn
                                 :positive-samples ,pos
                                 :negative-samples ,neg
                                 :samples          ,len)))))
        (fn :train train-in train-out)
        (fn :val val-in val-out))
      model)))

(defclass diff-classifier () ((feature :type 'fixnum))
  (:documentation "classifier that tries to predict the diff"))

(defmethod %train-effect ((model diff-classifier) params pres sucs feature)
  (setf (slot-value model 'feature) feature)
  (call-next-method model params pres (logxor (aref pres t feature) sucs) feature))

(defmethod predict ((model diff-classifier) input &key)
  (with-slots (feature) model
    (logxor (aref input t feature)
            (call-next-method))))


(defclass extrinsic-classifier () ((feature :type 'fixnum))
  (:documentation "classifier that tries to predict one bit from informations of
  other bits, excluding the former itself"))

(defmethod %train-effect ((model extrinsic-classifier) params pres sucs feature)
  (setf (slot-value model 'feature) feature)
  ;; learn the resulting feature bit from the data except the same feature in the previous state
  (call-next-method model params
                    (concatenate (list (aref pres t `(t ,feature))
                                       (aref pres t `(,(1+ feature) t)))
                                 :axis 1)
                    sucs feature))

(defmethod predict ((model extrinsic-classifier) input &key)
  (with-slots (feature) model
    (call-next-method model
                      (concatenate (list (aref input t `(t ,feature))
                                         (aref input t `(,(1+ feature) t)))
                                   :axis 1))))

