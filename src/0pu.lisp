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

+ Patching cl-random-forest so that it is suitable for stroage
+ Adding PU-learning to cl-random-forest

|#

(in-package :cl-random-forest)

(defmethod make-load-form ((instance forest) &optional env)
  (make-load-form-saving-slots instance :environment env))
(defmethod make-load-form ((instance dtree) &optional env)
  (make-load-form-saving-slots instance :environment env))
(defmethod make-load-form ((instance node) &optional env)
  (make-load-form-saving-slots instance :environment env))

;; variants that already predicts at the leaf node, and the vote based on the results.
;; predict-forest2 and test-forest2 calls this variant.

(defun class-distribution-forest2 (forest datamatrix datum-index)
  (let ((n-class (forest-n-class forest))
        (n-tree (forest-n-tree forest))
        (class-count-array (forest-class-count-array forest)))
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array double-float) datamatrix class-count-array)
             (type fixnum datum-index n-class n-tree))
    ;; init forest-class-count-array
    (loop for i fixnum from 0 below n-class do
      (setf (aref class-count-array i) 0d0))
    ;; whole count
    (dolist (dtree (forest-dtree-list forest))
      (incf (aref class-count-array
                  ;; already argmax-ed
                  (predict-dtree dtree datamatrix datum-index))))
    ;; divide by n-tree
    (loop for i fixnum from 0 below n-class do
      (setf (aref class-count-array i)
            (/ (aref class-count-array i) n-tree)))
    class-count-array))
(export 'class-distribution-forest2)

(defun predict-forest2 (forest datamatrix datum-index)
  (declare (optimize (speed 3) (safety 0))
           (type forest forest)
           (type (simple-array double-float) datamatrix)
           (type fixnum datum-index))
  (let ((dist (class-distribution-forest2 forest datamatrix datum-index)))
    (declare (type (simple-array double-float) dist))
    (argmax dist)))
(export 'predict-forest2)

(defun test-forest2 (forest datamatrix target &key quiet-p)
  (declare (optimize (speed 3) (safety 0))
           (type forest forest)
           (type (simple-array double-float) datamatrix)
           (type (simple-array fixnum) target))
  (let ((n-correct 0)
        (len (length target)))
    (declare (type fixnum n-correct len))
    (loop for i fixnum from 0 below len do
      (when (= (predict-forest2 forest datamatrix i)
               (aref target i))
        (incf n-correct)))
    (calc-accuracy n-correct len :quiet-p quiet-p)))
(export 'test-forest2)

;; pu learning

(defun predict-pu-forest (forest datamatrix datum-index pu-coefficient)
  (declare (optimize (speed 3) (safety 0))
           (type forest forest)
           (type (simple-array double-float) datamatrix)
           (type fixnum datum-index)
           (type double-float pu-coefficient))
  (let ((dist (class-distribution-forest forest datamatrix datum-index)))
    (declare (type (simple-array double-float) dist))
    (if (< 0.5 (/ (aref dist 1) pu-coefficient))
        1
        0)))
(export 'predict-pu-forest)

(defun test-pu-forest (forest datamatrix target pu-coefficient &key quiet-p)
  (declare (optimize (speed 3) (safety 0))
           (type forest forest)
           (type (simple-array double-float) datamatrix)
           (type (simple-array fixnum) target)
           (type double-float pu-coefficient))
  (let ((n-correct 0)
        (len (length target)))
    (declare (type fixnum n-correct len))
    (loop for i fixnum from 0 below len do
      (when (= (predict-pu-forest forest datamatrix i pu-coefficient)
               (aref target i))
        (incf n-correct)))
    (calc-accuracy n-correct len :quiet-p quiet-p)))
(export 'test-pu-forest)

(defun predict-pu-forest2 (forest datamatrix datum-index pu-coefficient)
  (declare (optimize (speed 3) (safety 0))
           (type forest forest)
           (type (simple-array double-float) datamatrix)
           (type fixnum datum-index)
           (type double-float pu-coefficient))
  (let ((dist (class-distribution-forest2 forest datamatrix datum-index)))
    (declare (type (simple-array double-float) dist))
    (if (< 0.5 (/ (aref dist 1) pu-coefficient))
        1
        0)))
(export 'predict-pu-forest2)

(defun test-pu-forest2 (forest datamatrix target pu-coefficient &key quiet-p)
  (declare (optimize (speed 3) (safety 0))
           (type forest forest)
           (type (simple-array double-float) datamatrix)
           (type (simple-array fixnum) target)
           (type double-float pu-coefficient))
  (let ((n-correct 0)
        (len (length target)))
    (declare (type fixnum n-correct len))
    (loop for i fixnum from 0 below len do
      (when (= (predict-pu-forest2 forest datamatrix i pu-coefficient)
               (aref target i))
        (incf n-correct)))
    (calc-accuracy n-correct len :quiet-p quiet-p)))
(export 'test-pu-forest2)

(defun balanced-bootstrap-sample-indices (n n-class target)
  (let* ((len (array-dimension target 0))
         (counters/class (make-array n-class :element-type 'fixnum :initial-element 0))
         (indices/class (coerce (loop :repeat n-class
                                   :collect (make-array len :element-type 'fixnum :initial-element -1))
                                'vector))
         (arr (make-array n :element-type 'fixnum :initial-element 0)))
    ;; collect indices for each class
    (loop for i from 0 below len do
         (let* ((class (aref target i))
                (index/class (aref counters/class class)))
           (setf (aref (aref indices/class class) index/class) i)
           (setf (aref counters/class class) (1+ index/class))))
    ;; collect the balanced bootstrap indices 
    (loop
       for i from 0 below n
       with class = 0
       for counter = (aref counters/class class)
       do
         (when (< 0 counter) 
           (setf (aref arr i)
                 (aref (aref indices/class class)
                       (random counter))))
         (setf class (mod (1+ class) n-class)))
    arr))

#+(or)
(print (balanced-bootstrap-sample-indices 8 2 #(1 1 1 1 1 1 1 1
                                                0 0 0 0 0 0 0 0
                                                0 0 0 0 0 0 0 0
                                                0 0 0 0 0 0 0 0
                                                0 0 0 0 0 0 0 0
                                                0 0 0 0 0 0 0 0
                                                0 0 0 0 0 0 0 0)))

(defun make-balanced-forest (n-class datamatrix target
                             &key (n-tree 100) (bagging-ratio 0.1) (max-depth 5) (min-region-samples 1)
                               (n-trial 10) (gain-test #'entropy)
                               (remove-sample-indices? t) (save-parent-node? nil))
  (let ((forest (%make-forest
                 :n-tree n-tree
                 :bagging-ratio bagging-ratio
                 :datamatrix datamatrix
                 :target target
                 :n-class n-class
                 :class-count-array (make-array n-class :element-type 'double-float)
                 :datum-dim (array-dimension datamatrix 1)
                 :max-depth max-depth
                 :min-region-samples min-region-samples
                 :n-trial n-trial
                 :gain-test gain-test
                 :index-offset (make-array n-tree :element-type 'fixnum :initial-element 0))))
    ;; make dtree list
    (push-ntimes n-tree (forest-dtree-list forest)
      (make-dtree n-class datamatrix target
                  :max-depth max-depth
                  :min-region-samples min-region-samples
                  :n-trial n-trial
                  :gain-test gain-test
                  :remove-sample-indices? remove-sample-indices?
                  :save-parent-node? save-parent-node?
                  :sample-indices (balanced-bootstrap-sample-indices
                                   (floor (* (array-dimension datamatrix 0) bagging-ratio))
                                   n-class
                                   target)))
    ;; set dtree-id
    (loop for dtree in (forest-dtree-list forest)
          for i from 0
          do (setf (dtree-id dtree) i))
    ;; set leaf-id
    (set-leaf-index-forest! forest)
    forest))
(export 'make-balanced-forest)
