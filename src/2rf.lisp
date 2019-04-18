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

(in-package :dsama)

(defclass random-forest (trainable-object)
  ((forest :initarg :forest)
   (params :initarg :params))
  (:metaclass funcallable-standard-class))

(defclass random-forest-classifier (random-forest classifier) ())
(defclass binary-random-forest-classifier (random-forest-classifier binary-classifier) ())
(defclass pu-random-forest-classifier     (binary-random-forest-classifier) (pu))
(defclass diff-random-forest-classifier   (diff-classifier binary-random-forest-classifier) ())
(defclass extrinsic-random-forest-classifier   (extrinsic-classifier binary-random-forest-classifier) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod predict  ((rf random-forest-classifier) input &key)
  (with-slots (forest) rf
    (let ((output (zeros (length input) :type 'fixnum))
          (input2 (-<> input
                    (asarray <> :type 'double-float) 
                    to-simple-array)))
      (dotimes (i (length input))
        (setf (aref output i)
              (predict-forest forest input2 i)))
      output)))

(defmethod evaluate ((rf random-forest-classifier) input output &key)
  (with-slots (forest) rf
    (test-forest forest
                 (-<> input
                   (asarray <> :type 'double-float) 
                   to-simple-array)
                 (-<> output
                   (asarray <> :type 'fixnum)
                   to-simple-array)
                 :quiet-p t)))

(defmethod train    ((rf random-forest-classifier) input output &key params &allow-other-keys)
  (with-slots (forest classes) rf
    (setf forest
          (apply #'make-balanced-forest classes
                 (-<> input
                   (asarray <> :type 'double-float) 
                   to-simple-array)
                 (-<> output
                   (asarray <> :type 'fixnum)
                   to-simple-array)
                 :allow-other-keys t
                 :n-trial (floor (sqrt (second (shape input))))
                 :gain-test 'clrf::entropy    ;not #'entropy
                 params)))
  rf)

(defmethod train :after ((m pu-random-forest-classifier) input output &key params val-input val-output &allow-other-keys)
  (with-slots (pu forest) m
    (let* ((sum 0.0)
           (count 0)
           (val-input (asarray val-input :type 'double-float))
           (val-input-simple (to-simple-array val-input)))
      ;; average the scores from the positive validation dataset
      (iter
        (for i below (length val-output))
        ;; Take the average of g over P, where P is the labelled (thus positive)
        ;; data in validation set V (Elkan, Noto KDD08, p.2, bottom right)
        (for o = (aref val-output i))
        (when (= o 1)
          (incf sum (cl:aref (clrf::class-distribution-forest
                              forest val-input-simple i)
                             1))
          (incf count 1))
        (finally
         (setf pu (handler-case (max double-float-epsilon (coerce (/ sum count) 'double-float))
                    (floating-point-invalid-operation () 1.0d0))))))))

(defmethod predict  ((rf pu-random-forest-classifier) input &key)
  (with-slots (pu forest) rf
    (let ((output (zeros (length input) :type 'fixnum))
          (input2 (-<> input
                    (asarray <> :type 'double-float) 
                    to-simple-array)))
      (dotimes (i (length input))
        (setf (aref output i)
              (predict-pu-forest forest input2 i pu)))
      output)))

(defmethod evaluate ((rf pu-random-forest-classifier) input output &key)
  (with-slots (pu forest) rf
    (test-pu-forest forest
                    (-<> input
                      (asarray <> :type 'double-float) 
                      to-simple-array)
                    (-<> output
                      (asarray <> :type 'fixnum)
                      to-simple-array)
                    pu
                    :quiet-p t)))

(defvar *effect-condition-cache*)

(defmethod to-preconditon ((m random-forest))
  (with-slots (forest pu) m
    (let ((*pu-coefficient* pu)
          (*dim-larger-than-feature-hook*
           (lambda (dim)
             ;; lookahead successor state.
             ;; When the state size is N, the input to RF is 2N.
             ;; Lookahead proposition in position i therefore corresponds to
             ;; the positive effect of (i-N)-th proposition.
             (gethash (list *action-current* (- dim *feature-max*))
                      *effect-condition-cache*))))
      (expand-forest forest))))

(defmethod to-effect ((m random-forest) i)
  (with-slots (forest) m
    (let ((*pu-coefficient* 1.0))
      (let ((ec (expand-forest forest)))
        (setf (gethash (list *action-current* i)
                       *effect-condition-cache*)
              ec)
        (match ec
          ((list 'or)            ;constantly false -- always delete
           `(not ,(z i)))
          ((list 'and)           ;constantly true -- always add
           (z i))
          ((eq (z i))            ; e.g. (when z0 z0) (when (not z0) (not z0)) --- no change
           nil)
          (_
           `(and (when ,ec       ,(z i))
                 (when (not ,ec) (not ,(z i))))))))))

(defmethod to-effect ((m diff-random-forest-classifier) i)
  (with-slots (forest) m
    (let ((*pu-coefficient* 1.0))
      (match (cl-sat:simplify-nnf (expand-forest forest))
        ((list 'or)            ;constantly false -- no change
         nil)
        (flip-condition
         `(and (when ,(cl-sat:simplify-nnf `(and ,(z i) ,flip-condition))
                (not ,(z i)))
              (when ,(cl-sat:simplify-nnf `(and (not ,(z i)) ,flip-condition))
                ,(z i))))))))

