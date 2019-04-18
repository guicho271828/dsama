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

(defclass serializable-array (serializable-object) ((array :initarg :array)))

(defun dump-tsv (input-tsv output)
  (save
   (make-instance 'serializable-array
                  :pathname output
                  :array
                  (dataloader:load input-tsv
                                   :mime "text/plain"
                                   :separator #\Space
                                   :data-map-fn #'read-from-string))
   :verbose t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-actions (input)
  (format t "~{~s~^~%~}"
          (first
           (nonzero
            (histogram
             (aref
              (asarray
               (slot-value
                (load-instance input)
                'array)
               :type 'fixnum)
              t -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition zero-examples (error) ((action :initarg :action)))

(defun extract-data-for-action (action transitions)
  "Extract the transitions that belongs to the specified action ACTION.
Return 4 values: pre, suc of the transitions with the specified action ID,
and otherpres, othersucs, which are from transitions whose IDs are different."
  (let ((actions (aref transitions t -1)))
    (iter (for %action in-vector actions with-index i)
          (if (= %action action)
              (collect (aref transitions i) into matched)
              (collect (aref transitions i) into others))
          (finally
           (when (null matched)
             (cerror "Continue without examples for this action" 'zero-examples :action action)
             (return
               (values (zeros `(0 ,@(rest (shape transitions))) :type (dtype transitions))
                       transitions)))
           (return (values (asarray matched)
                           (asarray others)))))))

;; (TRAIN-MODEL "test.tsv" "(:precondition 0 :rf :n-tree 10)" "output.model")

#+nil
(%output-filename "test.tsv" "(:precondition 0 :rf :n-tree 10)")
;; #P"PRECONDITION/0/RF-N-TREE-10.model"
#+nil
(%output-filename "samples/test.tsv" "(:precondition 0 :rf :n-tree 10)")
;; #P"samples/PRECONDITION/0/RF-N-TREE-10.model"

(defun %output-filename (input params)
  (let* ((input (pathname input))
         (level 2))
    (merge-pathnames
     (format nil "~{~a/~}~{~a~^-~}.model"
             (subseq params 0 level)
             (subseq params level))
     (uiop:pathname-directory-pathname input))))

#+(or)
(append-directory "aaa/bbb/ccc/x.lisp" "eee/ddd/")

(defun append-directory (pathname directory)
  (merge-pathnames (make-pathname :directory nil :defaults pathname)
                   (merge-pathnames directory
                                    (uiop:pathname-directory-pathname pathname))))

(defun train-precondition (valid fake valid2 invalid action model &rest params)
  (print ros:*argv*)
  (handler-case
      (let* ((*logs* nil)
             (action (read-from-string action))
             (model  (read-from-string model))
             (params (mapcar #'read-from-string params))
             (outfile (%output-filename valid (list* :precondition action model params)))
             (json    (make-pathname :type "json" :defaults outfile))
             (valid   (extract-data-for-action action (asarray (slot-value (load-instance valid)   'array))))
             (fake    (extract-data-for-action action (asarray (slot-value (load-instance fake)    'array))))
             (valid2  (handler-bind ((zero-examples #'continue))
                        (extract-data-for-action action (asarray (slot-value (load-instance valid2)  'array)))))
             (invalid (handler-bind ((zero-examples #'continue))
                        (extract-data-for-action action (asarray (slot-value (load-instance invalid) 'array)))))
             (N (/ (1- (second (shape valid))) 2)))
        (with-logging-to-json (json)
          (let ((valid   (aref valid   t `(0 ,N)))
                (fake    (aref fake    t `(0 ,N)))
                (valid2  (aref valid2  t `(0 ,N)))
                (invalid (aref invalid t `(0 ,N))))
            (-<> (make-instance model)
              (%train-precondition <> params valid fake valid2 invalid)
              (save <> :pathname (merge-pathnames outfile))))))
    (match-error ()
      (format *error-output* "~&Usage error: insufficient elements in the parameters~%"))
    (zero-examples (c)
      (format *error-output* "~&Data does not contain the transitions for action ~a~%"
              (slot-value c 'action)))))


(defun train-precondition-with-successors (valid fake valid2 invalid action model &rest params)
  "Trains the precondition, allowing it to see the successor states"
  (print ros:*argv*)
  (handler-case
      (let* ((*logs* nil)
             (action (read-from-string action))
             (model  (read-from-string model))
             (params (mapcar #'read-from-string params))
             (outfile (%output-filename valid (list* :precondition-with-successors action model params)))
             (json    (make-pathname :type "json" :defaults outfile))
             (valid   (extract-data-for-action action (asarray (slot-value (load-instance valid)   'array))))
             (fake    (extract-data-for-action action (asarray (slot-value (load-instance fake)    'array))))
             (valid2  (handler-bind ((zero-examples #'continue))
                        (extract-data-for-action action (asarray (slot-value (load-instance valid2)  'array)))))
             (invalid (handler-bind ((zero-examples #'continue))
                        (extract-data-for-action action (asarray (slot-value (load-instance invalid) 'array)))))
             (N (/ (1- (second (shape valid))) 2)))
        (with-logging-to-json (json)
          (-<> (make-instance model)
               (%train-precondition <> params valid fake valid2 invalid)
               (save <> :pathname (merge-pathnames outfile)))))
    (match-error ()
      (format *error-output* "~&Usage error: insufficient elements in the parameters~%"))
    (zero-examples (c)
      (format *error-output* "~&Data does not contain the transitions for action ~a~%"
              (slot-value c 'action)))))

(defun train-effect (valid action model &rest params)
  (print ros:*argv*)
  (handler-case
      (let* ((*logs* nil)
             (action (read-from-string action))
             (model  (read-from-string model))
             (params (mapcar #'read-from-string params))
             (outfile (%output-filename valid (list* :effect action model params)))
             (json    (make-pathname :type "json" :defaults outfile))
             (valid   (asarray (slot-value (load-instance valid)   'array)))
             (N (/ (1- (second (shape valid))) 2)))
        (multiple-value-bind (matched others) (extract-data-for-action action valid)
          (declare (ignorable others))
          (dotimes (feature N)
            (with-logging-to-json ((append-directory json (format nil "~a/" feature)))
              (-> (make-instance model)
                (%train-effect params (aref matched t `(0 ,N)) (aref matched t (+ N feature)) feature)
                (save :pathname (append-directory (merge-pathnames outfile)
                                                  (format nil "~a/" feature))))))))
    (match-error ()
      (format *error-output* "~&Usage error: insufficient elements in the parameters~%"))
    (zero-examples (c)
      (format *error-output* "~&Data does not contain the transitions for action ~a~%"
              (slot-value c 'action)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-instance-and-clean (path)
  (let ((i (load-instance path)))
    (with-slots (forest) i
      (with-slots (cl-random-forest::datamatrix
                   cl-random-forest::target
                   cl-random-forest::dtree-list)
          forest
        (setf cl-random-forest::datamatrix nil
              cl-random-forest::target     nil)
        (dolist (dtree cl-random-forest::dtree-list)
          (with-slots (cl-random-forest::datamatrix)
              dtree
            (setf cl-random-forest::datamatrix nil)))))
    i))

(defun to-actions (models)
  (iter (for *action-current* from 0 below (first (shape models)))
        (with *feature-max* = (second (shape models)))
        (format t "~%processing action ~a" *action-current*)
        (collecting
         (progn
          (let ((eff (to-effects (map 'vector #'load-instance-and-clean (aref models *action-current* '(1 t)))))
                ;; evaluation order is important
                (pre (to-preconditon (load-instance-and-clean (aref models *action-current* 0)))))
            (list :action       (a *action-current*)
                  :parameters   nil
                  :precondition pre 
                  :effect       eff))))))

(defun to-pddl (model-file-list outfile)
  (print ros:*argv*)
  (let* ((*predicates* (make-hash-table))
         (*formula-axiom* (make-hash-table :test 'equal))
         (*axiom-formula* (make-hash-table))
         (models (dataloader:load model-file-list
                                  :mime "text/plain"
                                  :separator #\Space))
         (*effect-condition-cache* (make-hash-table :test 'equal))
         (actions (to-actions models))
         (axioms  (dump-axioms))
         (result (substitute-vars
                  `(define (domain latent)
                       (:requirements :adl)
                     (:predicates
                      ,@(alexandria:hash-table-keys *predicates*))
                     ,@actions
                     ,@axioms))))
    (with-open-file (s outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format s "~&~(~:s~)~%" result))
    result))

(defun to-effects2 (add/a del/a)
  ;; initialize the cache with (z i), i.e.,
  ;; preserve the value if neither add or delete effect is present
  (iter (for p in-vector add/a with-index i)
        (setf (gethash (list *action-current* i)
                       *effect-condition-cache*)
              (z i)))
  `(and ,@(iter (for p in-vector add/a with-index i)
                (when (plusp p)
                  ;; note: we guarantee the add-effect and the delete-effect is
                  ;; mutually exclusive.
                  (setf (gethash (list *action-current* i)
                                 *effect-condition-cache*)
                        ;; overwrite the cache with True.
                        '(and))
                  (collect (z i))))
        ,@(iter (for p in-vector del/a with-index i)
                (when (plusp p)
                  ;; note : see above note
                  (setf (gethash (list *action-current* i)
                                 *effect-condition-cache*)
                        ;; overwrite the cache with False.
                        '(or))
                  (collect `(not ,(z i)))))))

(defun to-actions2 (models addfile delfile)
  (iter (for *action-current* from 0 below (first (shape models)))
        (with add = (dataloader:load addfile
                                     :mime "text/plain"
                                     :separator #\Space
                                     :data-map-fn #'read-from-string))
        (with del = (dataloader:load delfile
                                     :mime "text/plain"
                                     :separator #\Space
                                     :data-map-fn #'read-from-string))
        (with *feature-max* = (second (shape add)))
        (format t "~%processing action ~a" *action-current*)
        (collecting
         (progn
           (let ((eff (to-effects2 (aref add *action-current*) (aref del *action-current*)))
                 ;; evaluation order is important
                 (pre (to-preconditon (load-instance-and-clean (aref models *action-current* 0)))))
            (list :action       (a *action-current*)
                  :parameters   nil
                  :precondition pre 
                  :effect       eff))))))

(defun to-pddl2 (model-file-list addfile delfile outfile)
  (print ros:*argv*)
  (let* ((*predicates* (make-hash-table))
         (*formula-axiom* (make-hash-table :test 'equal))
         (*axiom-formula* (make-hash-table))
         (models (dataloader:load model-file-list
                                  :mime "text/plain"
                                  :separator #\Space))
         (*effect-condition-cache* (make-hash-table :test 'equal))
         (actions (to-actions2 models addfile delfile))
         (axioms  (dump-axioms))
         (result (substitute-vars
                  `(define (domain latent)
                       (:requirements :adl)
                     (:predicates
                      ,@(alexandria:hash-table-keys *predicates*))
                     ,@actions
                     ,@axioms))))
    (with-open-file (s outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format s "~&~(~:s~)~%" result))
    result))

(defun to-actions2-nopre (addfile delfile)
  (iter (with add = (dataloader:load addfile
                                     :mime "text/plain"
                                     :separator #\Space
                                     :data-map-fn #'read-from-string))
        (with del = (dataloader:load delfile
                                     :mime "text/plain"
                                     :separator #\Space
                                     :data-map-fn #'read-from-string))
        (with *feature-max* = (second (shape add)))
        (for *action-current* from 0 below (first (shape add)))
        (format t "~%processing action ~a" *action-current*)
        (collecting
         (progn
           (let ((eff (to-effects2 (aref add *action-current*) (aref del *action-current*))))
            (list :action       (a *action-current*)
                  :parameters   nil
                  :precondition '(and)
                  :effect       eff))))))

(defun to-pddl2-nopre (addfile delfile outfile)
  (print ros:*argv*)
  (let* ((*predicates* (make-hash-table))
         (*formula-axiom* (make-hash-table :test 'equal))
         (*axiom-formula* (make-hash-table))
         (*effect-condition-cache* (make-hash-table :test 'equal))
         (actions (to-actions2-nopre addfile delfile))
         (axioms  (dump-axioms))
         (result (substitute-vars
                  `(define (domain latent)
                       (:requirements :adl)
                     (:predicates
                      ,@(alexandria:hash-table-keys *predicates*))
                     ,@actions
                     ,@axioms))))
    (with-open-file (s outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format s "~&~(~:s~)~%" result))
    result))


