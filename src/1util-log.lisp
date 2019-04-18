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

(defvar *logs*)

(defun alist (&rest args &key &allow-other-keys)
  (iter (for (e1 e2 . rest) on args by #'cddr)
        (collect (cons e1 e2))))

(defun average-accuracy (what)
  (declare ((member :effects :preconditions) what))
  (iter (for log in (getf *logs* what))
        (match log
          ((alist (:samples . samples) (:train . train) (:test . test))
           (summing samples into sampless)
           ;; the number of examples in train is 0.9*samples, but
           ;; this is ok because it is going to be normalized later
           (summing (* samples train) into trains)
           (summing (* samples test)  into tests)))
        (finally
         (return
           (alist :train (/ trains sampless)
                  :test  (/ tests  sampless))))))

(defun call-with-logging-to-json (json fn)
  (sb-ext:call-with-timing
   (lambda (&rest args)
     #+(or)
     (setf (getf *logs* :accuracy)
           (alist :effects       (average-accuracy :effects)
                  :preconditions (average-accuracy :preconditions)))
     (appendf *logs* args)
     (use-explicit-encoder)
     (ensure-directories-exist json)
     (with-open-file (s json :direction :output :if-does-not-exist :create :if-exists :supersede)
       (cl-json:encode-json (cons :object *logs*) s)))
   fn))

(defmacro with-logging-to-json ((json) &body body)
  `(call-with-logging-to-json
    ,json
    (lambda ()
      ,@body)))
