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

;; binary classifier that constantly returns true

(defclass true (trainable-object) ())

(defmethod predict  ((o true) input &key)
  (declare (ignorable))
  (ones (length input)))

(defmethod evaluate ((o true) input output &key)
  (declare (ignorable input output))
  0)

(defmethod train    ((o true) input output &key &allow-other-keys)
  (declare (ignorable input output))
  o)

(defmethod to-preconditon ((o true)) '(and))

;; no effect compilation

