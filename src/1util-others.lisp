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

;; utilities

(in-package :dsama)


(defmacro in-current-package (&body body)
  "Bind *package* to the load-time-value of *package*.
Used frequently for controlling the default package for the reader."
  `(let ((*package* (load-time-value *package*)))
     ,@body))
