;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ARRAY-OPERATIONS/GENERIC -*-
;;; Copyright (c) 2012-2018 by Tamas Papp. All rights reserved.
;;; Copyright (c) 2018-2022 by Ben Dudson. All rights reserved.
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package :array-operations/all
  (:nicknames :array-operations :aops)
  (:use-reexport :array-operations/generic
                 :array-operations/reducing
                 :array-operations/matrices
                 :array-operations/creating
                 :array-operations/indexing
                 :array-operations/displacing
                 :array-operations/transforming
                 :array-operations/stacking)
  (:documentation "This top level package re-exports the individual packages: generic, reducing, matrices, creating, indexing, displacing, transforming and stacking.  It does not export utilities.  The reason for this structure is the use of the ASDF package-inferred-system, where each file is its own package.  None of Papp's other libraries use this, and it seems to have been added after he abandoned the library."))

