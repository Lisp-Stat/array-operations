;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ARRAY-OPERATIONS/MATRICES -*-
;;; Copyright (c) 2012-2018 by Tamas Papp. All rights reserved.
;;; Copyright (c) 2018-2022 by Ben Dudson. All rights reserved.
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(defpackage :array-operations/matrices
  (:use :cl :array-operations/generic)
  (:import-from :alexandria
                :length=)
  (:export :array-matrix
           :matrixp
           :square-matrix-p
           ;; the next two are deprecated aliases for the previous two
           :matrix?
           :square-matrix?)
  (:documentation "Functions for representing matrices as 2D arrays.  A matrix is a two-dimensional array often used for linear algebra.  See also the matrix functions in NUM-UTILS,  which should be migrated to AOPS."))

(in-package :array-operations/matrices)

(deftype array-matrix ()
  "A rank-2 array."
  '(array * (* *)))

(declaim (inline matrixp square-matrix-p))

(defun matrixp (matrix)
  "Return NIL if MATRIX does not have rank 2."
  (length= (dims matrix) 2))

(defun square-matrix-p (matrix)
  "Test if MATRIX has two dimensions and that they are equal."
  (let ((dims (dims matrix)))
    (and (length= dims 2)
         (= (first dims) (second dims)))))

;; Aliases for deprecated names 'matrix?' and 'square-matrix?'
(setf (fdefinition 'matrix?) #'matrixp)
(setf (fdefinition 'square-matrix?) #'square-matrix-p)
