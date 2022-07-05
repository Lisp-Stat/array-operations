;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ARRAY-OPERATIONS/REDUCING -*-
;;; Copyright (c) 2012-2018 by Tamas Papp. All rights reserved.
;;; Copyright (c) 2018-2022 by Ben Dudson. All rights reserved.
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(defpackage :array-operations/reducing
  (:use :cl)
  (:export :most
           :best
           :argmax
           :argmin
           :vectorize-reduce)
  (:documentation "Functions for reducing arrays, or performing reducing like operations over the elements of an array."))

(in-package :array-operations/reducing)

(defun most (fn array)
  "Finds the element of ARRAY that returns the value closest to positive infinity when FN is applied to the array value.
   Returns the row-major-aref index, and the winning value.

   Example: The maximum of an array is
     (most #'identity #(1 2 3))
     -> 2    (row-major index)
        3    (value)

   Minimum of an array is
      (most #'- #(1 2 3))
        0
        -1

   This function was adapted from Paul Graham's On Lisp
  "
  (let* ((wins 0)
         (max (funcall fn (row-major-aref array wins))))
    (dotimes (ind (array-total-size array))
      (let ((score (funcall fn (row-major-aref array ind))))
        (when (> score max)
          (setq wins ind
                max score))))
    (values wins max)))

(defun best (fn array)
  "FN must accept two inputs and return true/false. This function is applied
   to elements of ARRAY, to find the 'best'. The row-major-aref index is returned.

   Example: The index of the maximum is

   * (best #'> #(1 2 3 4))
    3   ; row-major index
    4   ; value

   This function was adapted from Paul Graham's On Lisp
  "
  (let ((wins 0)
        (score (row-major-aref array 0)))
    (loop for ind from 1 below (array-total-size array) do
         (if (funcall fn (row-major-aref array ind) score)
             (setq wins ind score (row-major-aref array ind))))
    (values wins score)))


(defun argmax (array)
  "Find the row-major-aref in ARRAY with the maximum value
   Returns both the index and the value of ARRAY at that index"
  (best #'> array))

(defun argmin (array)
  "Find the row-major-aref in ARRAY with the minimum value
   Returns both the index and the value of ARRAY at that index"
  (best #'< array))

;;; vectorize-reduce

(defmacro vectorize-reduce (fn variables &body body)
  "Performs a reduction using FN over all elements in a vectorized expression
   on array VARIABLES.

   VARIABLES must be a list of symbols bound to arrays.
   Each array must have the same dimensions. These are
   checked at compile and run-time respectively.

   Example: Maximum value in an array A

   (vectorize-reduce #'max (a) a)

   Example: Maximum absolute difference between two arrays A and B

   (vectorize-reduce #'max (a b) (abs (- a b)))
  "
  ;; Check that variables is a list of only symbols
  (dolist (var variables)
    (if (not (symbolp var))
        (error "~S is not a symbol" var)))

  (let ((size (gensym)) ; Total array size (same for all variables)
        (result (gensym)) ; Returned value
        (indx (gensym)))  ; Index inside loop from 0 to size

    ;; Get the size of the first variable
    `(let ((,size (array-total-size ,(first variables))))
       ;; Check that all variables have the same size
       ,@(mapcar (lambda (var) `(if (not (equal (array-dimensions ,(first variables))
                                                (array-dimensions ,var)))
                                    (error "~S and ~S have different dimensions" ',(first variables) ',var)))
              (rest variables))

       ;; Apply FN with the first two elements (or fewer if size < 2)
       (let ((,result (apply ,fn (loop for ,indx below (min ,size 2) collecting
                                      (let ,(map 'list (lambda (var) (list var `(row-major-aref ,var ,indx))) variables)
                                        (progn ,@body))))))

         ;; Loop over the remaining indices
         (loop for ,indx from 2 below ,size do
            ;; Locally redefine variables to be scalars at a given index
              (let ,(mapcar (lambda (var) (list var `(row-major-aref ,var ,indx))) variables)
                ;; User-supplied function body now evaluated for each index in turn
                (setf ,result (funcall ,fn ,result (progn ,@body)))))
         ,result))))
