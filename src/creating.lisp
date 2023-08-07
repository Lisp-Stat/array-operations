;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ARRAY-OPERATIONS/CREATING -*-
;;; Copyright (c) 2012-2018 by Tamas Papp. All rights reserved.
;;; Copyright (c) 2018-2022 by Ben Dudson. All rights reserved.
;;; Copyright (c) 2021-2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defpackage :array-operations/creating
  (:use :cl :array-operations/generic
            :array-operations/utilities)
  (:export :fill!
           :zeros    :zeros*    :zeros!
           :ones     :ones*     :ones!
           :rand     :rand*     :rand!
           :randn    :randn*    :randn!
           :linspace :linspace* :linspace!
           :similar-array
           :make-array-like
           :generate* :generate)
  (:documentation "Functions for creating arrays or data frames filled with various values."))

(in-package :array-operations/creating)

(defun fill! (array value)
  "Fills a given ARRAY with VALUE, coerced to the same element type as ARRAY"
  (let ((size (array-total-size array))
        (value-of-type (coerce value
                               (array-element-type array))))
    (dotimes (i size)
      (setf (row-major-aref array i) value-of-type))
    array))

(defun zeros! (array)
  "Fills the given ARRAY with zero values, coerced to the element type.
Returns ARRAY."
  (fill! array 0))

(defun zeros* (element-type dimensions)
  "Makes an array of shape DIMENSIONS and type ELEMENT-TYPE, filled with zeros  coerced to the specified type ELEMENT-TYPE."
  (make-array (ensure-dimensions dimensions)
              :element-type element-type
              :initial-element (coerce 0 element-type)))

(defun zeros (dimensions &optional element-type)
  "Make an array of shape DIMENSIONS filled with zeros.  Elements are of type T, unless ELEMENT-TYPE is given."
  (assert (or (eq element-type t)
	      (subtypep element-type 'number))
	  (element-type)
	  "Cannot create a ZEROS array because ~A is not a numeric type." element-type)
  (if element-type
      (zeros* element-type dimensions)
      (make-array (ensure-dimensions dimensions) :initial-element 0)))

(defun ones! (array)
  "Fills the given ARRAY with 1's, coerced to the element type.  Returns ARRAY."
  (fill! array 1))

(defun ones* (element-type dimensions)
  "Makes an array of shape DIMENSIONS and type ELEMENT-TYPE, filled with ones
   coerced to the specified type ELEMENT-TYPE."
  (make-array (ensure-dimensions dimensions)
              :element-type element-type
              :initial-element (coerce 1 element-type)))

(defun ones (dimensions &optional element-type)
  "Makes an array of shape DIMENSIONS filled with ones. Elements are of type T, unless ELEMENT-TYPE is given."
  (assert (or (eq element-type t)
	      (subtypep element-type 'number))
	  (element-type)
	  "Cannot create a ONES array because ~A is not a numeric type." element-type)
  (if element-type
      (ones* element-type dimensions)
      (make-array (ensure-dimensions dimensions) :initial-element 1)))

(defun rand! (array)
  "Fills a given ARRAY with random numbers, uniformly distributed between 0 and 1.  Uses the built-in RANDOM function.
Returns ARRAY."
  (let ((size (array-total-size array))
        (element-type (array-element-type array)))
    (dotimes (i size)
      (setf (row-major-aref array i) (coerce (random 1.0) element-type))))
  array)

(defun rand* (element-type dimensions)
  "Makes an array of shape DIMENSIONS and type ELEMENT-TYPE, filled with random numbers uniformly distributed between 0 and 1.

   Uses the built-in RANDOM function.

   (rand 3)  -> #(0.39319038 0.69693553 0.5021677)
   (rand '(2 2)) -> #2A((0.91003513 0.23208928) (0.5577954 0.94657767))

   NOTE: If it's important that these numbers are really random (e.g. cryptographic applications), then you should probably not use this function. "
  (rand! (make-array (ensure-dimensions dimensions)
                     :element-type element-type)))

(defun rand (dimensions &optional element-type)
  "Makes an array of shape DIMENSIONS filled with random numbers uniformly distributed between 0 and 1. Elements are of type T, unless ELEMENT-TYPE is given.

   Uses the built-in RANDOM function.

   (rand 3)  -> #(0.39319038 0.69693553 0.5021677)
   (rand '(2 2)) -> #2A((0.91003513 0.23208928) (0.5577954 0.94657767))

   NOTE: If it's important that these numbers are really random (e.g. cryptographic applications), then you should probably not use this function."
  (assert (subtypep element-type 'number)
	  (element-type)
	  "Cannot create a RAND array because ~A is not a numeric type." element-type)
  (if element-type
      (rand* element-type dimensions)
      (rand* t dimensions)))

(defun randn! (array)
  "Fills ARRAY with normally distributed numbers with a mean of zero and standard deviation of 1

   Uses the Box-Muller algorithm and built-in random number generator.

   NOTE: If it's important that these numbers are really random (e.g. cryptographic applications), then you should probably not use this function."
  (let ((element-type (array-element-type array))
        (size (array-total-size array)))
    (do ((i 0 (+ 2 i)))
        ((>= i (- size 1)))
      ;; Box-Muller algorithm
      ;; Generate two uniform random numbers, u1 and u2
      ;;
      ;;  r = sqrt(-2 log(u1))
      ;; then two normally-distributed numbers are
      ;;  z0 = r * cos(2pi u2)
      ;;  z1 = r * sin(2pi u2)
      (let* ((u1 (+ (random 1.0d0) least-positive-double-float))
             (2piu2 (* 2 pi (random 1.0d0))) ; 2 * pi * u2
             (r (sqrt (* -2 (log u1)))))
        (setf (row-major-aref array i) (coerce (* r (cos 2piu2)) element-type))
        (setf (row-major-aref array (1+ i)) (coerce (* r (sin 2piu2)) element-type))))
    ;; If size is odd then one extra random number is needed
    (if (not (zerop (logand size 1)))
        (let* ((u1 (+ (random 1.0d0) least-positive-double-float))
               (2piu2 (* 2 pi (random 1.0d0)))
               (r (sqrt (* -2 (log u1)))))
          (setf (row-major-aref array (1- size)) (coerce (* r (cos 2piu2)) element-type))))
    array))

(defun randn* (element-type dimensions)
  "Creates an array of shape DIMENSIONS and type ELEMENT-TYPE, and fills with normally distributed numbers with a mean of zero and standard deviation of 1

   Uses the Box-Muller algorithm and built-in random number generator.

   (rand 3)   -> #(-0.82067037 -0.60068226 -0.21494178)
   (randn '(2 2)) -> #2A((1.6905352 -2.5379088) (0.8461403 -1.505984))

   NOTE: If it's important that these numbers are really random
   (e.g. cryptographic applications), then you should probably
   not use this function.
   "
  (randn! (make-array (ensure-dimensions dimensions)
                      :element-type element-type)))

(defun randn (dimensions &optional element-type)
  "Creates an array of shape DIMENSIONS and type T, and fills with normally distributed numbers with a mean of zero and standard deviation of 1

   Uses the Box-Muller algorithm and built-in random number generator.

   (rand 3)   -> #(-0.82067037 -0.60068226 -0.21494178)
   (randn '(2 2)) -> #2A((1.6905352 -2.5379088) (0.8461403 -1.505984))

   NOTE: If it's important that these numbers are really random (e.g. cryptographic applications), then you should probably not use this function."
  (assert (subtypep element-type 'number)
	  (element-type)
	  "Cannot create a normal random array because ~A is not a numeric type." element-type)
  (if element-type
      (randn* element-type dimensions)
      (randn* t dimensions)))


(defun linspace! (array start stop)
  "Fill an array with evenly spaced numbers over an interval. The first element is equal to START and last element STOP, with constant difference between consecutive elements in ROW-MAJOR-INDEX."
  (assert (> stop start) (start stop) "Stop must be greater than start.")
  (let* ((size (array-total-size array))
         (element-type (array-element-type array))
         (delta (/ (- stop start) (- size 1)))) ; Difference between values

    (dotimes (i size)
      (setf (row-major-aref array i) (coerce (+ start (* delta i)) element-type)))
    array))

(defun linspace* (element-type start stop n)
  "Make a vector of N elements and type ELEMENT-TYPE, containing evenly spaced numbers over an interval. The first element is equal to START and last element STOP, with constant difference between consecutive elements."
  (linspace! (make-array n :element-type element-type) start stop))

(defun linspace (start stop n &optional element-type)
  "Make a vector of N elements containing evenly spaced numbers over an interval.  The first element is equal to START and last element STOP, with constant difference between consecutive elements.  Elements are of type T, unless ELEMENT-TYPE is given

  (linspace 0 4 5) -> #(0 1 2 3 4)
  (linspace 1 3 5) -> #(0 1/2 1 3/2 2)
  (linspace 0 4d0 3) -> #(0.0d0 2.0d0 4.0d0)"
  (assert (subtypep element-type 'number)
	  (element-type)
	  "Cannot create a linear sequence of numbers because ~A is not a numeric type." element-type)
  (if element-type
      (linspace* element-type start stop n)
      (linspace* t start stop n)))


(defun similar-array (array &key (dimensions (dims array))
                                 (adjustable (adjustable-array-p array))
                                 (element-type (array-element-type array))
                                 (initial-element nil initial-element-p)
                                 (fill-pointer (and (array-has-fill-pointer-p array)
                                                    (fill-pointer array))))
  "Returns new array with the same properties as ARRAY.
   Keyword arguments will override properties of ARRAY.
   If INITIAL-ELEMENT is specified, it is coerced to ELEMENT-TYPE."
  (if initial-element-p
      (make-array dimensions
              :adjustable adjustable
              :element-type element-type
              :initial-element (coerce initial-element element-type)
              :fill-pointer fill-pointer)
      (make-array dimensions
              :adjustable adjustable
              :element-type element-type
              :fill-pointer fill-pointer)))

;; Alias for backward compatibility
(setf (fdefinition 'make-array-like) #'similar-array)

;;; generating

(defun generate* (element-type function dimensions &optional arguments)
  "Return an array with given DIMENSIONS and ELEMENT-TYPE, with elements generated by calling FUNCTION.

Function is called with:

 - no arguments, when ARGUMENTS is nil
 - the position (= row major index), when ARGUMENTS is :POSITION
 - a list of subscripts, when ARGUMENTS is :SUBSCRIPTS
 - both when ARGUMENTS is :POSITION-AND-SUBSCRIPTS

The traversal order is unspecified and may be nonlinear."
  (let* ((dimensions (ensure-dimensions dimensions))
         (result (make-array dimensions :element-type element-type)))
    (ecase arguments
      ((nil)
       (dotimes (position (array-total-size result))
         (setf (row-major-aref result position)
               (funcall function))))
      (:position
       (walk-subscripts (dimensions subscripts position)
         (setf (row-major-aref result position) (funcall function position))))
      (:subscripts
       (walk-subscripts-list (dimensions subscripts position)
         (setf (row-major-aref result position)
               (funcall function subscripts))))
      (:position-and-subscripts
       (walk-subscripts-list (dimensions subscripts position)
         (setf (row-major-aref result position)
               (funcall function position subscripts)))))
    result))

(defun generate (function dimensions &optional arguments)
  "Like GENERATE*, with ELEMENT-TYPE T."
  (generate* t function dimensions arguments))
