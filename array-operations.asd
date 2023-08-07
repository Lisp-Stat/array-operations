;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ASDF -*-
;;; Copyright (c) 2012-2018 by Tamas Papp. All rights reserved.
;;; Copyright (c) 2019-2022 by Ben Dudson. All rights reserved.
;;; Copyright (c) 2021-2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defsystem "array-operations"
  :version "1.2.1"
  :description "Array operations library for Common Lisp"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Steve Nunez <steve@symbolics.tech>"
  :long-name "Array operations for array-like data structures"
  :homepage "https://lisp-stat.dev/docs/manuals/array-operations"
  :bug-tracker "https://github.com/Lisp-Stat/array-operations/issues"
  :license :MS-PL
  :class :package-inferred-system
  :pathname "src/"
  :depends-on ("let-plus"
               "array-operations/all")
  :in-order-to ((test-op (test-op :array-operations/tests))))

(defsystem #:array-operations/tests
  :description "Unit tests for the ARRAY-OPERATIONS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Steve Nunez"
  :license :MS-PL
  :depends-on ("array-operations"
               "alexandria"
               "clunit2")
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (o c)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (uiop:symbol-call :array-operations/tests :run))))
