#!/usr/bin/sbcl --script
(declaim (optimize (debug 3)))
(require 'asdf)

(asdf:load-system :lisp-unit)
(asdf:load-system :point-polygon)

(in-package :point-polygon)

(let* ((sym-to-export '("quadrant"))
       (pack (find-package :point-polygon))
       (is-point-polygon-sym (lambda (x) (and (eql (symbol-package x) pack)
                                        (member (symbol-name x) sym-to-export :test #'string-equal)))))
  (do-all-symbols (sym pack) (when (funcall is-point-polygon-sym sym) (export sym))))

(defpackage point-polygon-test
  (:use :common-lisp :lisp-unit :point-polygon))

(in-package :point-polygon-test)

(define-test quadrant-test
  (let ((x '(34.134134D0 94.341819D0)))
    (assert-equal :q1 (quadrant x '(35.2482D0 94.341819D0)))
    (assert-equal :q1 (quadrant x '(35.2482D0 102.43948D0)))
    (assert-equal :q2 (quadrant x '(34.134134D0 102.43948D0)))
    (assert-equal :q2 (quadrant x '(-24.34938D0 102.43948D0)))
    (assert-equal :q3 (quadrant x '(-24.34938D0 94.341819D0)))
    (assert-equal :q3 (quadrant x '(-24.34938D0 12.349839D0)))
    (assert-equal :q4 (quadrant x '(34.134134D0 12.349839D0)))
    (assert-equal :q4 (quadrant x '(35.2482D0 12.349839D0)))
    (assert-true (null (quadrant x '(34.134134D0 94.341819D0))))))

(setq *print-failures* t)
(setq *print-errors* t)
(run-tests :all)
