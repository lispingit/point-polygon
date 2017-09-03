#!/usr/bin/sbcl --script
(declaim (optimize (debug 3)))
(require 'asdf)

(asdf:load-system :lisp-unit)
(asdf:load-system :point-polygon)

(in-package :point-polygon)

(let* ((sym-to-export '("quadrant" "pair-eq" "abscissa-projection"))
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

(define-test pair-equal-test
  (assert-true (pair-eq '(35.2482D0 94.341819D0) '(35.2482D0 94.341819D0)))
  (assert-false (pair-eq '(35.2482D0 94.031819D0) '(35.2482D0 94.341819D0)))
  (assert-false (pair-eq '(35.2482D0 94.341819D0) '(32.2482D0 94.341819D0))))
  
(define-test abscissa-projection-test
  (assert-true
    (pair-eq '(1.0D0 -1.0D0)
             (abscissa-projection '(-1.0D0 -1.0D0) '(1.0D0 1.0D0) '(1.0D0 -2.0D0))))
  (assert-true
    (pair-eq '(1.0D0 1.0D0)
             (abscissa-projection '(0.0D0 1.0D0) '(1.0D0 1.0D0) '(2.0D0 0.0D0))))
  (assert-true
    (pair-eq '(0.0D0 0.0D0)
             (abscissa-projection '(0.0D0 0.0D0) '(1.0D0 -1.0D0) '(-2.0D0 2.0D0))))
  (assert-true
    (pair-eq '(0.0D0 1.0D0)
             (abscissa-projection '(0.0D0 1.0D0) '(0.0D0 1.0D0) '(-2.0D0 42.0D0))))
  (let ((proj (abscissa-projection '(-1.34D0 -32.99D0) '(3.0D0 -52.01D0) '(22.214D0 0.42D0))))
    (assert-true (= (cadr proj) -32.99D0))
    (assert-true (> (car proj) 9.97025D0))
    (assert-true (< (car proj) 9.97026D0))))
  
(setq *print-failures* t)
(setq *print-errors* t)
(run-tests :all)
