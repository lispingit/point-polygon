#!/usr/bin/sbcl --script
(declaim (optimize (debug 3)))
(require 'asdf)

(asdf:load-system :lisp-unit)
(asdf:load-system :point-polygon)
(asdf:load-system :jsown)

(in-package :point-polygon)

(let* ((sym-to-export '("quadrant" "pair-eq" "abscissa-projection" "calculate-inc" "is-inside-or-border"))
       (pack (find-package :point-polygon))
       (is-point-polygon-sym (lambda (x) (and (eql (symbol-package x) pack)
                                        (member (symbol-name x) sym-to-export :test #'string-equal)))))
  (do-all-symbols (sym pack) (when (funcall is-point-polygon-sym sym) (export sym))))

(defpackage point-polygon-test
  (:use :common-lisp :lisp-unit :point-polygon))

(in-package :point-polygon-test)

(defparameter *ireland-polygon-file-path*
  (make-pathname :directory (pathname-directory *load-pathname*)
                 :name "republic-of-ireland-seawater-boundaries-2017-09-03.geogson"
                 :type nil))

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
    
(define-test calculate-inc-test
  (assert-equal 0 (calculate-inc '(1.23D0 0.34D0) '(34.2D0 0.44D0) '(2.2D0 4.11D0)))
  (assert-equal 0 (calculate-inc '(1.23D0 0.34D0) '(34.2D0 0.44D0) '(2.2D0 0.34D0)))
  (assert-equal 1 (calculate-inc '(1.23D0 0.34D0) '(34.2D0 0.44D0) '(1.23D0 3.99D0)))
  (assert-equal 1 (calculate-inc '(1.23D0 0.34D0) '(4.2D0 2.44D0) '(-3.3D0 0.99D0)))
  (assert-equal 1 (calculate-inc '(-1.23D0 -0.34D0) '(-4.2D0 -2.44D0) '(3.3D0 -0.99D0)))
  (assert-equal 1 (calculate-inc '(-1.23D0 -0.34D0) '(3.3D0 -0.99D0) '(3.3D0 -0.1D0)))
  (assert-equal -1 (calculate-inc '(-1.23D0 -0.34D0) '(3.3D0 -0.1D0) '(3.3D0 -0.99D0)))
  (assert-equal -1 (calculate-inc '(1.23D0 0.34D0) '(-3.3D0 -0.99D0) '(1.23D0 2.44D0)))
  (assert-equal -1 (calculate-inc '(1.23D0 0.34D0) '(1.23D0 20.9D0) '(3.23D0 0.34D0)))
  (assert-equal -1 (calculate-inc '(1.23D0 0.34D0) '(1.43D0 20.9D0) '(1.23D0 -34.84D0)))
  (assert-equal 2 (calculate-inc '(0.0D0 0.0D0) '(1.0D0 1.1D0) '(-1.0D0 -1.0D0)))
  (assert-equal -2 (calculate-inc '(0.0D0 0.0D0) '(-1.0D0 -1.0D0) '(1.0D0 1.1D0)))
  (assert-equal -2 (calculate-inc '(0.0D0 0.0D0) '(1.0D0 0.9D0) '(-1.0D0 -1.0D0)))
  (assert-equal 2 (calculate-inc '(0.0D0 0.0D0) '(-1.0D0 -1.0D0) '(1.0D0 0.9D0)))
  (assert-equal -2 (calculate-inc '(-4.0D0 4.0D0) '(-6.0D0 6.1D0) '(-2.0D0 2.0D0)))
  (assert-equal 2 (calculate-inc '(-4.0D0 4.0D0) '(-2.0D0 2.0D0) '(-6.0D0 6.1D0)))
  (assert-equal 2 (calculate-inc '(-4.0D0 4.0D0) '(-6.0D0 6.0D0) '(-2.1D0 2.0D0)))
  (assert-equal -2 (calculate-inc '(-4.0D0 4.0D0) '(-2.1D0 2.0D0) '(-6.0D0 6.0D0)))
  (assert-equal 2 (calculate-inc '(-4.0D0 4.0D0) '(-6.0D0 6.0D0) '(-2.0D0 1.99D0)))
  (assert-equal -2 (calculate-inc '(-4.0D0 4.0D0) '(-2.0D0 1.99D0) '(-6.0D0 6.0D0)))
  (assert-equal 2 (calculate-inc '(-4.0D0 4.0D0) '(-1.99D0 2.0D0) '(-6.0D0 6.0D0)))
  (assert-equal -2 (calculate-inc '(-4.0D0 4.0D0) '(-6.0D0 6.0D0) '(-1.99D0 2.0D0)))
  (assert-true (null (calculate-inc '(-4.0D0 4.0D0) '(-2.0D0 6.0D0) '(-6.0D0 2.0D0))))
  (assert-true (null (calculate-inc '(0.0D0 0.0D0) '(-1.0D0 -1.0D0) '(0.5D0 0.5D0))))
  (assert-true (null (calculate-inc '(0.0D0 0.0D0) '(0.0D0 0.0D0) '(0.5D0 0.5D0))))
  (assert-true (null (calculate-inc '(1.3D0 4.0D0) '(-1.0D0 -1.0D0) '(1.3D0 4.0D0))))
  (assert-true (null (calculate-inc '(1.3D0 4.0D0) '(-1.0D0 4.0D0) '(2.3D0 4.0D0))))
  (assert-true (null (calculate-inc '(1.3D0 4.0D0) '(1.3D0 4.2D0) '(1.3D0 2.0D0)))))

(define-test is-inside-trapezoid-test
  (let ((trapezoid '((23.24D0 51.87D0) (27.29D0 53.99D0) (29.48D0 52.14D0) (25.07D0 50.99D0) (23.24D0 51.87D0))))
     (assert-true (is-inside-or-border '(27.90D0 52.76D0) trapezoid))
     (assert-true (is-inside-or-border '(29.48D0 52.14D0) trapezoid))
     (assert-true (is-inside-or-border '(26.9D0 51.76D0) trapezoid))
     (assert-true (is-inside-or-border '(24.6D0 51.43D0) trapezoid))
     (assert-false (is-inside-or-border '(22.6D0 51.43D0) trapezoid))
     (assert-false (is-inside-or-border '(22.6D0 53.43D0) trapezoid))
     (assert-false (is-inside-or-border '(28.6D0 53.99D0) trapezoid))
     (assert-false (is-inside-or-border '(28.6D0 51.79D0) trapezoid))))  

(define-test is-inside-spiralid-test
  (let ((spiralid '((29.71D0 52.96D0) (30.39D0 53.02D0) (30.33D0 53.26D0) (29.23D0 53.24D0) (29.03D0 52.70D0) (30.88D0 52.68D0)
                    (31.02D0 53.40D0) (29.52D0 53.76D0) (31.33D0 54.11D0) (31.65D0 52.28D0) (28.68D0 52.58D0) (28.88D0 53.54D0)
                    (30.73D0 53.33D0) (30.49D0 52.82D0) (29.71D0 52.96D0))))
     (assert-false (is-inside-or-border '(29.90D0 53.02D0) spiralid))
     (assert-false (is-inside-or-border '(30.90D0 53.02D0) spiralid))
     (assert-false (is-inside-or-border '(30.90D0 52.02D0) spiralid))
     (assert-false (is-inside-or-border '(31.40D0 54.02D0) spiralid))
     (assert-true (is-inside-or-border '(30.40D0 52.88D0) spiralid))
     (assert-true (is-inside-or-border '(31.33D0 54.11D0) spiralid))
     (assert-true (is-inside-or-border '(29.00D0 53.38D0) spiralid))
     (assert-true (is-inside-or-border '(29.00D0 52.58D0) spiralid))
     (assert-true (is-inside-or-border '(30.43D0 53.88D0) spiralid))))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))
      
(define-test is-inside-ireland-test
  (let* ((json (jsown:parse (file-get-contents *ireland-polygon-file-path*)))
         (ireland-polygon (mapcar (lambda (x) (list (coerce (car x) 'double-float) (coerce (cadr x) 'double-float)))
                                  (car (jsown:val  (jsown:val (car (jsown:val json "features")) "geometry") "coordinates"))))
         (dublin '(-6.2576209D0 53.3293802D0))
         (belfast '(-6.0670604D0 54.5949977D0)))
     (assert-true (is-inside-or-border dublin ireland-polygon))
     (assert-false (is-inside-or-border belfast ireland-polygon))
  ))

(setq *print-failures* t)
(setq *print-errors* t)
(run-tests :all)
