(defpackage point-polygon
  (:use :common-lisp)
  (:export :is-inside-or-border))

(in-package :point-polygon)

(defun is-inside-or-border (p polygon)
  "Tells if point p is inside or in the border of polygon.
  p is expected to be a list of two values, the coodinate pair, while polygon is a list of coordinate pairs
  such that the first element is pairwise equal to the last element."
  (is-inside-or-border-rec p polygon 0 nil))

(defun is-inside-or-border-rec (p polygon quadrant-inc last-point)
  (if (null polygon)
      (not (= quadrant-inc 0))
      (let ((next-inc (if (null last-point) 0
                          (calculate-inc p last-point (car polygon)))))
         (if (null next-inc) t
             (is-inside-or-border-rec p (cdr polygon) (+ quadrant-inc next-inc) (car polygon))))))

(defun calculate-inc (p last-point next-point)
  "Calculates the integer counterclockwise movement in quadrants from last-point to next-point.
   Returns NIL if p is on the border of the segment with last-point and next-point as vertex."
  (let ((last-quadrant (quadrant p last-point))
        (next-quadrant (quadrant p next-point)))
    (cond ((or (pair-eq p last-point) (pair-eq p next-point)) nil)
          ((eq last-quadrant next-quadrant) 0)
          ((and (eq last-quadrant :q1) (eq next-quadrant :q2)) 1)
          ((and (eq last-quadrant :q2) (eq next-quadrant :q3)) 1)
          ((and (eq last-quadrant :q3) (eq next-quadrant :q4)) 1)
          ((and (eq last-quadrant :q4) (eq next-quadrant :q1)) 1)
          ((and (eq last-quadrant :q4) (eq next-quadrant :q3)) -1)
          ((and (eq last-quadrant :q3) (eq next-quadrant :q2)) -1)
          ((and (eq last-quadrant :q2) (eq next-quadrant :q1)) -1)
          ((and (eq last-quadrant :q1) (eq next-quadrant :q4)) -1)
          (t (if (= (cadr last-point) (cadr next-point)) nil
			     (let* ((proj (abscissa-projection p last-point next-point))
			            (projection-before-p (< (car proj) (car p))))			       
                   (cond ((pair-eq p proj) nil)
                         ((and projection-before-p (eq last-quadrant :q1)) 2)
                         ((and projection-before-p (eq last-quadrant :q2)) 2)
                         ((and projection-before-p (eq last-quadrant :q3)) -2)
                         ((and projection-before-p (eq last-quadrant :q4)) -2)
                         ((and (not projection-before-p) (eq last-quadrant :q1)) -2)
                         ((and (not projection-before-p) (eq last-quadrant :q2)) -2)
                         ((and (not projection-before-p) (eq last-quadrant :q3)) 2)
                         (t 2))))))))

(defun abscissa-projection (p v1 v2)
  "Returns the abscissa projection of point p into segment with vertex v1 and v2.
   It expect the segment not parallel and intersecting the projection."  
  (list (+ (car v1)
           (/ (* (- (car v2) (car v1))
                 (- (cadr p) (cadr v1)))
              (- (cadr v2) (cadr v1))))
        (cadr p)))

(defun pair-eq (x y) 
  "Tells if first and second element of lists x and y are arithmetically equal."
  (and (= (car x) (car y)) (= (cadr x) (cadr y))))

(defun quadrant (x y)
  "Quadrant of y relative to x, NIL if they are the same."
  (cond ((and (> (car y) (car x)) (>= (cadr y) (cadr x))) :q1)
        ((and (>= (car x) (car y)) (> (cadr y) (cadr x))) :q2)
        ((and (> (car x) (car y)) (>= (cadr x) (cadr y))) :q3)
        ((and (<= (car x) (car y)) (> (cadr x) (cadr y))) :q4)
        (t nil)))
