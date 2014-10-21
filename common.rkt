#lang racket

(module+ test
  (require rackunit)
  (define ε 1e-10))

(provide
 lonlat
 lonlat-latitude
 lonlat-longitude

 two-or-more-members
 exactly-one-member

 pixel pixel-x pixel-y
 aeq-to-pixel
 pixel-to-aeq
 aeq-bounds
 within-aeq-bounds)

(define-struct lonlat (longitude latitude)
  #:transparent
  #:property prop:custom-write
  (lambda (lonlat port write?)
    (fprintf port (if write? "{~a, ~a}" "{~a,~a}")
             (lonlat-longitude lonlat) (lonlat-latitude lonlat))))

;;
;; Coordinate normalization
;;
(define (normalize-longitude longitude)
  (cond [(< longitude -180) (+ 360 longitude)]
        [(> longitude 180) (- longitude 360)]
        [else longitude]))

(module+ test
  (check-eq? 0 (normalize-longitude 0))
  (check-eq? -180 (normalize-longitude -180))
  (check-eq? 180 (normalize-longitude 180))
  (check-eq? -179 (normalize-longitude 181))
  (check-eq? 179 (normalize-longitude -181)))

;;
;; Functions on lists
;;
(define (two-or-more-members lst)
  (and
   lst
   (not (null? lst))
   (not (null? (cdr lst)))))

(module+ test
  (check-eq? #f (two-or-more-members '()))
  (check-eq? #f (two-or-more-members '(1)))
  (check-eq? #t (two-or-more-members '(1 2)))
  (check-eq? #t (two-or-more-members '(1 2 3))))

(define (exactly-one-member lst)
  (and
   lst
   (not (null? lst))
   (null? (cdr lst))))

(module+ test
  (check-eq? #f (exactly-one-member '()))
  (check-eq? #t (exactly-one-member '(1)))
  (check-eq? #f (exactly-one-member '(1 2))))

;;
;; Abstractions for map rendering
;;
(define-struct pixel
  [x y]

  #:property prop:custom-write
  (lambda (pixel port write?)
    (fprintf port (if write? "(~a, ~a)" "(~a,~a}")
             (pixel-x pixel) (pixel-y pixel))))


;;
;; AEQ projection
;;
(define (aeq-to-pixel coordinate resolution canvas-width canvas-height)
  (let* ([scaled-x (* resolution (lonlat-longitude coordinate))]
         [scaled-y (* resolution (lonlat-latitude coordinate))]
         [x (+ (/ canvas-width 2) scaled-x)]
         [y (- (/ canvas-height 2) scaled-y)])
    (pixel x y)))

(module+ test
  (let ([px (aeq-to-pixel (lonlat 0 0) 1 400 400)])
    (check-= (pixel-x px) 200 ε)
    (check-= (pixel-y px) 200 ε))

  (let ([px (aeq-to-pixel (lonlat -100 100) 1 400 400)])
    (check-= (pixel-x px) 100 ε)
    (check-= (pixel-y px) 100 ε))

  (let ([px (aeq-to-pixel (lonlat 100 100) 1 400 400)])
    (check-= (pixel-x px) 300 ε)
    (check-= (pixel-y px) 100 ε))

  (let ([px (aeq-to-pixel (lonlat -100 -100) 1 400 400)])
    (check-= (pixel-x px) 100 ε)
    (check-= (pixel-y px) 300 ε)))

(define (pixel-to-aeq px resolution canvas-width canvas-height)
  (let* ([centered-x (- (pixel-x px) (/ canvas-width 2))]
         [centered-y (- (- canvas-height (pixel-y px)) (/ canvas-height 2))]
         [x (/ centered-x resolution)]
         [y (/ centered-y resolution)])
    (lonlat x y)))

(module+ test
  (let ([coordinate (pixel-to-aeq (pixel 200 200) 1 400 400)])
    (check-= (lonlat-longitude coordinate) 0 ε)
    (check-= (lonlat-latitude coordinate) 0 ε))

  (let ([coordinate (pixel-to-aeq (pixel 100 100) 1 400 400)])
    (check-= (lonlat-longitude coordinate) -100 ε)
    (check-= (lonlat-latitude coordinate) 100 ε))

  (let ([coordinate (pixel-to-aeq (pixel 300 300) 1 400 400)])
    (check-= (lonlat-longitude coordinate) 100 ε)
    (check-= (lonlat-latitude coordinate) -100 ε))

  (let ([coordinate (pixel-to-aeq (pixel 100 300) 1 400 400)])
    (check-= (lonlat-longitude coordinate) -100 ε)
    (check-= (lonlat-latitude coordinate) -100 ε)))

(define (aeq-bounds resolution canvas-width canvas-height)
  (let ([upper-left (pixel-to-aeq (pixel 0 0) resolution canvas-width canvas-height)]
        [upper-right (pixel-to-aeq (pixel canvas-width 0) resolution canvas-width canvas-height)]
        [lower-left (pixel-to-aeq (pixel 0 canvas-height) resolution canvas-width canvas-height)]
        [lower-right (pixel-to-aeq (pixel canvas-width canvas-height) resolution canvas-width canvas-height)])
    (cons
     (lonlat
      (min (lonlat-longitude upper-left)
           (lonlat-longitude upper-right)
           (lonlat-longitude lower-left)
           (lonlat-longitude lower-right))
      (min (lonlat-latitude upper-left)
           (lonlat-latitude upper-right)
           (lonlat-latitude lower-left)
           (lonlat-latitude lower-right)))
     (lonlat
      (max (lonlat-longitude upper-left)
           (lonlat-longitude upper-right)
           (lonlat-longitude lower-left)
           (lonlat-longitude lower-right))
      (max (lonlat-latitude upper-left)
           (lonlat-latitude upper-right)
           (lonlat-latitude lower-left)
           (lonlat-latitude lower-right))))))

(define (within-aeq-bounds bounds coordinate)
  (let ([min-bound (car bounds)]
        [max-bound (cdr bounds)])
    (and
     (> (lonlat-longitude coordinate) (lonlat-longitude min-bound))
     (> (lonlat-latitude coordinate) (lonlat-latitude min-bound))
     (< (lonlat-longitude coordinate) (lonlat-longitude max-bound))
     (< (lonlat-latitude coordinate) (lonlat-latitude max-bound)))))

(module+ test
  (let* ([bounds (cons
                  (lonlat -1 -1)
                  (lonlat 1 1))])
    (check-eq? #t (within-aeq-bounds bounds (lonlat 0 0)))
    (check-eq? #t (within-aeq-bounds bounds (lonlat -0.5 -0.5)))
    (check-eq? #t (within-aeq-bounds bounds (lonlat 0.5 0.5)))))
