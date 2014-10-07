#lang racket

(provide
 lonlat
 lonlat-latitude
 lonlat-longitude)

(define-struct lonlat (longitude latitude)
  #:property prop:custom-write
  (lambda (lonlat port write?)
    (fprintf port (if write? "{~a, ~a}" "{~a,~a}")
             (lonlat-latitude lonlat) (lonlat-longitude lonlat))))
