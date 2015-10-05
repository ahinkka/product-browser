;; -*- geiser-scheme-implementation: racket -*-
#lang racket

(require ffi/unsafe
         ffi/unsafe/define
	 racket/math)
(require "common.rkt")

(module+ test
  (require rackunit)
  (define ε 1e-10))


(provide projection-transform
	 WGS-84
	 EPSG-3857
	 aeq-projection
	 aeq-projection-proj4-code

	 wgs84-to-web-mercator
	 web-mercator-to-aeq
	 wgs84-to-aeq
	 aeq-to-wgs84)

(define-ffi-definer define-proj (ffi-lib "libproj.so.0"))

;; projPJ pj_init_plus(const char *);
(define-proj pj_init_plus (_fun _string -> _pointer))

;; int pj_transform( projPJ src, projPJ dst, long point_count, int point_offset,
;;                   double *x, double *y, double *z );
(define-proj pj_transform (_fun _pointer _pointer _long _int _pointer _pointer _pointer -> _int))

;; void pj_free(projPJ);
(define-proj pj_free (_fun _pointer -> _void))

;; http://spatialreference.org/ref/epsg/wgs-84/
(define WGS-84 (pj_init_plus "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
;; http://spatialreference.org/ref/sr-org/6864/
(define EPSG-3857 (pj_init_plus "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

(unless EPSG-3857
  (printf "Couldn't define Web Mercator!~%"))

(define (aeq-projection center-coordinate)
  (let ([projection (pj_init_plus (aeq-projection-proj4-code center-coordinate))])
    (register-finalizer projection pj_free)
    projection))

(define (aeq-projection-proj4-code center-coordinate)
  (format "+proj=aeqd +lat_0=~a +lon_0=~a"
	  (lonlat-latitude center-coordinate)
	  (lonlat-longitude center-coordinate)))

(define (projection-transform from-projection to-projection coordinate)
  (let ([x-ptr (malloc (ctype-sizeof _double) 'atomic)]
	[y-ptr (malloc (ctype-sizeof _double) 'atomic)])
    (let ([x-coordinate (if (eq? from-projection WGS-84)
			    (degrees->radians (lonlat-longitude coordinate))
			    (lonlat-longitude coordinate))]
	  [y-coordinate (if (eq? from-projection WGS-84)
			    (degrees->radians (lonlat-latitude coordinate))
			    (lonlat-latitude coordinate))])
      (ptr-set! x-ptr _double x-coordinate)
      (ptr-set! y-ptr _double y-coordinate)

      (let ([return-code (pj_transform from-projection to-projection 1 1 x-ptr y-ptr #f)])
	(unless (= 0 return-code)
	  (error (format "pj_transform returned ~a" return-code))))

      (let ([x (ptr-ref x-ptr _double)]
	    [y (ptr-ref y-ptr _double)])
	(if (eq? to-projection WGS-84)
	    (lonlat (radians->degrees x) (radians->degrees y))
	    (lonlat x y))))))

(define (wgs84-to-web-mercator coordinate)
  (projection-transform WGS-84 EPSG-3857 coordinate))

(define (web-mercator-to-aeq aeq-center coordinate)
  (let ([aeq-projection* (aeq-projection aeq-center)])
    (projection-transform EPSG-3857 WGS-84 coordinate)))

(define (wgs84-to-aeq aeq-center coordinate)
  (let ([aeq-projection* (aeq-projection aeq-center)])
    (projection-transform aeq-projection* WGS-84 coordinate)))

(define (aeq-to-wgs84 aeq-center coordinate)
  (let ([aeq-projection* (aeq-projection aeq-center)])
    (projection-transform aeq-projection* WGS-84 coordinate)))

(module+ test
  (let* ([helsinki-in-wgs84 (lonlat 24.935 60.19)]
         [aeq-helsinki (aeq-projection helsinki-in-wgs84)]
         [helsinki-in-aeq (projection-transform WGS-84 aeq-helsinki helsinki-in-wgs84)]
         [back-in-wgs84 (projection-transform aeq-helsinki WGS-84 helsinki-in-aeq)])
    (check-= (lonlat-longitude helsinki-in-aeq) 0 ε)
    (check-= (lonlat-latitude helsinki-in-aeq) 0 ε)
    (check-= (lonlat-longitude back-in-wgs84) (lonlat-longitude helsinki-in-wgs84) ε)
    (check-= (lonlat-latitude back-in-wgs84) (lonlat-latitude helsinki-in-wgs84) ε)))
