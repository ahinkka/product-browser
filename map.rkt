#lang racket
(require racket/gui/base)
(require "common.rkt")
(require "shape-reader.rkt")

(provide make-map)

(define borders-shape-file "data/TM_WORLD_BORDERS-0.3.shp")
(define borders-index-file "data/TM_WORLD_BORDERS-0.3.idx")

(define-struct translation
  [min
   max
   pixel-height
   pixel-width] #:transparent)


(define-struct pixel
  [x y]

  #:property prop:custom-write
  (lambda (pixel port write?)
    (fprintf port (if write? "(~a, ~a)" "(~a,~a}")
             (pixel-x pixel) (pixel-y pixel))))


(define (translate translation coordinate)
  (let* ([min-degrees (translation-min translation)]
         [max-degrees (translation-max translation)]
         [degree-width (- (lonlat-longitude max-degrees) (lonlat-longitude min-degrees))]
         [degree-height (- (lonlat-latitude max-degrees) (lonlat-latitude min-degrees))]
         [horizontal-density (/ (translation-pixel-width translation) degree-width)]
         [vertical-density (/ (translation-pixel-height translation) degree-height)])
    
    (pixel
     (* (- (lonlat-longitude max-degrees) (lonlat-longitude coordinate))
        horizontal-density)
     (* (- (lonlat-latitude coordinate) (lonlat-latitude min-degrees))
        vertical-density))))


(define (within-translation translation coordinate)
  (let ([result
	 (let ([min (translation-min translation)]
	       [max (translation-max translation)])
	   (and 
	    [>= (lonlat-latitude max) (lonlat-latitude coordinate)]
	    [<= (lonlat-latitude min) (lonlat-latitude coordinate)]
	    
	    [>= (lonlat-longitude max) (lonlat-longitude coordinate)]
	    [<= (lonlat-longitude min) (lonlat-longitude coordinate)]))])
    result))


(define (find-cdr proc lst)
  (if (not (pair? lst))
      #f
      (if (proc (car lst))
          lst
          (if (and 
               (pair? (cdr lst))
               (not (empty? (cdr lst))))
              (if (proc (cadr lst))
                  lst
                  (find-cdr proc (cdr lst)))
              #f))))


(define (make-map parent)
  (let* ([min-coordinate (lonlat 14.0 49.0)]
	 [max-coordinate (lonlat 36.0 71.0)]
	 [polygons (find-polygons borders-shape-file borders-index-file min-coordinate max-coordinate)])

    (new canvas% [parent parent]
	 [paint-callback
	  (lambda (canvas dc)
	    (let-values ([(canvas-width canvas-height) (send dc get-size)])
	      (let
		  ([current-translation (translation min-coordinate max-coordinate
						     canvas-width canvas-height)])
		(for-each
		 (lambda (polygon)
		   (for-each
		    (lambda (ring)
		      (let ([start-from (find-cdr
					 (lambda (item) (within-translation current-translation item))
					 ring)]
			    [dc-path (new dc-path%)])
			(when start-from
			  (let* ([translated (translate current-translation (car start-from))]
				 [x (- canvas-width (pixel-x translated))]
				 [y (- canvas-height (pixel-y translated))])
			    (send dc-path move-to x y))
			
			  (for-each
			   (lambda (point)
			     (let* ([translated (translate current-translation point)]
				    [x (- canvas-width (pixel-x translated))]
				    [y (- canvas-height (pixel-y translated))])
			       (send dc-path line-to x y)))
			   (cdr start-from))
			  (send dc draw-path dc-path))))
		    (polygon-rings polygon)))
		 polygons))))])))
