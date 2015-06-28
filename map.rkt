#lang racket
(require profile)
(require racket/gui/base)
(require "common.rkt")
(require "shape-reader.rkt")
(require "projection.rkt")

(provide make-map map-logger)

(define-logger map)

(module+ test
  (require rackunit)
  (define ε 1e-10))

(define borders-shape-file "data/TM_WORLD_BORDERS-0.3.shp")
(define cities-shape-file "data/cities.shp")
(define borders-index-file "data/TM_WORLD_BORDERS-0.3.idx")
(define cities-index-file "data/cities.idx")

(define (halved number times)
  (if (<= times 1)
      (/ number 2)
      (halved (/ number 2) (- times 1))))

(define (map-state-mixin %)
  (class % (super-new)

    (field (zoom-level 4))
    (field (resolutions (reverse
                         (stream->list (stream-map (lambda (x) (halved 1.0 x)) (in-range 1 15))))))
    (field (map-center #f))

    (define/public (zoom-out)
      (log-map-info "Zoom out from ~a~%" zoom-level)
      (let ([new-zoom-level (- zoom-level 1)])
	(when (> new-zoom-level 0)
	  (set! zoom-level new-zoom-level)
	  (send this refresh))))

    (define/public (zoom-in)
      (log-map-info "Zoom in from ~a~%" zoom-level)
      (let ([new-zoom-level (+ zoom-level 1)])
	(when (< new-zoom-level (length resolutions))
	  (set! zoom-level new-zoom-level)
	  (send this refresh))))

    (define/public (set-center new-center) (set! map-center new-center) (send this refresh))
    (define/public (get-center) map-center)
    (define/public (get-resolution) (list-ref resolutions zoom-level))))

(define (doubleclick-mixin %)
  (class % (super-new)

    (field (last-left-down #f))
    (field (doubleclick-delay 300))

    (define/override (on-event event)
      (when (eq? (send event get-event-type) 'left-down)
        (if last-left-down
            (if (< (- (current-inexact-milliseconds) last-left-down) doubleclick-delay)
                (let ([x (send event get-x)]
                      [y (send event get-y)])
                  (let-values ([(canvas-width canvas-height) (send this get-size)])
                    (let* ([aeq-coordinate (pixel-to-aeq (pixel x y) (send this get-resolution) canvas-width canvas-height)]
                           [coordinate (aeq-to-wgs84 (send this get-center) aeq-coordinate)])
                      (log-map-info "Centering on ~a~%" coordinate)
                      (send this set-center coordinate)))
                  (set! last-left-down #f))
                (set! last-left-down (current-inexact-milliseconds)))
            (set! last-left-down (current-inexact-milliseconds))))
      (super on-event event))))

(define (draggable-mixin %)
  (class % (super-new)

    (field (drag-start-event #f))

    (define/override (on-event event)
      (let ([x (send event get-x)]
            [y (send event get-y)])

        (when (send event dragging?)
          (unless drag-start-event (set! drag-start-event event))
          (log-map-debug "Dragging: ~a (~a, ~a)~%" event x y))

        (when (and drag-start-event (eq? (send event get-event-type) 'left-up))
          (log-map-debug "Drag start: ~a (~a, ~a)~%" drag-start-event
                  (send drag-start-event get-x) (send drag-start-event get-y))
          (log-map-debug "Drag end: ~a (~a, ~a)~%" event x y)

          (let-values ([(canvas-width canvas-height) (send this get-size)])
            (let* ([map-center (send this get-center)]
                   [resolution (send this get-resolution)]
                   [drag-start-aeq (pixel-to-aeq (pixel (send drag-start-event get-x)
                                                        (send drag-start-event get-y))
                                                 resolution canvas-width canvas-height)]
                   [drag-end-aeq (pixel-to-aeq (pixel x y) resolution canvas-width canvas-height)]

                   [drag-start-coordinate (aeq-to-wgs84 map-center drag-start-aeq)]
                   [drag-end-coordinate (aeq-to-wgs84 map-center drag-end-aeq)]

                   [delta-x-amount (abs (- (lonlat-longitude drag-start-coordinate) (lonlat-longitude drag-end-coordinate)))]
                   [delta-y-amount (abs (- (lonlat-latitude drag-start-coordinate) (lonlat-latitude drag-end-coordinate)))]

                   [delta-x (if (> (lonlat-longitude drag-start-coordinate) (lonlat-longitude drag-end-coordinate))
                                delta-x-amount
                                (* -1 delta-x-amount))]
                   [delta-y (if (< (lonlat-latitude drag-start-coordinate) (lonlat-latitude drag-end-coordinate))
                                (* -1 delta-y-amount)
                                delta-y-amount)])

              (send this set-center (lonlat (+ (lonlat-longitude map-center) delta-x)
                                            (+ (lonlat-latitude map-center) delta-y))))
            (set! drag-start-event #f)))))))

(define (zoomable-mixin %)
  (class % (super-new)
    (define/override (on-char event)
      (match (send event get-key-code)
        ['wheel-up (send this zoom-in)]
        ['wheel-down (send this zoom-out)]
        [else #f]))))

(define (map-rendering-mixin %)
  (class % (super-new)

    (define/private (paint-borders dc resolution map-center polygons)
      (let-values ([(canvas-width canvas-height) (send dc get-size)])
        (let ([aeq-projection* (aeq-projection map-center)]
              [bounds (aeq-bounds resolution canvas-width canvas-height)])
          (for-each
           (lambda (polygon)
             (for-each
              (lambda (ring)
                (render-polygon-points dc aeq-projection* bounds resolution canvas-width canvas-height ring))
              (polygon-rings polygon)))
           polygons))))

    (define/private (paint-cities dc resolution map-center points)
      (let-values ([(canvas-width canvas-height) (send dc get-size)])
        (let ([aeq-projection* (aeq-projection map-center)]
              [bounds (aeq-bounds resolution canvas-width canvas-height)]
              [original-brush (send dc get-brush)])
          (send dc set-brush (new brush% [color "black"]))
          (for-each
           (lambda (point)
             (let* ([coordinate (projection-transform WGS-84 aeq-projection* (point-coordinate point))]
                    [px (aeq-to-pixel coordinate resolution canvas-width canvas-height)]
                    [square-side-length (* resolution 10000)])
               (send dc draw-rectangle 
                     (- (pixel-x px) (/ square-side-length 2))
                     (- (pixel-y px) (/ square-side-length 2))
                     square-side-length square-side-length)))
           points)
          (send dc set-brush original-brush))))

    (define/public (paint-canvas canvas dc)
      (let-values ([(canvas-width canvas-height) (send dc get-size)])
        (let* ([start (current-inexact-milliseconds)]
               [resolution (send canvas get-resolution)]
               [map-center (send canvas get-center)]

               [viewport-x-max-aeq (/ (/ canvas-width 2) resolution)]
               [viewport-y-max-aeq (/ (/ canvas-height 2) resolution)]
               [viewport-x-min-aeq (* -1.0 viewport-x-max-aeq)]
               [viewport-y-min-aeq (* -1.0 viewport-x-max-aeq)]
               [viewport-min-aeq (lonlat viewport-x-min-aeq viewport-y-min-aeq)]
               [viewport-max-aeq (lonlat viewport-x-max-aeq viewport-y-max-aeq)]

               [viewport-min-wgs84 (aeq-to-wgs84 map-center viewport-min-aeq)]
               [viewport-max-wgs84 (aeq-to-wgs84 map-center viewport-max-aeq)])
          (log-map-debug "paint-canvas [~a, ~a] @ ~a~%" viewport-min-wgs84 viewport-max-wgs84 resolution)

          (send canvas suspend-flush)
          ;; (profile-thunk
          ;;  (thunk
          (paint-borders dc resolution map-center
                         (find-shapes-cached borders-shape-file borders-index-file viewport-min-wgs84 viewport-max-wgs84))
          (paint-cities dc resolution map-center
                        (find-shapes-cached cities-shape-file cities-index-file viewport-min-wgs84 viewport-max-wgs84))
          ;; ))
          (send canvas resume-flush)

          (log-map-debug "paint-canvas took ~a ms~%" (- (current-inexact-milliseconds) start)))))))


(define map-canvas% (map-rendering-mixin (zoomable-mixin (doubleclick-mixin (draggable-mixin (map-state-mixin canvas%))))))


;;;
;;; Polygon rendering
;;;
(define (render-polygon-points-two-or-more dc projection bounds resolution canvas-width canvas-height points
                                           #:current-coordinate [current-coordinate #f]
                                           #:dc-path [dc-path (new dc-path%)]
                                           #:path-started [path-started #f])
  (let* ([current-in-aeq (if current-coordinate current-coordinate
                             (projection-transform WGS-84 projection (car points)))]
         [next-in-aeq (projection-transform WGS-84 projection (cadr points))]
         [current-in-bounds (within-aeq-bounds bounds current-in-aeq)]
         [next-in-bounds (within-aeq-bounds bounds next-in-aeq)])

    (if (or current-in-bounds next-in-bounds)
        (let ([px (aeq-to-pixel current-in-aeq resolution canvas-width canvas-height)])
          (if path-started
              (send dc-path line-to (pixel-x px) (pixel-y px))
              (send dc-path move-to (pixel-x px) (pixel-y px)))

          (render-polygon-points dc projection bounds resolution canvas-width canvas-height (cdr points)
                                 #:current-coordinate next-in-aeq
                                 #:dc-path dc-path
                                 #:path-started #t))

        (if path-started
            (let ([px (aeq-to-pixel current-in-aeq resolution canvas-width canvas-height)])
              (send dc-path line-to (pixel-x px) (pixel-y px))
              (send dc draw-path dc-path)
              (render-polygon-points dc projection bounds resolution canvas-width canvas-height (cdr points)
                                     #:current-coordinate next-in-aeq))
            (render-polygon-points dc projection bounds resolution canvas-width canvas-height (cdr points)
                                   #:current-coordinate next-in-aeq
                                   #:dc-path dc-path)))))

(define (render-polygon-points-exactly-one dc projection bounds resolution canvas-width canvas-height points
                                           #:current-coordinate [current-coordinate #f]
                                           #:dc-path [dc-path (new dc-path%)]
                                           #:path-started [path-started #f])
  (when path-started
    (let ([px (aeq-to-pixel
               (if current-coordinate current-coordinate
                   (projection-transform WGS-84 projection (car points)))
               resolution canvas-width canvas-height)])
      (send dc-path line-to (pixel-x px) (pixel-y px))
      (send dc draw-path dc-path))))

(define (render-polygon-points dc projection bounds resolution canvas-width canvas-height points
                               #:current-coordinate [current-coordinate #f]
                               #:dc-path [dc-path (new dc-path%)]
                               #:path-started [path-started #f])
  (cond
    [(two-or-more-members points)
     (render-polygon-points-two-or-more dc projection bounds resolution canvas-width canvas-height points
                                        #:current-coordinate current-coordinate
                                        #:dc-path dc-path
                                        #:path-started path-started)]

    [(exactly-one-member points)
     (render-polygon-points-exactly-one dc projection bounds resolution canvas-width canvas-height points
                                        #:current-coordinate current-coordinate
                                        #:dc-path dc-path
                                        #:path-started path-started)]
    [else
     (log-map-warning "No match: ~a~%" points)]))

;;
;; Exposed
;;
(define (make-map parent status-line-callback)
  (let* ([initial-wgs84-center (lonlat 24.935 60.19)]
         [callback-wrapper (lambda (canvas dc) (send canvas paint-canvas canvas dc))]
	 [map-canvas (new map-canvas% [parent parent] [paint-callback callback-wrapper])])
    (send map-canvas set-center initial-wgs84-center)
    map-canvas))
