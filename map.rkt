#lang racket
(require profile)
(require racket/gui/base)
(require "common.rkt")
(require "projection.rkt")

(provide make-map map-logger)

(define-logger map)


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

    (define/private (paint-map dc resolution map-center)
      (let-values ([(canvas-width canvas-height) (send dc get-size)])
        (let ([aeq-projection-code (aeq-projection-proj4-code map-center)]
              [bounds (aeq-bounds resolution canvas-width canvas-height)])

	  (log-map-info "In paint-map~%")
	  (let-values
	      ([(p p-out p-in p-err)
		(subprocess #f #f #f "/usr/bin/python" "mapnik_render.py"
			    (number->string canvas-width) (number->string canvas-height)
			    (number->string (lonlat-longitude (car bounds)))
			    (number->string (lonlat-latitude (car bounds)))
			    (number->string (lonlat-longitude (cdr bounds)))
			    (number->string (lonlat-latitude (cdr bounds)))
			    aeq-projection-code)])
	    (log-map-debug "Waiting for rendering to end...~%")
	    (sync/timeout 1.5 p)
	    ;; (subprocess-wait p)
	    (log-map-debug "Subprocess done.~%")
	    (let ([status (subprocess-status p)]) 
	      (when (eq? 'running status)
		(subprocess-kill p #t)
		(set! status 'killed))
	      (if (not (eq? 0 status))
		  (begin
		    (log-map-debug "Process not finished: ~a~%" p)
		    ;; (log-map-debug "~a~%" (port->string p-err))
		    (log-map-debug "exit code: ~a~%" status))
		  (begin
		    (let* ([bitmap (read-bitmap p-out 'png #f #t)])
		      (log-map-debug "Bitmap loaded.~%")
		      (send dc draw-bitmap bitmap 0 0)
		      (log-map-debug "Bitmap drawn.~%")))))

	    (close-input-port p-out)
	    (close-output-port p-in)
	    (close-input-port p-err)))))

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
          (paint-map dc resolution map-center)
          (send canvas resume-flush)

          (log-map-debug "paint-canvas took ~a ms~%" (- (current-inexact-milliseconds) start)))))))

(define map-canvas% (map-rendering-mixin (zoomable-mixin (doubleclick-mixin (draggable-mixin (map-state-mixin canvas%))))))


;;
;; Exposed
;;
(define (make-map parent status-line-callback)
  (let* ([initial-wgs84-center (lonlat 24.935 60.19)]
         [callback-wrapper (lambda (canvas dc) (send canvas paint-canvas canvas dc))]
	 [map-canvas (new map-canvas% [parent parent] [paint-callback callback-wrapper])])
    (send map-canvas set-center initial-wgs84-center)
    map-canvas))
