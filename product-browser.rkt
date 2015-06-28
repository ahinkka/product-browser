;; -*- geiser-scheme-implementation: racket -*-
#lang racket
(require racket/gui/base)
(require framework)
(require "common.rkt")
(require "map.rkt")

(define main-logger (make-logger 'product-browser #f))

(define frame
  (new frame%
       [label "Product Browser"]
       [width 640]
       [height 480]))

(define menu-bar
  (new menu-bar%
       [parent frame]))


;;; File menu
(define file-menu
  (new menu%
       [label "&File"]
       [parent menu-bar]))

(define quit-item
  (new menu-item%
       [parent file-menu]
       [label "&Quit"]
       [callback (lambda (menu event-type)
		   (exit:exit))]))


;;; Layout
(define splitter
  (new vertical-panel%
       [parent frame]))


;;; Date selection part
(define top-container
  (new horizontal-panel%
       [parent splitter]
       [min-height 80]
       [stretchable-height #f]))

(define date-selection
  (new group-box-panel%
       [parent top-container]
       [label "Date selection"]))

(define date-dropdown
  (new combo-field%
       [label "Date"]
       [parent date-selection]
       [choices (list "Today")]
       [init-value "Today"]))


;;; Lower part
(define canvas (make-map splitter (lambda (line) (send frame set-status-text line))))


;;; Program proper
(send frame create-status-line)

(define map-log-receiver (make-log-receiver map-logger 'info))

(void
 (thread
  (lambda ()
    (let loop ()
      (define v (sync map-log-receiver))
      (printf "[~a] ~a" (vector-ref v 0) (vector-ref v 1))
      (loop)))))

(send frame show #t)
