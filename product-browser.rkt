;; -*- geiser-scheme-implementation: racket -*-
#lang racket
(require racket/gui/base)
(require framework)
(require "common.rkt")
(require "map.rkt")

(define frame (new frame%
                   [label "Product Browser"]
                   [width 640]
                   [height 480]))


;; (define splitter (new vertical-panel%
;;                       [parent frame]))

;; (define top-container (new horizontal-panel%
;;                             [parent splitter]
;;                             [min-height 80]
;;                             [stretchable-height #f]))

(define menu-bar
  (new menu-bar%
       [parent frame]))

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



;; (new menu%
;;      (label "&Edit")
;;      (parent menu-bar))
;; (new menu%
;;      (label "&Help")
;;      (parent menu-bar))

;; (define date-selection
;;   (new group-box-panel%
;;        [parent top-container]
;;        [label "Date selection"]))

;; (define date-dropdown
;;   (new combo-field%
;;        [label "Date"]
;;        [parent date-selection]
;;        [choices (list "Today")]
;;        [init-value "Today"]))
  


;; (define canvas (make-map splitter))
(define canvas (make-map frame (lambda (line) (send frame set-status-text line))))

;; (make-map frame)
(send frame create-status-line)
(send frame show #t)
