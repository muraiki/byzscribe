#lang racket
(require "../byzscribe.rkt")

(define test-chant
  (chant
   ["Lord," (oligon+kentema-side klasma-left)]
   ["have" _ (ison ypporoe+gorgon)]
   ["mer" - (oligon oligon+kentemata-below gorgon elaphron)]
   ["cy" (apostrophos klasma-right)]
   ["" (martyria-ni)]
  )
)

(define test-chant2 (chant ["hear" _ (kamele+petaste klasma-below-right apostrophos kentemata)]))

; Run the following to render one line of chant:
; (render test-chant)

; Run the following to render multiple lines of chant as one image:
; (chant-page (list test-chant test-chant2))