#lang racket

(require 2htdp/image)
(require "neumes.rkt")

(define NEUME-SIZE 28)
(define FONT-SIZE 18)

; phrase
; defines a syllable and accompanying notes
(struct phrase
  (text                   ; string: the word to be displayed
   notes)                 ; list-of-neumes
  #:transparent)

; FUNCTION DEFINITIONS ---------------------

; render : list of phrases -> image
(define (render chant-list)
  (cond
    [(empty? chant-list) (square 1 "solid" "white")] 
    [else (beside (render-phrase (first chant-list)) (render (rest chant-list)))]))

; render-phrase : phrase -> image
(define (render-phrase a-phrase)
  (above/align "left"
               (render-neumes (phrase-notes a-phrase))
               (text/font (phrase-text a-phrase) FONT-SIZE "black" "EZ Omega" 'modern 'normal 'normal #f)))

; render-neumes : list of neumes -> image
; for now uses same font for whole phrase
(define (render-neumes notes)
  (text/font (concat-neumes notes) NEUME-SIZE "black" (neume-font (first notes)) 'modern 'normal 'normal #f))

; concat-neumes : list-of-neumes -> string
; concat all the neumes' character-codes together
(define (concat-neumes notes)
  (cond
    [(empty? notes) ""]
    [else (string-append (neume-character-code (first notes)) (concat-neumes (rest notes)))]))

; DEMONSTRATION ---------------------

; Without macro
;(define test-chant (list (phrase "Ky" (list ison klasma)) (phrase "ri" (list oligon)) (phrase "e" (list ison apostrophos gorgon-right))))

(define-syntax-rule (chant [word (notes ...)] ...)
  (list [phrase word (list notes ...)] ...))

(define test-chant
  (chant
   ["Lord," (oligon+kentema-side klasma-left)]
   ["have__" (ison ypporoe-gorgon)]
   ["mer - - - - - - - -" (ison oligon+kentemata-below gorgon elaphron)]
   ["cy" (apostrophos klasma-right)]
   ["" (martyria-vou)]
  )
)

(render test-chant)

;
