#lang racket

(require 2htdp/image)
(require "neumes.rkt")

(define NEUME-SIZE 28)
(define FONT-SIZE 18)

; STRUCTS -----------------------

; phrase
; defines a syllable and accompanying notes
(struct phrase
  (text                   ; string: the word to be displayed
   notes)                 ; list-of-neumes
  #:transparent)

; MACROS -------------------------

; Macro to make defining lispy chant a bit more readable
(define-syntax-rule (chant [word (notes ...)] ...)
  (list [phrase word (list notes ...)] ...))

; Without macro (for reference)
;(define test-chant (list (phrase "Ky" (list ison klasma)) (phrase "ri" (list oligon)) (phrase "e" (list ison apostrophos gorgon-right))))


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

; print-all-neumes : hash -> list-of-images
; given a names hash generate a list of images of every neume and its name
; call using default hash with (print-all-neumes neume-names)
; will insert an ison for neumes that modify a preceeding neume (otherwise it will not print)
; Because it is a hash, it is unordered output. In the future, it'd be nice to have a better function for this
; that can be used for documentation, but this will suffice for now.
(define (print-all-neumes neume-hash)
  (for/list ([(key value) neume-hash])
    (cond
      [(false? (neume-modifier? value)) (render (list (phrase (first (neume-aliases value)) (list value))))]
      [else
       (render (list (phrase (first (neume-aliases value)) (list ison value))))])))
       
; DEMONSTRATION OF USE ---------------------

; Use the program in the following way:

(define test-chant
  (chant
   ["Lord," (oligon+kentema-side klasma-left)]
   ["have__" (ison ypporoe-gorgon)]
   ["mer - - - - - - - -" (ison oligon+kentemata-below gorgon elaphron)]
   ["cy" (apostrophos klasma-right)]
   ["" (martyria-vou)]
  )
)

; Run the following in the interaction window below to render the test chant:
; (render test-chant)