#lang racket

; Download neume fonts from http://www.dblab.ntua.gr/~stef/mysite/php/index.php?pg=pgbyz&lang=en
; Download EZ Omega font from http://www.stanthonysmonastery.org/music/ByzMusicFonts.html
;   EZ Omega is in the EZ Byzantine music fonts package. It is used for rendering English text.
;   If you want to use a different text font, modify (define TEXT-FONT) below.

(require 2htdp/image)
(require "neumes.rkt")

(define NEUME-SIZE 28)
(define FONT-SIZE 18)
(define TEXT-FONT "EZ Omega")

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
               (text/font (phrase-text a-phrase) FONT-SIZE "black" TEXT-FONT 'modern 'normal 'normal #f)))

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
; call using default hash with (print-all-neumes neume-names)
(define (print-all-neumes neume-hash)
  (apply above/align "left" (list-all-neumes neume-hash)))

; list-all-neumes: hash -> list-of-images
; given a names hash generate a list of images of every neume and its name
; will insert an ison for neumes that modify a preceeding neume (otherwise it will not print)
; Because it is a hash, it is unordered output. In the future, it'd be nice to have a better function for this
; that has a more ordered output for documentation, but this will suffice for now.
; TODO: See if I can get the chant macro to work here instead of using (list (phrase etc.
(define (list-all-neumes neume-hash)
  (for/list ([(key value) neume-hash])
    (cond
      [(false? (neume-modifier? value))
       (render
        (list (phrase (first (neume-aliases value)) (list value))))]
      [else
       (render
        (list (phrase (first (neume-aliases value)) (list ison value))))])))

; chant-page : list-of-chant -> image
(define (chant-page list-of-chant)
  (cond
    [(empty? list-of-chant) (square 0 "solid" "white")]
    [else
     (above/align "left" (render (first list-of-chant)) (chant-page (rest list-of-chant)))]))
       
; DEMONSTRATION OF USE ---------------------

; Use the program in the following way:

(define test-chant
  (chant
   ["Lord," (oligon+kentema-side klasma-left)]
   ["have__" (ison ypporoe-gorgon)]
   ["mer - - - - - - - -" (oligon oligon+kentemata-below gorgon elaphron)]
   ["cy" (apostrophos klasma-right)]
   ["" (martyria-vou)]
  )
)

; Run the following in the interaction window below to render the test chant:
; (render test-chant)

; Run the following to render multiple lines of chant as one image:
; (chant-page (list test-chant test-chant test-chant))