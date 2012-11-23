#lang racket

(provide neume
         neume-aliases
         neume-font
         neume-character-code
         neume-modifier?
         neume-melodic-value
         names
         elements
         name->element
         element->name
         ison
         oligon
         pentaste
         oligon+kentema-side
         apostrophos
         elaphron
         klasma
         klasma-right
         klasma-left
         gorgon
         gorgon-right
         ypporoe
         ypporoe-gorgon
         oligon+kentemata-below
         martyria-vou)

; neume
(struct neume
  (aliases                ; string : for alternative input mechanisms
   font                   ; string : the various neumes are scattered across a few fonts
   character-code         ; string : The roman character code for the desired neume; such as "0" for ison
   modifier?              ; boolean : Whether or not it modifies another note (such as klasma)
   melodic-value)         ; string : Its melodic value (+1, -2, etc), for future use in audio playback  
   #:transparent)         ; lets us see into the structure; helpful for errors
;TODO: Need to add something for time value?

; MACROS -----------------------

; names : hash listing of all neumes
; from http://queue.acm.org/detail.cfm?id=2068896
; Not really necessary for now, but will be needed in the future
(define names (make-hash))    ; symbol to neume
(define elements (make-hash)) ;  neume to symbol
(define (record-element! name val)
 (hash-set! names name val)
 (hash-set! elements val name))
(define (name->element name)  (hash-ref names name))
(define (element->name obj)  (hash-ref elements obj))

; macro to make defining neumes easier
; doesn't do much of anything except automatically add neume to hash list, but might be more useful in the future
(define-syntax-rule (define-neume name aliases font character-code modifier? melodic-value)
  (begin
    (define name (neume aliases font character-code modifier? melodic-value))
    (record-element! 'name name)))

; NEUME DEFINITIONS ---------------
; Download fonts from http://www.stanthonysmonastery.org/music/ByzMusicFonts.html

(define-neume ison
  (list "ison" "0")
  "EZ Psaltica"
  "0"
  #f
  "0")

(define-neume oligon
  (list "oligon" "+1")
  "EZ Psaltica"
  "1"
  #f
  "+1")

(define-neume pentaste
  (list "pentaste" "+1f")
  "EZ Psaltica"
  "q"
  #f
  "+1f")

(define-neume oligon+kentema-side
  (list "oligon+kentema-side" "oks")
  "EZ Psaltica"
  "1~"
  #f
  "+3")

(define-neume apostrophos
  (list "apostrophos" "-1")
  "EZ Psaltica"
  "!"
  #f
  "-1")

(define-neume elaphron
  (list "elaphron" "-2")
  "EZ Psaltica"
  "@"
  #f
  "-2")


(define-neume klasma
  (list "klasma" "k")
  "EZ Psaltica"
  "a"
  #t
  "0")

(define-neume klasma-right
  (list "klasma-right" "kr")
  "EZ Psaltica"
  "A"
  #t
  "0")

(define-neume klasma-left
  (list "klasma-left" "kl")
  "EZ Psaltica"
  "Z"
  #t
  "0")

(define-neume gorgon
  (list "gorgon" "g")
  "EZ Psaltica"
  "s"
  #t
  "0")

(define-neume gorgon-right
  (list "gorgon-right" "g-r")
  "EZ Psaltica"
  "S"
  #t
  "0")

(define-neume ypporoe
  (list "ypporoe" "yp")
  "EZ Psaltica"
  ")"
  #f
  "-1-1")

(define-neume ypporoe-gorgon
  (list "ypporoe-gorgon" "yp-gorgon")
  "EZ Psaltica"
  "-"
  #f
  "-1-1")

(define-neume oligon+kentemata-below
  (list "oligon+kentemata-below" "okb")
  "EZ Psaltica"
  "o"
  #f
  "+1+1")

(define-neume martyria-vou
  (list "martyria-vou" "mv")
  "EZ Psaltica"
  "cC"
  #f
  "0")