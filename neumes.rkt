#lang racket

(provide (except-out (all-defined-out) record-element! define-neume))

; neume
(struct neume
  (aliases                ; string : for alternative input mechanisms
   font                   ; string : the various neumes are scattered across a few fonts
   character-code         ; string : The roman character code for the desired neume; such as "0" for ison
   modifier?              ; boolean : Whether or not it modifies another note (such as klasma)
   )
  #:transparent)        ; lets us see into the structure; helpful for errors

; MACROS -----------------------

; names : hash listing of all neumes
; from http://queue.acm.org/detail.cfm?id=2068896
(define neume-names (make-hash))    ; symbol to neume
(define neume-elements (make-hash)) ; neume to symbol
(define (record-element! name val)
 (hash-set! neume-names name val)
 (hash-set! neume-elements val name))
(define (neume-name->neume-element name)  (hash-ref neume-names name))
(define (neume-element->neume-name obj)  (hash-ref neume-elements obj))

; macro to make defining neumes easier
(define-syntax-rule (define-neume name aliases font character-code modifier?)
  (begin
    (define name (neume aliases font character-code modifier?))
    (record-element! 'name name)))

; NEUME DEFINITIONS ---------------
; Download fonts from http://www.stanthonysmonastery.org/music/ByzMusicFonts.html

; Neutral --------------

(define-neume ison
  (list "ison" "0")
  "EZ Psaltica"
  "0"
  #f
  )

(define-neume ison+petaste
  (list "ison+petaste" "ip" "0f")
  "EZ Psaltica"
  "p"
  #f
  )

; Ascending ------------

(define-neume oligon
  (list "oligon" "+1")
  "EZ Psaltica"
  "1"
  #f
  )

(define-neume oligon+kentema-below
  (list "oligon+kentema-below")
  "EZ Psaltica"
  "2"
  #f
  )

(define-neume oligon+kentema-above
  (list "oligon+kentema-above" "+3")
  "EZ Psaltica"
  "3"
  #f
  )

(define-neume oligon+ypsele-right
  (list "oligon+ypsele-right" "+4")
  "EZ Psaltica"
  "4"
  #f
  )

(define-neume oligon+ypsele-left
  (list "oligon+ypsele-left" "+5")
  "EZ Psaltica"
  "5"
  #f
  )

(define-neume oligon+kentema+ypsele-right
  (list "oligon+kentema+ypsele-right" "+6")
  "EZ Psaltica"
  "6"
  #f
  )

(define-neume oligon+kentema+ypsele-center
  (list "oligon+kentema+ypsele-center" "+7")
  "EZ Psaltica"
  "7"
  #f
  )

(define-neume oligon+ypsele-double
  (list "oligon+ypsele-double" "+8")
  "EZ Psaltica"
  "8"
  #f
  )

(define-neume oligon+kentemata+ypsele-double
  (list "oligon+kentemata+ypsele-double" "+9")
  "EZ Psaltica"
  "9"
  #f
  )

; left off here

(define-neume kentemata
  (list "kentemata" "+1u")
  "EZ Psaltica"
  "`"
  #f
  )

(define-neume petaste
  (list "petaste" "+1f")
  "EZ Psaltica"
  "q"
  #f
  )

(define-neume oligon+kentema-side
  (list "oligon+kentema-side" "oks")
  "EZ Psaltica"
  "1~"
  #f
  )

; Descending ---------------------

(define-neume apostrophos
  (list "apostrophos" "-1")
  "EZ Psaltica"
  "!"
  #f
  )

(define-neume elaphron
  (list "elaphron" "-2")
  "EZ Psaltica"
  "@"
  #f
  )

; Time --------------------

(define-neume klasma
  (list "klasma" "k")
  "EZ Psaltica"
  "a"
  #t
  )

(define-neume klasma-right
  (list "klasma-right" "kr")
  "EZ Psaltica"
  "A"
  #t
  )

(define-neume klasma-left
  (list "klasma-left" "kl")
  "EZ Psaltica"
  "Z"
  #t
  )

(define-neume gorgon
  (list "gorgon" "g")
  "EZ Psaltica"
  "s"
  #t
  )

(define-neume gorgon-right
  (list "gorgon-right" "g-r")
  "EZ Psaltica"
  "S"
  #t
  )

(define-neume ypporoe
  (list "ypporoe" "yp")
  "EZ Psaltica"
  ")"
  #f
  )

(define-neume ypporoe-gorgon
  (list "ypporoe-gorgon" "yp-gorgon")
  "EZ Psaltica"
  "-"
  #f
  )

(define-neume oligon+kentemata-below
  (list "oligon+kentemata-below" "okb")
  "EZ Psaltica"
  "o"
  #f
  )

; Martyria ---------------------

(define-neume martyria-vou
  (list "martyria-vou" "mv")
  "EZ Psaltica"
  "cC"
  #f
  )