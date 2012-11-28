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
; Download fonts from http://www.dblab.ntua.gr/~stef/mysite/php/index.php?pg=pgbyz&lang=en

; Neutral --------------

(define-neume ison
  (list "ison" "0")
  "BZ Byzantina"
  "a"
  #f
  )

(define-neume ison+petaste
  (list "ison+petaste" "ip" "0f")
  "BZ Byzantina"
  "A"
  #f
  )

; Ascending ------------

(define-neume oligon
  (list "oligon" "+1")
  "BZ Byzantina"
  "s"
  #f
  )

(define-neume oligon+kentema-below
  (list "oligon+kentema-below")
  "BZ Byzantina"
  "d"
  #f
  )

(define-neume oligon+kentema-above
  (list "oligon+kentema-above" "+3")
  "BZ Byzantina"
  "f"
  #f
  )

(define-neume oligon+kentemata-below
  (list "oligon+kentemata-below")
  "BZ Byzantina"
  "c"
  #f
  )

(define-neume oligon+ypsele-right
  (list "oligon+ypsele-right" "+4")
  "BZ Byzantina"
  "g"
  #f
  )

(define-neume oligon+ypsele-left
  (list "oligon+ypsele-left" "+5")
  "BZ Loipa"
  "A"
  #f
  )

(define-neume oligon+kentema+ypsele-right
  (list "oligon+kentema+ypsele-right" "+6")
  "BZ Loipa"
  "a"
  #f
  )

(define-neume oligon+kentema+ypsele-center
  (list "oligon+kentema+ypsele-center" "+7")
  "BZ Loipa"
  "f"
  #f
  )

(define-neume oligon+ypsele-double
  (list "oligon+ypsele-double" "+8")
  "BZ Loipa"
  "g"
  #f
  )

(define-neume oligon+kentemata+ypsele-double
  (list "oligon+kentemata+ypsele-double" "+9")
  "BZ Loipa"
  "h"
  #f
  )

(define-neume kentemata
  (list "kentemata" "+1u")
  "BZ Byzantina"
  "x"
  #f
  )

(define-neume petaste
  (list "petaste" "+1f")
  "BZ Byzantina"
  "S"
  #f
  )

(define-neume oligon+kentema-side
  (list "oligon+kentema-side" "oks")
  "BZ Byzantina"
  "sC"
  #f
  )

; Descending ---------------------

(define-neume apostrophos
  (list "apostrophos" "-1")
  "BZ Byzantina"
  "j"
  #f
  )

(define-neume elaphron
  (list "elaphron" "-2")
  "BZ Byzantina"
  "k"
  #f
  )

; Time --------------------

(define-neume klasma
  (list "klasma" "k")
  "BZ Byzantina"
  "u"
  #t
  )

(define-neume klasma-right
  (list "klasma-right" "kr")
  "BZ Byzantina"
  "i"
  #t
  )

(define-neume klasma-left
  (list "klasma-left" "kl")
  "BZ Byzantina"
  "U"
  #t
  )

(define-neume gorgon
  (list "gorgon" "g")
  "BZ Byzantina"
  "e"
  #t
  )

(define-neume gorgon-right
  (list "gorgon-right" "g-r")
  "BZ Byzantina"
  "r"
  #t
  )

(define-neume ypporoe
  (list "ypporoe" "yp")
  "BZ Byzantina"
  "'"
  #f
  )

(define-neume ypporoe-gorgon
  (list "ypporoe-gorgon" "yp-gorgon")
  "BZ Byzantina"
  ":"
  #f
  )

; Martyria ---------------------

(define-neume martyria-vou
  (list "martyria-vou" "mv")
  "BZ Byzantina"
  "7&"
  #f
  )