#lang racket

; Byzscribe: A program for rendering plaintext byzantine chant code
; Neume definitions file
; Copyright (C) 2012 Erik Ferguson

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

; -------------------------------------

(provide (except-out (all-defined-out) record-element! define-neume))

; neume
(struct neume
  (aliases                ; string : for alternative input mechanisms
   font                   ; string : the various neumes are scattered across a few fonts
   character-code         ; string : The roman character code for the desired neume; such as "0" for ison
   modifier?              ; boolean : Whether or not it modifies another note (such as klasma)
   color                  ; string : color of the neume
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
; TODO: Modify macro so that the first alias is the name itself, to eliminate repetition when writing new neumes
(define-syntax-rule (define-neume name aliases font character-code modifier? color)
  (begin
    (define name (neume aliases font character-code modifier? color))
    (record-element! 'name name)))

; NEUME DEFINITIONS ---------------
; Download neume fonts from http://www.dblab.ntua.gr/~stef/mysite/php/index.php?pg=pgbyz&lang=en
; Download EZ Omega font from http://www.stanthonysmonastery.org/music/ByzMusicFonts.html
;   use the EZ Byzantine music fonts package. EZ Omega is used for rendering English text.

; Neutral --------------

(define-neume ison
  (list "ison" "0")
  "BZ Byzantina"
  "a"
  #f
  "black"
  )

(define-neume ison+petaste
  (list "ison+petaste" "ip" "0f")
  "BZ Byzantina"
  "A"
  #f
  "black"
  )

; Ascending ------------

(define-neume oligon
  (list "oligon" "+1")
  "BZ Byzantina"
  "s"
  #f
  "black"
  )

(define-neume oligon-petaste
  (list "oligon-petaste" "+2")
  "BZ Byzantina"
  "S"
  #f
  "black"
  )

(define-neume oligon+kentema-below
  (list "oligon+kentema-below")
  "BZ Byzantina"
  "d"
  #f
  "black"
  )

(define-neume oligon+kentema
  (list "oligon+kentema-above" "+3")
  "BZ Byzantina"
  "f"
  #f
  "black"
  )

(define-neume petaste+kentema
  (list "petaste+kentema-above" "+3")
  "BZ Byzantina"
  "F"
  #f
  "black"
  )

(define-neume oligon+kentemata-below
  (list "oligon+kentemata-below")
  "BZ Byzantina"
  "c"
  #f
  "black"
  )

(define-neume oligon+ypsele-right
  (list "oligon+ypsele-right" "+4")
  "BZ Byzantina"
  "g"
  #f
  "black"
  )

(define-neume oligon+ypsele-left
  (list "oligon+ypsele-left" "+5")
  "BZ Loipa"
  "A"
  #f
  "black"
  )

(define-neume oligon+kentema+ypsele-right
  (list "oligon+kentema+ypsele-right" "+6")
  "BZ Loipa"
  "a"
  #f
  "black"
  )

(define-neume oligon+kentema+ypsele-center
  (list "oligon+kentema+ypsele-center" "+7")
  "BZ Loipa"
  "f"
  #f
  "black"
  )

(define-neume oligon+ypsele-double
  (list "oligon+ypsele-double" "+8")
  "BZ Loipa"
  "g"
  #f
  "black"
  )

(define-neume oligon+kentemata+ypsele-double
  (list "oligon+kentemata+ypsele-double" "+9")
  "BZ Loipa"
  "h"
  #f
  "black"
  )

(define-neume kentemata
  (list "kentemata" "+1u")
  "BZ Byzantina"
  "x"
  #f
  "black"
  )

(define-neume petaste
  (list "petaste" "+1f")
  "BZ Byzantina"
  "S"
  #f
  "black"
  )

(define-neume oligon+kentema-side
  (list "oligon+kentema-side" "oks")
  "BZ Byzantina"
  "sC"
  #f
  "black"
  )

; Descending ---------------------

(define-neume apostrophos
  (list "apostrophos" "-1")
  "BZ Byzantina"
  "j"
  #f
  "black"
  )

(define-neume apostrophos-petaste
  (list "apostrophos-petaste" "-1")
  "BZ Byzantina"
  "J"
  #f
  "black"
  )

(define-neume elaphron
  (list "elaphron" "-2")
  "BZ Byzantina"
  "k"
  #f
  "black"
  )

(define-neume elaphron-petaste
  (list "elaphron" "-2f")
  "BZ Byzantina"
  "K"
  #f
  "black"
  )

(define-neume syneches-elaphron
  (list "syneches-elaphron")
  "BZ Byzantina"
  "h"
  #f
  "black"
  )

(define-neume syneches-elaphron-petaste
  (list "syneches-elaphron-petaste")
  "BZ Byzantina"
  "H"
  #f
  "black"
  )

(define-neume elaphron-apostrophos
  (list "elaphron-apostrophos" "-3")
  "BZ Byzantina"
  "l"
  #f
  "black"
  )

(define-neume elaphron-apostrophos-petaste
  (list "elaphron-apostrophos-petaste" "-3f")
  "BZ Byzantina"
  "L"
  #f
  "black"
  )

(define-neume kamele
  (list "kamele" "-4")
  "BZ Byzantina"
  ";"
  #f
  "black"
  )

(define-neume kamele-petaste
  (list "kamele" "-4f")
  "BZ Loipa"
  "x"
  #f
  "black"
  )

(define-neume kamele-apostrophos
  (list "kamele-apostrophos" "-5")
  "BZ Loipa"
  "x"
  #f
  "black"
  )

(define-neume kamele-apostrophos-petaste
  (list "kamele-apostrophos-petaste" "-5f")
  "BZ Loipa"
  "X"
  #f
  "black"
  )

(define-neume kamele-elaphron
  (list "kamele-elaphron" "-6")
  "BZ Loipa"
  "c"
  #f
  "black"
  )

(define-neume kamele-elaphron-petaste
  (list "kamele-elaphron" "-6f")
  "BZ Loipa"
  "C"
  #f
  "black"
  )

(define-neume kamele-elaphron-apostrophos
  (list "kamele-elaphron-apostrophos" "-7")
  "BZ Loipa"
  "v"
  #f
  "black"
  )

(define-neume kamele-elaphron-apostrophos-petaste
  (list "kamele-elaphron-apostrophos-petaste" "-7")
  "BZ Loipa"
  "V"
  #f
  "black"
  )

(define-neume kamele-double
  (list "kamele-double" "-8")
  "BZ Loipa"
  "b"
  #f
  "black"
  )

(define-neume kamele-double-petaste
  (list "kamele-double-petaste" "-8f")
  "BZ Loipa"
  "B"
  #f
  "black"
  )

(define-neume ypporoe
  (list "ypporoe" "yp")
  "BZ Byzantina"
  "'"
  #f
  "black"
  )

(define-neume ypporoe-gorgon
  (list "ypporoe-gorgon" "yp-gorgon")
  "BZ Byzantina"
  ":"
  #f
  "black"
  )

(define-neume ypporoe-petaste
  (list "ypporoe-petaste" "yp-f")
  "BZ Byzantina"
  "\""
  #f
  "black"
  )

; Time --------------------

(define-neume klasma
  (list "klasma" "k")
  "BZ Byzantina"
  "u"
  #t
  "black"
  )

(define-neume klasma-right
  (list "klasma-right" "kr")
  "BZ Byzantina"
  "i"
  #t
  "black"
  )

(define-neume klasma-left
  (list "klasma-left" "kl")
  "BZ Byzantina"
  "U"
  #t
  "black"
  )

(define-neume klasma-below
  (list "klasma-below" "kb")
  "BZ Byzantina"
  "I"
  #t
  "black"
  )

(define-neume klasma-below-right
  (list "klasma-below-right" "kbr")
  "BZ Loipa"
  "z"
  #t
  "black"
  )

(define-neume aple
  (list "aple")
  "BZ Byzantina"
  "8"
  #t
  "black"
  )

(define-neume aple-right
  (list "aple-right")
  "BZ Byzantina"
  "*"
  #t
  "black"
  )

(define-neume diple
  (list "diple")
  "BZ Byzantina"
  "9"
  #t
  "black"
  )

(define-neume diple-right
  (list "diple-right")
  "BZ Byzantina"
  "("
  #t
  "black"
  )

(define-neume triple
  (list "triple")
  "BZ Byzantina"
  "0"
  #t
  "black"
  )

(define-neume triple-right
  (list "triple-right")
  "BZ Byzantina"
  ")"
  #t
  "black"
  )


(define-neume gorgon
  (list "gorgon" "g")
  "BZ Byzantina"
  "e"
  #t
  "red"
  )

(define-neume gorgon-right
  (list "gorgon-right" "g-r")
  "BZ Byzantina"
  "r"
  #t
  "red"
  )

(define-neume gorgon-below
  (list "gorgon-below" "g")
  "BZ Byzantina"
  "E"
  #t
  "red"
  )

(define-neume gorgon-below-right
  (list "gorgon-below-right" "g")
  "BZ Byzantina"
  "R"
  #t
  "red"
  )

(define-neume digorgon
  (list "digorgon")
  "BZ Palaia"
  "t"
  #t
  "red"
  )

(define-neume digorgon-right
  (list "digorgon-right")
  "BZ Palaia"
  "T"
  #t
  "red"
  )

(define-neume trigorgon
  (list "trigorgon")
  "BZ Palaia"
  "y"
  #t
  "red"
  )

(define-neume trigorgon-right
  (list "trigorgon-right")
  "BZ Palaia"
  "Y"
  #t
  "red"
  )

(define-neume argon
  (list "argon")
  "BZ Byzantina"
  "w"
  #t
  "red"
  )


; Martyria ---------------------

(define-neume martyria-vou
  (list "martyria-vou" "mv")
  "BZ Byzantina"
  "7&"
  #f
  "red"
  )