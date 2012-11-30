#lang racket

; Byzscribe: A program for rendering plaintext byzantine chant code
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

(require 2htdp/image)
(require "neumes.rkt")

(define NEUME-SIZE 28)
(define TEXT-SIZE 18)
(define TEXT-FONT "EZ Omega")

(define FILLER (square 0 "solid" "white"))

; STRUCTS -----------------------

; phrase
; defines a syllable and accompanying notes
(struct phrase
  (text                   ; string: the word to be displayed
   notes)                 ; a list (neumes) of lists of notes (symbol-groupings)
  #:transparent)

; MACROS -------------------------

(define-syntax-rule (chant [word (notes ...)] ...)
  (list [phrase word (list notes ...)] ...))

; FUNCTION DEFINITIONS ---------------------

; render : list of phrases -> image
(define (render chant-list)
  ; beside expects two arguments. add an empty filler in case there's only one phrase.
  (apply beside (cons FILLER (map render-phrase chant-list))))

; render-phrase : phrase -> image
(define (render-phrase a-phrase)
  (above/align "left"
               ; beside expects two arguments. add an empty filler in case there's only one composite neume
               (apply beside (cons FILLER (map render-composite-neume (phrase-notes a-phrase))))
               (text/font (phrase-text a-phrase) TEXT-SIZE "black" TEXT-FONT 'modern 'normal 'normal #f)))

; render-composite-neumes : list of composite neumes -> image
; receives: (list (list oligon+kentema-side klasma-left) (list ison))
(define (render-composite-neume composite-neumes)
  ; TODO: modifier neumes not aligned properly, but at least are printing without needing a dummy neume
  ; overlay expects two arguments. add an empty FILLER in case there's only one neume to composite
  (apply overlay (cons FILLER (map render-neume composite-neumes))))
  
; render-neume : neume -> image
; renders a single neume
(define (render-neume a-neume)
  ; TODO: How to handle modifier neumes? currently render, but are offset
  (if (neume-modifier? a-neume) (render-modifier-neume a-neume)      
  (text/font (neume-character-code a-neume) NEUME-SIZE (neume-color a-neume) (neume-font a-neume) 'modern 'normal 'normal #f)))

; render-modifier-neume : neume -> image
; Currently doesn't do anything differently from render-neume
(define (render-modifier-neume a-neume)
          (text/font (neume-character-code a-neume) NEUME-SIZE (neume-color a-neume) (neume-font a-neume) 'modern 'normal 'normal #f))

; print-all-neumes : hash -> list-of-images
; call using default hash with (print-all-neumes neume-names)
(define (print-all-neumes neume-hash)
  (apply above/align "left" (list-all-neumes neume-hash)))

; list-all-neumes: hash -> list-of-images
; given a names hash generate a list of images of every neume and its name
; will insert an ison for neumes that modify a preceeding neume (otherwise it will not print)
; Because it is a hash, it is unordered output. In the future, it'd be nice to have a better function for this
; that has a more ordered output for documentation, but this will suffice for now.
(define (list-all-neumes neume-hash)
  (for/list ([(key value) neume-hash])
    (render (list (phrase (first (neume-aliases value)) (list (list value)))))))

; chant-page : list-of-chant -> image
; Used for rendering multiple lines of chant
; TODO: convert to using higher order function instead of my manual recursion
(define (chant-page list-of-chant)
  (cond
    [(empty? list-of-chant) (square 0 "solid" "white")]
    [else
     (above/align "left" (render (first list-of-chant)) (chant-page (rest list-of-chant)))]))

; -------------------------------------

; TEST - note that the chant macro is currently broken for the new format
; chant should be in the following format:
; (list (phrase "TEXT" (list (list A-NEUME A-NEUME) (list (ANOTHER-NEUME)))))
;     neumes per phrase^     ^multiple neumes to composite    ^one neume by itself, to follow the first composite neume
(render (list (phrase "Lord" (list (list oligon+kentema-side klasma-left) (list ison) (list ypporoe-gorgon) (list oligon) (list oligon+kentemata-below gorgon) (list elaphron) (list apostrophos klasma)))))
