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
(define TEXT-FONT "EZ Omega")
(define TEXT-SIZE 18)
(define TEXT-COLOR "black")

; FILLER is used in a number of places to prepend a blank image to functions that return an image.
; This is necessary because functions such as "beside" are used, and they expect to receive at least 2 arguments
; FILLER guarantees that there are at least two arguments. There's probably a better way to do this. :)
(define FILLER (square 0 "solid" "white"))

(define HYPHEN (text/font " -" TEXT-SIZE TEXT-COLOR TEXT-FONT 'modern 'normal 'normal #f))

; STRUCTS -----------------------

; phrase
; defines a syllable and accompanying notes
(struct phrase
  (text                   ; string: the word to be displayed
   notes)                 ; a list of neumes
  #:transparent)

; MACROS -------------------------

(define-syntax-rule (chant [word (notes ...)] ...)
  (list [phrase word (list notes ...)] ...))

; FUNCTION DEFINITIONS ---------------------

; render : list of phrases -> image
(define (render chant-list)
  (apply beside (cons FILLER (map render-phrase chant-list))))

; render-phrase : phrase -> image
(define (render-phrase a-phrase)
  (let ([notes (apply beside (cons FILLER (map render-neume (phrase-notes a-phrase))))])
  ;(above/align (left-or-center (phrase-notes a-phrase)) <-- See notes for left-or-center below
  (above/align "center"
               notes
               (render-text (phrase-text a-phrase) (image-width notes)))))

; render-text : string integer -> image
(define (render-text a-phrase neumes-width)
  (let ([text (text/font a-phrase TEXT-SIZE TEXT-COLOR TEXT-FONT 'modern 'normal 'normal #f)])
  (if (> neumes-width (+ (image-width (render-neume ison)) (image-width text)))
      (hyphenate text neumes-width)
      text)))

; hyphenate : image integer -> image
; receives rendered text and an int that is the image width of the neumes above it
(define (hyphenate text-image neumes-width)
  (if (>= (+ (image-width HYPHEN) (image-width text-image)) neumes-width) text-image
    (hyphenate (beside text-image HYPHEN) neumes-width)))

; left-or-center : phrase-notes -> string
; Possibly not needed anymore now that auto-hyphenation is in.
; Now, things seem to look best when always centered.
(define (left-or-center some-phrase-notes)
  (if (> (number-nonmodifier-neumes some-phrase-notes) 2) "left"
      "center"))

; number-nonmodifier-neumes : phrase-notes -> integer
(define (number-nonmodifier-neumes some-phrase-notes)
  (- (length some-phrase-notes) (length (filter neume-modifier? some-phrase-notes))))

; render-neume : neume -> image
; renders a single neume
(define (render-neume a-neume)
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
    (if (neume-modifier? value)
        (render (list (phrase (first (neume-aliases value)) (list ison value))))
        (render (list (phrase (first (neume-aliases value)) (list value)))))))

; chant-page : list-of-chant -> image
; Used for rendering multiple lines of chant
(define (chant-page list-of-chant)
  (apply above/align "left" (cons FILLER (map render list-of-chant))))

; --- HOW TO USE -------------------------------------

(define test-chant
  (chant
   ["Lord," (oligon+kentema-side klasma-left)]
   ["have" (ison ypporoe+gorgon)]
   ["mer" (oligon oligon+kentemata-below gorgon elaphron)]
   ["cy" (apostrophos klasma-right)]
   ["" (martyria-ni)]
  )
)

; Run the following to render one line of chant:
; (render test-chant)

; Run the following to render multiple lines of chant as one image:
; (chant-page (list test-chant test-chant test-chant))