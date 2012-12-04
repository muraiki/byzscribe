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

(provide chant
         phrase
         print-all-neumes
         list-all-neumes
         render-neume
         render
         chant-page
         (all-from-out "neumes.rkt"))

(require 2htdp/image)
(require "neumes.rkt")

(define NEUME-SIZE 28)
(define TEXT-FONT "EZ Omega")
(define TEXT-SIZE 18)
(define TEXT-COLOR "black")

; FILLER is used in a number of places to prepend a blank image to functions that return an image.
; This is necessary because functions such as "beside" are used, and they expect to receive at least 2 arguments
; There's probably a better way to do this, but for now this avoids a bunch of ifs and/or additional function declarations. :)
(define FILLER (square 0 "solid" "white"))

; Defines for the hyphenator
(define HYPHEN (text/font "  - " TEXT-SIZE TEXT-COLOR TEXT-FONT 'modern 'normal 'normal #f))
(define UNDERSCORE (text/font "_" TEXT-SIZE TEXT-COLOR TEXT-FONT 'modern 'normal 'normal #f))

; STRUCTS -----------------------

; phrase
; defines a syllable and accompanying notes
(struct phrase
  (text                   ; string: the word to be displayed
   notes)                 ; a list of neumes
  #:transparent)          ; lets us see into the struct, helpful for debugging

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
  (above/align "center"
               notes
               (render-hyphenate-text (phrase-text a-phrase) (image-width notes)))))

; render-hyphenate-text : string integer -> image
(define (render-hyphenate-text a-text neumes-width)
  (match a-text
    [(regexp #rx"--$") (hyphenate
                        (render-text (remove-last-two a-text))
                         neumes-width HYPHEN)]
    [(regexp #rx"__$") (hyphenate
                        (render-text (remove-last-two a-text))
                         neumes-width UNDERSCORE)]
    [else (render-text a-text)]))

; remove-last-two : string -> string
; Strips off the last two characters of a string. Used for cleaning up hyphenated text before rendering
; Example: "text__" -> "text"
(define (remove-last-two a-text)
  (substring a-text 0 (- (string-length a-text) 2)))

; hyphenate : image integer -> image
; receives rendered text and an int that is the image width of the neumes above it
(define (hyphenate text-image neumes-width hyphenate-symbol)
  (if (>= (+ (image-width hyphenate-symbol) (image-width text-image)) neumes-width) text-image
    (hyphenate (beside text-image hyphenate-symbol) neumes-width hyphenate-symbol)))

; render-neume : neume -> image
; renders a single neume
(define (render-neume a-neume)
  (text/font (neume-character-code a-neume) NEUME-SIZE (neume-color a-neume) (neume-font a-neume) 'modern 'normal 'normal #f))

; render-text : string -> image
(define (render-text a-text)
  (text/font a-text TEXT-SIZE TEXT-COLOR TEXT-FONT 'modern 'normal 'normal #f))

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
