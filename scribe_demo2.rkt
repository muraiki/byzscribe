#lang racket
(require "byzscribe.rkt")

(define line1
  (chant
   ["" (martyria-ni)]
   ["Lord," (oligon+kentema-below apostrophos)]
   ["I" (oligon)]
   ["have" (oligon)]
   ["cried" _ (oligon apostrophos oligon+kentemata-below gorgon apostrophos apostrophos gorgon-right)]
   ["un" - (apostrophos klasma-right oligon kentemata)]
   ["to" _ (ison klasma ypporoe+gorgon oligon+kentemata-below gorgon kentemata gorgon-below-right)]
   ["Thee" (ison apostrophos diple-right)]
   ["hear" _ (kamele+petaste klasma-below-right apostrophos kentemata)]
   ["me" (ison diple)]
   ["" (martyria-ni)]
  )
)

; TODO: Bug causing "hear" to render lower than the rest of the text
(render line1)