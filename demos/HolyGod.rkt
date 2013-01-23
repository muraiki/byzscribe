#lang racket
(require "../byzscribe.rkt")

(define line1 
  (chant 
   ["" (siopein)]
   ["A" (elaphron+apostrophos)]
   ["men." (oligon+kentema klasma-below)]
   ["" (martyria-thi)]
   ["" (vareia)]
   ["Ho" - (oligon+kentema-below apostrophos digorgon-right+peristagmeni-left oligon+kentemata-below apostrophos+petaste
                 apostrophos apostrophos gorgon-right ison oligon+kentema-below apostrophos ypporoe+gorgon+peristagmeni-left
                 oligon klasma-left)]
   ["ly" _ (oligon kentemata)]
   ["" (vareia)]
   ["Go" - (oligon ypporoe+digorgon+peristagmeni-left oligon+kentemata apostrophos)]
   ["d" (apostrophos)]
   )
  )

(render line1)