#lang scheme

;;; MMN 11 ;;;

(define my_append
  (lambda (list1 list2)
    (if (null? list1)
         list2
         (cons (car list1) (my_append (cdr list1) list2)))))
         

(define my_append_fr
  (lambda (list1 list2)
    (foldr cons list2 list1)))


(define filter
  (lambda (pred list)
    (foldr (lambda (item acc) (if (pred item) (cons item acc) acc)) '() list)))


;(define powerset
     

;;; MMN 12 ;;;

#|

 constructors: 
  zero: () -> P
  make-poly: Int x Int -> P
  add-poly: P x P -> P
 
 observers:
  degree: P -> Int
  coeff: P x Int -> Int
  print-poly: P -> List
  calc-poly: P x Int -> Int

 predicates:
  is-zero?: P -> Bool
  
|#

#|
 
 (zero

|#







