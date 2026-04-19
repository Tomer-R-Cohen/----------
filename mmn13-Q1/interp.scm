(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (apply-env env var))

        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
        
        ;; add support for auto cast in condition
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (num-val (n) (if (zero? n)
                              (value-of exp3 env)
                              (value-of exp2 env)))
              (bool-val (b) (if #t 
                              (value-of exp2 env)
                              (value-of exp3 env))))))

        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        ;;(value-of (cast-exp typ exp) p = )
        (cast-exp (typ exp)
          (let ((val (value-of exp env)))
            (cases type typ
              (int-type () 
                (cases expval val
                  (num-val (n) (num-val n))
                  (bool-val (b) (if b (num-val 1) (num-val 0)))))
              (bool-type ()
                (cases expval val
                  (num-val (n) (if (zero? n) (bool-val #t) (bool-val #f))) 
                  (bool-val (b) b))))))
        )))

        


  )

