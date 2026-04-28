(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")
  (require (only-in racket/base foldl))

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

  ;;helpers
  (define sum-lists
    (lambda (list1 list2) (map + list1 list2)))

  (define extend-env-multi
    (lambda (ids vals env)
      (foldl (lambda (id val curr-env)
                (extend-env id val curr-env))
              env
              ids
              vals)))

  (define (do-loop ids steps bools results orig-bools orig-results env)
    (if (null? bools)
        (let ([new-vals (map (lambda (id step)
                       (num-val (+ (expval->num (apply-env env id))
                                   (expval->num (value-of step env)))))
                     ids
                     steps)])
          (let ([updated-env (extend-env-multi ids new-vals env)])
            (do-loop ids steps orig-bools orig-results orig-bools orig-results updated-env)))
        (if (expval->bool (value-of (car bools) env))
            (value-of (car results) env)
            (do-loop ids steps (cdr bools) (cdr results) orig-bools orig-results env))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        

        
        ;;added do-exp extention 
        (do-exp (ids inits steps bools results)
          (cond 
            [(null? ids) (eopl:error "do loop without variables is forbidden")]
            [(null? bools) (eopl:error "do loop without booleans is forbidden")]
            [else  (let ([updated-env (foldl (lambda (id init curr-env)
                                          (extend-env id (value-of init curr-env) curr-env))
                                        env
                                        ids
                                        inits)])
                (do-loop ids steps bools results bools results updated-env))]))

      )))


  )

