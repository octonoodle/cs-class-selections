#lang pl

#|
The grammar:
  <TIDBIT> ::= <num>
            | { + <TIDBIT> <TIDBIT> }
            | { - <TIDBIT> <TIDBIT> }
            | { * <TIDBIT> <TIDBIT> }
            | { / <TIDBIT> <TIDBIT> }
            | { with { <id> <TIDBIT> } <TIDBIT> }
            | <id>
            | { fun { <id> ... ... } <TIDBIT> }
            | { call <TIDBIT> <TIDBIT> ... }
|#

(define-type TIDBIT
  [Num  Number]
  [Add  TIDBIT TIDBIT]
  [Sub  TIDBIT TIDBIT]
  [Mul  TIDBIT TIDBIT]
  [Div  TIDBIT TIDBIT]
  [Id   Symbol]
  [With Symbol TIDBIT TIDBIT]
  [Fun  Symbol (Listof Symbol) TIDBIT]
  [Call TIDBIT (Listof TIDBIT)]) ; fun args

;; Exercise III
(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  ;; Exercise IV
  [CIdx  Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> TIDBIT)
;; parses s-expressions into TIDBITs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (cons (symbol: arg1) (list (symbol: args) ...)) body)
        (Fun arg1 args (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun args ...)
     (Call (parse-sexpr fun) (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> TIDBIT)
;; parses a string containing a TIDBIT expression to a TIDBIT AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, xxxand a lookup functionxxx
;; Exercise V[I]

(define-type ENV = (Listof VAL))

(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

;; Exercise VII
(define-type BINDING-DEPTH = (Symbol -> Natural))
(: empty-depth : BINDING-DEPTH)
(define empty-depth (λ (symbol) (error symbol "binding layer not found! (empty depth)")))
(: add-binding-to : (Symbol BINDING-DEPTH -> BINDING-DEPTH))
(define (add-binding-to new binder)
  (λ (symb) (if (symbol=? new symb) 0 (add1 (binder symb)))))
(: JUSTONE : BINDING-DEPTH)
(define JUSTONE (λ (symb) (if (symbol=? symb 'foo) 0 (empty-depth symb))))
(test (JUSTONE 'foo) => 0)
(test (JUSTONE 'fbr) =error> "fbr: binding layer not found! (empty depth)")
(define ANOTHER (add-binding-to 'bar JUSTONE))
(test (ANOTHER 'foo) => 1)
(test (ANOTHER 'bar) => 0)
(test (ANOTHER 'fbr) =error> "fbr: binding layer not found! (empty depth)")

;; Exercise VIII
(: preprocess : (TIDBIT BINDING-DEPTH -> CORE))
;; convert a TidBIT into CORE representation
(define (preprocess source binder)
  (cases source
    [(Num  n) (CNum n)]
    [(Add a b) (CAdd (preprocess a binder) (preprocess b binder))]
    [(Sub a b) (CSub (preprocess a binder) (preprocess b binder))]
    [(Mul a b) (CMul (preprocess a binder) (preprocess b binder))]
    [(Div a b) (CDiv (preprocess a binder) (preprocess b binder))]
    [(Id symb) (CIdx (binder symb))]
    [(With symb substitution bound-body)
     (CCall (CFun (preprocess bound-body (add-binding-to symb binder))) (preprocess substitution binder))]
    [(Fun arg1 args body)
     (curry source binder)
     #;(if (null? args)
           (CFun (preprocess body (add-binding-to arg1 binder)))
           (CCall (CFun (preprocess (Fun arg1 (rest args) body) (add-binding-to (first args)))) (preprocess (first args) binder)))]
    [(Call a b) #;(CCall (preprocess a binder) (preprocess b binder))
                (decurry source binder)]))

#|
(λ (a b c) (... a b c))
((λ (a) (λ (b c) (... a b c))) a)


|#

(: curry : (TIDBIT BINDING-DEPTH -> CORE))
;; turn a multi-argument function into a curried single-arg function (must be decurried to actually use it)
(define (curry source binder)
  (cases source
    [(Fun arg1 args body)
     (if (null? args)
         (CFun (preprocess body (add-binding-to arg1 binder)))
         (CFun (curry (Fun arg1 (rest args) body) (add-binding-to (first args) binder))))]
    [else (error 'curry "cannot curry a non-function")]))
(test (curry (Fun 'a '() (Id 'a)) empty-depth) => (CFun (CIdx 0)))
(test (curry (Fun 'a '(b c) (Add (Id 'a) (Mul (Id 'b) (Id 'c)))) empty-depth) =>
      #;(fun 'b '() (fun 'c '() (fun 'a '() (Add (Id 'a) (Mul (Id 'b) (Id 'c))))))
      (CFun (CFun (CFun (CAdd (CIdx 0) (CMul (CIdx 2) (CIdx 1)))))))


(: decurry : (TIDBIT BINDING-DEPTH -> CORE))
;; call a function on muliple arguments assuming the input function has been curried already
(define (decurry source binder)
  (cases source
    [(Call func all-args)
     (match all-args
       [(cons arg1 (cons arg2 more)) (CCall (decurry (Call func (cons arg1 more)) binder) (preprocess arg2 binder))]
       [(cons arg1 '()) (CCall (preprocess func binder) (preprocess arg1 binder))]
       [else (error 'decurry "argument number mismatch")])]
    [else (error 'decurry "can only decurry a function call")]))
(test (decurry (Call (Fun 'a '() (Id 'a)) (list (Num 3))) empty-depth) => (CCall (CFun (CIdx 0)) (CNum 3)))
(test (decurry (Call (Fun 'a '(b c) (Add (Id 'a) (Mul (Id 'b) (Id 'c)))) (list (Num 2) (Num 4) (Num 6))) empty-depth) =>
      #;(Call (Call (Call <curried> (Num 2)) (Num 6)) (Num 4))
      (CCall (CCall (CCall (curry (Fun 'a '(b c) (Add (Id 'a) (Mul (Id 'b) (Id 'c)))) empty-depth) (CNum 2)) (CNum 6)) (CNum 4)))



(: NumV->number : VAL -> Number)
;; convert a TIDBIT runtime numeric value to a Racket one
(define (NumV->number val)
  (cases val
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" val)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : CORE ENV -> VAL)
;; evaluates TIDBIT expressions by reducing them to values. Exercise VI
(define (eval expr env)
  (: add-new-binding-to-the-environment-because-typing-errors : (VAL ENV -> ENV))
  (define (add-new-binding-to-the-environment-because-typing-errors thing things) (cons thing things))
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    #;[(CWith named-expr bound-body)
       (eval bound-body
             (add-new-binding-to-the-environment-because-typing-errors (eval named-expr env) env))]
    [(CIdx id) (list-ref env id)]
    [(CFun bound-body)
     (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body
                (cons (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]))

(: run : String -> Number)
;; evaluate a TIDBIT program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) empty-depth) '())])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{with {f {with {x 3} {fun {y} {+ x y}}}}
              {with {x 100}
                {call f 4}}}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)

;; Exercise II
(test (run "{+ x y}") =error> "x: binding layer not found! (empty depth)")
(test (run "{call 38 49}") =error> "eval: `call' expects a function, got: (NumV 38)")
(test (run "{fun}") =error> "parse-sexpr: bad `fun' syntax in (fun)")
(test (run "{foobar bing bong}") =error> "parse-sexpr: bad syntax in (foobar bing bong)")
(test (run "{with {x y z} something}") =error> "parse-sexpr: bad `with' syntax in (with (x y z) something)")

;; Exercise XI
;; test multi-arg / currying
(test (run "{call {fun {x y} {+ x y}} 2 3}") => 5)
(test (run "{call {fun {w x y z} {+ {* x z} {* w y}}} 2 3 10 7}") => 41)
(test (run "{call {with {add1sum {fun {x y} {+ {+ x y} 1}}} {fun {w x y z} {* {call add1sum w x} {call add1sum y z}}}} 2 3 4 5}") => 60)
(test (run "{call {call {fun {x} {fun {y} {/ x y}}} 5} 6}") => 5/6) ; just because.