(module fn
		(main main))


;;----------------------------------------------------
;
;; (define (extract-remainder-from-parser p)
;;   (if (null? p)
;; 	("")
;; 	((car (car p)))))
;; 
;; (define (extract-result-from-parser p)
;;   (if (null? p)
;; 	("")
;; 	((cdr (car p)))))
;
;; (define (pure a) 
;;   (lambda (s) `( (,s . ,a) ) ))
;; 
;; 
;; ; (<*>) :: Parser (a -> b)       -> Parser a        -> Parser b
;; ;       :: \s -> [(s, (a -> b)]  -> \s -> [(s, a)]  -> \s -> [(s, b)]
;; ; f <*> a = \s -> [(s1, g b) | (s0, g) <- f s, (s1, b) <- a s0]
;; 
;; (define (<*> f a)
;;   (lambda (s)
;; 	(let* ((p0 (f s))
;; 		  (g  (extract-result-from-parser p0))
;; 		  (s0 (extract-remainder-from-parser p0))
;; 		  (p1 (a s0))
;; 		  (s1 (extract-remainder-from-parser p1))
;; 		  (b  (extract-result-from-parser p1)))
;; 	  `((,s1 . (,g ,b))))))
;; 
;; ; (<|>) :: Parser a -> Parser a -> Parser a
;; ; a <|> b = \s -> take 1 (a s ++ b s)
;; 
;; (define (<?> a b)
;;   (lambda (s) 
;; 	(take (append (a s) (b s)) 1)))
;; 
;; (define (?> a b)
;;   (pure (<*> (<*> (lambda (x y) (y)) a) b)))
;; 
;; (define (<? a b)
;;   (pure (<*> (<*> (lambda (x y) (x)) a) b)))
;
;
;;----------------------------------------------------
;
;; (define reserved-chars "\t\n\r\0()[]\"\'")
;; (define reserved-prefixes (string-append "'.0123456789" reserved-chars))
;
;; (define fn-grammar
;;   (regular-grammar ((number (+ digit))
;; 					(quotation (: "[" "]"))
;; 					(str ())
;; 					(word (: (out reserved-prefixes) (* (out reserved-chars))))
;
;(define (tag-word type word)
;  `(,type . ,word))
;
;(define rpn-calculator-grammar
;	(regular-grammar ((number (+ digit))
;					  (operator (in "+-*/")))
;				     (number      (tag-word 'number (the-fixnum)) )
;					 (operator    (tag-word 'word   (the-string)) )
;					 ((+ blank)   (ignore))
;				     (else (if (eof-object? (the-string)) (the-string) (quit)))))
;
;(define (make-string-parser str)
;  (let ((port (open-input-string str)))
;    (lambda () (read/rp rpn-calculator-grammar port))))
;
;;(define parse
;;;  (lambda (str) ())
;;  (lambda () (read/rp rpn-calculator-grammar (current-input-port))))
;
;
;(define (main argv)
;  (let ((p (make-string-parser "1 2 +")))
;	(let loop ((found (p)))
;	  (if (eof-object? found)
;		(exit)
;		((print found)
;		 (newline)
;		 (loop (p)))
;		))))

;(define forth-grammar
;	(regular-grammar ((word (+ (out blank)))
;					  (number (+ digit)))
;					 (number (cons 'int (the-fixnum)))
;					 ("[]" '())
;					 ;("]" '())
;					 ;("[" (let* ((head (ignore))
;								 ;(tail (ignore)))
;							;(cons 'quo (cons head tail))))
;					 (word (cons 'sym (the-string)))
;					 ((+ blank) (ignore))
;					 (else (the-failure))))
;
; (define (parse port)
  ; (let ((p (lambda () (read/rp forth-grammar port))))
	; (let loop ((found (p)))
	  ; ;(print found)
	  ; (if (eof-object? found)
		; '()
		; (cons found (loop (p))) ))))

;(define (comp n)
;  (let loop ((i n))
;	(if (= i 0)
;	  '()
;;	  (cons i (loop (- i 1))))))
;
;
;(define (parser port)
;  (lambda () (read/rp forth-grammar port)))
;
;(define (parse p)
;  (let ((word (p)))
;	(if (eof-object? word)
;	  '()
;	  (cons word (parse p)))))
;
;(define (parse-string str)
;  (let ((port (open-input-string str)))
;	(parse (parser port))))
;
;; (label . defn)
;
;(define prim 
;  '(("[" . start-list)
;	("]" . end-list)
;	))
;
;(define (lookup sym)
;  (if (= sym '(sym . paired))
;	'cons
;	sym))
;
;
;(define (main argv)
;  (print (parse-string "[] 1 paired 2 paired 3 paired 4 paired 5 paired num defined ; [] var paired + paired addNum defined ; 3 addVar print")))
;
; [ 1 ... ] comp head head -> 1 2
; [ 1 .. 10 ] comp 

; [ 1 ... ] head -> [ 2 ... ] 1
; 


;(define (pure a)
  ;(lambda (s) (cons s a)))
;
;(define (p-or a b)
  ;(lambda (s) (take (append (a s) (b s)) 1)))
;
;(define (p-tr f a)
  ;(lambda (s) (




(define (eval-funeral word)
  (if (eq? word #eof-object)
	(exit)
	(push word)))

(define (repl)
  (eval-funeral (read))
  (repl))



(define (main argv)
  (repl))




