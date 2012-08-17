(module fn
		(main main))


(define (extract-remainder-from-parser p)
  (if (null? p)
	("")
	((car (car p)))))

(define (extract-result-from-parser p)
  (if (null? p)
	("")
	((cdr (car p)))))

;----------------------------------------------------

(define (pure a) 
  (lambda (s) `( (,s . ,a) ) ))


; (<*>) :: Parser (a -> b)       -> Parser a        -> Parser b
;       :: \s -> [(s, (a -> b)]  -> \s -> [(s, a)]  -> \s -> [(s, b)]
; f <*> a = \s -> [(s1, g b) | (s0, g) <- f s, (s1, b) <- a s0]

(define (<*> f a)
  (lambda (s)
	(let* ((p0 (f s))
		  (g  (extract-result-from-parser p0))
		  (s0 (extract-remainder-from-parser p0))
		  (p1 (a s0))
		  (s1 (extract-remainder-from-parser p1))
		  (b  (extract-result-from-parser p1)))
	  `((,s1 . (,g ,b))))))

; (<|>) :: Parser a -> Parser a -> Parser a
; a <|> b = \s -> take 1 (a s ++ b s)

(define (<?> a b)
  (lambda (s) 
	(take (append (a s) (b s)) 1)))

(define (?> a b)
  (pure (<*> (<*> (lambda (x y) (y)) a) b)))

(define (<? a b)
  (pure (<*> (<*> (lambda (x y) (x)) a) b)))



;----------------------------------------------------

(define rpn-calculator-grammar
  (regular-grammar ((number (+ digit))
					(operator (in "+-*/")))
				   (number   (cons 'number   (the-string)))
				   (operator (cons 'operator (the-string)))
				   (else     '()) ))


(define parser 
  (regular-grammar () 
				   ((+ (out blank)) (the-string) ) ))


(define parse
;  (lambda (str) ())
  (lambda () (read/rp rpn-calculator-grammar (current-input-port))))


(define (main argv)
	(print (with-input-from-string "+ 2 3" parse))
	(print (with-input-from-string "1 2 3" parse)))
