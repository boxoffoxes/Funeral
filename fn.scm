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
	
(define forth-grammar
	(regular-grammar ()
					 ((+ (out blank)) (the-string))
					 ((+ blank) (ignore))
					 (else (quit))))


(define (main argv)
;  (let ((port (open-input-string "hello world")))
  (let ((port (current-input-port)))
	(let ((p    (lambda () (read/rp forth-grammar port))))
	  (let loop ((found (p)))
	    (if (eof-object? found)
	  	  (exit)
		  ((print found)
		  (loop (p)
				)))))))



