;;;; PART 2

(load "streams.scm")

;; Q1(b)
(define (number->list n)
	(if (number? n)
		(map
			(lambda (x) (- (char->integer x) 48))
			(string->list (number->string n))
		)
	)
)

(define (list->stream l)
	(if (null? l)
		the-empty-stream
		(cons-stream
			(car l)
			(list->stream (cdr l))
		)
	)
)

(define (mult-stream m strm)
	(define (prepend x y)
		(cond
			(
				(<= (length x) (length y))
				(prepend (append (list 0) x) y)
			)
			(else x)
		)
	)

	(define (action a a-list s)
		(define pow (expt 10 (- (length a-list) 1)))
		(cond
			(
				(stream-null? s)
				(list->stream a-list)
			)
			; produce
			(
				(and
					(not (null? a-list))
					(< (+ m (modulo a pow)) pow)
				)
				(cons-stream
					(car a-list)
					(action
						(modulo a pow)
						(cdr a-list)
						s
					)
				)
			)
			; consume
			(else
				(let
					((x (+ (* a 10) (* m (stream-car s)))))
					(action
						x
						(prepend (number->list x) a-list)
						(stream-cdr s)
					)
				)
			)
		)
	)

	(if (list? strm)
		(action 0 '() (list->stream strm))
		(action 0 '() strm)
	)
)

; (define l '(9 8 7 4 3 6 9 1 7))
; (display-n (list->stream l) 15)
; (display-n (mult-stream 87 l) 15)
; (display-n (mult-stream 2 integers) 15)


;; Q3
(define (make-matrix a b c d) (list a b c d))

(define (m-a m) (list-ref m 0))
(define (m-b m) (list-ref m 1))
(define (m-c m) (list-ref m 2))
(define (m-d m) (list-ref m 3))

(define (m-add x y) (map + x y))

(define (m-multiply x y)
	(make-matrix
		(+
			(* (m-a x) (m-a y))
			(* (m-b x) (m-c y))
		)
		(+
			(* (m-a x) (m-b y))
			(* (m-b x) (m-d y))
		)
		(+
			(* (m-c x) (m-a y))
			(* (m-d x) (m-c y))
		)
		(+
			(* (m-c x) (m-b y))
			(* (m-d x) (m-d y))
		)
	)
)

(define (m-apply m n)
	(quotient
		(+
			(* (m-a m) n)
			(m-b m)
		)
		(+
			(* (m-c m) n)
			(m-d m)
		)
	)
)

(define (pi)
	(define (action a out)
		(let
			(
				(apply-3 (m-apply a 3))
				(apply-4 (m-apply a 4))
			)
			(if (= apply-3 apply-4)
				(cons-stream
					apply-3
					(action
						(m-multiply
							(make-matrix
								10
								(* -10 apply-3)
								0
								1
							)
							a
						)
						out
					)
				)
				(action
					(m-multiply a (stream-car out))
					(stream-cdr out)
				)
			)
		)
	)

	(define (in)
		(define (f k)
			(cons-stream
				(make-matrix
					k
					(+ (* 4 k) 2)
					0
					(+ (* 2 k) 1)
				)
				(f (+ k 1))
			)
		)
		(f 1)
	)

	(action
		(stream-car (in))
		(stream-cdr (in))
	)
)

; (display-n (pi) 15)