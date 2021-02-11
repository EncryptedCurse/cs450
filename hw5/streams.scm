;;;; PART 1

(define-syntax cons-stream
	(syntax-rules
		()
		((_ a b) (cons a (delay b)))
	)
)

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
	(if (stream-null? x)
		'done
		(begin
			(f (stream-car x))
			(stream-foreach f (stream-cdr x))
		)
	)
)

(define (stream-filter pred stream)
	(cond
		(
			(stream-null? stream)
			the-empty-stream
		)
		(
			(pred (stream-car stream))
			(cons-stream
				(stream-car stream)
				(stream-filter pred (stream-cdr stream))
			)
		)
		(else
			(stream-filter pred (stream-cdr stream))
		)
	)
)


;; Q1
(define (display-n stream n)
	(cond
		(
			(and
				(> n 0)
				(list? stream)
				(not (null? stream))
			)
			(display (car stream))
			(newline)
			(display-n (cdr stream) (- n 1))
		)
		(
			(and
				(> n 0)
				(not (stream-null? stream))
			)
			(display (stream-car stream))
			(newline)
			(display-n (stream-cdr stream) (- n 1))
		)
	)
)


;; Q2 (exercise 3.50)
(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply
				proc
				(map stream-car argstreams)
			)
			(apply
				stream-map
				(cons proc (map stream-cdr argstreams))
			)
		)
	)
)

; (define (integers-from n) (cons-stream n (integers-from (+ n 1))))
; (define integers-from-2 (integers-from 2))
; (display-n integers-from-2 10)
; (display-n (stream-map + integers-from-2 integers-from-2) 10)


;; Q3
(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))