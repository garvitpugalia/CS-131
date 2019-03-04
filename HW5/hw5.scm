(define (null-ld? obj)
  (cond ((and (not (null? obj)) (pair? obj)) (eq? (car obj) (cdr obj)))
	(else #f)))
  
(define (ld? obj)
  (cond ((null-ld? obj) #t)
	((or (null? obj) (not (pair? obj)) (not (pair? (car obj)))) #f)
	(else (ld? (cons (cdr (car obj)) (cdr obj))))))

(define (cons-ld obj listdiff)
  (cond ((not (ld? listdiff)) (error "Argument is not a listdiff"))
	(else (cons (cons obj (car listdiff)) (cdr listdiff)))))

(define (car-ld listdiff)
  (cond ((or (not (ld? listdiff)) (null-ld? listdiff)) (error "Unrecognized listdiff format"))
	(else (car (car listdiff)))))

(define (cdr-ld listdiff)
  (cond ((or (not (ld? listdiff)) (null-ld? listdiff)) (error "Unrecognized listdiff format"))
	(else (cons (cdr (car listdiff)) (cdr listdiff)))))

; Similar
(define (ld obj . params)
  (cons (cons obj params) '()))

(define (t-length-ld listdiff t-length)
  (cond ((not (ld? listdiff)) (error "Not a listdiff"))
	((null-ld? listdiff) t-length)
	(else (t-length-ld (cdr-ld listdiff) (+ 1 t-length)))))

(define (length-ld listdiff)
  (t-length-ld listdiff 0))

(define (t-append-ld param)
  (cond ((null? param) '())
	(else (append (ld->list (car param)) (t-append-ld (cdr param))))))

(define (append-ld listdiff . params)
  (cons (t-append-ld (cons listdiff params)) '()))

(define (ld-tail listdiff k)
  (cond ((< k 0) (error "K is less than 0"))
	((> k (length-ld listdiff)) (error "K is greater than length"))
	((eq? k 0) listdiff)
	(else (ld-tail (cdr-ld listdiff) (- k 1)))))

(define (list->ld list)
  (cond ((not (list? list)) (error "Not a list"))
	((null? list) (cons list list))
	(else (ld (cons (car list) (cdr list))))))

(define (ld->list listdiff)
  (cond ((not (ld? listdiff)) (error "Not a listdiff"))
	(else (take (car listdiff) (length-ld listdiff)))))

(define (any-null? list)
  (and (pair? list)
       (or (null-ld? (car list))
	   (any-null? (cdr list)))))

(define (get-elements pos list)
  (cond ((null? list) '())
	(else (cons (pos (car list)) (get-elements pos (cdr list))))))

(define (map-ld proc . lds)
  (cond ((any-null? lds) '(()))
	(else (cons-ld (apply proc (get-elements car-ld lds))
		       (apply map-ld proc (get-elements cdr-ld lds))))))

(define (exchange word)
  (cond ((equal? word 'list) 'ld)
	((equal? word 'map) 'map-ld)
	((equal? word 'null?) 'null-ld?)
	((equal? word 'list?) 'ld?)
	((equal? word 'cons) 'cons-ld)
	((equal? word 'car) 'car-ld)
	((equal? word 'cdr) 'cdr-ld)
	((equal? word 'length) 'length-ld)
	((equal? word 'append) 'append-ld)
	((equal? word 'list-tail) 'ld-tail)
	(else word)))

(define (expr2ld expr)
  (cond ((null? expr) null)
	((list? expr) (cons (expr2ld (car expr)) (expr2ld (cdr expr))))
	(else (exchange expr))))
