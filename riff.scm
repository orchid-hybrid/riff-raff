(define (for-each-between f comma list)
  (if (null? list)
      #t
      (if (null? (cdr list))
          (f (car list))
          (begin (f (car list))
                 (comma)
                 (for-each-between f comma (cdr list))))))

(define (quoted-string s)
  (let ((escape (lambda (khar)
                  (case khar
                    ((#\\) (list #\\ #\\))
                    ((#\") (list #\\ #\"))
                    ((#\newline) (list #\\ #\n))
                    (else (list khar))))))
    (list->string (append (list #\")
                          (flatten (map  escape (string->list s)))
                          (list #\")))))

(define (main)
  (let ((s '((number (x) (+ x 3))
             (main () (print (number 7))))))
    (let ((t (build-symbol-table s)))
      (display ";; char* symbols[] = {")
      (for-each (lambda (s)
                  (display (quoted-string (symbol->string s)))
                  (display ", "))
                t)
      (display "NULL")
      (print "};")
      (newline)
      (print `(define ((struct scm) build-sexp)
                . ,(append (build-sexp t s 'r)
                           (list `(return r))))))))
