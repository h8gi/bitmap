(use bitmap)
(define white (bmp-make-pixel 255 255 255))
(define black (bmp-make-pixel 0 0 0))

(define (random-invader-list #!optional (random-color? #f))
  (let ((col (if random-color? (bmp-make-pixel (random 256) (random 256) (random 256)) white)))
    (map (lambda (y)
           (map (lambda (x)
                  (if (zero? (random 2)) black
                      col))
                (make-list 3)))
         (make-list 5))))

(define (int->invader-list int)
  (let* ((str (sprintf "~B" int))
         (dif (- (string-length str) 15)))
    (map (lambda (x)
           (if (char=? #\0 x) 0 1))
         (string->list (cond ((< 0 dif) (string-drop str dif))
                             ((< dif 0) (string-append (make-string (abs dif) #\0)
                                                       str))
                             (else str))))))

(define (format-invader-list inv-lst)
  (map (lambda (y)
         (append y (cdr (reverse y))))
       inv-lst))

(define (make-random-invader-bmp #!optional (random-color? #f))
  (make-bmp (format-invader-list (random-invader-list random-color?))))

(write-bmp "invader.bmp" (make-random-invader-bmp))
