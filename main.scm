(use srfi-1 srfi-4 ports data-structures)
(define (integer->u8vector int limit #!optional (le #t))
  (define (inner int acc)
    (cond ((zero? (quotient int 256)) (cons int acc))
          (else (inner (quotient int 256)
                       (cons (modulo int 256) acc)))))
  (let* ((lst (inner int '()))
         (len (-  (length lst) limit)))
    (list->u8vector ((if le reverse! identity)
                     (cond ((positive? len) (list-tail lst len))
                           ((negative? len) (append (make-list (- len) 0) lst))
                           (else lst))))))

;;; ファイルヘッダ =========================================================
(define (write-bmp-fileheader width height #!optional port)
  (with-output-to-port (if port port (current-output-port))
    (lambda ()
      (write-u8vector bmp-file-type)
      (write-u8vector (bmp-file-size width height))
      (write-u8vector bmp-reserveed-area-1)
      (write-u8vector bmp-reserveed-area-2)
      (write-u8vector bmp-offset-to-image))))
;;; ファイルタイプ "BM"
(define bmp-file-type (u8vector 66 77))
;;; ファイルサイズ
(define (bmp-file-size width height)
  (integer->u8vector (+ 54 (calc-image-size width height)) 4))
;;; 予約領域
(define bmp-reserveed-area-1 (u8vector 0 0))
(define bmp-reserveed-area-2 (u8vector 0 0))
;;; 画像データまでのオフセット
(define bmp-offset-to-image (integer->u8vector 54 4))



;;; 情報ヘッダ =============================================================
;;; 情報ヘッダサイズ
(define (write-bmp-infoheader width height #!optional port)
  (with-output-to-port (if port port (current-output-port))
    (lambda ()
      (write-u8vector bmp-infoheader-size)
      (write-u8vector (bmp-image-width width))
      (write-u8vector (bmp-image-height height))
      (write-u8vector bmp-plane-number)
      (write-u8vector bmp-color/pixel)
      (write-u8vector bmp-compress)
      (write-u8vector (bmp-image-size width height))
      (write-u8vector bmp-xpix/meter)
      (write-u8vector bmp-ypix/meter)
      (write-u8vector bmp-palette-color)
      (write-u8vector bmp-palette-color-important))))

(define bmp-infoheader-size (integer->u8vector 40 4))
;;; 画像の幅
(define (bmp-image-width width)
  (integer->u8vector width 4))
;;; 画像の高さ
(define (bmp-image-height height)
  (integer->u8vector height 4))
;;; プレーン数
(define bmp-plane-number (integer->u8vector 1 2))
;;; 色/画素 
(define bmp-color/pixel (integer->u8vector 24 2))
;;; 圧縮形式
(define bmp-compress (integer->u8vector 0 4))
;;; 画像データのサイズ
(define (bmp-image-size width height)
  (integer->u8vector (calc-image-size width height) 4))
;;; 解像度
(define bmp-xpix/meter (integer->u8vector 1 4))
(define bmp-ypix/meter (integer->u8vector 1 4))
;;; palette
(define bmp-palette-color (integer->u8vector 0 4))
(define bmp-palette-color-important (integer->u8vector 0 4))

;;; まとめ =====================================================
(define (make-bmp data)
  (let ((height (length data))
        (width (length (car data))))
    (let* ((widthx3 (* 3 width))
           (mod-wid (modulo widthx3 4)))
      (list (if (zero? mod-wid) data
                (map (lambda (y) (append y (make-list (- 4 mod-wid) (u8vector 0))))
                     data))
            width height))))

(define (bmp-width bmp) (cadr bmp))
(define (bmp-height bmp) (caddr bmp))
(define (bmp-data bmp) (car bmp))

(define (write-bmp bmp #!optional port)
  (with-output-to-port (if port port (current-output-port))
    (lambda ()
      (let ((width (bmp-width bmp))
            (height (bmp-height bmp))
            (data (bmp-data bmp)))
        (write-bmp-fileheader width height)
        (write-bmp-infoheader width height)
        (for-each (lambda (y)
                    (for-each (lambda (x)
                                (write-u8vector x))
                              y))
                  data)))))


;;; その他の計算 ==================================================
(define (calc-image-size width height)
  (let* ((widthx3 (* 3 width))
         (mod-wid (modulo widthx3 4)))
    (* (if (zero? mod-wid) widthx3 (+ widthx3 (- 4 mod-wid)))
       height)))

(define (bmp-make-pixel r g b #!optional (le #t))
  (if le (u8vector b g r)
      (u8vector r g b)))



