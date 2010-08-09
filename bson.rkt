#lang racket
(require srfi/1)

(define (read-document in)
  (define document-length (- (read-int32 in) 5)) ; not including the int32 or the terminal byte
  (define document-byte-string  (read-bytes document-length in))
  (begin0 (read-element-list (open-input-bytes document-byte-string))
    (read-null-byte in)))

(define (read-element-list in)
  (unfold eof-object?
          values
          (lambda (_) (read-element in))
          (read-element in)))

(define (read-element in)
  (if (eof-object? (peek-byte in))
      eof
      (let* ([code (read-byte in)]
             [e-name (read-e-name in)]
             [decoder (get-decoder code)]
             [value  (decoder in)])
        (cons e-name
              value))))

(define (read-e-name in)
  (read-c-string in))

(define (read-c-string in)
  (bytes->string/utf-8
   (let loop ()
     (define next-byte (read-bytes 1 in))
     (cond [(equal? next-byte #"\x00") #""]
           [(eof-object? next-byte) (raise (make-exn:bson "unexpected eof"))]
           [else (bytes-append next-byte
                               (loop))]))))

(define (read-string in)
  (define length (sub1 (read-int32 in))) ; don't include trailing "\x00" byte
  (begin0 (bytes->string/utf-8 (read-bytes length in))
    (let ([terminal-byte (read-bytes 1 in)])
      (unless (equal? terminal-byte #"\x00")
        (error (exn:bson (format "expected terminal byte \\x00 but got: ~a\n"
                                 terminal-byte)))))))

(define (read-int32 in)
  (integer-bytes->integer (read-bytes 4 in)
                          #t
                          #f))

(define (read-int64 in)
  (integer-bytes->integer (read-bytes 8 in)
                          #t
                          #f))

(define (read-double in)
  (floating-point-bytes->real (read-bytes 8 in)
                              #f))

(define (read-null-byte in)
  (define next-byte (read-bytes 1 in))
  (unless (equal? next-byte
                  #"\x00")
    (raise exn:bson (format "expected null, got: ~a" next-byte))))

(define-struct exn:bson (message))

(define (read-binary in)
  (define length (read-int32 in))
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-javascript-code in)
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-javascript-code-with-scope in)
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-regex in)
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-utc-datetime in)
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-binary-data in)
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-maxkey in)
  +inf.0)

(define (read-minkey in)
  -inf.0)

(define (read-symbol in)
  (string->symbol (read-string in)))

(define (read-null in)
  (error (make-exn:bson "not implemented"))) ;; TODO

(define (read-true in)
  #t)

(define (read-false in)
  #f)

(define (read-array in)
  (list->vector
   (map cdr (read-document in))))

(define (read-timestamp in)
  (error (exn:bson "not implemented")))

(define (get-decoder code)
  (match code
         [#x01 read-double]
         [#x02 read-string]
         [#x03 read-document]
         [#x04 read-array]
         [#x05 read-binary-data]
         [#x07 read-false]
         [#x08 read-true]
         [#x09 read-utc-datetime]
         [#x0A read-null]
         [#x0B read-regex]
         [#x0D read-javascript-code]
         [#x0E read-symbol]
         [#x0F read-javascript-code-with-scope]
         [#x10 read-int32]
         [#x11 read-timestamp]
         [#x12 read-int64]
         [#xFF read-minkey]
         [#x7F read-maxkey]))
