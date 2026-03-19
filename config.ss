(define buged-scheme-indent
    (lambda ()
        (define open-paren? (lambda (i) (memq (buged-utf8-ref buged-buffer i) '(40 91 123))))
        (define close-paren? (lambda (i) (memq (buged-utf8-ref buged-buffer i) '(41 93 125))))
        (define quoted? (lambda (i) (memq (buged-utf8-ref buged-buffer i) '(39 96 64))))
        (define newline? (lambda (i) (fx= (buged-utf8-ref buged-buffer i) 10)))
        (let loop ([depth 0]
                   [i (buged-check-with-gap-start (buged-back-char buged-gap-start))])
            (cond
              ((fx<= i 0) 0)
              ((and (fx>= depth 0) (open-paren? i))
               (let-values ([(ni count) (buged-line-start-count i)])
                   (cond
                     ((or (quoted? (buged-back-char i)) (open-paren? (buged-forward-char i))) (fx1+ count))
                     (else (fx+ buged-tab-size count)))))
              ((and (newline? i) (newline? (buged-forward-char i)) (newline? (buged-forward-char (buged-forward-char i)))) 0)
              ((fx= (buged-utf8-ref buged-buffer i) 10) (loop depth (buged-back-char i)))
              ((close-paren? i) (loop (fx1- depth) (buged-back-char i)))
              ((open-paren? i) (loop (fx1+ depth) (buged-back-char i)))
              (else (loop depth (buged-back-char i)))))))

(define buged-file-extension
    (lambda ()
        (let loop ([i (fx1- (string-length buged-file-name))])
            (cond
              ((fx< i 0) "")
              ((char=? (string-ref buged-file-name i) #\.) (substring buged-file-name i (string-length buged-file-name)))
              (else (loop (fx1- i)))))))

(define buged-insert-indentation
    (lambda ()
        (cond
          ((and (not buged-mark) (member (buged-file-extension) '(".ss" ".sls" ".scm")))
           (buged-inschs (cons #\newline (map (lambda (e) #\space) (iota (buged-scheme-indent))))))
          (else (buged-insch #\newline)))))

(define buged-auto-complete
    (let ([last-word ""]
          [matches '()])
        (lambda ()
            (define get-word-start
                (lambda (i)
                    (let loop ([i (buged-back-char i)])
                        (if (or (fxzero? i)
                                (memq (buged-utf8-ref buged-buffer (buged-back-char i)) '(32 10 40 91 123 41 93 125)))
                            i
                            (loop (buged-back-char i))))))
            (let ([word-start (get-word-start buged-gap-start)]
                  [bvp (open-bytevector-input-port buged-buffer (make-transcoder (utf-8-codec)))])
                (set-port-position! bvp word-start)
                (let ([prefix (get-string-n bvp (fx- buged-gap-start word-start))])
                    (cond
                      ((string=? prefix "") #f)
                      ((string=? prefix last-word)
                       (unless (null? matches)
                           (buged-delete-selection word-start)
                           (buged-inschs (string->list (car matches)))
                           (unless (memq (buged-utf8-ref buged-buffer buged-gap-end) '(32 10 40 91 123 41 93 125))
                               (buged-delete-selection (fx- (buged-forward-word buged-gap-start) (fx- buged-gap-end buged-gap-start))))
                           (buged-move-gap (fx+ word-start (bytevector-length (string->utf8 prefix))))
                           (set! matches (cdr matches))))
                      (else
                       (set! matches (filter 
                                         (lambda (e)
                                            (if (fx< (string-length e) (string-length prefix))
                                                #f
                                                (string=? prefix (substring e 0 (string-length prefix)))))
                                        (map symbol->string (oblist))))
                       (unless (null? matches)
                           (set! last-word prefix)
                           (set-cdr! (last-pair matches) matches)
                           (buged-delete-selection word-start)
                           (buged-inschs (string->list (car matches)))
                           (unless (memq (buged-utf8-ref buged-buffer buged-gap-end) '(32 10 40 91 123 41 93 125))
                               (buged-delete-selection (fx- (buged-forward-word buged-gap-start) (fx- buged-gap-end buged-gap-start))))
                           (buged-move-gap (fx+ word-start (bytevector-length (string->utf8 prefix))))
                           (set! matches (cdr matches))))))))))

#;(set! buged-keyword-highlights
  `((,(lambda (str)
          (member 
              str 
              '("define" "define-syntax" "define-record-type" "define-enumeration" "define-values"
              "let" "let*" "letrec" "letrec*" "let-values" "let*-values" "fluid-let" "parameterize"
              "lambda" "case-lambda" "set!" "quote" "quasiquote" "unquote" "unquote-splicing"
              "if" "cond" "case" "and" "or" "when" "unless" "begin" "do" "else" "=>"
              "syntax-case" "syntax-rules" "with-syntax" "syntax" "quasisyntax" "unsyntax" 
              "library" "export" "import" "module" "rename" "only" "except" "prefix"
              "trace-define" "trace-lambda" "make-engine" "call/cc" "dynamic-wind"
              "foreign-procedure" "foreign-callable" "printf" "format" "record-case")))
     ,(lambda () (buged-set-color 35)))
    (,(lambda (str) (string->number str))
     ,(lambda () (buged-set-color 36)))))

(set! buged-bindings (cons (list (buged-ctrl #\w)
                                 (lambda () (define old-bg buged-bg-color)
                                            (set! buged-bg-color 40)
                                            (buged-render)
                                            (buged-write-file)
                                            (sleep (make-time 'time-duration 100000000 0))
                                            (set! buged-bg-color old-bg)))
                           buged-bindings))
    
(set! buged-bindings (cons (list #\newline
                                 (lambda ()
                                     (buged-insert-indentation)))
                           buged-bindings))

(set! buged-bindings (cons (list (buged-esc "[46;5u")
                                 (lambda ()
                                     (buged-auto-complete)))
                           buged-bindings))    