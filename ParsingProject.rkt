#lang racket
(require parser-tools/lex)
(require(prefix-in : parser-tools/lex-sre))

; Max Diaz
; Parser Project
; Sources attached on a word document
;

(define scannedTokens                                   ; Scanner: I only tokenized the characters needed for the project                       
  (lexer
   [(eof)  `(())]
   ["read"  (cons `read   (scannedTokens input-port))]              
   ["write" (cons `write  (scannedTokens input-port))]              
   [(:+(:or (char-range #\a #\z) (char-range #\A #\Z)))
            (cons `ID     (scannedTokens input-port))]
   [(::(:+  (char-range #\0 #\9)))
            (cons `INT    (scannedTokens input-port))]
   [":="    (cons `asm_op (scannedTokens input-port))]  
   [#\(     (cons `LPAREN (scannedTokens input-port))]
   [#\)     (cons `RPAREN (scannedTokens input-port))]
   [#\+     (cons `add    (scannedTokens input-port))] 
   [#\-     (cons `sub    (scannedTokens input-port))]
   [#\*     (cons `mult   (scannedTokens input-port))]
   [#\/     (cons `div    (scannedTokens input-port))]
   [whitespace            (scannedTokens input-port)]
   ["$$"   '(EOF)]))

(define (parse inputFile)                                                           ; Parse function
  (define file (open-input-file inputFile))
  (define tokens (scannedTokens file))
  (cond [(empty? (program tokens)) (display "Accept")]
        [else                      (display "Error")]))

(define (match funcToken scannerTokens)                                            ;Match function removes first token, returns rest            
  (cond [(equal? funcToken (first scannerTokens)) (rest scannerTokens)]
        [else(error(format "Syntax error found on match function. Token --> ~a" (first scannerTokens)))]))

(define (program tokens)                                                          ;The start of recirsive call/start program
  (cond [(or(equal? (first tokens) `ID)
            (equal? (first tokens) `read)
            (equal? (first tokens) `write) 
            (equal? (first tokens) `$$)) (program (stmt_list tokens))]              ; call the next functions
        [(equal? (first tokens) 'EOF) (match 'EOF  tokens)] 
        [else (error(format "Parsing error found on program function. Token --> ~a" (first tokens)))])) 

(define (stmt_list tokens)  
  (cond [(or (equal? (first tokens) `ID)
             (equal? (first tokens) `read) 
             (equal? (first tokens) `write)) (stmt_list (stmt tokens))]              ; call the next functions(nested call)
        [(equal? (first tokens) `EOF) tokens]
        [else(error(format "Parsing error found on stmt_list function. Token --> ~a" (first tokens)))]))       

(define (stmt tokens)
  (cond [(equal?(first tokens) 'ID)    (expr (match 'asm_op (match `ID tokens)))]     ;nest call fucntions
        [(equal?(first tokens) 'read)  (match 'ID (match 'read tokens))]
        [(equal?(first tokens) `write) (expr (match 'write tokens))]
        [else (error(format "Parsing error found on stmt function. Token --> ~a" (first tokens)))]))
        
(define (expr tokens)
  (cond [(or (equal? (first tokens) 'ID)
             (equal? (first tokens) 'INT)
             (equal? (first tokens) 'LPAREN)) (term_tail (term tokens))]              ;nested call fucntions
        [else (error (format "Parsing error found on expr function. Token --> ~a" (first tokens)))]))  

(define (term_tail tokens) 
  (cond [(or (equal? (first tokens) 'add)       
         (equal? (first tokens) 'sub)) 
         (term_tail (term(add_op tokens)))]                                          ;nested call fucntions
        [(or (equal? (first tokens) 'RPAREN)
         (equal? (first tokens) 'ID)
         (equal? (first tokens) 'read)
         (equal? (first tokens) 'write) 
         (equal? (first tokens) 'EOF)) tokens]
        [else (error(format "Parsing error found on term_tail function. Token --> ~a" (first tokens)))])) 

(define (term tokens)
  (cond [(or (equal? (first tokens) 'ID)
             (equal? (first tokens) 'INT)
             (equal? (first tokens) 'LPAREN)) (factor_tail (factor tokens))]               ;nested call fucntions
        [else (error(format  "Parsing error found on term function token. Token --> ~a" (first tokens)))]))

(define (factor_tail tokens)
  (cond [(or (equal? (first tokens) 'mult)
             (equal? (first tokens) 'div)) (factor_tail (factor (mult_op tokens)))]        ;nested call fucntions
        [(or (equal? (first tokens) 'add)
             (equal? (first tokens) 'sub)
             (equal? (first tokens) 'RPAREN)
             (equal? (first tokens) 'ID) 
             (equal? (first tokens) 'read)
             (equal? (first tokens) 'write)
             (equal? (first tokens) 'EOF))  tokens]                                        ; returns tokens to previous call if nothing is foind
        [else (error(format "Parsing error found on factor_tail. Token --> ~a" (first tokens)))]))

(define (factor tokens)
  (cond [(equal? (first tokens) 'ID)     (match 'ID tokens)]
        [(equal? (first tokens) 'INT)    (match 'INT tokens)]
        [(equal? (first tokens) 'LPAREN) (match 'RPAREN (expr (match 'LPAREN tokens)))]     ;match function calls
        [ else   (error(format "Parsing error found on factor. Token --> ~a")(first tokens))]))

(define (add_op tokens)
  (cond [(equal? (first tokens) 'add) (match 'add tokens)]                                  ;match functions calls
        [(equal? (first tokens) 'sub) (match 'sub tokens)]
        [else (error(format "Parsing error found on add_op. Token --> `a"(first tokens)))]))

(define (mult_op tokens)
  (cond [(equal? (first tokens) 'mult) (match 'mult tokens)]                                ;match functions calls
        [(equal? (first tokens) 'div)  (match 'div tokens)] 
        [else (error(format "Parsing error found on mult_op function. Token --> ~a")(first tokens))]))


; call the first file
(display "input file 1\n")
(parse "input01.txt") 

(display "\ninput file 2\n")
(parse "input02.txt")

(display "\ninput file 3\n")
(parse "input03.txt")   ; It gets to this file and it fails

(parse "input04.txt")   ; never gets here
(parse "input05.txt")   ; I comment the other one in orser to test the other files.