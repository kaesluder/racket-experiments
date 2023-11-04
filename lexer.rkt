#lang racket
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/lex)

(define foo
  (lexer 
   (:: 
  