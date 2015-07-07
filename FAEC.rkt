#! /usr/bin/env racket
#lang racket

(require "SECD.rkt")

(define (cmp input-file output-file)
  (let ([prog (file->value input-file)])
    (spim-compile-to-file prog output-file) 
    (string-append "Compiled to " output-file)))

(define greeting
  (command-line
   ;#:once-each
   #:args
   (input-file output-file) (cmp input-file output-file)))