#!/usr/bin/env racket
#lang racket/base

(define (greeting name)
  (printf "Hello Racket ~a\n" name))

(greeting "quickrun")
