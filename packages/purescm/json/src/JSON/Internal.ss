(library (JSON.Internal.foreign)
  (export _parse
          _fromNumberWithDefault
          _case
          toArray
          fromArray
          _fromEntries
          _insert
          _delete
          _entries
          _lookup
          empty
          length
          _index
          _append
          isNull)
  (import (except (chezscheme) length)
          (prefix (srfi :214) srfi:214:)
          (prefix (purescm json) json:)
          (purescm pstring))

  (define (_parse left right s)
    (call/cc
      (lambda (k)
        (with-exception-handler
          ; TODO return the condition irritants
          (lambda (e) (k (left (string->pstring (condition-message e)))))
          (lambda () (right (json:json-parse s)))))))

  (define (_fromNumberWithDefault fallback n)
    (if (or (nan? n) (not (finite? n)))
      fallback
      n))

  (define (_case isNull isBool isNum isStr isArr isObj j)
    (cond
      [(pstring? j) (isStr j)]
      [(eq? 'null j) (isNull j)]
      [(boolean? j) (isBool j)]
      [(number? j) (isNum (inexact j))]
      [(srfi:214:flexvector? j) (isArr j)]
      [(list? j) (isObj j)]
      [else (error #f "Value is not JSON")]))

  (define (toArray v) v)

  (define (fromArray v) v)

  (define (_fromEntries fst snd entries)
    (srfi:214:flexvector-fold (lambda (tail entry)
                                (cons (cons (pstring->symbol (fst entry)) (snd entry)) tail))
                              '()
                              entries))

  (define (_insert k v obj)
    (cons (cons k v) (_delete k obj)))

  (define (_delete k obj)
    (if (null? obj)
      obj
      (if (eq? (caar obj) k)
        (cdr obj)
        (cons (car obj) (_delete k (cdr obj))))))

  (define (_entries tuple obj)
    (srfi:214:list->flexvector (map (lambda (entry)
                                      ((tuple (symbol->pstring (car entry))) (cdr entry)))
                                    obj)))

  (define (_lookup nothing just key obj)
    (let ([res (assq (pstring->symbol key) obj)])
      (if (not res)
        nothing
        (just (cdr res)))))

  (define empty (srfi:214:flexvector))

  (define length srfi:214:flexvector-length)

  (define (_index nothing just ix arr)
    (if (and (fx>=? ix 0) (fx<? ix (srfi:214:flexvector-length arr)))
      (just (srfi:214:flexvector-ref arr ix))
      nothing))

  (define _append srfi:214:flexvector-append)

  (define (isNull j) (eq? j 'null))

  )
