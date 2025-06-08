(library (JSON.foreign)
  (export _null
          fromBoolean
          fromInt
          fromString
          fromJArray
          fromJObject
          print
          printIndented)
  (import (chezscheme)
          (prefix (purescm json) json:))

  (define (coerce x) x)

  (define _null 'null)

  (define fromBoolean coerce)

  (define fromInt inexact)

  (define fromString coerce)

  (define fromJArray coerce)

  (define fromJObject coerce)

  (define print json:json-stringify)

  (define printIndented json:json-stringify)

  )
