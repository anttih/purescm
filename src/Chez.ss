(library (Chez foreign)
  (export argv (rename exit_ exit))
  (import (chezscheme)
          (purs runtime pstring))

  (define argv
    (lambda () (map string->pstring (command-line))))


  (define exit_ exit)

  )
