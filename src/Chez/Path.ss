(library (Chez.Path foreign)
  (export directorySeparator extname)
  (import (chezscheme)
          (purs runtime pstring))

  (define directorySeparator (pstring (directory-separator)))

  (define (extname path)
    (string->pstring (path-extension (pstring->string path))))
  )
