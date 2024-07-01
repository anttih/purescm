(library (Chez.Path foreign)
  (export directorySeparator extname)
  (import (chezscheme))

  (define directorySeparator (directory-separator))

  (define (extname path)
    (path-extension path))
  )
