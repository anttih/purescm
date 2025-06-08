#!r6rs
#!chezscheme
(library (Data.Function.Uncurried.foreign)
  (export
    mkFn0
    mkFn2
    mkFn3
    mkFn4
    mkFn5
    mkFn6
    mkFn7
    mkFn8
    mkFn9
    mkFn10
    runFn0
    runFn2
    runFn3
    runFn4
    runFn5
    runFn6
    runFn7
    runFn8
    runFn9
    runFn10)
  (import (prefix (chezscheme) scm:)
          (prefix (purescm runtime) rt:))

  (scm:define mkFn0
    (scm:lambda (fn)
      (scm:lambda ()
        (fn (scm:quote unit)))))

  (scm:define (mkFn2 fn)
    (scm:lambda (a b)
      ((fn a) b)))

  (scm:define (mkFn3 fn)
    (scm:lambda (a b c)
      (((fn a) b) c)))

  (scm:define (mkFn4 fn)
    (scm:lambda (a b c d)
      ((((fn a) b) c) d)))

  (scm:define (mkFn5 fn)
    (scm:lambda (a b c d e)
      (((((fn a) b) c) d) e)))

  (scm:define (mkFn6 fn)
    (scm:lambda (a b c d e f)
      ((((((fn a) b) c) d) e) f)))

  (scm:define (mkFn7 fn)
    (scm:lambda (a b c d e f g)
      (((((((fn a) b) c) d) e) f) g)))

  (scm:define (mkFn8 fn)
    (scm:lambda (a b c d e f g h)
      ((((((((fn a) b) c) d) e) f) g) h)))

  (scm:define (mkFn9 fn)
    (scm:lambda (a b c d e f g h i)
      (((((((((fn a) b) c) d) e) f) g) h) i)))

  (scm:define (mkFn10 fn)
    (scm:lambda (a b c d e f g h i j)
      ((((((((((fn a) b) c) d) e) f) g) h) i) j)))

  (scm:define runFn0
    (scm:lambda (fn)
      (fn)))

  (scm:define (runFn1 fn)
    (scm:lambda (a)
      (fn a)))

  (scm:define (runFn2 fn)
    (rt:lambda-curried (a b)
      (fn a b)))

  (scm:define (runFn3 fn)
    (rt:lambda-curried (a b c)
      (fn a b c)))

  (scm:define (runFn4 fn)
    (rt:lambda-curried (a b c d)
      (fn a b c d)))

  (scm:define (runFn5 fn)
    (rt:lambda-curried (a b c d e)
      (fn a b c d e)))

  (scm:define (runFn6 fn)
    (rt:lambda-curried (a b c d e f)
      (fn a b c d e f)))

  (scm:define (runFn7 fn)
    (rt:lambda-curried (a b c d e f g)
      (fn a b c d e f g)))

  (scm:define (runFn8 fn)
    (rt:lambda-curried (a b c d e f g h)
      (fn a b c d e f g h)))

  (scm:define (runFn9 fn)
    (rt:lambda-curried (a b c d e f g h i)
      (fn a b c d e f g h i)))

  (scm:define (runFn10 fn)
    (rt:lambda-curried (a b c d e f g h i j)
      (fn a b c d e f g h i j)))

  )
