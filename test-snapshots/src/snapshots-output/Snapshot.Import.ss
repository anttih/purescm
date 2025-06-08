#!r6rs
#!chezscheme
(library
  (Snapshot.Import)
  (export
    foo
    fortyThree
    fst)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purescm runtime) rt:)
    (prefix (Snapshot.Import.Constructor) Snapshot.Import.Constructor.)
    (prefix (Snapshot.Import.Impl) Snapshot.Import.Impl.)
    (prefix (Snapshot.Import.Product) Snapshot.Import.Product.))

  (scm:define fst
    (scm:lambda (v0)
      (Snapshot.Import.Product.Product-value0 v0)))

  (scm:define fortyThree
    ((Snapshot.Import.Impl.addImpl 21) 22))

  (scm:define foo
    Snapshot.Import.Constructor.Foo))
