#!r6rs
#!chezscheme
(library
  (Snapshot.PrimUndefined)
  (export
    main
    testCase)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purescm runtime) rt:)
    (prefix (Data.Ring) Data.Ring.)
    (prefix (Test.Assert) Test.Assert.))

  (scm:define testCase
    (scm:lambda (dictRing0)
      (rt:record-ref ((rt:record-ref dictRing0 (scm:string->symbol "Semiring0")) (scm:quote undefined)) (scm:string->symbol "add"))))

  (scm:define main
    (Test.Assert.assert (scm:fx=? (((testCase Data.Ring.ringInt) 1) 1) 2))))
