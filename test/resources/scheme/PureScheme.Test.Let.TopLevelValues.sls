(library (PureScheme.Test.Let.TopLevelValues lib) (export foo) (import (rnrs)) (define foo (let ((b 2) (a 1)) (+ a b))))