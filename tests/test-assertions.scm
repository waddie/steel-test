;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; test-assertions.scm - Self-tests for the is assertion layer.
;;;
;;; Run from anywhere in file mode: steel tests/test-assertions.scm
;;; FAIL/ERROR blocks below are the library printing deliberate failures;
;;; the META-PASS line and exit code are the verdict.

(require "meta.scm")
(require "../test.scm")

(displayln "--- deliberate FAIL/ERROR output follows; META lines are the verdict ---")

;; Truthy form
(meta-check! "(is #t) returns #t" (equal? #t (is #t)))
(meta-check! "(is 5) is truthy, returns #t" (equal? #t (is 5)))
(meta-check! "(is #f) returns #f" (equal? #f (is #f)))
(meta-check! "(is #f msg) returns #f" (equal? #f (is #f "expected failure message")))

;; Equality special form
(meta-check! "(is (= 1 1)) returns #t" (equal? #t (is (= 1 1))))
(meta-check! "(is (= 1 2)) returns #f" (equal? #f (is (= 1 2))))
(meta-check! "(is (= 1 2) msg) returns #f" (equal? #f (is (= 1 2) "expected failure message")))

;; Inequality special form. The (= ...) interception matters beyond cosmetics:
;; steel's = aborts the VM on non-numeric operands, so these would take the
;; process down if they reached the truthy path.
(meta-check! "(is (not (= 1 2))) returns #t" (equal? #t (is (not (= 1 2)))))
(meta-check! "(is (not (= 1 1))) returns #f" (equal? #f (is (not (= 1 1)))))
(meta-check! "(is (not (= \"a\" \"b\"))) returns #t" (equal? #t (is (not (= "a" "b")))))
(meta-check! "(is (not (= '(1 2) '(1 2)))) returns #f"
  (equal? #f (is (not (= '(1 2) '(1 2))))))
(meta-check! "(is (not (= 'x 'x)) msg) returns #f"
  (equal? #f (is (not (= 'x 'x)) "expected failure message")))

;; not= spelling, same runner
(meta-check! "(is (not= \"a\" \"b\")) returns #t" (equal? #t (is (not= "a" "b"))))
(meta-check! "(is (not= \"a\" \"a\")) returns #f" (equal? #f (is (not= "a" "a"))))
(meta-check! "(is (not= 1 1) msg) returns #f"
  (equal? #f (is (not= 1 1) "expected failure message")))

;; A plain (not x) is not an inequality form and keeps the truthy path
(meta-check! "(is (not #f)) returns #t" (equal? #t (is (not #f))))
(meta-check! "(is (not #t)) returns #f" (equal? #f (is (not #t))))

;; thrown?
(meta-check! "(is (thrown? ...)) passes when body raises"
  (equal? #t (is (thrown? (error! "boom")))))
(meta-check! "(is (thrown? ...)) fails when body returns"
  (equal? #f (is (thrown? 42))))

;; thrown-with-msg?
(meta-check! "thrown-with-msg? passes on substring match"
  (equal? #t (is (thrown-with-msg? "boom" (error! "boom town")))))
(meta-check! "thrown-with-msg? fails on substring mismatch"
  (equal? #f (is (thrown-with-msg? "nope" (error! "zap")))))

;; Errors inside assertions are caught and recorded as errors
(meta-check! "error inside (is (= ...)) returns #f, does not crash"
  (equal? #f (is (= 1 (error! "kaboom")))))
(meta-check! "error inside (is expr) returns #f, does not crash"
  (equal? #f (is (error! "kaboom"))))

;; Stats: 9 passes, 12 failures, 2 errors, 23 assertions
(define stats (test-stats))
(meta-check! "stats counts assertions" (equal? 23 (hash-ref stats 'assertions)))
(meta-check! "stats counts passes" (equal? 9 (hash-ref stats 'passes)))
(meta-check! "stats counts failures" (equal? 12 (hash-ref stats 'failures)))
(meta-check! "stats counts errors" (equal? 2 (hash-ref stats 'errors)))

;; reset-tests! zeroes everything
(reset-tests!)
(define zeroed (test-stats))
(meta-check! "reset zeroes assertions" (equal? 0 (hash-ref zeroed 'assertions)))
(meta-check! "reset zeroes passes" (equal? 0 (hash-ref zeroed 'passes)))
(meta-check! "reset zeroes failures" (equal? 0 (hash-ref zeroed 'failures)))
(meta-check! "reset zeroes errors" (equal? 0 (hash-ref zeroed 'errors)))

(meta-done! "test-assertions")
