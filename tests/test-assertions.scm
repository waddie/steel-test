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

;; Stats: 5 passes, 6 failures, 2 errors, 13 assertions
(define stats (test-stats))
(meta-check! "stats counts assertions" (equal? 13 (hash-ref stats 'assertions)))
(meta-check! "stats counts passes" (equal? 5 (hash-ref stats 'passes)))
(meta-check! "stats counts failures" (equal? 6 (hash-ref stats 'failures)))
(meta-check! "stats counts errors" (equal? 2 (hash-ref stats 'errors)))

;; reset-tests! zeroes everything
(reset-tests!)
(define zeroed (test-stats))
(meta-check! "reset zeroes assertions" (equal? 0 (hash-ref zeroed 'assertions)))
(meta-check! "reset zeroes passes" (equal? 0 (hash-ref zeroed 'passes)))
(meta-check! "reset zeroes failures" (equal? 0 (hash-ref zeroed 'failures)))
(meta-check! "reset zeroes errors" (equal? 0 (hash-ref zeroed 'errors)))

(meta-done! "test-assertions")
