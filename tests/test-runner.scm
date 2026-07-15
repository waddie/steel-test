;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; test-runner.scm - Self-tests for deftest, testing, and run-tests.
;;;
;;; Run from anywhere in file mode: steel tests/test-runner.scm
;;; FAIL/ERROR blocks below are the library printing deliberate failures;
;;; the META lines and exit code are the verdict.

(require "meta.scm")
(require "../test.scm")

(displayln "--- deliberate FAIL/ERROR output follows; META lines are the verdict ---")

;; Scenario A: registration, execution order, error isolation, stats.
(define *journal* (box '()))

(define (log! tag)
  (set-box! *journal* (cons tag (unbox *journal*)))
  #t)

(deftest t-pass
  (log! 't-pass)
  (is (= 1 1)))

(deftest t-fail
  (log! 't-fail)
  (testing "context shown in header"
    (is (= 1 2))))

(deftest t-err
  (log! 't-err)
  (error! "deliberate test error")
  (is #t))

(deftest t-ctx
  (log! 't-ctx)
  (testing "outer"
    (testing "inner"
      (is #t))))

(define stats (run-tests))

(meta-check! "runs all registered tests" (equal? 4 (hash-ref stats 'tests)))
(meta-check! "counts executed assertions" (equal? 3 (hash-ref stats 'assertions)))
(meta-check! "counts passes" (equal? 2 (hash-ref stats 'passes)))
(meta-check! "counts failures" (equal? 1 (hash-ref stats 'failures)))
(meta-check! "uncaught test error recorded, suite continues"
  (equal? 1 (hash-ref stats 'errors)))
(meta-check! "tests run in definition order"
  (equal? '(t-pass t-fail t-err t-ctx) (reverse (unbox *journal*))))
(meta-check! "run-tests returns the same hash as test-stats"
  (equal? stats (test-stats)))

;; Scenario B: deftest defines a directly callable function; direct calls
;; bypass the runner (no 'tests increment) but count assertions.
(reset-tests!)

(deftest t-direct
  (is (= 2 2)))

(t-direct)

(define direct-stats (test-stats))
(meta-check! "direct call counts assertion" (equal? 1 (hash-ref direct-stats 'assertions)))
(meta-check! "direct call passes" (equal? 1 (hash-ref direct-stats 'passes)))
(meta-check! "direct call does not count a runner test" (equal? 0 (hash-ref direct-stats 'tests)))

;; Scenario C: reset-tests! clears the registry.
(reset-tests!)
(define empty-stats (run-tests))
(meta-check! "reset clears registry" (equal? 0 (hash-ref empty-stats 'tests)))
(meta-check! "reset clears assertions" (equal? 0 (hash-ref empty-stats 'assertions)))

;; Scenario D: run-tests! returns stats when clean, raises on failure so
;; file mode exits nonzero.
(reset-tests!)

(deftest d-pass
  (is #t))

(define clean-result (with-handler (lambda (err) 'raised) (run-tests!)))
(meta-check! "run-tests! returns stats when clean"
  (equal? 1 (hash-ref clean-result 'tests)))

(reset-tests!)

(deftest d-fail
  (is #f))

(define failed-result (with-handler (lambda (err) 'raised) (run-tests!)))
(meta-check! "run-tests! raises on failure" (equal? 'raised failed-result))

(meta-done! "test-runner")
