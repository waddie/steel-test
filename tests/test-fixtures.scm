;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; test-fixtures.scm - Self-tests for use-fixtures.
;;;
;;; Run from anywhere in file mode: steel tests/test-fixtures.scm
;;; ERROR output below is deliberate; META lines and exit code are the
;;; verdict.

(require "meta.scm")
(require "../test.scm")

(displayln "--- deliberate ERROR output follows; META lines are the verdict ---")

(define *journal* (box '()))

(define (log! tag)
  (set-box! *journal* (cons tag (unbox *journal*)))
  #t)

;; Scenario A: once wraps the whole run; each fixtures wrap every test in
;; registration order (first registered outermost); an error escaping a
;; test body still runs each-fixture teardown.
(use-fixtures 'once
  (lambda (run)
    (log! 'once-before)
    (run)
    (log! 'once-after)
    #t))

(use-fixtures 'each
  (lambda (run)
    (log! 'each1-before)
    (run)
    (log! 'each1-after)
    #t))

(use-fixtures 'each
  (lambda (run)
    (log! 'each2-before)
    (run)
    (log! 'each2-after)
    #t))

(deftest ft-pass
  (log! 'ft-pass)
  (is #t))

(deftest ft-err
  (log! 'ft-err)
  (error! "deliberate test error")
  (is #t))

(define stats (run-tests))

(meta-check! "fixture and test ordering"
  (equal? '(once-before
            each1-before
            each2-before
            ft-pass
            each2-after
            each1-after
            each1-before
            each2-before
            ft-err
            each2-after
            each1-after
            once-after)
    (reverse (unbox *journal*))))
(meta-check! "both tests ran" (equal? 2 (hash-ref stats 'tests)))
(meta-check! "passing assertion counted" (equal? 1 (hash-ref stats 'passes)))
(meta-check! "test error recorded" (equal? 1 (hash-ref stats 'errors)))

;; Scenario B: reset-tests! clears fixtures.
(reset-tests!)
(set-box! *journal* '())

(deftest plain
  (log! 'plain)
  (is #t))

(run-tests)

(meta-check! "reset clears fixtures" (equal? '(plain) (reverse (unbox *journal*))))

;; Scenario C: unknown kind raises.
(define bad-kind-raised
  (with-handler (lambda (err) #t)
    (begin
      (use-fixtures 'sometimes (lambda (run) (run)))
      #f)))
(meta-check! "unknown fixture kind raises" (equal? #t bad-kind-raised))

(meta-done! "test-fixtures")
