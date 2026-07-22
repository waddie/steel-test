;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; test-json.scm - Self-tests for run-tests-json, run-tests-json!, and
;;; test-summary.
;;;
;;; Run from anywhere in file mode: steel tests/test-json.scm
;;; The JSON blobs are captured with with-output-to-string, so nothing is
;;; printed; the META lines and exit code are the verdict.

(require "meta.scm")
(require "../test.scm")

;; ---------------------------------------------------------------------------
;; Scenario A: a failing suite. Capture both the returned summary hash and the
;; emitted JSON blob from one run.

(deftest json-pass
  (is (= 2 2)))

(deftest json-fail
  (testing "arithmetic"
    (testing "small"
      (is (= 4 (+ 1 2))))))

(deftest json-error
  (error! "boom"))

(define *captured* (box #f))
(define blob
  (with-output-to-string
    (lambda () (set-box! *captured* (run-tests-json)))))
(define summary (unbox *captured*))

;; Returned summary hash.
(define sc (hash-ref summary 'summary))
(meta-check! "summary counts tests" (equal? 3 (hash-ref sc 'tests)))
(meta-check! "summary counts assertions" (equal? 2 (hash-ref sc 'assertions)))
(meta-check! "summary counts passes" (equal? 1 (hash-ref sc 'passes)))
(meta-check! "summary counts failures" (equal? 1 (hash-ref sc 'failures)))
(meta-check! "summary counts errors" (equal? 1 (hash-ref sc 'errors)))
(meta-check! "summary success is #f when problems exist"
  (equal? #f (hash-ref sc 'success)))
(meta-check! "problems holds both failure and error"
  (equal? 2 (length (hash-ref summary 'problems))))

(define fail-rec (list-ref (hash-ref summary 'problems) 0))
(meta-check! "fail record kind" (equal? "fail" (hash-ref fail-rec 'kind)))
(meta-check! "fail record type" (equal? "equal" (hash-ref fail-rec 'type)))
(meta-check! "fail record test name" (equal? "json-fail" (hash-ref fail-rec 'test)))
(meta-check! "fail record context is outer-first"
  (equal? '("arithmetic" "small") (hash-ref fail-rec 'context)))
(meta-check! "fail record expected" (equal? "4" (hash-ref fail-rec 'expected)))
(meta-check! "fail record actual" (equal? "3" (hash-ref fail-rec 'actual)))

(define err-rec (list-ref (hash-ref summary 'problems) 1))
(meta-check! "error record kind" (equal? "error" (hash-ref err-rec 'kind)))
(meta-check! "error record type" (equal? "test-error" (hash-ref err-rec 'type)))

;; Emitted JSON, parsed back. Numbers come back as floats and keys as symbols,
;; so compare with = and use symbol keys.
(define j (string->jsexpr blob))
(define js (hash-ref j 'summary))
(meta-check! "json summary tests" (= 3.0 (hash-ref js 'tests)))
(meta-check! "json summary success false" (equal? #f (hash-ref js 'success)))
(meta-check! "json problems length" (= 2.0 (length (hash-ref j 'problems))))
(meta-check! "json first problem is the fail"
  (equal? "fail" (hash-ref (list-ref (hash-ref j 'problems) 0) 'kind)))

;; Suppression: no human FAIL header, and the whole blob is one JSON object.
(meta-check! "human FAIL header suppressed"
  (not (string-contains? blob "FAIL in")))
(meta-check! "blob is a single parseable object" (hash? j))

;; ---------------------------------------------------------------------------
;; Scenario B: a clean suite. success true, problems empty.

(reset-tests!)

(deftest json-clean
  (is #t))

(define clean-cap (box #f))
(with-output-to-string
  (lambda () (set-box! clean-cap (run-tests-json))))
(define clean (unbox clean-cap))
(meta-check! "clean success true"
  (equal? #t (hash-ref (hash-ref clean 'summary) 'success)))
(meta-check! "clean problems empty" (empty? (hash-ref clean 'problems)))

;; reset-tests! cleared the record accumulator from scenario A.
(meta-check! "reset cleared prior problems"
  (equal? 0 (length (hash-ref clean 'problems))))

;; ---------------------------------------------------------------------------
;; Scenario B2: the not-equal record. Only one value is meaningful, so expected
;; is the "not <value>" rendering of it.

(reset-tests!)

(deftest json-not-equal
  (is (not (= '(1 2) '(1 2)))))

(define ne-cap (box #f))
(with-output-to-string
  (lambda () (set-box! ne-cap (run-tests-json))))
(define ne-rec (list-ref (hash-ref (unbox ne-cap) 'problems) 0))
(meta-check! "not-equal record kind" (equal? "fail" (hash-ref ne-rec 'kind)))
(meta-check! "not-equal record type" (equal? "not-equal" (hash-ref ne-rec 'type)))
(meta-check! "not-equal record expected" (equal? "not (1 2)" (hash-ref ne-rec 'expected)))
(meta-check! "not-equal record actual" (equal? "(1 2)" (hash-ref ne-rec 'actual)))
(meta-check! "not-equal record form is the not spelling"
  (equal? "(not (= (quote (1 2)) (quote (1 2))))" (hash-ref ne-rec 'form)))

;; not= reports through the same runner, so its form renders identically.
(reset-tests!)

(deftest json-not-eq-alias
  (is (not= '(1 2) '(1 2))))

(define alias-cap (box #f))
(with-output-to-string
  (lambda () (set-box! alias-cap (run-tests-json))))
(define alias-rec (list-ref (hash-ref (unbox alias-cap) 'problems) 0))
(meta-check! "not= record type matches the not form"
  (equal? "not-equal" (hash-ref alias-rec 'type)))
(meta-check! "not= record form renders as the not spelling"
  (equal? "(not (= (quote (1 2)) (quote (1 2))))" (hash-ref alias-rec 'form)))

;; ---------------------------------------------------------------------------
;; Scenario C: run-tests-json! raises on failure, returns the summary clean.

(reset-tests!)

(deftest json-bang-fail
  (is #f))

(define bang-fail
  (with-handler (lambda (err) 'raised)
    (with-output-to-string (lambda () (run-tests-json!)))))
(meta-check! "run-tests-json! raises on failure" (equal? 'raised bang-fail))

(reset-tests!)

(deftest json-bang-pass
  (is #t))

(define bang-cap (box #f))
(with-output-to-string
  (lambda () (set-box! bang-cap (run-tests-json!))))
(define bang-clean (unbox bang-cap))
(meta-check! "run-tests-json! returns summary when clean"
  (equal? 1 (hash-ref (hash-ref bang-clean 'summary) 'tests)))

(meta-done! "test-json")
