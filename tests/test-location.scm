;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; test-location.scm - Self-tests for source locations in failure output.
;;;
;;; Run from anywhere in file mode: steel tests/test-location.scm
;;; The checks pin exact line numbers, so edits above the capture block
;;; below mean re-pinning the NN in each check.

(require "meta.scm")
(require "../test.scm")

;; Capture block: each assertion's line number is pinned in a check below.
(define fail-equal-output
  (with-output-to-string (lambda () (is (= 1 2))))) ;; line 15
(define fail-truthy-output
  (with-output-to-string (lambda () (is #f)))) ;; line 17
(define fail-thrown-output
  (with-output-to-string (lambda () (is (thrown? 42))))) ;; line 19
(define fail-wrong-msg-output
  (with-output-to-string (lambda () (is (thrown-with-msg? "nope" (error! "zap")))))) ;; line 21
(define error-in-is-output
  (with-output-to-string (lambda () (is (= 1 (error! "kaboom")))))) ;; line 23
(define pass-output
  (with-output-to-string (lambda () (is (= 1 1)))))

(reset-tests!)
(deftest exploding-test (error! "boom")) ;; line 28
(define test-error-output
  (with-output-to-string (lambda () (run-tests))))
(reset-tests!)

(meta-check! "(= ...) failure carries file:line"
  (string-contains? fail-equal-output "FAIL (test-location.scm:15)"))
(meta-check! "truthy failure carries file:line"
  (string-contains? fail-truthy-output "FAIL (test-location.scm:17)"))
(meta-check! "thrown? failure carries file:line"
  (string-contains? fail-thrown-output "FAIL (test-location.scm:19)"))
(meta-check! "thrown-with-msg? mismatch carries file:line"
  (string-contains? fail-wrong-msg-output "FAIL (test-location.scm:21)"))
(meta-check! "error inside is carries file:line"
  (string-contains? error-in-is-output "ERROR (test-location.scm:23)"))
(meta-check! "uncaught test error carries the deftest line"
  (string-contains? test-error-output "(test-location.scm:28)"))
(meta-check! "passing assertion prints nothing"
  (equal? "" pass-output))

(meta-done! "test-location")
