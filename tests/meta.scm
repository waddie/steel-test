;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; meta.scm - Raw checks for testing steel-test itself.
;;;
;;; The library under test cannot verify itself, so these helpers use
;;; nothing from test.scm. meta-done! raises on failure so file mode
;;; (steel tests/<file>.scm) exits nonzero.

(provide meta-check!
  meta-done!)

(define *meta-checks* (box 0))
(define *meta-failures* (box 0))

;;@doc
;; Record a raw check; print a META-FAIL line when ok is #f.
(define (meta-check! label ok)
  (set-box! *meta-checks* (+ 1 (unbox *meta-checks*)))
  (if ok
    #t
    (begin
      (set-box! *meta-failures* (+ 1 (unbox *meta-failures*)))
      (displayln "META-FAIL: " label)
      #f)))

;;@doc
;; Print the verdict; raise when any meta check failed.
(define (meta-done! name)
  (if (= 0 (unbox *meta-failures*))
    (begin
      (displayln "META-PASS " name " (" (unbox *meta-checks*) " checks)")
      #t)
    (error! "META-FAIL " name ": " (unbox *meta-failures*) " of " (unbox *meta-checks*) " checks failed")))
