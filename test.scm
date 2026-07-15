;; Copyright (c) 2026 Tom Waddington
;; SPDX-License-Identifier: MIT

;;; test.scm - clojure.test-style unit testing for Steel.
;;;
;;; Run test files in file mode (steel tests/test-foo.scm): requires resolve
;;; relative to the test file, then ~/.steel/cogs, and an uncaught error
;;; exits nonzero. run-tests! raises on failure, so the exit code is the
;;; verdict.

(provide is
  deftest
  testing
  use-fixtures
  run-tests
  run-tests!
  test-stats
  reset-tests!)

;; ---------------------------------------------------------------------------
;; State

(define *assertions* (box 0))
(define *passes* (box 0))
(define *failures* (box 0))
(define *errors* (box 0))
(define *tests-run* (box 0))
(define *current-test* (box #f))
(define *context* (box '()))

;; Registry entries are (name . thunk) pairs, most recent first; reversed
;; before a run. Built with cons: no struct-carrying map (Steel quirk).
(define *tests* (box '()))

;; Fixture functions, most recent first: newest is innermost when wrapped.
(define *each-fixtures* (box '()))
(define *once-fixtures* (box '()))

(define (inc! b)
  (set-box! b (+ 1 (unbox b))))

;; ---------------------------------------------------------------------------
;; Reporting

(define (join-strings lst sep)
  (if (empty? lst)
    ""
    (let loop ([acc (car lst)]
               [rest (cdr lst)])
      (if (empty? rest)
        acc
        (loop (string-append acc sep (car rest)) (cdr rest))))))

;; Header such as "FAIL in (test-name) [outer > inner]"; test name and
;; context are omitted when absent. The if results must be bound with let
;; before the string-append call: in a module, a call with multiple if
;; argument subexpressions can mis-evaluate and pass #f (steel 0.8.2).
(define (fail-header kind)
  (let ([name (unbox *current-test*)]
        [ctx (unbox *context*)])
    (let ([name-part (if name (string-append " in (" (symbol->string name) ")") "")]
          [ctx-part (if (empty? ctx)
                     ""
                     (string-append " [" (join-strings (reverse ctx) " > ") "]"))])
      (string-append kind name-part ctx-part))))

(define (print-msg msg)
  (if msg (displayln (string-append "  " msg)) #t))

(define (record-pass)
  (inc! *passes*)
  #t)

;; form-str is the assertion form already rendered to a string; the = form
;; is rendered from its operand forms to avoid printing the macro-mangled
;; name of the = literal.
(define (report-fail-truthy form-str msg)
  (displayln (fail-header "FAIL"))
  (print-msg msg)
  (displayln (string-append "  expected: " form-str))
  (displayln "  actual:   #false")
  (inc! *failures*)
  #f)

(define (report-fail-equal form-str expected actual msg)
  (displayln (fail-header "FAIL"))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln (string-append "  expected: " (to-string expected)))
  (displayln (string-append "  actual:   " (to-string actual)))
  (inc! *failures*)
  #f)

(define (report-fail-no-throw form-str msg)
  (displayln (fail-header "FAIL"))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln "  expected an error, none was raised")
  (inc! *failures*)
  #f)

(define (report-fail-wrong-msg form-str err substr msg)
  (displayln (fail-header "FAIL"))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln (string-append "  expected error containing: " substr))
  (displayln (string-append "  actual error: " (to-string err)))
  (inc! *failures*)
  #f)

(define (report-error form-str err msg)
  (displayln (fail-header "ERROR"))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln (string-append "  " (to-string err)))
  (inc! *errors*)
  #f)

;; ---------------------------------------------------------------------------
;; Assertion runners

(define (assert-truthy form thunk msg)
  (inc! *assertions*)
  (let ([form-str (to-string form)])
    (with-handler (lambda (err) (report-error form-str err msg))
      (if (thunk) (record-pass) (report-fail-truthy form-str msg)))))

(define (assert-equal expected-form actual-form expected-thunk actual-thunk msg)
  (inc! *assertions*)
  (let ([form-str (string-append "(= " (to-string expected-form) " " (to-string actual-form) ")")])
    (with-handler (lambda (err) (report-error form-str err msg))
      (let ([expected (expected-thunk)])
        (let ([actual (actual-thunk)])
          (if (equal? expected actual)
            (record-pass)
            (report-fail-equal form-str expected actual msg)))))))

(define (assert-thrown form thunk substr msg)
  (inc! *assertions*)
  (let ([form-str (to-string form)])
    (with-handler (lambda (err)
                   (if (or (not substr) (string-contains? (to-string err) substr))
                     (record-pass)
                     (report-fail-wrong-msg form-str err substr msg)))
      (begin
        (thunk)
        (report-fail-no-throw form-str msg)))))

;;@doc
;; Assert a form. Special forms: (is (= expected actual)) compares with
;; equal? and reports both values; (is (thrown? body ...)) passes when the
;; body raises; (is (thrown-with-msg? "substr" body ...)) additionally
;; requires the error text to contain substr. Any other form passes when it
;; evaluates truthy. All variants accept a trailing message string, return
;; #t or #f, and catch errors raised by the form (recorded as errors, not
;; crashes).
(define-syntax is
  (syntax-rules (= thrown? thrown-with-msg?)
    [(is (= expected actual))
      (assert-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) #f)]
    [(is (= expected actual) msg)
      (assert-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) msg)]
    [(is (thrown? body ...))
      (assert-thrown (quote (thrown? body ...)) (lambda () body ...) #f #f)]
    [(is (thrown? body ...) msg)
      (assert-thrown (quote (thrown? body ...)) (lambda () body ...) #f msg)]
    [(is (thrown-with-msg? substr body ...))
      (assert-thrown (quote (thrown-with-msg? substr body ...)) (lambda () body ...) substr #f)]
    [(is (thrown-with-msg? substr body ...) msg)
      (assert-thrown (quote (thrown-with-msg? substr body ...)) (lambda () body ...) substr msg)]
    [(is expr)
      (assert-truthy (quote expr) (lambda () expr) #f)]
    [(is expr msg)
      (assert-truthy (quote expr) (lambda () expr) msg)]))

;; ---------------------------------------------------------------------------
;; Test definition and grouping

(define (register-test! name thunk)
  (set-box! *tests* (cons (cons name thunk) (unbox *tests*)))
  #t)

;;@doc
;; Define a zero-arg test function and register it with the suite. Calling
;; (name) directly runs its assertions without the runner or fixtures.
(define-syntax deftest
  (syntax-rules ()
    [(deftest name body ...)
      (begin
        (define (name)
          body
          ...)
        (register-test! (quote name) name))]))

(define (push-context! desc)
  (set-box! *context* (cons desc (unbox *context*)))
  #t)

(define (pop-context!)
  (let ([ctx (unbox *context*)])
    (if (empty? ctx)
      #t
      (begin
        (set-box! *context* (cdr ctx))
        #t))))

;;@doc
;; Group assertions under a context string shown in failure headers.
;; Nestable. An error raised between assertions skips the pop; the runner
;; resets the context stack after each test.
(define-syntax testing
  (syntax-rules ()
    [(testing desc body ...)
      (begin
        (push-context! desc)
        (let ([result (begin body ...)])
          (pop-context!)
          result))]))

;; ---------------------------------------------------------------------------
;; Fixtures

;;@doc
;; Register a fixture, a function taking the test thunk: (lambda (run)
;; setup (run) teardown). kind 'each wraps every test; 'once wraps the
;; whole run. Fixtures compose in registration order, first registered
;; outermost.
(define (use-fixtures kind fixture)
  (cond
    [(equal? kind 'each)
      (set-box! *each-fixtures* (cons fixture (unbox *each-fixtures*)))
      #t]
    [(equal? kind 'once)
      (set-box! *once-fixtures* (cons fixture (unbox *once-fixtures*)))
      #t]
    [else (error! "use-fixtures: kind must be 'each or 'once, got: " (to-string kind))]))

;; fixtures arrive newest first, so wrap the thunk starting with the newest
;; (innermost); the first registered fixture ends up outermost.
(define (wrap-fixtures fixtures thunk)
  (let loop ([rest fixtures]
             [acc thunk])
    (if (empty? rest)
      acc
      (loop (cdr rest)
        (let ([fx (car rest)]
              [inner acc])
          (lambda ()
            (fx inner)
            #t))))))

;; ---------------------------------------------------------------------------
;; Runner

;; Uncaught error escaping a test body: counted as an error, no assertion.
(define (report-test-error err)
  (displayln (fail-header "ERROR"))
  (displayln (string-append "  " (to-string err)))
  (inc! *errors*)
  #f)

(define (run-one-test entry)
  (let ([name (car entry)]
        [thunk (cdr entry)])
    (set-box! *current-test* name)
    (inc! *tests-run*)
    ;; The inner handler sits against the test body so each-fixture
    ;; teardown still runs when the body raises; the outer handler catches
    ;; errors raised by fixture code itself.
    (let ([protected (lambda ()
                      (with-handler (lambda (err) (report-test-error err))
                        (begin
                          (thunk)
                          #t)))])
      (with-handler (lambda (err) (report-test-error err))
        ((wrap-fixtures (unbox *each-fixtures*) protected))))
    (set-box! *current-test* #f)
    (set-box! *context* '())
    #t))

(define (print-summary stats)
  (displayln (string-append "Ran "
              (to-string (hash-ref stats 'tests))
              " tests containing "
              (to-string (hash-ref stats 'assertions))
              " assertions."))
  (displayln (string-append (to-string (hash-ref stats 'failures))
              " failures, "
              (to-string (hash-ref stats 'errors))
              " errors."))
  #t)

;;@doc
;; Run every registered test in definition order through the registered
;; fixtures, print a summary, and return the stats hash. Never raises; see
;; run-tests! for the raising variant that makes file mode exit nonzero on
;; failure.
(define (run-tests)
  (let ([run-all (lambda ()
                  (let loop ([rest (reverse (unbox *tests*))])
                    (if (empty? rest)
                      #t
                      (begin
                        (run-one-test (car rest))
                        (loop (cdr rest))))))])
    (with-handler (lambda (err) (report-test-error err))
      ((wrap-fixtures (unbox *once-fixtures*) run-all))))
  (let ([stats (test-stats)])
    (print-summary stats)
    stats))

;;@doc
;; Run the suite like run-tests, then raise when any failure or error was
;; recorded so file mode (steel tests/foo.scm) exits nonzero. Returns the
;; stats hash when clean. The normal last form of a test file.
(define (run-tests!)
  (let ([stats (run-tests)])
    (if (= 0 (+ (hash-ref stats 'failures) (hash-ref stats 'errors)))
      stats
      (error! "test failures"))))

;; ---------------------------------------------------------------------------
;; Stats

;;@doc
;; Counters as a hash: 'tests 'assertions 'passes 'failures 'errors.
;; 'errors includes assertion errors and uncaught test errors.
(define (test-stats)
  (hash 'tests (unbox *tests-run*)
    'assertions
    (unbox *assertions*)
    'passes
    (unbox *passes*)
    'failures
    (unbox *failures*)
    'errors
    (unbox *errors*)))

;;@doc
;; Clear the registry, fixtures, counters, and reporting state.
(define (reset-tests!)
  (set-box! *tests* '())
  (set-box! *each-fixtures* '())
  (set-box! *once-fixtures* '())
  (set-box! *tests-run* 0)
  (set-box! *assertions* 0)
  (set-box! *passes* 0)
  (set-box! *failures* 0)
  (set-box! *errors* 0)
  (set-box! *current-test* #f)
  (set-box! *context* '())
  #t)
