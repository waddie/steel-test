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
;; Source locations
;;
;; Assertion macros inline (#%syntax-span x), a quoted (start end source-id)
;; list with byte offsets, and a #%syntax/raw probe whose originating file
;; resolves the path. Both are steel 0.8.2 expander hooks, the same ones the
;; stdlib's quasisyntax uses.

;; Last source file read, as a (path . text) pair.
(define *source-cache* (box #f))

(define (source-text path)
  (let ([cached (unbox *source-cache*)])
    (if (and cached (equal? (car cached) path))
      (cdr cached)
      (let ([text (read-port-to-string (open-input-file path))])
        (set-box! *source-cache* (cons path text))
        text))))

;; Spans count bytes; strings index chars.
(define (char-utf8-width c)
  (let ([cp (char->integer c)])
    (cond
      [(< cp #x80) 1]
      [(< cp #x800) 2]
      [(< cp #x10000) 3]
      [else 4])))

;; 1-based line containing the byte offset.
(define (offset->line text offset)
  (let loop ([rest (string->list text)]
             [bytes 0]
             [line 1])
    (if (or (empty? rest) (>= bytes offset))
      line
      (let ([c (car rest)])
        (loop (cdr rest)
          (+ bytes (char-utf8-width c))
          (if (equal? c #\newline) (+ line 1) line))))))

(define (path-basename path)
  (let ([parts (split-many path "/")])
    (if (empty? parts) path (car (reverse parts)))))

;; "file.scm:line" for the capture site, or #f when the source has no path
;; (stdin) or cannot be read back.
(define (location-string span stx)
  (with-handler (lambda (err) #f)
    (let ([path (syntax-originating-file stx)])
      (if path
        (let ([line (offset->line (source-text path) (car span))])
          (string-append (path-basename path) ":" (to-string line)))
        #f))))

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

;; Header such as "FAIL in (test-name) [outer > inner] (file.scm:12)"; test
;; name, context, and location are omitted when absent. The if results must
;; be bound with let before the string-append call: in a module, a call with
;; multiple if argument subexpressions can mis-evaluate and pass #f
;; (steel 0.8.2).
(define (fail-header kind loc)
  (let ([name (unbox *current-test*)]
        [ctx (unbox *context*)])
    (let ([name-part (if name (string-append " in (" (symbol->string name) ")") "")]
          [ctx-part (if (empty? ctx)
                     ""
                     (string-append " [" (join-strings (reverse ctx) " > ") "]"))]
          [loc-part (if loc (string-append " (" loc ")") "")])
      (string-append kind name-part ctx-part loc-part))))

(define (print-msg msg)
  (if msg (displayln (string-append "  " msg)) #t))

(define (record-pass)
  (inc! *passes*)
  #t)

;; form-str is the assertion form already rendered to a string; the = form
;; is rendered from its operand forms to avoid printing the macro-mangled
;; name of the = literal.
(define (report-fail-truthy form-str msg span stx)
  (let ([loc (location-string span stx)])
    (displayln (fail-header "FAIL" loc)))
  (print-msg msg)
  (displayln (string-append "  expected: " form-str))
  (displayln "  actual:   #false")
  (inc! *failures*)
  #f)

(define (report-fail-equal form-str expected actual msg span stx)
  (let ([loc (location-string span stx)])
    (displayln (fail-header "FAIL" loc)))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln (string-append "  expected: " (to-string expected)))
  (displayln (string-append "  actual:   " (to-string actual)))
  (inc! *failures*)
  #f)

(define (report-fail-no-throw form-str msg span stx)
  (let ([loc (location-string span stx)])
    (displayln (fail-header "FAIL" loc)))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln "  expected an error, none was raised")
  (inc! *failures*)
  #f)

(define (report-fail-wrong-msg form-str err substr msg span stx)
  (let ([loc (location-string span stx)])
    (displayln (fail-header "FAIL" loc)))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln (string-append "  expected error containing: " substr))
  (displayln (string-append "  actual error: " (to-string err)))
  (inc! *failures*)
  #f)

(define (report-error form-str err msg span stx)
  (let ([loc (location-string span stx)])
    (displayln (fail-header "ERROR" loc)))
  (print-msg msg)
  (displayln (string-append "  " form-str))
  (displayln (string-append "  " (to-string err)))
  (inc! *errors*)
  #f)

;; ---------------------------------------------------------------------------
;; Assertion runners

(define (assert-truthy form thunk msg span stx)
  (inc! *assertions*)
  (let ([form-str (to-string form)])
    (with-handler (lambda (err) (report-error form-str err msg span stx))
      (if (thunk) (record-pass) (report-fail-truthy form-str msg span stx)))))

(define (assert-equal expected-form actual-form expected-thunk actual-thunk msg span stx)
  (inc! *assertions*)
  (let ([form-str (string-append "(= " (to-string expected-form) " " (to-string actual-form) ")")])
    (with-handler (lambda (err) (report-error form-str err msg span stx))
      (let ([expected (expected-thunk)])
        (let ([actual (actual-thunk)])
          (if (equal? expected actual)
            (record-pass)
            (report-fail-equal form-str expected actual msg span stx)))))))

(define (assert-thrown form thunk substr msg span stx)
  (inc! *assertions*)
  (let ([form-str (to-string form)])
    (with-handler (lambda (err)
                   (if (or (not substr) (string-contains? (to-string err) substr))
                     (record-pass)
                     (report-fail-wrong-msg form-str err substr msg span stx)))
      (begin
        (thunk)
        (report-fail-no-throw form-str msg span stx)))))

;;@doc
;; Assert a form. Special forms: (is (= expected actual)) compares with
;; equal? and reports both values; (is (thrown? body ...)) passes when the
;; body raises; (is (thrown-with-msg? "substr" body ...)) additionally
;; requires the error text to contain substr. Any other form passes when it
;; evaluates truthy. All variants accept a trailing message string, return
;; #t or #f, and catch errors raised by the form (recorded as errors, not
;; crashes). Failure headers carry the assertion's file:line, captured at
;; expansion time; omitted when the source has no path (stdin).
(define-syntax is
  (syntax-rules (= thrown? thrown-with-msg?)
    [(is (= expected actual))
      (assert-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) #f
        (#%syntax-span expected)
        (#%syntax/raw 'loc 'loc (#%syntax-span expected)))]
    [(is (= expected actual) msg)
      (assert-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) msg
        (#%syntax-span expected)
        (#%syntax/raw 'loc 'loc (#%syntax-span expected)))]
    [(is (thrown? body0 body ...))
      (assert-thrown (quote (thrown? body0 body ...)) (lambda () body0 body ...) #f #f
        (#%syntax-span body0)
        (#%syntax/raw 'loc 'loc (#%syntax-span body0)))]
    [(is (thrown? body0 body ...) msg)
      (assert-thrown (quote (thrown? body0 body ...)) (lambda () body0 body ...) #f msg
        (#%syntax-span body0)
        (#%syntax/raw 'loc 'loc (#%syntax-span body0)))]
    [(is (thrown-with-msg? substr body ...))
      (assert-thrown (quote (thrown-with-msg? substr body ...)) (lambda () body ...) substr #f
        (#%syntax-span substr)
        (#%syntax/raw 'loc 'loc (#%syntax-span substr)))]
    [(is (thrown-with-msg? substr body ...) msg)
      (assert-thrown (quote (thrown-with-msg? substr body ...)) (lambda () body ...) substr msg
        (#%syntax-span substr)
        (#%syntax/raw 'loc 'loc (#%syntax-span substr)))]
    [(is expr)
      (assert-truthy (quote expr) (lambda () expr) #f
        (#%syntax-span expr)
        (#%syntax/raw 'loc 'loc (#%syntax-span expr)))]
    [(is expr msg)
      (assert-truthy (quote expr) (lambda () expr) msg
        (#%syntax-span expr)
        (#%syntax/raw 'loc 'loc (#%syntax-span expr)))]))

;; ---------------------------------------------------------------------------
;; Test definition and grouping

(define (register-test! name thunk span stx)
  (set-box! *tests* (cons (list name thunk span stx) (unbox *tests*)))
  #t)

;;@doc
;; Define a zero-arg test function and register it with the suite. Calling
;; (name) directly runs its assertions without the runner or fixtures. The
;; definition site is recorded: an error escaping the test body reports the
;; deftest's file:line.
(define-syntax deftest
  (syntax-rules ()
    [(deftest name body ...)
      (begin
        (define (name)
          body
          ...)
        (register-test! (quote name) name
          (#%syntax-span name)
          (#%syntax/raw 'loc 'loc (#%syntax-span name))))]))

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
;; loc is the deftest's file:line, or #f outside any test.
(define (report-test-error err loc)
  (displayln (fail-header "ERROR" loc))
  (displayln (string-append "  " (to-string err)))
  (inc! *errors*)
  #f)

(define (run-one-test entry)
  (let ([name (car entry)]
        [thunk (list-ref entry 1)]
        [span (list-ref entry 2)]
        [stx (list-ref entry 3)])
    (set-box! *current-test* name)
    (inc! *tests-run*)
    ;; The inner handler sits against the test body so each-fixture
    ;; teardown still runs when the body raises; the outer handler catches
    ;; errors raised by fixture code itself.
    (let ([protected (lambda ()
                      (with-handler (lambda (err)
                                     (report-test-error err (location-string span stx)))
                        (begin
                          (thunk)
                          #t)))])
      (with-handler (lambda (err)
                     (report-test-error err (location-string span stx)))
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
    (with-handler (lambda (err) (report-test-error err #f))
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
