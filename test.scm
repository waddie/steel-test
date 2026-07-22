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
  run-tests-json
  run-tests-json!
  test-stats
  test-summary
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

;; Failure/error records, newest first; reversed on read. Populated at every
;; report site regardless of mode so human output stays byte-identical.
(define *results* (box '()))

;; When #t, report-* build records but suppress the inline human lines so
;; stdout carries only the JSON blob.
(define *json-mode* (box #f))

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
  (if msg (displayln (string-append "  \"" msg "\":")) #t))

(define (record-pass)
  (inc! *passes*)
  #t)

(define (push-result! rec)
  (set-box! *results* (cons rec (unbox *results*)))
  #t)

;; Common record fields from current runner state. Conditionals are let-bound
;; before the hash call (a call with several if argument subexpressions can
;; mis-evaluate to #f in a module, steel 0.8.2). loc is #f or "file.scm:line".
;; type-specific keys are added by the caller with hash-insert (immutable).
(define (make-record kind type form-str loc)
  (let ([name (unbox *current-test*)]
        [ctx (unbox *context*)])
    (let ([test-field (if name (symbol->string name) #f)]
          [form-field (if form-str form-str #f)]
          [ctx-field (reverse ctx)])
      (hash 'kind kind
        'type
        type
        'test
        test-field
        'context
        ctx-field
        'location
        loc
        'form
        form-field))))

;; form-str is the assertion form already rendered to a string; the = form
;; is rendered from its operand forms to avoid printing the macro-mangled
;; name of the = literal.
(define (report-fail-truthy form-str msg span stx)
  (let ([loc (location-string span stx)]
        [msg-field (if msg msg #f)])
    (push-result!
      (hash-insert
        (hash-insert
          (hash-insert (make-record "fail" "truthy" form-str loc) 'message msg-field)
          'expected
          form-str)
        'actual
        "#false"))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "FAIL" loc))
        (print-msg msg)
        (displayln (string-append "  expected: " form-str))
        (displayln "  actual:   #false"))))
  (inc! *failures*)
  #f)

(define (report-fail-equal form-str expected actual msg span stx)
  (let ([loc (location-string span stx)]
        [exp-str (to-string expected)]
        [act-str (to-string actual)]
        [msg-field (if msg msg #f)])
    (push-result!
      (hash-insert
        (hash-insert
          (hash-insert (make-record "fail" "equal" form-str loc) 'message msg-field)
          'expected
          exp-str)
        'actual
        act-str))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "FAIL" loc))
        (print-msg msg)
        (displayln (string-append "  " form-str))
        (displayln (string-append "  expected: " exp-str))
        (displayln (string-append "  actual:   " act-str)))))
  (inc! *failures*)
  #f)

;; Inequality failure: the two values compared equal, so there is only one
;; value to show. expected is rendered as "not <value>" to keep the field pair
;; readable as display strings.
(define (report-fail-not-equal form-str actual msg span stx)
  (let ([loc (location-string span stx)]
        [act-str (to-string actual)]
        [msg-field (if msg msg #f)])
    (push-result!
      (hash-insert
        (hash-insert
          (hash-insert (make-record "fail" "not-equal" form-str loc) 'message msg-field)
          'expected
          (string-append "not " act-str))
        'actual
        act-str))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "FAIL" loc))
        (print-msg msg)
        (displayln (string-append "  " form-str))
        (displayln (string-append "  expected: not " act-str))
        (displayln (string-append "  actual:   " act-str)))))
  (inc! *failures*)
  #f)

(define (report-fail-no-throw form-str msg span stx)
  (let ([loc (location-string span stx)]
        [msg-field (if msg msg #f)])
    (push-result!
      (hash-insert
        (hash-insert (make-record "fail" "no-throw" form-str loc) 'message msg-field)
        'detail
        "expected an error, none was raised"))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "FAIL" loc))
        (print-msg msg)
        (displayln (string-append "  " form-str))
        (displayln "  expected an error, none was raised"))))
  (inc! *failures*)
  #f)

(define (report-fail-wrong-msg form-str err substr msg span stx)
  (let ([loc (location-string span stx)]
        [err-str (to-string err)]
        [msg-field (if msg msg #f)])
    (push-result!
      (hash-insert
        (hash-insert
          (hash-insert (make-record "fail" "wrong-message" form-str loc) 'message msg-field)
          'substring
          substr)
        'error
        err-str))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "FAIL" loc))
        (print-msg msg)
        (displayln (string-append "  " form-str))
        (displayln (string-append "  expected error containing: " substr))
        (displayln (string-append "  actual error: " err-str)))))
  (inc! *failures*)
  #f)

(define (report-error form-str err msg span stx)
  (let ([loc (location-string span stx)]
        [err-str (to-string err)]
        [msg-field (if msg msg #f)])
    (push-result!
      (hash-insert
        (hash-insert (make-record "error" "assertion" form-str loc) 'message msg-field)
        'error
        err-str))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "ERROR" loc))
        (print-msg msg)
        (displayln (string-append "  " form-str))
        (displayln (string-append "  " err-str)))))
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

;; Inverse of assert-equal. Both inequality spellings route here, so the form
;; is rendered as (not (= ...)) either way rather than echoing the source.
(define (assert-not-equal expected-form actual-form expected-thunk actual-thunk msg span stx)
  (inc! *assertions*)
  (let ([form-str (string-append "(not (= "
                   (to-string expected-form)
                   " "
                   (to-string actual-form)
                   "))")])
    (with-handler (lambda (err) (report-error form-str err msg span stx))
      (let ([expected (expected-thunk)])
        (let ([actual (actual-thunk)])
          (if (equal? expected actual)
            (report-fail-not-equal form-str actual msg span stx)
            (record-pass)))))))

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
;; equal? and reports both values; (is (not (= expected actual))) and its
;; (is (not= expected actual)) spelling are the inverse; (is (thrown? body
;; ...)) passes when the body raises; (is (thrown-with-msg? "substr" body
;; ...)) additionally requires the error text to contain substr. Any other
;; form passes when it evaluates truthy. All variants accept a trailing
;; message string, return #t or #f, and catch errors raised by the form
;; (recorded as errors, not crashes). Failure headers carry the assertion's
;; file:line, captured at expansion time; omitted when the source has no
;; path (stdin).
;;
;; The inequality forms are rewritten here rather than evaluated, because
;; steel's = is numeric-only and aborts the process on other types (a VM
;; panic, not a catchable error). not= is recognised only inside is; it is
;; a macro literal, not a binding.
(define-syntax is
  (syntax-rules (not = not= thrown? thrown-with-msg?)
    [(is (not (= expected actual)))
      (assert-not-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) #f
        (#%syntax-span expected)
        (#%syntax/raw 'loc 'loc (#%syntax-span expected)))]
    [(is (not (= expected actual)) msg)
      (assert-not-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) msg
        (#%syntax-span expected)
        (#%syntax/raw 'loc 'loc (#%syntax-span expected)))]
    [(is (not= expected actual))
      (assert-not-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) #f
        (#%syntax-span expected)
        (#%syntax/raw 'loc 'loc (#%syntax-span expected)))]
    [(is (not= expected actual) msg)
      (assert-not-equal (quote expected) (quote actual) (lambda () expected) (lambda () actual) msg
        (#%syntax-span expected)
        (#%syntax/raw 'loc 'loc (#%syntax-span expected)))]
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
  (let ([err-str (to-string err)])
    (push-result!
      (hash-insert (make-record "error" "test-error" #f loc) 'error err-str))
    (if (unbox *json-mode*)
      #t
      (begin
        (displayln (fail-header "ERROR" loc))
        (displayln (string-append "  " err-str)))))
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
;; Run every registered test through the once-fixtures, collecting counters and
;; records but printing no summary. Escaping errors are caught, so this never
;; raises. Shared by run-tests and run-tests-json.
(define (run-suite!)
  (let ([run-all (lambda ()
                  (let loop ([rest (reverse (unbox *tests*))])
                    (if (empty? rest)
                      #t
                      (begin
                        (run-one-test (car rest))
                        (loop (cdr rest))))))])
    (with-handler (lambda (err) (report-test-error err #f))
      ((wrap-fixtures (unbox *once-fixtures*) run-all)))))

(define (run-tests)
  (run-suite!)
  (let ([stats (test-stats)])
    (print-summary stats)
    stats))

(define (run-tests-impl! span)
  (let ([stats (run-tests)])
    (if (= 0 (+ (hash-ref stats 'failures) (hash-ref stats 'errors)))
      stats
      (error-with-span span "test failures"))))

;;@doc
;; Run the suite like run-tests but emit one JSON summary (test-summary
;; serialized) to stdout, suppressing the inline FAIL/ERROR blocks so stdout is
;; a single parseable object. Never raises; returns the summary hash. See
;; run-tests-json! for the raising variant.
(define (run-tests-json)
  (set-box! *json-mode* #t)
  (run-suite!)
  (set-box! *json-mode* #f)
  (let ([summary (test-summary)])
    (displayln (value->jsexpr-string summary))
    summary))

;; Decide the raise from the boxes, not by hash-ref-ing the returned summary:
;; hash-ref on a returned multi-key hash inside a module function corrupts
;; (steel 0.8.2). The nested lets force run-tests-json to run (populating the
;; boxes) before they are read.
(define (run-tests-json-impl! span)
  (let ([summary (run-tests-json)])
    (let ([bad (+ (unbox *failures*) (unbox *errors*))])
      (if (= 0 bad)
        summary
        (error-with-span span "test failures")))))

;;@doc
;; Run the suite like run-tests, then raise when any failure or error was
;; recorded so file mode (steel tests/foo.scm) exits nonzero. Returns the
;; stats hash when clean. The normal last form of a test file. The raise
;; carries the call site's span, so steel's report is a single block
;; pointing at the (run-tests!) form rather than at this file's internals.
(define-syntax run-tests!
  (syntax-rules ()
    [(run-tests!)
      (run-tests-impl! (#%syntax-span (run-tests!)))]))

;;@doc
;; Like run-tests-json, then raise when any failure or error was recorded so
;; file mode exits nonzero. The JSON is written to stdout before the raise, so
;; a tool reading stdout still gets a clean blob while steel's error block goes
;; to stderr. Returns the summary hash when clean. A macro: call it, don't pass
;; it as a value. The raise carries the call site's span.
(define-syntax run-tests-json!
  (syntax-rules ()
    [(run-tests-json!)
      (run-tests-json-impl! (#%syntax-span (run-tests-json!)))]))

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

;; Counts plus 'success, built inline in one hash and returned from its own
;; function. Binding an inline multi-key hash in a let and then embedding it as
;; a value (via hash-insert or a further hash call) corrupts under module
;; compilation (steel 0.8.2): the value leaks to the hash's first key. Returning
;; it from a helper dodges that, as does the caller's step-by-step hash-insert
;; wrapping. The keys duplicate test-stats deliberately for the same reason:
;; hash-insert on the test-stats *result* hits the same bug.
(define (counts-with-success success)
  (hash 'tests (unbox *tests-run*)
    'assertions
    (unbox *assertions*)
    'passes
    (unbox *passes*)
    'failures
    (unbox *failures*)
    'errors
    (unbox *errors*)
    'success
    success))

;;@doc
;; Rich result as a hash: 'summary is the counts (see test-stats) plus 'success
;; (#t when no failures or errors); 'problems is the list of failure and error
;; records collected during the run, in run order, context outer-first within
;; each. Serialized by run-tests-json; use directly to build your own output.
(define (test-summary)
  (let ([recs (reverse (unbox *results*))]
        [success (= 0 (+ (unbox *failures*) (unbox *errors*)))])
    (let ([counts (counts-with-success success)])
      (let ([base (hash-insert (hash) 'summary counts)])
        (hash-insert base 'problems recs)))))

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
  (set-box! *results* '())
  (set-box! *json-mode* #f)
  #t)
