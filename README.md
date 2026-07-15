# steel-test

Unit testing for [Steel](https://github.com/mattwparas/steel), modelled
on `clojure.test`. Tests are plain Steel files run in file mode; the exit
code (`0`|`1`) is the verdict.

## Install

```sh
forge pkg install --git https://github.com/waddie/steel-test
```

Or copy the package directory to `~/.steel/cogs/steel-test/`.

## Usage

```scheme
;; tests/test-foo.scm
(require "steel-test/test.scm")
(require "../foo.scm") ; file mode resolves relative to this file

(deftest addition
  (testing "basic"
    (is (= 4 (add 2 2)))))

(run-tests!)
```

Run with `steel tests/test-foo.scm`. Exit code 0 means the suite passed.

## API

- `(deftest name body ...)` defines a zero-arg function `name` and registers
  it with the suite. Call `(name)` to run its assertions without the runner
  or fixtures.
- `(testing "desc" body ...)` groups assertions under a context string shown
  in failure headers. Nestable.
- `(is form)` asserts. Special forms:

  - `(is (= expected actual))` compares with `equal?`, reports both values
  - `(is (thrown? body ...))` passes when the body raises
  - `(is (thrown-with-msg? "substr" body ...))` also requires the error
    text to contain `substr`
  - any other form passes when it evaluates truthy

  All variants take an optional trailing message string and return `#t` or
  `#f`. An error raised by the form is caught and recorded as an error, not
  a crash.

- `(use-fixtures 'each f)` wraps every test in fixture `f`, a function
  `(lambda (run) setup (run) teardown)`. `(use-fixtures 'once f)` wraps the
  whole run. Fixtures compose in registration order, first registered
  outermost. Teardown of `'each` fixtures runs even when the test body
  raises.
- `(run-tests)`/`(run-tests!)` runs the suite and prints a human-friendly
  summary. The `!` variant additionally raises when any failure or error
  was recorded, so Steel exits nonzero. The raise points at the `(run-tests!)`
  call site.
- `(run-tests-json)`/`(run-tests-json!)` as above, but with a JSON summary
  for easier parsing with tooling, LLMs, etc.
- `(test-stats)` returns counters: `'tests 'assertions 'passes 'failures
'errors`.
- `(test-summary)` returns the rich hash serialized by `run-tests-json`:
  `'summary` (the counters plus `'success`) and `'problems` (the failure and
  error records). Use it to build your own output.
- `(reset-tests!)` clears the registry, fixtures, counters, and records.

## Human-friendly output

From `(run-tests)`/`(run-tests!)`:

```sh
FAIL in (addition) [basic] (test-foo.scm:7)
  (= 4 (add 2 2))
  expected: 4
  actual:   5
Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
error[E11]: Generic
  ┌─ tests/test-foo.scm:9:2
  │
9 │ (run-tests!)
  │  ^^^^^^^^^^  test failures
```

The `(file:line)` suffix points at the failing assertion; an uncaught error
escaping a test body reports the `deftest`’s line. Locations are omitted when
the file is run via `stdin`.

The trailing `error[E11]` block is the raise from `run-tests!` that makes
file mode exit nonzero; it carries the call site’s span, so it points at the
`(run-tests!)` form. As far as I can tell, Steel currently has no way to set
the exit code without raising (which I guess does make sense for an
interpreter intended for embedding).

## JSON output

From `(run-tests-json)`/`(run-tests-json!)`:

```json
{
  "summary": {
    "tests": 1,
    "assertions": 1,
    "passes": 0,
    "failures": 1,
    "errors": 0,
    "success": false
  },
  "problems": [
    {
      "kind": "fail",
      "type": "equal",
      "test": "addition",
      "context": ["basic"],
      "location": "test-foo.scm:7",
      "form": "(= 4 (add 2 2))",
      "message": false,
      "expected": "4",
      "actual": "5"
    }
  ]
}
```

`summary.success` is the single boolean to check. Every record in `problems`
carries `kind` (`"fail"` or `"error"`) and a finer `type`; `context` is an
outer-first array. `expected`, `actual`, and `error` are stringified, so any
value serializes. `location` and `message` are `false` when absent (`stdin`
source, or no assertion message).

## Best practice

### Shadowing

Do not name a `deftest` the same as any function being exercised. `deftest`
expands to `(define (name) ...)`, so the test would shadow that binding.

Suffix the test name instead: `(deftest resolve-revs-basic ...)` for a
`resolve-revs` function, or describe the behaviour (`commit-rev-resolution`).

## Running multiple files

```sh
sh tests/run-all.sh
```

Copy `tests/run-all.sh` into your project, or loop `steel tests/test-*.scm`
and aggregate exit codes.

## License

Copyright © 2026 Tom Waddington

Distributed under the MIT License. See LICENSE file for details.
