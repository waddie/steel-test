# steel-test

Unit testing for [Steel](https://github.com/mattwparas/steel), modeled on
clojure.test. Tests are plain Steel files run in file mode; the exit code is
the verdict.

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
    text to contain substr
  - any other form passes when it evaluates truthy

  All variants take an optional trailing message string and return `#t` or
  `#f`. An error raised by the form is caught and recorded as an error, not
  a crash.

- `(use-fixtures 'each f)` wraps every test in fixture `f`, a function
  `(lambda (run) setup (run) teardown)`. `(use-fixtures 'once f)` wraps the
  whole run. Fixtures compose in registration order, first registered
  outermost. Teardown of `'each` fixtures runs even when the test body
  raises.
- `(run-tests!)` runs the suite, prints a summary, and raises when any
  failure or error was recorded, so file mode exits nonzero. The normal
  last form of a test file.
- `(run-tests)` is the non-raising variant; returns the stats hash.
- `(test-stats)` returns counters: `'tests 'assertions 'passes 'failures
'errors`.
- `(reset-tests!)` clears the registry, fixtures, and counters.

## Failure output

```sh
FAIL in (addition) [basic]
  (= 4 (add 2 2))
  expected: 4
  actual:   5
Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
```

## Running multiple files

```sh
sh tests/run-all.sh
```

Copy `tests/run-all.sh` into your project, or loop `steel tests/test-*.scm`
and aggregate exit codes.

## License

Copyright © 2026 Tom Waddington

Distributed under the MIT License. See LICENSE file for details.
