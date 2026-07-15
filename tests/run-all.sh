#!/bin/sh
# Run every test file in file mode and aggregate exit codes. Run from the
# repo root or any directory: paths resolve relative to this script.

dir=$(dirname "$0")
fail=0
for t in "$dir"/test-*.scm; do
    if steel "$t" >/dev/null 2>&1; then
        echo "PASS $t"
    else
        echo "FAIL $t"
        steel "$t" 2>&1 | sed 's/^/    /'
        fail=1
    fi
done
exit $fail
