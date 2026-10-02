package suites

import (
	"fmt"
	"strconv"
	"strings"
)

// ItemLinePrefix opens every per-item timing line a chunk prints:
//
//	TESTRUN-ITEM <item> <seconds>
//
// The ERT driver (testrun/ert/driver.el) prints one per test file, and
// bin/lib-test-split.sh one per harness group. ParseItemLines is the one
// reader of that line, so the two producers are held to one contract.
const ItemLinePrefix = "TESTRUN-ITEM"

// ParseItemLines reads a chunk's item timing lines and insists on exactly one
// for every item the chunk was given. A line that opens with the prefix but
// is not a well-formed timing is an error, never noise: it is the chunk
// breaking its timing contract.
func ParseItemLines(out []byte, want []string) (map[string]float64, error) {
	got := map[string]float64{}
	for line := range strings.SplitSeq(string(out), "\n") {
		if !strings.HasPrefix(line, ItemLinePrefix+" ") {
			continue
		}
		fields := strings.Fields(line)
		if len(fields) != 3 {
			return nil, fmt.Errorf("unreadable item timing %q", line)
		}
		secs, err := strconv.ParseFloat(fields[2], 64)
		if err != nil || secs < 0 {
			return nil, fmt.Errorf("unreadable item timing %q", line)
		}
		if _, dup := got[fields[1]]; dup {
			return nil, fmt.Errorf("item %s reported twice", fields[1])
		}
		got[fields[1]] = secs
	}
	return got, matchItems(got, want)
}

// matchItems insists the reported items are exactly the wanted ones.
func matchItems(got map[string]float64, want []string) error {
	var missing []string
	for _, w := range want {
		if _, ok := got[w]; !ok {
			missing = append(missing, w)
		}
	}
	if len(missing) > 0 {
		return fmt.Errorf("no timing reported for %v", missing)
	}
	if len(got) != len(want) {
		return fmt.Errorf("timings reported for %d items, but the chunk ran %d", len(got), len(want))
	}
	return nil
}
