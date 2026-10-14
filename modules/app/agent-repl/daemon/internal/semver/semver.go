// Package semver is agent-repl's own version arithmetic: the bare
// `MAJOR.MINOR.PATCH` tags the merge queue reads and writes on agent-repl's
// master (docs/protobuf-design/merge-version-increment.md).
//
// A TAG IS A VERSION ONLY WHEN IT IS EXACTLY THREE DECIMAL NUMBERS. No `v`
// prefix, no pre-release or build suffix, no leading zeros: every other tag a
// repository carries (`start/*`, `wip-*`, a stray `v0.0.1`) is not a version
// and is ignored rather than refused, because those tags exist for other
// reasons and are none of the version line's business.
package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Part is the version number a merge moves. MAJOR is not a Part: it moves
// only by a human tagging master by hand, so the queue has no way to ask for
// it.
type Part int

const (
	// Minor is a new capability or a changed behavior, breaking or not: MINOR
	// goes up by one and PATCH resets to zero.
	Minor Part = iota + 1
	// Patch is a fix or internal change: PATCH goes up by one.
	Patch
)

// String names the part for the log record and the command file.
func (p Part) String() string {
	switch p {
	case Minor:
		return "minor"
	case Patch:
		return "patch"
	}
	return fmt.Sprintf("Part(%d)", int(p))
}

// ParsePart reads a part from its String spelling.
func ParsePart(s string) (Part, error) {
	switch s {
	case "minor":
		return Minor, nil
	case "patch":
		return Patch, nil
	}
	return 0, fmt.Errorf("semver: %q is not a version part (minor or patch)", s)
}

// Valid reports whether p is one of the declared parts.
func (p Part) Valid() bool { return p == Minor || p == Patch }

// Version is one `MAJOR.MINOR.PATCH`.
type Version struct {
	Major, Minor, Patch int
}

// String spells the version as its tag.
func (v Version) String() string {
	return fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
}

// Parse reads a version tag. ok is false for anything that is not exactly
// three decimal numbers without leading zeros.
func Parse(tag string) (Version, bool) {
	fields := strings.Split(tag, ".")
	if len(fields) != 3 {
		return Version{}, false
	}
	var nums [3]int
	for i, f := range fields {
		if f == "" || (len(f) > 1 && f[0] == '0') {
			return Version{}, false
		}
		for _, c := range f {
			if c < '0' || c > '9' {
				return Version{}, false
			}
		}
		n, err := strconv.Atoi(f)
		if err != nil {
			return Version{}, false
		}
		nums[i] = n
	}
	return Version{Major: nums[0], Minor: nums[1], Patch: nums[2]}, true
}

// Less orders versions numerically.
func (v Version) Less(w Version) bool {
	if v.Major != w.Major {
		return v.Major < w.Major
	}
	if v.Minor != w.Minor {
		return v.Minor < w.Minor
	}
	return v.Patch < w.Patch
}

// Highest answers the highest version among tags, ignoring every tag that is
// not a version. ok is false when none is.
func Highest(tags []string) (Version, bool) {
	var (
		best  Version
		found bool
	)
	for _, tag := range tags {
		v, ok := Parse(tag)
		if !ok {
			continue
		}
		if !found || best.Less(v) {
			best, found = v, true
		}
	}
	return best, found
}

// Next is v moved by part. A part that is not declared is a defect in the
// caller, and panics: every part reaching here was validated where it entered.
func (v Version) Next(part Part) Version {
	switch part {
	case Minor:
		return Version{Major: v.Major, Minor: v.Minor + 1}
	case Patch:
		return Version{Major: v.Major, Minor: v.Minor, Patch: v.Patch + 1}
	}
	panic(fmt.Sprintf("semver: no increment for %s", part))
}

// Split is a version spelled around the number part moved: the text before
// it, the number, and the text after it (empty when part is the last number).
// The three concatenate to the version's tag.
func (v Version) Split(part Part) (leading, incremented, trailing string) {
	switch part {
	case Minor:
		return fmt.Sprintf("%d.", v.Major), strconv.Itoa(v.Minor), fmt.Sprintf(".%d", v.Patch)
	case Patch:
		return fmt.Sprintf("%d.%d.", v.Major, v.Minor), strconv.Itoa(v.Patch), ""
	}
	panic(fmt.Sprintf("semver: no split for %s", part))
}
