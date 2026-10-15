package gomeasure

import (
	"regexp"
	"strings"
	"time"
)

// Outcome is a test's result line.
type Outcome int

const (
	Pass Outcome = iota + 1
	Fail
	Skip
)

// Run is one test binary's -test.v output, read.
type Run struct {
	// Ran are the names a `=== RUN` line started.
	Ran map[string]bool
	// Outcomes are the names a `--- PASS|FAIL|SKIP` line ended.
	Outcomes map[string]Outcome
	// Elapsed is, per name, from its `=== RUN` to the first line after it
	// that ends it: a result line, or the start of a test outside it.
	Elapsed map[string]time.Duration
	// TimedOut are the tests a `-test.timeout` panic says were running.
	TimedOut []string
}

var (
	runLine     = regexp.MustCompile(`^=== RUN\s+(\S+)$`)
	resultLine  = regexp.MustCompile(`^\s*--- (PASS|FAIL|SKIP): (\S+) \(`)
	runningLine = regexp.MustCompile(`^\s+(\S+) \(`)
)

// ParseRun reads a test binary's -test.v output.
func ParseRun(lines []Line) Run {
	r := Run{Ran: map[string]bool{}, Outcomes: map[string]Outcome{}, Elapsed: map[string]time.Duration{}}
	started := map[string]time.Duration{}
	var open []string
	closeOpen := func(at time.Duration, keep func(string) bool) {
		var still []string
		for _, n := range open {
			if keep(n) {
				still = append(still, n)
				continue
			}
			r.Elapsed[n] = at - started[n]
		}
		open = still
	}
	for i := 0; i < len(lines); i++ {
		l := lines[i]
		if m := runLine.FindStringSubmatch(l.Text); m != nil {
			name := m[1]
			closeOpen(l.At, func(n string) bool { return strings.HasPrefix(name, n+"/") })
			r.Ran[name] = true
			started[name] = l.At
			open = append(open, name)
			continue
		}
		if m := resultLine.FindStringSubmatch(l.Text); m != nil {
			closeOpen(l.At, func(string) bool { return false })
			r.Outcomes[m[2]] = map[string]Outcome{"PASS": Pass, "FAIL": Fail, "SKIP": Skip}[m[1]]
			continue
		}
		if strings.HasPrefix(l.Text, "panic: test timed out after ") {
			for i+1 < len(lines) && strings.TrimSpace(lines[i+1].Text) != "running tests:" {
				i++
			}
			for i++; i+1 < len(lines); i++ {
				m := runningLine.FindStringSubmatch(lines[i+1].Text)
				if m == nil {
					break
				}
				r.TimedOut = append(r.TimedOut, m[1])
			}
		}
	}
	return r
}

// RunPattern is the -test.run pattern that selects exactly the named test,
// a subtest by every level of its name.
func RunPattern(name string) string {
	parts := strings.Split(name, "/")
	for i, p := range parts {
		parts[i] = "^" + regexp.QuoteMeta(p) + "$"
	}
	return strings.Join(parts, "/")
}

// TopPattern selects every named top-level test.
func TopPattern(tops []string) string {
	q := make([]string, len(tops))
	for i, t := range tops {
		q[i] = regexp.QuoteMeta(t)
	}
	return "^(" + strings.Join(q, "|") + ")$"
}

// Top is the top-level test a case belongs to.
func Top(name string) string {
	top, _, _ := strings.Cut(name, "/")
	return top
}
