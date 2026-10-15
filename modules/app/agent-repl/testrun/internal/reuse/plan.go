package reuse

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"

	"agentrepl/testrun/roster"
)

// PlanVersion is the `testrun plan --json` shape's version. A field may be
// added without a bump; renaming or removing one bumps it.
const PlanVersion = 1

// PlanDoc is `testrun plan --json`: what a run with the same arguments would
// run and reuse, without running anything.
type PlanDoc struct {
	Version int `json:"version"`
	// NoReuseFlag is the flag that forces every selected suite to run.
	NoReuseFlag string `json:"no_reuse_flag"`
	// ReuseAvailable is false when this host cannot confine a suite (the
	// planner is itself sandboxed): then every suite runs and none records.
	ReuseAvailable bool        `json:"reuse_available"`
	Suites         []PlanSuite `json:"suites"`
	// Unplaced are the checkout's files no suite reads and no list places; a
	// run with any fails its placement check.
	Unplaced []string `json:"unplaced"`
}

// PlanSuite is one selected suite's line of the plan.
type PlanSuite struct {
	Suite string `json:"suite"`
	// Kind is "unit+integration" for a keyed system suite, "e2e" for the
	// cross-system suites, "harness" for the script suites.
	Kind     string `json:"kind"`
	InputKey string `json:"input_key,omitempty"`
	Reused   bool   `json:"reused"`
	// Why is why a suite runs: "always runs", "no record", "changed since its
	// newest green record: <inputs>", "--no-reuse", ... Empty when reused.
	Why string `json:"why,omitempty"`
	// PassedAt, Branch, Sha and Dirty name the run a reused suite's record
	// came from. Dirty means Sha does not hold the passed bytes.
	PassedAt *time.Time `json:"passed_at,omitempty"`
	Branch   string     `json:"branch,omitempty"`
	Sha      string     `json:"sha,omitempty"`
	Dirty    bool       `json:"dirty,omitempty"`
	// Width is how many core slots the suite's widest unit holds.
	Width int `json:"width"`
	// Command runs exactly this suite, for a suite that will run.
	Command string `json:"command,omitempty"`
	// Cases, when set, narrows the suite to named tests, each group with the
	// one targeted command that runs exactly its tests: the merge gate's
	// failing-test brief shape. Unset until per-test selection exists.
	Cases []PlanCases `json:"cases,omitempty"`
}

// PlanCases is one targeted invocation and the tests it runs.
type PlanCases struct {
	Command string   `json:"command"`
	Tests   []string `json:"tests"`
}

// SuiteKind names a roster entry's kind for the plan.
func SuiteKind(s roster.Suite) string {
	switch {
	case s.Reuse != nil:
		return "unit+integration"
	case s.Kind == roster.E2E || s.Name == "e2e-emacs":
		return "e2e"
	case s.Kind == roster.Script || s.Kind == roster.SplitScript:
		return "harness"
	}
	return "unit+integration"
}

// BuildPlan is the plan document for verdicts and the placement result.
func BuildPlan(verdicts []Verdict, unplaced []string, reuseAvailable bool) PlanDoc {
	doc := PlanDoc{Version: PlanVersion, NoReuseFlag: "--no-reuse", ReuseAvailable: reuseAvailable, Unplaced: unplaced}
	if doc.Unplaced == nil {
		doc.Unplaced = []string{}
	}
	for _, v := range verdicts {
		ps := PlanSuite{Suite: v.Suite.Name, Kind: SuiteKind(v.Suite), Width: max(v.Suite.Slots, 1)}
		if v.Keyed {
			ps.InputKey = v.Key.Digest
		}
		if r := v.Reused; r != nil {
			ps.Reused = true
			at := r.PassedAt
			ps.PassedAt, ps.Branch, ps.Sha, ps.Dirty = &at, r.Branch, r.Commit, r.Dirty
		} else {
			ps.Why = v.Why
			ps.Command = "bin/test-all.sh --suites " + v.Suite.Name
		}
		doc.Suites = append(doc.Suites, ps)
	}
	return doc
}

// WritePlanJSON prints doc as one JSON document.
func WritePlanJSON(w io.Writer, doc PlanDoc) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	if err := enc.Encode(doc); err != nil {
		return fmt.Errorf("reuse: encode the plan: %w", err)
	}
	return nil
}

// WritePlanText prints doc as lines a person reads.
func WritePlanText(w io.Writer, doc PlanDoc) error {
	var b strings.Builder
	if !doc.ReuseAvailable {
		b.WriteString("reuse is unavailable here (no sandbox): every suite runs\n")
	}
	for _, s := range doc.Suites {
		switch {
		case s.Reused:
			src := s.Branch + " " + short(s.Sha)
			if s.Dirty {
				src += " with uncommitted changes"
			}
			fmt.Fprintf(&b, "%-28s reuse   inputs %s passed %s on %s\n", s.Suite, short(s.InputKey), s.PassedAt.Local().Format("2006-01-02 15:04"), src)
		default:
			fmt.Fprintf(&b, "%-28s run     %s: %s\n", s.Suite, s.Command, s.Why)
		}
	}
	for _, f := range doc.Unplaced {
		fmt.Fprintf(&b, "placement: %s is read by no suite and listed nowhere\n", f)
	}
	fmt.Fprintf(&b, "%s runs every selected suite regardless of earlier passes\n", doc.NoReuseFlag)
	_, err := io.WriteString(w, b.String())
	return err
}

func short(s string) string {
	if len(s) > 12 {
		return s[:12]
	}
	return s
}
