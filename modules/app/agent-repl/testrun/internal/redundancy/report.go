package redundancy

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"slices"
	"sort"
	"strings"
	"time"
)

// MutantResult is one mutant's fate.
type MutantResult struct {
	Key string
	// Status is killed, survived (reached, no case failed), unreached (no
	// case reaches it) or dropped (its site did not type-check).
	Status   string
	KilledBy []string `json:",omitempty"`
}

// Cost is a measurement's own price.
type Cost struct {
	Wall      time.Duration
	CPU       time.Duration
	Processes int
	// Phases is the wall time of each phase, in order.
	Phases []Phase
}

// Phase is one step's wall time.
type Phase struct {
	Name string
	Wall time.Duration
}

// Measurement is what one adapter run found: every candidate case and every
// mutant. It is written as JSON so a report can be redrawn without measuring
// again.
type Measurement struct {
	// Adapter is the language adapter that measured it ("go").
	Adapter string
	// Subject names the test files measured.
	Subject string
	// Scope names what was mutated.
	Scope []string
	// Modules are the module paths the measured blocks belong to; each is
	// a system of the coverage budget.
	Modules []string `json:",omitempty"`
	Cases   []Case
	Mutants []MutantResult
	// Anomalies are failures the measurement could not attribute.
	Anomalies []string `json:",omitempty"`
	// Statements maps every block of the measured scope, reached or not, to
	// its statement count: what the coverage budget weighs.
	Statements map[string]int `json:",omitempty"`
	// Hangs are the kills that were a case hanging alone past its bound,
	// listed because load, not the mutant, can cause one.
	Hangs []string `json:",omitempty"`
	Cost  Cost
}

// WriteMeasurement saves m to path.
func WriteMeasurement(path string, m *Measurement) error {
	b, err := json.MarshalIndent(m, "", "  ")
	if err != nil {
		return fmt.Errorf("redundancy: encode the measurement: %w", err)
	}
	if err := os.WriteFile(path, append(b, '\n'), 0o644); err != nil {
		return fmt.Errorf("redundancy: write %s: %w", path, err)
	}
	return nil
}

// ReadMeasurement loads a measurement WriteMeasurement saved.
func ReadMeasurement(path string) (*Measurement, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("redundancy: read %s: %w", path, err)
	}
	var m Measurement
	if err := json.Unmarshal(b, &m); err != nil {
		return nil, fmt.Errorf("redundancy: decode %s: %w", path, err)
	}
	return &m, nil
}

// Options are what a report is drawn under.
type Options struct {
	Universe Universe
	Overhead time.Duration
	Budget   Budget
}

// budgeted is one universe's minimization held to the coverage budget.
func budgeted(m *Measurement, u Universe, o Options) (Result, []string, []SystemCoverage, error) {
	r, err := Minimize(m.Cases, u, o.Overhead)
	if err != nil {
		return Result{}, nil, nil, err
	}
	return ApplyBudget(m.Cases, r, m.Statements, m.Modules, o.Budget, o.Overhead)
}

// Report writes the markdown report of m: the minimization under the chosen
// universe held to the coverage budget, each system's coverage around it, the
// cases the other universe disagrees on, the mutants and the measurement's
// own cost.
func Report(w io.Writer, m *Measurement, o Options) error {
	u := o.Universe
	main, restored, systems, err := budgeted(m, u, o)
	if err != nil {
		return err
	}
	other := Strict
	if u == Strict {
		other = KillsOnly
	}
	alt, _, _, err := budgeted(m, other, o)
	if err != nil {
		return err
	}
	var b strings.Builder
	var total time.Duration
	pinned := 0
	for _, c := range m.Cases {
		total += c.Cost
		if c.Pinned != "" {
			pinned++
		}
	}
	saved := total - main.KeptCost
	fmt.Fprintf(&b, "# Redundancy: %s\n\n", m.Subject)
	fmt.Fprintf(&b, "- Adapter: %s; mutated: %s\n", m.Adapter, strings.Join(m.Scope, ", "))
	fmt.Fprintf(&b, "- Universe: %s (every %s the whole set catches stays caught)\n", u, universeWhat(u))
	fmt.Fprintf(&b, "- Cases: %d, pinned: %d, removable: %d, kept: %d\n", len(m.Cases), pinned, len(main.Removed), len(main.Kept))
	fmt.Fprintf(&b, "- Case runtime: %s total, %s kept, %s saved\n", round(total), round(main.KeptCost), round(saved))
	fmt.Fprintf(&b, "- Proven cheapest: %t\n\n", main.Optimal)

	b.WriteString("## Statement coverage\n\n")
	b.WriteString("| System | Reference | Statements | Before | After | Drop | Floor |\n|---|---|---|---|---|---|---|\n")
	for _, sc := range systems {
		fmt.Fprintf(&b, "| %s | %s | %d | %.2f%% | %.2f%% | %.2f | %.2f%% |\n", sc.System, sc.Reference, sc.Statements, sc.Before, sc.After, sc.Drop(), sc.Floor)
	}
	fmt.Fprintf(&b, "\nAfter counts a statement lost when only removed cases reach it, so it errs low. Request limit: %.2f points.\n\n", o.Budget.MaxDrop)
	if len(restored) > 0 {
		fmt.Fprintf(&b, "Put back by the budget (%d), best statements per runtime first:\n\n", len(restored))
		for _, id := range restored {
			fmt.Fprintf(&b, "- `%s`\n", id)
		}
		b.WriteString("\n")
	}

	b.WriteString("## Mutants\n\n")
	status := map[string]int{}
	for _, r := range m.Mutants {
		status[r.Status]++
	}
	for _, s := range []string{"killed", "survived", "unreached", "dropped"} {
		fmt.Fprintf(&b, "- %s: %d\n", s, status[s])
	}
	if reached := status["killed"] + status["survived"]; reached > 0 {
		fmt.Fprintf(&b, "- score over reached mutants: %.1f%%\n", 100*float64(status["killed"])/float64(reached))
	}
	b.WriteString("\n")

	fmt.Fprintf(&b, "## Removable cases (%s)\n\n", u)
	if len(main.Removed) == 0 {
		b.WriteString("None.\n\n")
	} else {
		b.WriteString("| Case | Saves | Caught instead by |\n|---|---|---|\n")
		for _, r := range main.Removed {
			by := strings.Join(r.CoveredBy, ", ")
			if r.DominatedBy != "" {
				by = "dominated by " + r.DominatedBy
			}
			if by == "" {
				by = "nothing: it catches nothing in this universe"
			}
			fmt.Fprintf(&b, "| `%s` | %s | %s |\n", r.Case, round(r.Saves), by)
		}
		b.WriteString("\n")
	}

	mainSet := removedSet(main)
	altSet := removedSet(alt)
	var diff []Removal
	for _, r := range alt.Removed {
		if !mainSet[r.Case] {
			diff = append(diff, r)
		}
	}
	var only []Removal
	for _, r := range main.Removed {
		if !altSet[r.Case] {
			only = append(only, r)
		}
	}
	fmt.Fprintf(&b, "## Under %s instead\n\n", other)
	fmt.Fprintf(&b, "- Removable: %d, saving %s\n", len(alt.Removed), round(total-alt.KeptCost))
	writeDiff(&b, fmt.Sprintf("Removable under %s only", other), diff)
	writeDiff(&b, fmt.Sprintf("Removable under %s only", u), only)

	if len(m.Anomalies) > 0 {
		b.WriteString("## Anomalies\n\n")
		for _, a := range m.Anomalies {
			fmt.Fprintf(&b, "- %s\n", a)
		}
		b.WriteString("\n")
	}

	if len(m.Hangs) > 0 {
		fmt.Fprintf(&b, "## Kills by hang (%d)\n\n", len(m.Hangs))
		for _, h := range m.Hangs {
			fmt.Fprintf(&b, "- %s\n", h)
		}
		b.WriteString("\n")
	}

	b.WriteString("## Pinned cases\n\n")
	var pins []string
	for _, c := range m.Cases {
		if c.Pinned != "" {
			pins = append(pins, fmt.Sprintf("- `%s`: %s\n", c.ID, c.Pinned))
		}
	}
	if len(pins) == 0 {
		b.WriteString("None.\n")
	}
	for _, p := range pins {
		b.WriteString(p)
	}
	b.WriteString("\n## Measurement cost\n\n")
	fmt.Fprintf(&b, "- Wall: %s, CPU: %s, processes: %d\n", round(m.Cost.Wall), round(m.Cost.CPU), m.Cost.Processes)
	for _, p := range m.Cost.Phases {
		fmt.Fprintf(&b, "- %s: %s\n", p.Name, round(p.Wall))
	}
	_, err = io.WriteString(w, b.String())
	if err != nil {
		return fmt.Errorf("redundancy: write the report: %w", err)
	}
	return nil
}

func universeWhat(u Universe) string {
	if u == Strict {
		return "killed mutant and reached block"
	}
	return "killed mutant"
}

func removedSet(r Result) map[string]bool {
	out := map[string]bool{}
	for _, rm := range r.Removed {
		out[rm.Case] = true
	}
	return out
}

func writeDiff(b *strings.Builder, title string, rs []Removal) {
	var saves time.Duration
	names := make([]string, 0, len(rs))
	for _, r := range rs {
		saves += r.Saves
		names = append(names, r.Case)
	}
	sort.Strings(names)
	fmt.Fprintf(b, "- %s: %d, saving %s\n", title, len(rs), round(saves))
	for _, n := range slices.Compact(names) {
		fmt.Fprintf(b, "  - `%s`\n", n)
	}
	b.WriteString("\n")
}

// round prints a duration at a resolution its measurement supports.
func round(d time.Duration) string {
	switch {
	case d >= time.Second:
		return d.Round(10 * time.Millisecond).String()
	case d >= time.Millisecond:
		return d.Round(10 * time.Microsecond).String()
	}
	return d.Round(time.Microsecond).String()
}

// Logger is the module's one logging path (run.Log), as the report command
// uses it.
type Logger interface {
	Errorf(format string, args ...any)
}

// ReportCommand is `testrun redundancy report`: redraw a saved measurement's
// report, under any universe, without measuring again.
func ReportCommand(log Logger, argv []string, out io.Writer) int {
	fs := flag.NewFlagSet("testrun redundancy report", flag.ContinueOnError)
	in := fs.String("in", "", "a measurement.json (required)")
	universe := fs.String("universe", "kills-only", "kills-only or strict")
	overhead := fs.Duration("overhead", 100*time.Microsecond, "fixed weight per kept case")
	maxDrop := fs.Float64("max-coverage-drop", DefaultMaxDrop, "the most statement-coverage points any system may lose")
	baseline := fs.String("baseline", "", "the pinned coverage baseline (testrun/coverage-baseline.json); without it each system is held to the measured population's own coverage")
	if err := fs.Parse(argv); err != nil {
		log.Errorf("redundancy report: %v", err)
		return 2
	}
	if *in == "" || fs.NArg() != 0 {
		log.Errorf("redundancy report: -in is required and nothing else is taken")
		return 2
	}
	u, err := ParseUniverse(*universe)
	if err != nil {
		log.Errorf("redundancy report: %v", err)
		return 2
	}
	m, err := ReadMeasurement(*in)
	if err != nil {
		log.Errorf("redundancy report: %v", err)
		return 1
	}
	b, err := LoadBudget(*maxDrop, *baseline)
	if err != nil {
		log.Errorf("redundancy report: %v", err)
		return 1
	}
	if err := Report(out, m, Options{Universe: u, Overhead: *overhead, Budget: b}); err != nil {
		log.Errorf("redundancy report %s: %v", *in, err)
		return 1
	}
	return 0
}
