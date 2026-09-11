//go:build realtest

package realtest

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// THE MANIFEST IS THE ARTIFACT THE OWNER RULES ON. Step 3 of the realtest loop
// is "surface them to the owner, with evidence, VERBATIM. Nothing is fixed
// here" (docs/REALTEST-PLAN.md), which sets two requirements this file exists
// to meet:
//
//	Every finding's original line is reproduced UNCHANGED. Not summarized, not
//	pretty-printed, not deduplicated. A paraphrase is not evidence, and a
//	verdict the owner cannot check against the raw line is not one they can
//	rule on.
//
//	The measurements sit BESIDE the findings in one document. A slow phase and a
//	warning are the same kind of question — "is this how it should read?" — and
//	splitting them across two files makes the second one the one nobody opens.

// Manifest is one run's whole account.
type Manifest struct {
	Title string
	// Started and Ended bound the harvest window.
	Started time.Time
	Ended   time.Time
	// Notes are the run's own statements: which launch method was used, which
	// left focus alone, whether the daemon was adopted or booted, whether the
	// phase budgets were enforced. Each is one line.
	Notes []string
	// Runs are the per-cold-start measurement tables.
	Runs []ManifestRun
	// Findings is every warning, error, malformed line, stderr line,
	// attribution conflict and rotation the harvest produced.
	Findings []Finding
	// InfoCounts is source -> operation -> count, reported and never failing.
	InfoCounts map[string]map[string]int
	// Workspaces is what the state database held, so the report says what the
	// assertion was made against.
	Workspaces []Workspace
	// BudgetBreaches is every phase over budget, already phrased.
	BudgetBreaches []string
}

// ManifestRun is one cold start.
type ManifestRun struct {
	Index        int
	Method       LaunchMethod
	SpawnedAt    time.Time
	DaemonPath   string
	FrontBefore  string
	FrontAfter   string
	Disturbed    bool
	Measurements []Measurement
}

// Write renders the manifest to dir/MANIFEST.md.
func (m Manifest) Write(dir string) (string, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("create the run directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, "MANIFEST.md")
	if err := os.WriteFile(path, []byte(m.render()), 0o644); err != nil {
		return "", fmt.Errorf("write %s: %w", path, err)
	}
	// The full harvest travels WITH the manifest, always: the manifest reports
	// a repeated class once with its count, and the file beside it is where
	// every one of those records is kept verbatim.
	if _, err := WriteFullHarvest(dir, m.Findings); err != nil {
		return "", err
	}
	return path, nil
}

func (m Manifest) render() string {
	var b strings.Builder

	fmt.Fprintf(&b, "# %s\n\n", m.Title)
	fmt.Fprintf(&b, "Run window: `%s` to `%s` (%s).\n\n",
		m.Started.Format(time.RFC3339Nano), m.Ended.Format(time.RFC3339Nano),
		m.Ended.Sub(m.Started).Round(time.Millisecond))

	if len(m.Notes) > 0 {
		b.WriteString("## What the run did\n\n")
		for _, note := range m.Notes {
			fmt.Fprintf(&b, "- %s\n", note)
		}
		b.WriteString("\n")
	}

	b.WriteString("## Workspaces the state database holds\n\n")
	if len(m.Workspaces) == 0 {
		b.WriteString("None. Every per-workspace assertion below is vacuous, which is\n")
		b.WriteString("itself worth reading: a realtest against an empty roster measures\n")
		b.WriteString("nothing about workspaces.\n\n")
	} else {
		b.WriteString("| id | name | directory |\n|---|---|---|\n")
		for _, ws := range m.Workspaces {
			fmt.Fprintf(&b, "| `%s` | %s | `%s` |\n", ws.ID, ws.Name, ws.Dir)
		}
		b.WriteString("\n")
	}

	for _, run := range m.Runs {
		fmt.Fprintf(&b, "## Cold start\n\n")
		fmt.Fprintf(&b, "- launch method: `%s`\n", run.Method)
		fmt.Fprintf(&b, "- spawned at: `%s`\n", run.SpawnedAt.Format(time.RFC3339Nano))
		fmt.Fprintf(&b, "- daemon: %s\n", orUnknown(run.DaemonPath))
		// The launch method is repeated ON the focus line rather than left to
		// the line above it: whether focus moved is a fact ABOUT a method.
		fmt.Fprintf(&b, "- frontmost application before: %s; after: %s%s (launch method `%s`)\n",
			orUnknown(run.FrontBefore), orUnknown(run.FrontAfter),
			map[bool]string{true: " — **FOCUS MOVED**", false: " — focus unchanged"}[run.Disturbed],
			run.Method)
		b.WriteString("\n| phase | workspace | from spawn | budget |\n|---|---|---|---|\n")
		for _, measurement := range run.Measurements {
			budget := "not measured yet"
			if b2, ok := BudgetFor(measurement.Phase); ok && b2.Limit != unmeasured {
				budget = b2.Limit.String()
			}
			elapsed := measurement.Note
			if elapsed == "" {
				elapsed = measurement.Elapsed.Round(time.Millisecond).String()
			}
			fmt.Fprintf(&b, "| %s | %s | %s | %s |\n",
				phaseLabel(measurement.Phase), measurement.Workspace, elapsed, budget)
		}
		b.WriteString("\n")
	}

	if len(m.BudgetBreaches) > 0 {
		b.WriteString("## Phases over budget\n\n")
		for _, breach := range m.BudgetBreaches {
			fmt.Fprintf(&b, "- %s\n", breach)
		}
		b.WriteString("\n")
	}

	b.WriteString("## The log harvest\n\n")
	fmt.Fprintf(&b, "%d finding(s). There is no allowlist: a realtest is remediated if and only\n", len(m.Findings))
	b.WriteString("if every one of these is resolved.\n\n")
	if len(m.Findings) == 0 {
		b.WriteString("No warning, error, malformed record, stray stderr line, attribution\nconflict or rotation inside the run window.\n\n")
	} else {
		fmt.Fprintf(&b, "A finding class repeated inside one workspace is reported ONCE, with its\n")
		fmt.Fprintf(&b, "count and one sample record. Every record is kept verbatim in `%s`\n", fullHarvestName)
		b.WriteString("beside this file; nothing is dropped and no count is hidden.\n\n")
		byWorkspace := make(map[string][]Finding)
		for _, finding := range m.Findings {
			byWorkspace[finding.Workspace] = append(byWorkspace[finding.Workspace], finding)
		}
		keys := make([]string, 0, len(byWorkspace))
		for key := range byWorkspace {
			keys = append(keys, key)
		}
		sort.Strings(keys)
		for _, key := range keys {
			fmt.Fprintf(&b, "### %s\n\n", key)
			for _, class := range CollapseFindings(byWorkspace[key]) {
				renderClass(&b, class)
			}
			b.WriteString("\n")
		}
	}

	b.WriteString("## Unexpected INFO\n\n")
	b.WriteString("Counts per operation, inside the window. These do NOT fail a run: a count\n")
	b.WriteString("that jumps is a lead, not a verdict.\n\n")
	if len(m.InfoCounts) == 0 {
		b.WriteString("No info record inside the window.\n")
	} else {
		sources := make([]string, 0, len(m.InfoCounts))
		for source := range m.InfoCounts {
			sources = append(sources, source)
		}
		sort.Strings(sources)
		for _, source := range sources {
			fmt.Fprintf(&b, "### %s\n\n| operation | count |\n|---|---|\n", source)
			operations := make([]string, 0, len(m.InfoCounts[source]))
			for operation := range m.InfoCounts[source] {
				operations = append(operations, operation)
			}
			sort.Slice(operations, func(i, j int) bool {
				a, bb := m.InfoCounts[source][operations[i]], m.InfoCounts[source][operations[j]]
				if a != bb {
					return a > bb
				}
				return operations[i] < operations[j]
			})
			for _, operation := range operations {
				fmt.Fprintf(&b, "| `%s` | %d |\n", operation, m.InfoCounts[source][operation])
			}
			b.WriteString("\n")
		}
	}

	return b.String()
}

// phaseLabel renders a measurement's phase name for the manifest table.
// PhasePanelPainted (and, downstream of it, PhaseTotal) get an explicit
// qualifier: both are now measured in the SHOW phase, not the hidden one
// (owner ruling 2026-09-11, phases.go says why), and a reader skimming the
// table for "why is panel-painted's elapsed time so much larger than
// tab-drawn's" should not have to go find that out from the source.
func phaseLabel(phase PhaseName) string {
	switch phase {
	case PhasePanelPainted:
		return "panel-painted (on first show)"
	case PhaseTotal:
		return "total (spawn to shown-and-painted)"
	default:
		return string(phase)
	}
}

func orUnknown(value string) string {
	if value == "" {
		return "(not established)"
	}
	return value
}

// renderClass writes one finding class: the count, the sample verbatim, and
// nothing paraphrased. A class of one reads exactly as a single finding did
// before the collapsing existed, because it is one.
func renderClass(b *strings.Builder, class FindingClass) {
	finding := class.Sample
	at := "(no timestamp)"
	if !finding.Timestamp.IsZero() {
		at = finding.Timestamp.Format(time.RFC3339Nano)
	}
	fmt.Fprintf(b, "- **%s** in `%s` (`%s`", finding.Kind, finding.Source, finding.Path)
	if finding.Line > 0 {
		fmt.Fprintf(b, " line %d", finding.Line)
	}
	fmt.Fprintf(b, ") at %s\n", at)
	if class.Count > 1 {
		fmt.Fprintf(b, "  - **%d records** of operation `%s` at level `%s`, identical in why they are a finding. "+
			"One sample follows; all %d are in `%s`.\n",
			class.Count, class.Operation, orUnknown(class.Level), class.Count, fullHarvestName)
	}
	if finding.Note != "" {
		fmt.Fprintf(b, "  - %s\n", finding.Note)
	}
	if finding.Raw != "" {
		fmt.Fprintf(b, "  - verbatim:\n\n    ```\n    %s\n    ```\n", finding.Raw)
	}
}
