package merge

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"strings"
	"time"

	"agent-repld/internal/wsm"
)

// THE INTAKE'S FACTS (owner ruling 3, 2026-11-14). The intake runs on a
// weaker model that only states problems, so the daemon does the sorting it
// would otherwise reason out: every warning and error since the deploy began
// is grouped by its operation and message, and each group is marked NEW (no
// record of it in the same span before the deploy) or SEEN BEFORE (with its
// count then). Only a NEW group can be a problem the deploy caused; a SEEN
// BEFORE group is filed elsewhere and never holds the queue (ruling 1).

// LogRecord is one warning or error a system logged, as the log reader read
// it.
type LogRecord struct {
	Level     string
	Operation string
	Message   string
	// Raw is the record's line, verbatim: the evidence the intake quotes.
	Raw string
}

// LogReader reads every warning and error every system logged from SINCE
// through UNTIL (bin/logs.sh --all --level warn), for the repository at
// REPODIR, whose reader it runs.
type LogReader func(ctx context.Context, repoDir string, since, until time.Time) ([]LogRecord, error)

// ParseLogLines reads the JSONL the log reader prints into records, keeping
// each line verbatim. The reader's own account of its sinks shares its
// output and is no record: only a line opening a JSON object is one, and one
// that does not decode is an error naming it.
func ParseLogLines(text string) ([]LogRecord, error) {
	var out []LogRecord
	for i, line := range strings.Split(text, "\n") {
		if !strings.HasPrefix(line, "{") {
			continue
		}
		var rec struct {
			Level     string `json:"level"`
			Operation string `json:"operation"`
			Message   string `json:"message"`
		}
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			return nil, fmt.Errorf("merge: log line %d is no record: %w", i+1, err)
		}
		out = append(out, LogRecord{Level: rec.Level, Operation: rec.Operation, Message: rec.Message, Raw: line})
	}
	return out, nil
}

// FindingGroup is the window's records of one operation and message.
type FindingGroup struct {
	Operation string
	Message   string
	// Level is the most severe level among them.
	Level string
	// Count is how many the window holds, and Before how many the same span
	// before the deploy held: 0 is a NEW group.
	Count  int
	Before int
	// Sample is the group's first record, verbatim.
	Sample string
}

// New reports whether the group was not seen in the span before the deploy.
func (g FindingGroup) New() bool { return g.Before == 0 }

// SortFindings groups WINDOW's records by operation and message against
// BEFORE's, the same span before the deploy: NEW groups first, then the most
// severe and the most frequent.
func SortFindings(window, before []LogRecord) []FindingGroup {
	type key struct{ op, msg string }
	seen := map[key]int{}
	for _, r := range before {
		seen[key{r.Operation, r.Message}]++
	}
	groups := map[key]*FindingGroup{}
	var order []key
	for _, r := range window {
		k := key{r.Operation, r.Message}
		g, ok := groups[k]
		if !ok {
			g = &FindingGroup{Operation: r.Operation, Message: r.Message, Level: r.Level, Before: seen[k], Sample: r.Raw}
			groups[k] = g
			order = append(order, k)
		}
		g.Count++
		if severity(r.Level) > severity(g.Level) {
			g.Level = r.Level
		}
	}
	out := make([]FindingGroup, 0, len(order))
	for _, k := range order {
		out = append(out, *groups[k])
	}
	sort.SliceStable(out, func(i, j int) bool {
		a, b := out[i], out[j]
		if a.New() != b.New() {
			return a.New()
		}
		if severity(a.Level) != severity(b.Level) {
			return severity(a.Level) > severity(b.Level)
		}
		return a.Count > b.Count
	})
	return out
}

// severity orders the levels a finding carries.
func severity(level string) int {
	switch strings.ToLower(level) {
	case "error":
		return 2
	case "warn", "warning":
		return 1
	}
	return 0
}

// RenderFindings is the brief's facts: one block per group, its sample
// quoted verbatim, or a line saying the window holds none.
func RenderFindings(groups []FindingGroup) string {
	if len(groups) == 0 {
		return "No system logged a warning or an error since the deploy began."
	}
	var b strings.Builder
	for i, g := range groups {
		if i > 0 {
			b.WriteString("\n")
		}
		standing := "NEW"
		if !g.New() {
			standing = fmt.Sprintf("SEEN BEFORE (%d in the span before)", g.Before)
		}
		fmt.Fprintf(&b, "- %s: %d %s of %s: %s\n  %s\n", standing, g.Count, strings.ToUpper(g.Level), g.Operation, g.Message, g.Sample)
	}
	return b.String()
}

// intakeBrief composes the intake's brief of check M on queue Q: the deploy,
// the chain so far against its bound, and the window's findings sorted NEW
// or SEEN BEFORE against the same span before the deploy.
func (o *orchestrator) intakeBrief(ctx context.Context, q checkedQueue, m wsm.Mediation) (string, error) {
	now := o.deps.Now()
	span := now.Sub(m.OwedAt)
	window, err := o.deps.ReadLogs(ctx, q.repository.Dir, m.OwedAt, now)
	if err != nil {
		return "", fmt.Errorf("merge: read the deploy's window: %w", err)
	}
	before, err := o.deps.ReadLogs(ctx, q.repository.Dir, m.OwedAt.Add(-span), m.OwedAt)
	if err != nil {
		return "", fmt.Errorf("merge: read the span before the deploy: %w", err)
	}
	chain, err := o.chainSoFar(ctx, q, m)
	if err != nil {
		return "", err
	}
	return o.deps.Briefs(BriefDeployIntake, map[string]string{
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"repo_dir":       q.repository.Dir,
		"version":        mediationVersion(m),
		"chain":          chain,
		"bound":          fmt.Sprint(q.bound),
		"facts":          RenderFindings(SortFindings(window, before)),
	})
}

// chainSoFar is the brief's account of check M's chain: its first deploy, or
// how many remediations it deployed in a row and what each landed.
func (o *orchestrator) chainSoFar(ctx context.Context, q checkedQueue, m wsm.Mediation) (string, error) {
	if m.ChainRedeploys == 0 && m.Remediation == "" {
		return "this is the chain's first deploy; no remediation has been dispatched.", nil
	}
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		return "", fmt.Errorf("merge: read what the chain's remediations landed: %w", err)
	}
	var b strings.Builder
	fmt.Fprintf(&b, "the chain began at %s and has deployed %d remediations in a row; its last remediation workspace is %s.",
		m.ChainStartedAt.UTC().Format(time.RFC3339), m.ChainRedeploys, o.workspaceName(ctx, m.Remediation))
	for _, f := range finishes {
		if !sameQueue(f.Repo, q.key) || f.LandedHead == "" {
			continue
		}
		fmt.Fprintf(&b, " %s landed %s, changing %s.", o.workspaceName(ctx, f.Workspace), f.LandedHead, strings.Join(f.LandedPaths, ", "))
	}
	return b.String(), nil
}
