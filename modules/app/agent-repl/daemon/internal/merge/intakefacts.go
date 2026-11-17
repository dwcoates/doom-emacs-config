package merge

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"strings"
	"time"

	"agent-repld/internal/ids"
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
	// Class is how the group stands against the problems the repository's
	// deploy checks keep (classifyFindings); Owner names the problem's owner
	// and Findings what its owners were told, for an owned or recurred one.
	Class    wsm.WindowClass
	Owner    string
	Findings string
	// Fixes is how many fixes of the group's problem deployed.
	Fixes int
}

// Key is the group's problem key.
func (g FindingGroup) Key() wsm.ProblemKey {
	return wsm.ProblemKey{Operation: g.Operation, Message: g.Message}
}

// classifyFindings stands each of GROUPS against PROBLEMS, the problems the
// repository's deploy checks keep, naming each problem's owner through NAME:
// a group a problem names is OWNED while its owner is at work and RECURRED
// once its fix deployed; any other is NEW or SEEN BEFORE by the span before.
// An owner whose name cannot be read is NAME's error, answered.
func classifyFindings(groups []FindingGroup, problems []wsm.DeployProblem, name func(ids.WorkspaceID) (string, error)) ([]FindingGroup, error) {
	byKey := make(map[wsm.ProblemKey]wsm.DeployProblem, len(problems))
	for _, p := range problems {
		byKey[p.Key] = p
	}
	out := make([]FindingGroup, len(groups))
	for i, g := range groups {
		p, kept := byKey[g.Key()]
		switch {
		case kept && p.FixedAt != nil:
			g.Class = wsm.WindowRecurred
		case kept:
			g.Class = wsm.WindowOwned
		case g.New():
			g.Class = wsm.WindowNew
		default:
			g.Class = wsm.WindowSeenBefore
		}
		if kept {
			owner, err := name(p.Owner)
			if err != nil {
				return nil, err
			}
			g.Owner, g.Findings, g.Fixes = owner, p.Findings, p.Fixes
		}
		out[i] = g
	}
	return out, nil
}

// windowGroups is GROUPS as a check's window records them.
func windowGroups(groups []FindingGroup) []wsm.WindowGroup {
	out := make([]wsm.WindowGroup, 0, len(groups))
	for _, g := range groups {
		out = append(out, wsm.WindowGroup{Operation: g.Operation, Message: g.Message, Class: g.Class, Fixes: g.Fixes})
	}
	return out
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

// RenderFindings is the brief's facts: one block per group, its standing,
// its sample quoted verbatim and, for a problem the repository keeps, its
// owner and its earlier findings, or a line saying the window holds none.
func RenderFindings(groups []FindingGroup) string {
	if len(groups) == 0 {
		return "No system logged a warning or an error in the window."
	}
	var b strings.Builder
	for i, g := range groups {
		if i > 0 {
			b.WriteString("\n")
		}
		var standing string
		switch g.Class {
		case wsm.WindowNew:
			standing = "NEW"
		case wsm.WindowSeenBefore:
			standing = fmt.Sprintf("SEEN BEFORE, owned by nobody (%d in the span before)", g.Before)
		case wsm.WindowOwned:
			standing = fmt.Sprintf("OWNED by %s, still at work on it", g.Owner)
		case wsm.WindowRecurred:
			standing = fmt.Sprintf("RECURRED after %s's fix deployed", g.Owner)
			if pastItsFixes(g.Class, g.Fixes) {
				standing = fmt.Sprintf("RECURRED after %s's fix deployed, after %d fixes in all: only an escalation is accepted", g.Owner, g.Fixes)
			}
		default:
			panic(fmt.Sprintf("merge: a finding group with no class: %+v", g))
		}
		b.WriteString(fmt.Sprintf("- %s: %d %s of %s: %s\n  %s\n", standing, g.Count, strings.ToUpper(g.Level), g.Operation, g.Message, g.Sample))
		if g.Findings != "" {
			b.WriteString("  Its owners were told: " + strings.ReplaceAll(g.Findings, "\n", " / ") + "\n")
		}
	}
	return b.String()
}

// intakeBrief composes the intake's brief of check M on queue Q: the deploy,
// the chain so far against its bound, and the window's findings, each stood
// against the span before the window and the problems the repository's
// deploy checks keep. It answers the window as the brief tells it.
//
// THE WINDOW STARTS WHERE THE REPOSITORY'S LAST CHECK READ TO, never at the
// deploy's start (owner ruling, 2026-11-16): a landing logs between its
// merge and its deploy, and a window from the deploy's start left those
// records unread by every check (the durable log sink warning, whose
// landing removed the worktree just before the deploy began). An intake told
// before, whose turn is reattached, is told the window it was told then.
func (o *orchestrator) intakeBrief(ctx context.Context, q checkedQueue, m wsm.Mediation) (string, wsm.CheckWindow, error) {
	now := o.deps.Now()
	since, err := o.windowStart(ctx, q.key, m)
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	span := now.Sub(since)
	window, err := o.deps.ReadLogs(ctx, q.repository.Dir, since, now)
	if err != nil {
		return "", wsm.CheckWindow{}, fmt.Errorf("merge: read the deploy's window: %w", err)
	}
	before, err := o.deps.ReadLogs(ctx, q.repository.Dir, since.Add(-span), since)
	if err != nil {
		return "", wsm.CheckWindow{}, fmt.Errorf("merge: read the span before the window: %w", err)
	}
	problems, err := o.deps.DB.DeployProblems(ctx, q.key)
	if err != nil {
		return "", wsm.CheckWindow{}, fmt.Errorf("merge: read the repository's deploy problems: %w", err)
	}
	groups, err := classifyFindings(SortFindings(window, before), problems, func(ws ids.WorkspaceID) (string, error) {
		return o.workspaceName(ctx, opMediation, ws)
	})
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	chain, err := o.chainSoFar(ctx, q, m)
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	gate, gated, err := lastGateReuseBefore(LogsDir(o.deps.StateDir), m.OwedAt)
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	brief, err := o.deps.Briefs(BriefDeployIntake, map[string]string{
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"ending":         RenderEnding(m.DeployEnding),
		"window_started": since.UTC().Format(time.RFC3339),
		"repo_dir":       q.repository.Dir,
		"version":        mediationVersion(m),
		"chain":          chain,
		"bound":          fmt.Sprint(q.bound),
		"facts":          RenderFindings(groups),
		"reuse":          RenderReuse(gate, gated),
	})
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	return brief, wsm.CheckWindow{Start: since, Through: now, Groups: windowGroups(groups)}, nil
}

// windowStart is where check M's window starts: the start of the window its
// intake was told, when it was told one; otherwise where REPO's last check
// read to; the deploy's start for a repository never checked, or a check a
// previous build told, which recorded no window.
func (o *orchestrator) windowStart(ctx context.Context, repo wsm.RepoKey, m wsm.Mediation) (time.Time, error) {
	if m.Prompted() {
		told, found, err := o.deps.DB.MediationWindowOf(ctx, repo)
		if err != nil {
			return time.Time{}, fmt.Errorf("merge: read the window the intake was told: %w", err)
		}
		if found {
			return told.Start, nil
		}
		return m.OwedAt, nil
	}
	through, found, err := o.deps.DB.CheckedThrough(ctx, repo)
	if err != nil {
		return time.Time{}, fmt.Errorf("merge: read where the last check read to: %w", err)
	}
	if found {
		return through, nil
	}
	return m.OwedAt, nil
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
	last, err := o.workspaceName(ctx, opMediation, m.Remediation)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString(fmt.Sprintf("the chain began at %s and has deployed %d remediations in a row; its last remediation workspace is %s.",
		m.ChainStartedAt.UTC().Format(time.RFC3339), m.ChainRedeploys, last))
	for _, f := range finishes {
		if !sameQueue(f.Repo, q.key) || f.LandedHead == "" {
			continue
		}
		name, err := o.workspaceName(ctx, opMediation, f.Workspace)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf(" %s landed %s, changing %s.", name, f.LandedHead, strings.Join(f.LandedPaths, ", ")))
	}
	return b.String(), nil
}

// RenderEnding is how the checked deploy ended, as the intake's brief says
// it: a deploy that stopped short is a problem of the deploy itself, its
// records in the window like any other's.
func RenderEnding(ending wsm.DeployEnding) string {
	switch ending {
	case wsm.DeployFinished:
		return "It finished: every component built and rolled out."
	case wsm.DeployUnchanged:
		return "It finished, changing nothing."
	case wsm.DeployStoppedShort:
		return "It STOPPED SHORT of finishing: a build, a restart or a rollout failed or could not finish. That failure is this deploy's problem; its records are among the findings below."
	case "":
		return "How it ended was not recorded; read it from the findings below."
	}
	panic(fmt.Sprintf("merge: a deploy ending the store never records: %q", ending))
}
