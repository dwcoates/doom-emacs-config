package merge

import (
	"context"
	"encoding/json"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE WINDOW'S GROUPS. Every warning and error since the repository's last
// check is grouped by its operation and message, and each group is stood
// against the span before the window and the problems the mediator took on.
// The groups are what a decision is judged against (DecideMediation); the
// brief states none of them (intakeBrief): the mediator reads the logs
// itself.

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
//
// A RECORD MARKED TRANSIENT (dlog.TransientKey) is left out: a passing
// outage of something outside agent-repl is no problem a deploy caused or a
// fixer could fix (owner ruling, 2026-11-17).
func ParseLogLines(text string) ([]LogRecord, error) {
	var out []LogRecord
	for i, line := range strings.Split(text, "\n") {
		if !strings.HasPrefix(line, "{") {
			continue
		}
		var rec struct {
			Level     string         `json:"level"`
			Operation string         `json:"operation"`
			Message   string         `json:"message"`
			Context   map[string]any `json:"context"`
		}
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			return nil, fmt.Errorf("merge: log line %d is no record: %w", i+1, err)
		}
		if transient, _ := rec.Context[dlog.TransientKey].(bool); transient {
			continue
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
	// deploy checks keep (classifyFindings); Findings is what the mediator
	// stated of the problem, for an owned or recurred one.
	Class    wsm.WindowClass
	Findings string
	// Fixes is how many fixes of the group's problem deployed.
	Fixes int
}

// Key is the group's problem key.
func (g FindingGroup) Key() wsm.ProblemKey {
	return wsm.ProblemKey{Operation: g.Operation, Message: g.Message}
}

// classifyFindings stands each of GROUPS against PROBLEMS, the problems the
// mediator took on: a group a problem names is TAKEN ON while the mediator is
// at work on it and RECURRED once its fix deployed; any other is NEW or SEEN
// BEFORE by the span before.
func classifyFindings(groups []FindingGroup, problems []wsm.DeployProblem) []FindingGroup {
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
			g.Findings, g.Fixes = p.Findings, p.Fixes
		}
		out[i] = g
	}
	return out
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

// LeftOut is one kind of record a window's read left out, and how many of
// the window's records were of it.
type LeftOut struct {
	Kind  wsm.IgnoredRecordKind
	Count int
}

// leaveOut drops from RECORDS every record of one of KINDS, answering what
// is left and, per kind that matched, how many it dropped, in KINDS' order.
func leaveOut(records []LogRecord, kinds []wsm.IgnoredRecordKind) ([]LogRecord, []LeftOut) {
	counts := make([]int, len(kinds))
	kept := make([]LogRecord, 0, len(records))
	for _, r := range records {
		dropped := false
		for i, k := range kinds {
			if k.Matches(r.Operation, r.Message) {
				counts[i]++
				dropped = true
				break
			}
		}
		if !dropped {
			kept = append(kept, r)
		}
	}
	var left []LeftOut
	for i, k := range kinds {
		if counts[i] > 0 {
			left = append(left, LeftOut{Kind: k, Count: counts[i]})
		}
	}
	return kept, left
}

// IgnoreRecords implements Orchestrator.
func (o *orchestrator) IgnoreRecords(ctx context.Context, ws ids.WorkspaceID, kind wsm.IgnoredRecordKind) error {
	fields := dlog.Context{"workspace": string(ws), "operation": kind.Operation, "message_contains": kind.MessageContains, "why": kind.Why}
	if err := o.mediatorOnly(ctx, ws, fields); err != nil {
		return err
	}
	kind.AddedAt = o.deps.Now()
	if err := o.deps.DB.IgnoreRecordKind(ctx, kind); err != nil {
		o.deps.Log.Global().Error(opMediation, "the kind of record to leave out of the deploy checks could not be recorded", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opMediation, "the deploy mediator recorded a kind of record the deploy checks leave out", fields)
	return nil
}

// UnignoreRecords implements Orchestrator.
func (o *orchestrator) UnignoreRecords(ctx context.Context, ws ids.WorkspaceID, operation, messageContains string) error {
	fields := dlog.Context{"workspace": string(ws), "operation": operation, "message_contains": messageContains}
	if err := o.mediatorOnly(ctx, ws, fields); err != nil {
		return err
	}
	removed, err := o.deps.DB.UnignoreRecordKind(ctx, operation, messageContains)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the kind of record the deploy checks leave out could not be withdrawn", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opMediation, "the deploy mediator withdrew a kind of record the deploy checks leave out", withField(fields, "was_recorded", removed))
	return nil
}

// mediatorOnly refuses WS unless it is a repository's deploy mediator.
func (o *orchestrator) mediatorOnly(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) error {
	_, isMediator, err := o.deps.DB.MediatorRepoOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read whether the workspace changing the ignored records is a deploy mediator", withField(fields, "error", err.Error()))
		return err
	}
	if !isMediator {
		o.deps.Log.Global().Warn(opMediation, "refused a change to the ignored records from a workspace that is no repository's deploy mediator", fields)
		return refuse(ArmNotMediator, ws, "this workspace is no repository's deploy mediator")
	}
	return nil
}

// DigestSite is one logging site's records in a window: every record one
// operation logged at one level.
type DigestSite struct {
	Level     string
	Operation string
	Count     int
	// Sample is the first of its records, verbatim.
	Sample string
}

// DigestSites groups WINDOW by the site that logged each record: its
// operation and level. A record's MESSAGE is left out of the key on purpose:
// messages carry workspace names, page ids and paths, so one site logging
// about thirty workspaces is one line here, not thirty. The sites are sorted
// errors first, then by count.
func DigestSites(window []LogRecord) []DigestSite {
	type key struct{ level, operation string }
	sites := map[key]*DigestSite{}
	var order []key
	for _, r := range window {
		k := key{strings.ToLower(r.Level), r.Operation}
		site, ok := sites[k]
		if !ok {
			site = &DigestSite{Level: k.level, Operation: r.Operation, Sample: r.Raw}
			sites[k] = site
			order = append(order, k)
		}
		site.Count++
	}
	out := make([]DigestSite, 0, len(order))
	for _, k := range order {
		out = append(out, *sites[k])
	}
	sort.SliceStable(out, func(i, j int) bool {
		if severity(out[i].Level) != severity(out[j].Level) {
			return severity(out[i].Level) > severity(out[j].Level)
		}
		return out[i].Count > out[j].Count
	})
	return out
}

// RenderDigest is the digest file's text: a table of every logging site with
// its count, then one verbatim sample record per site, in the same order.
func RenderDigest(sites []DigestSite, left []LeftOut, since, through time.Time) string {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("Warnings and errors from %s through %s, one line per logging site (level and operation).\n\n",
		since.UTC().Format(time.RFC3339), through.UTC().Format(time.RFC3339)))
	b.WriteString("COUNT  LEVEL  OPERATION\n")
	for _, site := range sites {
		b.WriteString(fmt.Sprintf("%5d  %-5s  %s\n", site.Count, site.Level, site.Operation))
	}
	b.WriteString("\nOne sample record per site, verbatim, in the same order:\n\n")
	for _, site := range sites {
		b.WriteString(site.Sample + "\n")
	}
	if len(left) > 0 {
		b.WriteString("\nLeft out, as kinds of record known to be transient or ignorable (count, operation, message text, why):\n\n")
		for _, l := range left {
			b.WriteString(fmt.Sprintf("%5d  %s  %q  %s\n", l.Count, l.Kind.Operation, l.Kind.MessageContains, l.Kind.Why))
		}
	}
	return b.String()
}

// DigestPath is where the digest of the check owed at OWED is written, in
// the queue's logs directory DIR. It is one of the queue's logs (IsLog), so
// the retention pass prunes it with the rest.
func DigestPath(dir string, owed time.Time) string {
	return filepath.Join(dir, fmt.Sprintf("deploy_check-%d.log", owed.Unix()))
}

// writeDigest writes WINDOW's digest for the check owed at OWED and answers
// its path.
func (o *orchestrator) writeDigest(window []LogRecord, left []LeftOut, owed, since, through time.Time) (string, error) {
	path := DigestPath(LogsDir(o.deps.StateDir), owed)
	if err := writeLog(path, RenderDigest(DigestSites(window), left, since, through)); err != nil {
		return "", fmt.Errorf("merge: write the check's digest: %w", err)
	}
	return path, nil
}

// intakeBrief composes the intake's brief of check M on queue Q, and answers
// the window the check covers.
//
// THE BRIEF STATES THE DEPLOY AND WHAT THE MEDIATOR HAS TO INVESTIGATE IT
// WITH, AND NOTHING IT FOUND (owner ruling, 2026-11-19). The mediator reads
// the logs, the metrics and the doctor itself and decides what matters; the
// brief names where they are and what each decision does. The window's
// records are still read and sorted here, for the window the store keeps
// (CheckWindow.Groups), which a decision is judged against.
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
	// THE KINDS OF RECORD KNOWN TO BE TRANSIENT OR IGNORABLE ARE LEFT OUT
	// (wsm ignoredrecords.go), of the window and of the span before it alike:
	// a window holding only such records holds nothing for the mediator.
	kinds, err := o.deps.DB.IgnoredRecordKinds(ctx)
	if err != nil {
		return "", wsm.CheckWindow{}, fmt.Errorf("merge: read the kinds of record the checks leave out: %w", err)
	}
	window, left := leaveOut(window, kinds)
	before, _ = leaveOut(before, kinds)
	problems, err := o.deps.DB.DeployProblems(ctx, q.key)
	if err != nil {
		return "", wsm.CheckWindow{}, fmt.Errorf("merge: read the repository's deploy problems: %w", err)
	}
	groups := classifyFindings(SortFindings(window, before), problems)
	gate, gated, err := lastGateReuseBefore(LogsDir(o.deps.StateDir), m.OwedAt)
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	gateLog := "none logged one"
	if gated {
		gateLog = "`" + gate.Path + "`"
	}
	// THE DIGEST IS WRITTEN BEFORE THE MEDIATOR IS TOLD, so its first read is
	// one small file rather than every record: each logging site once, with
	// its count and one sample.
	digest, err := o.writeDigest(window, left, m.OwedAt, since, now)
	if err != nil {
		return "", wsm.CheckWindow{}, err
	}
	brief, err := o.deps.Briefs(BriefDeployIntake, map[string]string{
		"occasion":       RenderOccasion(m.DeployEnding, q.repository.Dir, m.OwedAt, mediationVersion(m)),
		"ending":         RenderEnding(m.DeployEnding),
		"window_started": since.UTC().Format(time.RFC3339),
		"gate_log":       gateLog,
		"digest":         digest,
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

// RenderOccasion is the brief's first sentence: what the check owed at OWED
// in REPO checks. A check of a deploy names the deploy and the build it
// installed (VERSION); a check of a crash names no deploy, since none ran:
// the daemon restarted because the one before it died without its orderly
// exit.
func RenderOccasion(ending wsm.DeployEnding, repo string, owed time.Time, version string) string {
	at := owed.UTC().Format(time.RFC3339)
	if ending == wsm.DaemonCrashed {
		return fmt.Sprintf("No deploy ran: the daemon serving `%s` restarted at %s (%s) because the one before it died without its orderly exit.", repo, at, version)
	}
	return fmt.Sprintf("A deploy of `%s` began at %s (%s).", repo, at, version)
}

// RenderEnding is how the checked deploy ended, as the intake's brief says
// it: a deploy that stopped short is a problem of the deploy itself, its
// records in the window like any other's.
func RenderEnding(ending wsm.DeployEnding) string {
	switch ending {
	case wsm.DaemonCrashed:
		return "Find what killed it and fix it: `daemon.boot.predecessor_died` in the digest names the panic and the site it died at."
	case wsm.DeployFinished:
		return "It finished: every component built and rolled out."
	case wsm.DeployUnchanged:
		return "It finished, changing nothing."
	case wsm.DeployStoppedShort:
		return "It STOPPED SHORT of finishing: a build, a restart or a rollout failed or could not finish."
	case "":
		return "How it ended was not recorded."
	}
	panic(fmt.Sprintf("merge: a deploy ending the store never records: %q", ending))
}
