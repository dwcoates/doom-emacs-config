package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"sort"
	"strings"
	"time"
)

// HeldReason names the authoritative fact that prevented spool attribution.
// These codes are deliberately diagnostic facts, never guessed identities.
type HeldReason string

const (
	HeldReasonAwaitingOwner HeldReason = "missing_live_launch_observation"
	HeldReasonHistorical    HeldReason = "historical_spool_absent_from_open_task_state"
	HeldReasonClosed        HeldReason = "closed_task_or_session"
	HeldReasonConflict      HeldReason = "conflicting_authoritative_owner"
	HeldReasonInvalid       HeldReason = "no_authoritative_owner"
)

// HeldTarget is the file-plane evidence lifecycle classification needs. ModTime
// must come from stat of the discovered file; lifecycle code never reads files.
type HeldTarget struct {
	Path    string
	Root    string
	TaskID  string
	ModTime time.Time
}

// HeldEvidence contains only authoritative lifecycle facts. A false active or
// open value means the corresponding authoritative source was queried and says
// the task or session is not open; unknown is not represented as false.
type HeldEvidence struct {
	ModTime               time.Time
	ActiveTaskKnown       bool
	ActiveTask            bool
	OpenSessionKnown      bool
	OpenSession           bool
	CursorPastLaunchKnown bool
	CursorPastLaunch      bool
}

// HeldState determines whether a spool is rechecked or permanently retired.
type HeldState string

const (
	HeldStateResolved HeldState = "resolved"
	HeldStateActive   HeldState = "active"
	HeldStateTerminal HeldState = "terminal"
)

// HeldDecision reports the action the caller must take. Only Resolved carries
// a session and lifecycle never derives that session from a path.
type HeldDecision struct {
	State     HeldState
	Reason    HeldReason
	SessionID string
}

// HeldSample is safe for bounded diagnostic reporting: PathHash never exposes
// a cross-project temporary path, while task and root remain actionable.
type HeldSample struct {
	PathHash  string
	TaskID    string
	Root      string
	Reason    HeldReason
	AgeBucket string
}

// HeldSnapshot separates active ingestion risk from terminal historical data.
type HeldSnapshot struct {
	ActiveCount   int
	TerminalTotal int
	ByReason      map[HeldReason]int
	ByRoot        map[string]int
	ByAgeBucket   map[string]int
	Samples       []HeldSample
}

// HeldReadiness deliberately ignores terminal history. A large historical
// population is visible in diagnostics but cannot keep a healthy live service
// unready after all active unresolved spools have drained.
type HeldReadiness struct {
	Ready              bool
	ActiveUnresolved   int
	ActiveThreshold    int
	HistoricalTerminal int
	Reason             string
}

// HeldLogRecord is the canonical lifecycle reporting boundary. The sidecar
// supplies a reporter backed by internal/logging; tests use an in-memory sink.
type HeldLogRecord struct {
	Operation string
	State     HeldState
	Reason    HeldReason
	PathHash  string
	TaskID    string
	Root      string
	AgeBucket string
	Message   string
	Verbose   bool
}

type heldEntry struct {
	target HeldTarget
	reason HeldReason
}

// HeldLifecycle owns active and terminal spool classifications. Terminal
// entries are never reconsidered: retrying them would recreate the backlog.
type HeldLifecycle struct {
	active      map[string]heldEntry
	terminal    map[string]heldEntry
	sampleLimit int
	report      func(HeldLogRecord)
	lastReport  string
}

// NewHeldLifecycle creates an empty lifecycle. A nil reporter violates the
// sidecar logging contract and is rejected rather than silently disabling logs.
func NewHeldLifecycle(sampleLimit int, report func(HeldLogRecord)) *HeldLifecycle {
	if sampleLimit <= 0 {
		panic("sidecar held lifecycle requires a positive sample limit")
	}
	if report == nil {
		panic("sidecar held lifecycle requires canonical reporting")
	}
	l := &HeldLifecycle{active: map[string]heldEntry{}, terminal: map[string]heldEntry{}, sampleLimit: sampleLimit, report: report}
	l.report(HeldLogRecord{Operation: "held-lifecycle-new", Message: fmt.Sprintf("sample_limit=%d", sampleLimit), Verbose: true})
	return l
}

// Observe classifies one spool using an owner result supplied by owner.go.
// Owner lookup is intentionally absent: lifecycle consumes its structured
// outcome and proves terminality only from authoritative evidence.
func (l *HeldLifecycle) Observe(target HeldTarget, owner OwnerResolution, evidence HeldEvidence, now time.Time) (HeldDecision, error) {
	l.report(HeldLogRecord{Operation: "held-observe", PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, Message: fmt.Sprintf("owner_outcome=%s owner_source=%s", owner.Outcome, owner.Source), Verbose: true})
	if err := validateHeldInput(target, owner, evidence, now); err != nil {
		l.report(HeldLogRecord{Operation: "held-observe", PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, Message: "error=" + err.Error()})
		return HeldDecision{}, err
	}
	if terminal, ok := l.terminal[target.Path]; ok {
		decision := HeldDecision{State: HeldStateTerminal, Reason: terminal.reason}
		l.report(HeldLogRecord{Operation: "held-transition", State: decision.State, Reason: decision.Reason, PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, AgeBucket: heldAgeBucket(now.Sub(target.ModTime)), Message: "already terminal", Verbose: true})
		return decision, nil
	}

	if owner.Outcome == OwnerResolvedPath || owner.Outcome == OwnerResolvedTask {
		if err := validateResolvedOwner(owner); err != nil {
			l.report(HeldLogRecord{Operation: "held-observe", PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, Message: "error=" + err.Error()})
			return HeldDecision{}, err
		}
		delete(l.active, target.Path)
		decision := HeldDecision{State: HeldStateResolved, SessionID: owner.SessionID}
		l.report(HeldLogRecord{Operation: "held-transition", State: decision.State, PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, AgeBucket: heldAgeBucket(now.Sub(target.ModTime)), Message: "authoritative owner resolved", Verbose: true})
		return decision, nil
	}
	if err := validateUnresolvedEvidence(evidence); err != nil {
		l.report(HeldLogRecord{Operation: "held-observe", PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, Message: "error=" + err.Error()})
		return HeldDecision{}, err
	}

	if terminallyAbsent(target, evidence, now) {
		reason := HeldReasonHistorical
		entry := heldEntry{target: target, reason: reason}
		l.terminal[target.Path] = entry
		delete(l.active, target.Path)
		decision := HeldDecision{State: HeldStateTerminal, Reason: reason}
		l.report(HeldLogRecord{Operation: "held-transition", State: decision.State, Reason: decision.Reason, PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, AgeBucket: heldAgeBucket(now.Sub(target.ModTime)), Message: "terminal historical or closed spool without session assignment", Verbose: true})
		return decision, nil
	}
	if owner.Outcome == OwnerUnresolvedConflict || (owner.Outcome == OwnerUnresolvedInvalid && (evidence.ActiveTask || evidence.OpenSession)) {
		reason := terminalHeldReason(owner, evidence)
		l.active[target.Path] = heldEntry{target: target, reason: reason}
		decision := HeldDecision{State: HeldStateActive, Reason: reason}
		l.report(HeldLogRecord{Operation: "held-transition", State: decision.State, Reason: decision.Reason, PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, AgeBucket: heldAgeBucket(now.Sub(target.ModTime)), Message: "active ownership anomaly remains unassigned", Verbose: true})
		return decision, nil
	}

	if owner.MayArrive() {
		entry := heldEntry{target: target, reason: HeldReasonAwaitingOwner}
		l.active[target.Path] = entry
		decision := HeldDecision{State: HeldStateActive, Reason: entry.reason}
		l.report(HeldLogRecord{Operation: "held-transition", State: decision.State, Reason: decision.Reason, PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, AgeBucket: heldAgeBucket(now.Sub(target.ModTime)), Message: "awaiting named authoritative owner source", Verbose: true})
		return decision, nil
	}

	reason := terminalHeldReason(owner, evidence)
	entry := heldEntry{target: target, reason: reason}
	l.terminal[target.Path] = entry
	delete(l.active, target.Path)
	decision := HeldDecision{State: HeldStateTerminal, Reason: reason}
	l.report(HeldLogRecord{Operation: "held-transition", State: decision.State, Reason: decision.Reason, PathHash: heldPathHash(target.Path), TaskID: target.TaskID, Root: target.Root, AgeBucket: heldAgeBucket(now.Sub(target.ModTime)), Message: "terminal without assigning a session", Verbose: true})
	return decision, nil
}

func validateHeldInput(target HeldTarget, owner OwnerResolution, evidence HeldEvidence, now time.Time) error {
	if target.Path == "" || target.Root == "" || target.TaskID == "" || target.ModTime.IsZero() {
		return fmt.Errorf("held target requires path root task_id and stat modtime")
	}
	if !evidence.ModTime.IsZero() && !evidence.ModTime.Equal(target.ModTime) {
		return fmt.Errorf("held evidence modtime differs from stat modtime")
	}
	if target.ModTime.After(now) {
		return fmt.Errorf("spool stat modtime is in the future")
	}
	if owner.TaskID != target.TaskID || owner.OutputPath != target.Path {
		return fmt.Errorf("owner resolution does not identify observed task and output path")
	}
	return nil
}

func validateResolvedOwner(owner OwnerResolution) error {
	if owner.SessionID == "" {
		return fmt.Errorf("resolved owner outcome lacks session id")
	}
	return nil
}

func validateUnresolvedEvidence(evidence HeldEvidence) error {
	if !evidence.ActiveTaskKnown {
		return fmt.Errorf("unresolved lifecycle requires authoritative open-task evidence")
	}
	return nil
}

func terminalHeldReason(owner OwnerResolution, evidence HeldEvidence) HeldReason {
	if evidence.CursorPastLaunchKnown && evidence.CursorPastLaunch {
		return HeldReasonHistorical
	}
	if owner.Outcome == OwnerUnresolvedConflict {
		return HeldReasonConflict
	}
	if owner.Outcome == OwnerUnresolvedInvalid {
		return HeldReasonInvalid
	}
	if !evidence.ActiveTask && !evidence.OpenSession {
		return HeldReasonClosed
	}
	panic(fmt.Sprintf("sidecar held lifecycle received non-retryable unknown owner outcome=%s", owner.Outcome))
}

// terminallyAbsent retires an old spool only after the authoritative open-task
// snapshot says its task is absent. That establishes a historical spool even
// when the original launch's transcript cannot be identified for a cursor.
func terminallyAbsent(target HeldTarget, evidence HeldEvidence, now time.Time) bool {
	return now.Sub(target.ModTime) >= UnownedSpoolWindow && !evidence.ActiveTask
}

// Snapshot returns bounded diagnostics for a health endpoint or log report.
func (l *HeldLifecycle) Snapshot(now time.Time) HeldSnapshot {
	s := HeldSnapshot{ByReason: map[HeldReason]int{}, ByRoot: map[string]int{}, ByAgeBucket: map[string]int{}}
	entries := make([]heldEntry, 0, len(l.active)+len(l.terminal))
	for _, e := range l.active {
		entries = append(entries, e)
	}
	for _, e := range l.terminal {
		entries = append(entries, e)
	}
	s.ActiveCount, s.TerminalTotal = len(l.active), len(l.terminal)
	sort.Slice(entries, func(i, j int) bool { return entries[i].target.Path < entries[j].target.Path })
	for _, e := range entries {
		bucket := heldAgeBucket(now.Sub(e.target.ModTime))
		s.ByReason[e.reason]++
		s.ByRoot[e.target.Root]++
		s.ByAgeBucket[bucket]++
		if len(s.Samples) < l.sampleLimit {
			s.Samples = append(s.Samples, HeldSample{PathHash: heldPathHash(e.target.Path), TaskID: e.target.TaskID, Root: e.target.Root, Reason: e.reason, AgeBucket: bucket})
		}
	}
	s.ByRoot = boundHeldRoots(s.ByRoot, l.sampleLimit)
	fingerprint := heldSnapshotFingerprint(s)
	if fingerprint != l.lastReport {
		l.lastReport = fingerprint
		l.report(HeldLogRecord{Operation: "held-report", Message: fmt.Sprintf("active=%d terminal=%d reasons=%v roots=%v ages=%v samples=%v", s.ActiveCount, s.TerminalTotal, s.ByReason, s.ByRoot, s.ByAgeBucket, s.Samples)})
	}
	return s
}

func boundHeldRoots(counts map[string]int, limit int) map[string]int {
	if len(counts) <= limit {
		return counts
	}
	type rootCount struct {
		root  string
		count int
	}
	roots := make([]rootCount, 0, len(counts))
	for root, count := range counts {
		roots = append(roots, rootCount{root, count})
	}
	sort.Slice(roots, func(i, j int) bool {
		if roots[i].count != roots[j].count {
			return roots[i].count > roots[j].count
		}
		return roots[i].root < roots[j].root
	})
	bounded, other := map[string]int{}, 0
	for i, entry := range roots {
		if i < limit {
			bounded[entry.root] = entry.count
		} else {
			other += entry.count
		}
	}
	bounded["other_roots"] = other
	return bounded
}

func heldSnapshotFingerprint(s HeldSnapshot) string {
	parts := []string{fmt.Sprintf("active=%d", s.ActiveCount), fmt.Sprintf("terminal=%d", s.TerminalTotal)}
	appendCounts := func(counts map[string]int) {
		keys := make([]string, 0, len(counts))
		for key := range counts {
			keys = append(keys, key)
		}
		sort.Strings(keys)
		for _, key := range keys {
			parts = append(parts, key+"="+fmt.Sprint(counts[key]))
		}
		parts = append(parts, "|")
	}
	reasons := map[string]int{}
	for key, count := range s.ByReason {
		reasons[string(key)] = count
	}
	appendCounts(reasons)
	appendCounts(s.ByRoot)
	appendCounts(s.ByAgeBucket)
	for _, sample := range s.Samples {
		parts = append(parts, string(sample.Reason)+":"+sample.PathHash+":"+sample.TaskID+":"+sample.Root+":"+sample.AgeBucket)
	}
	return strings.Join(parts, ",")
}

// Readiness evaluates only active unresolved work against its explicit limit.
func (l *HeldLifecycle) Readiness(activeThreshold int, now time.Time) HeldReadiness {
	if activeThreshold < 0 {
		panic("sidecar held lifecycle readiness threshold must not be negative")
	}
	s := l.Snapshot(now)
	reason := "active unresolved spools are within threshold"
	if s.ActiveCount > activeThreshold {
		reason = "active unresolved spools exceed threshold"
	}
	r := HeldReadiness{Ready: s.ActiveCount <= activeThreshold, ActiveUnresolved: s.ActiveCount, ActiveThreshold: activeThreshold, HistoricalTerminal: s.TerminalTotal, Reason: reason}
	l.report(HeldLogRecord{Operation: "held-readiness", Message: fmt.Sprintf("ready=%t active=%d threshold=%d terminal=%d", r.Ready, r.ActiveUnresolved, r.ActiveThreshold, r.HistoricalTerminal), Verbose: true})
	return r
}

func heldPathHash(path string) string {
	sum := sha256.Sum256([]byte(path))
	return hex.EncodeToString(sum[:8])
}

func heldAgeBucket(age time.Duration) string {
	switch {
	case age < time.Minute:
		return "under_1m"
	case age < time.Hour:
		return "under_1h"
	case age < 24*time.Hour:
		return "under_1d"
	default:
		return "over_1d"
	}
}
