package merge

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// This file is a merge's DURABLE PROGRESS: the record a run writes at every
// step boundary, and that the next boot resumes it from.
//
// A MERGE ALWAYS RESUMES WHERE IT LEFT OFF (owner ruling, 2026-10-06). The
// boot never guesses from the trees what a dead run had done; it reads where
// the run SAID it was, and reads from git only what that one step can
// legitimately have left (a rebase stopped at a break or a conflict, a
// fast-forward done or not). A tree that contradicts the record fails the
// merge, naming exactly what contradicted it.
//
// THE RECORD IS WRITTEN BEFORE THE STEP ACTS. A step's checkpoint names
// everything its resume needs -- the turn it is about to submit, the tip it is
// about to rebase onto, the merge commit it is about to fast-forward to -- so
// a run that dies anywhere inside the step is resumed by a reader that knows
// what the step was doing. The record names the merge's lease, which is also
// its ledger and its bubble's identity: the resumed merge is the same bubble.

// progressVersion is the record's own shape version. A record of another
// version is a record this build cannot interpret, which refuses the boot like
// every other undecodable row.
const progressVersion = 1

// progressDoc is the durable record.
type progressDoc struct {
	Version int `json:"version"`
	// Step is the tab kind the run stands on: TabQueue, TabPrePrompt,
	// TabRebasing, TabConflicts, TabTests, TabFixes, TabCommitting,
	// TabUpdatingMain or TabPostPrompt.
	Step string `json:"step"`
	// Repo is the queue the merge was admitted from.
	Repo string `json:"repo"`
	// QueuedMS and StartedMS are the bubble head's clock origin and the queue
	// tab's end.
	QueuedMS  int64 `json:"queued_ms"`
	StartedMS int64 `json:"started_ms"`
	// Subject is what the merge lands, resolved ONCE at admission: a resume
	// never re-resolves it (a branch worktree the merge made would be made
	// again).
	Subject subjectDoc `json:"subject"`
	// EmacsRepo and SelfCheckout select the method and the self-reload.
	EmacsRepo    bool `json:"emacs_repo"`
	SelfCheckout bool `json:"self_checkout"`
	// Rounds, Active and Open are the bubble's tab rounds: how many of each
	// kind were opened, the one live now, and those whose ledger interval is
	// still open.
	Rounds map[string]int `json:"rounds"`
	Active roundDoc       `json:"active"`
	Open   []roundDoc     `json:"open"`
	// Facts are the footer's standing facts, drawn again at once on resume.
	Facts factsDoc `json:"facts"`
	// Machinery is the merge machinery the branch changed before the first
	// repair; MachineryNoted says it was read.
	Machinery      []string `json:"machinery"`
	MachineryNoted bool     `json:"machinery_noted"`
	// Displaced is the user turn the admission displaced, put back when the
	// merge ends.
	Displaced *displacedDoc `json:"displaced,omitempty"`

	// PromptIndex is the configured prompt a pre or post step is on, and Turn
	// the agent turn any agentic step (a prompt, a conflict resolution, a
	// fixing attempt) submitted.
	PromptIndex int    `json:"prompt_index"`
	Turn        string `json:"turn,omitempty"`

	// The Emacs-repo method's attempt: the target's branch and the tip the
	// branch is rebased onto, the branch's head before the rebase began, the
	// commits replayed and how many are on the tip, the rebasing tab's
	// narration, the conflicted files, the fixing attempt and the suites it
	// fixes, the gated head, the scratch trees made, and the merge commit the
	// target is fast-forwarded to.
	TargetBranch  string      `json:"target_branch,omitempty"`
	Tip           string      `json:"tip,omitempty"`
	BranchHead    string      `json:"branch_head,omitempty"`
	Commits       []commitDoc `json:"commits,omitempty"`
	Replayed      int         `json:"replayed"`
	Lines         []string    `json:"lines,omitempty"`
	ConflictFiles []string    `json:"conflict_files,omitempty"`
	FixAttempt    int         `json:"fix_attempt"`
	FailingSuites []string    `json:"failing_suites,omitempty"`
	// FailingArchive and FailingTail are the failing gate run a fixing
	// attempt's brief names, kept so a brief the restart cut off before it
	// reached the session is composed again whole.
	FailingArchive string `json:"failing_archive,omitempty"`
	FailingTail    string `json:"failing_tail,omitempty"`
	Head           string `json:"head,omitempty"`
	TreeAttempts   int    `json:"tree_attempts"`
	MergeCommit    string `json:"merge_commit,omitempty"`
	// Before is the main worktree's tip before updating main moved it.
	Before string `json:"before,omitempty"`
	// Outcome is what the method concluded, recorded once the target moved:
	// the post-merge prompts run after it, and the terminal needs it.
	Outcome *outcomeDoc `json:"outcome,omitempty"`
}

// subjectDoc is the durable subject.
type subjectDoc struct {
	Branch    string `json:"branch"`
	Dir       string `json:"dir"`
	TargetDir string `json:"target_dir"`
	Made      bool   `json:"made"`
	Closes    string `json:"closes,omitempty"`
	Other     string `json:"other,omitempty"`
}

// roundDoc is one durable tab round.
type roundDoc struct {
	Kind      string `json:"kind"`
	N         int    `json:"n"`
	StartedMS int64  `json:"started_ms"`
}

// factsDoc is the footer facts a resume draws before its step speaks again.
type factsDoc struct {
	Step        string `json:"step"`
	Replayed    int    `json:"replayed"`
	Total       int    `json:"total"`
	Attempt     int    `json:"attempt"`
	MaxAttempts int    `json:"max_attempts"`
	TestsRound  int    `json:"tests_round"`
}

// displacedDoc is the durable displaced turn.
type displacedDoc struct {
	Turn string `json:"turn"`
	Text string `json:"text"`
}

// commitDoc is one durable commit.
type commitDoc struct {
	SHA     string    `json:"sha"`
	Subject string    `json:"subject"`
	Author  string    `json:"author,omitempty"`
	At      time.Time `json:"at"`
}

// outcomeDoc is the durable outcome of a method that moved its target.
type outcomeDoc struct {
	Landed    string      `json:"landed"`
	Commits   []commitDoc `json:"commits,omitempty"`
	AlreadyOn string      `json:"already_on,omitempty"`
}

// toCommitDocs and fromCommitDocs carry commits through the record.
func toCommitDocs(commits []gitclient.Commit) []commitDoc {
	if len(commits) == 0 {
		return nil
	}
	out := make([]commitDoc, len(commits))
	for i, c := range commits {
		out[i] = commitDoc{SHA: c.SHA, Subject: c.Subject, Author: c.Author, At: c.At}
	}
	return out
}

func fromCommitDocs(docs []commitDoc) []gitclient.Commit {
	if len(docs) == 0 {
		return nil
	}
	out := make([]gitclient.Commit, len(docs))
	for i, d := range docs {
		out[i] = gitclient.Commit{SHA: d.SHA, Subject: d.Subject, Author: d.Author, At: d.At}
	}
	return out
}

// toOutcomeDoc and outcome carry a landing through the record.
func toOutcomeDoc(out outcome) *outcomeDoc {
	return &outcomeDoc{Landed: out.landed, Commits: toCommitDocs(out.commits), AlreadyOn: out.alreadyOn}
}

func (d *outcomeDoc) outcome() outcome {
	if d == nil {
		return outcome{}
	}
	return outcome{landed: d.Landed, commits: fromCommitDocs(d.Commits), alreadyOn: d.AlreadyOn}
}

// subject and toSubjectDoc carry the subject through the record.
func (d subjectDoc) subject() subject {
	return subject{
		branch: d.Branch, dir: d.Dir, targetDir: d.TargetDir, made: d.Made,
		closes: ids.WorkspaceID(d.Closes), other: ids.WorkspaceID(d.Other),
	}
}

func toSubjectDoc(s subject) subjectDoc {
	return subjectDoc{
		Branch: s.branch, Dir: s.dir, TargetDir: s.targetDir, Made: s.made,
		Closes: string(s.closes), Other: string(s.other),
	}
}

// round and toRoundDoc carry a tab round through the record.
func (d roundDoc) round() tabRound {
	return tabRound{kind: d.Kind, n: d.N, started: time.UnixMilli(d.StartedMS)}
}

func toRoundDoc(t tabRound) roundDoc {
	return roundDoc{Kind: t.kind, N: t.n, StartedMS: t.started.UnixMilli()}
}

// footerFacts is the record's facts as the footer draws them.
func (d factsDoc) footerFacts(at time.Time) footer.MergeFacts {
	return footer.MergeFacts{
		State: StateMerging, Step: footer.MergeStep(d.Step), LineAt: at,
		Replayed: d.Replayed, Total: d.Total, Attempt: d.Attempt, MaxAttempts: d.MaxAttempts,
		TestsRound: d.TestsRound,
	}
}

// decodeProgress reads a stored record. A record that will not decode, or
// that carries another version, is the DecodeError it is: corruption refuses
// the load, it is never resumed by a guess.
func decodeProgress(stored wsm.MergeProgress) (progressDoc, error) {
	var doc progressDoc
	if err := json.Unmarshal(stored.Document, &doc); err != nil {
		return progressDoc{}, &wsm.DecodeError{Table: "merge_progress", Row: string(stored.Workspace), Field: "document", Err: err}
	}
	if doc.Version != progressVersion {
		return progressDoc{}, &wsm.DecodeError{Table: "merge_progress", Row: string(stored.Workspace), Field: "document",
			Err: fmt.Errorf("the record is version %d; this build reads version %d", doc.Version, progressVersion)}
	}
	if doc.Step == "" || doc.Repo == "" || doc.Subject.TargetDir == "" || doc.Active.Kind == "" {
		return progressDoc{}, &wsm.DecodeError{Table: "merge_progress", Row: string(stored.Workspace), Field: "document",
			Err: fmt.Errorf("the record names no step, queue, target or live tab")}
	}
	return doc, nil
}

// checkpoint records the run's progress at a step boundary: the step it now
// stands on, with the step's own facts applied by mutate, and the tab rounds,
// footer facts and machinery the run holds now.
//
// A RECORD THAT CANNOT BE WRITTEN STOPS THE MERGE. A step taken without its
// record is a step a restart could not resume, so the failure is returned and
// the run ends on it, loudly, rather than going on unrecorded.
func (r *run) checkpoint(ctx context.Context, step string, mutate func(*progressDoc)) error {
	const op = "daemon.merge.progress"
	// A SUSPENDED RUN RECORDS NOTHING: its record stays at the stopping point
	// the resume continues from.
	if r.exiting() {
		return errMergeStopping
	}
	r.mu.Lock()
	doc := r.prog
	doc.Version = progressVersion
	doc.Step = step
	doc.Repo = string(r.repo)
	doc.QueuedMS, doc.StartedMS = r.queuedMS, r.startedMS
	doc.Subject = toSubjectDoc(r.subject)
	doc.EmacsRepo, doc.SelfCheckout = r.emacsRepo, r.selfCheckout
	doc.Rounds = make(map[string]int, len(r.rounds))
	for kind, n := range r.rounds {
		doc.Rounds[kind] = n
	}
	doc.Active = toRoundDoc(r.active)
	doc.Open = doc.Open[:0:0]
	for _, round := range r.openRounds {
		doc.Open = append(doc.Open, toRoundDoc(round))
	}
	sort.Slice(doc.Open, func(i, j int) bool {
		return roundKey(doc.Open[i].Kind, doc.Open[i].N) < roundKey(doc.Open[j].Kind, doc.Open[j].N)
	})
	doc.Facts = factsDoc{
		Step: string(r.facts.Step), Replayed: r.facts.Replayed, Total: r.facts.Total,
		Attempt: r.facts.Attempt, MaxAttempts: r.facts.MaxAttempts, TestsRound: r.facts.TestsRound,
	}
	doc.TreeAttempts = r.attempts
	doc.MachineryNoted = r.machinery != nil
	doc.Machinery = doc.Machinery[:0:0]
	for path := range r.machinery {
		doc.Machinery = append(doc.Machinery, path)
	}
	sort.Strings(doc.Machinery)
	if r.displaced != nil {
		doc.Displaced = &displacedDoc{Turn: string(r.displaced.Turn), Text: r.displaced.Text}
	} else {
		doc.Displaced = nil
	}
	if mutate != nil {
		mutate(&doc)
	}
	r.prog = doc
	r.mu.Unlock()
	encoded, err := json.Marshal(doc)
	if err != nil {
		return fmt.Errorf("merge: encoding the merge's progress record: %w", err)
	}
	if err := r.o.deps.DB.PutMergeProgress(ctx, wsm.MergeProgress{
		Workspace: r.ws, Lease: r.lease.ID, UpdatedAt: r.o.deps.Now(), Document: encoded,
	}); err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not record the merge's progress; the merge stops rather than take a step a restart could not resume",
			dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "step": step, "error": err.Error()})
		return fmt.Errorf("merge: recording the merge's progress at %s: %w", step, err)
	}
	r.o.log(ctx, r.ws).Debug(op, "recorded the merge's progress", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "step": step, "round": doc.Active.N, "turn": doc.Turn})
	return nil
}

// restoreFrom stands a resumed run where its record left it: the tab rounds,
// the footer facts, the machinery and the displaced turn.
func (r *run) restoreFrom(doc progressDoc) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.prog = doc
	r.subject = doc.Subject.subject()
	r.emacsRepo, r.selfCheckout = doc.EmacsRepo, doc.SelfCheckout
	for kind, n := range doc.Rounds {
		r.rounds[kind] = n
	}
	r.active = doc.Active.round()
	for _, round := range doc.Open {
		t := round.round()
		r.openRounds[t.key()] = t
	}
	if queue, open := r.openRounds[roundKey(TabQueue, 1)]; open {
		r.queueRound = queue
	} else {
		r.queueRound = tabRound{kind: TabQueue, n: 1, started: time.UnixMilli(doc.QueuedMS)}
	}
	r.facts = doc.Facts.footerFacts(r.o.deps.Now())
	r.attempts = doc.TreeAttempts
	if doc.MachineryNoted {
		r.machinery = map[string]bool{}
		for _, path := range doc.Machinery {
			r.machinery[path] = true
		}
	}
	if doc.Displaced != nil {
		r.displaced = &Displaced{Turn: ids.TurnID(doc.Displaced.Turn), Text: doc.Displaced.Text}
	}
}

// resumingAt reports whether this run is a resume standing at one step, and
// hands that step its record. A step consumes the record once it has taken
// its resume path (doneResuming), so a LATER pass of the same step -- a second
// attempt after the target moved -- runs fresh.
func (r *run) resumingAt(steps ...string) (*progressDoc, bool) {
	if r.resume == nil {
		return nil, false
	}
	for _, step := range steps {
		if r.resume.Step == step {
			return r.resume, true
		}
	}
	return nil, false
}

// doneResuming marks the resume consumed: every step from here on runs fresh.
func (r *run) doneResuming() { r.resume = nil }

// resumeStepOrder is the method's step order, which is how a resume skips the
// steps the dead run had already finished.
var resumeStepOrder = map[string]int{
	TabQueue: 0, TabPrePrompt: 1, TabRebasing: 2, TabConflicts: 2, TabTests: 2, TabFixes: 2,
	TabCommitting: 2, TabUpdatingMain: 2, TabPostPrompt: 3,
}

// resumingPast reports whether this run resumes at a step AFTER the one named:
// the named step was finished by the dead run and is skipped.
func (r *run) resumingPast(step string) bool {
	return r.resume != nil && resumeStepOrder[r.resume.Step] > resumeStepOrder[step]
}

// doneResumingAt consumes the resume when it stands at that step, which is how
// a step with no resume path of its own (the queue) is passed.
func (r *run) doneResumingAt(step string) {
	if r.resume != nil && r.resume.Step == step {
		r.resume = nil
	}
}
