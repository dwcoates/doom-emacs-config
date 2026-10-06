package merge

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/paint"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// This file holds the merge orchestrator's fakes.
//
// GIT IS NEVER CALLED DURING TESTING (user directive): every git fact a merge
// depends on — the default branch, the merge's outcome, the conflicted files,
// the landed range, the changed paths — is scripted on fakeGit, and the test
// gate is a scripted runner rather than the repository's own suite. No test
// creates a repository, and none spawns a process.

// fakeDB is the durable state, in memory. It embeds wsm.DB so the fake declares
// only what the orchestrator actually calls: anything else would panic loudly
// rather than quietly answering a zero value.
type fakeDB struct {
	wsm.DB

	mu sync.Mutex

	workspaces map[ids.WorkspaceID]wsm.Workspace
	jobs       map[ids.WorkspaceID]wsm.CreationJob
	// jobDecodeErrs makes one workspace's creation_jobs row undecodable, which
	// is the corruption the boot must refuse rather than absorb.
	jobDecodeErrs map[ids.WorkspaceID]error
	sessions      map[ids.WorkspaceID]wsm.Session
	leases        map[ids.WorkspaceID]wsm.Lease
	turns         map[ids.WorkspaceID][]wsm.Turn
	ledger        map[ids.WorkspaceID][]wsm.MergeLedgerEntry
	queues        map[wsm.RepoKey][]wsm.MergeQueueEntry
	paused        map[wsm.RepoKey]bool
	repos         []wsm.Repository
	seq           int
	// onPausedRead, when set, runs at the start of every MergeQueuePaused
	// read, outside the fake's lock, so a test can hold an admission step in
	// flight; pausedReads counts those reads.
	onPausedRead func()
	pausedReads  int

	// mergedAt, closed and releasedLeases are what the teardown's ordering is
	// asserted against.
	mergedAt       map[ids.WorkspaceID]time.Time
	closed         map[ids.WorkspaceID]bool
	releasedLeases []wsm.LeaseID
	policies       map[wsm.LeaseID]wsm.LeasePolicy
	dropped        []string
	// enqueueErr, when set, fails the next EnqueueMerge.
	enqueueErr error
	// shut stands for the state client the daemon's orderly exit has already
	// closed: every write refuses, exactly as a write against a closed
	// handle does. A test sets it after the drain, so any durable work a run
	// attempts on the way out is a visible failure rather than a silent one.
	shut bool
	// retired records the displaced turns whose mark was claimed, which is
	// what "exactly once" is asserted against.
	retired []wsm.TurnID
	// beforeRelease, when set, runs under the fake's lock as the lease
	// release begins: the last instant a prompt can be held under the merge.
	beforeRelease func(f *fakeDB)
	// queueReadErr, when set, fails every MergeQueue read.
	queueReadErr error
	// progress is each workspace's merge progress record, and progressSteps
	// every step a record was written at, in order. adopted records the merge
	// leases adopted for a resume; held stands for the prompt queue's held
	// prompts, by turn.
	progress      map[ids.WorkspaceID]wsm.MergeProgress
	progressSteps []string
	progressErr   error
	adopted       []wsm.LeaseID
	held          map[wsm.TurnID]wsm.HeldPrompt
}

// bindHeldPrompt stands for wsm.bindMergeHold: a prompt held under the
// workspace's merge marks its queue entry to keep the requester open. The
// caller holds the lock.
func (f *fakeDB) bindHeldPrompt(id ids.WorkspaceID) {
	for repo, entries := range f.queues {
		for i := range entries {
			if entries[i].Workspace == id && entries[i].Source.Kind.ClosesRequester() {
				f.queues[repo][i].Source.KeepOpen = true
			}
		}
	}
}

func newFakeDB() *fakeDB {
	return &fakeDB{
		workspaces:    map[ids.WorkspaceID]wsm.Workspace{},
		jobs:          map[ids.WorkspaceID]wsm.CreationJob{},
		jobDecodeErrs: map[ids.WorkspaceID]error{},
		sessions:      map[ids.WorkspaceID]wsm.Session{},
		leases:        map[ids.WorkspaceID]wsm.Lease{},
		turns:         map[ids.WorkspaceID][]wsm.Turn{},
		ledger:        map[ids.WorkspaceID][]wsm.MergeLedgerEntry{},
		queues:        map[wsm.RepoKey][]wsm.MergeQueueEntry{},
		paused:        map[wsm.RepoKey]bool{},
		mergedAt:      map[ids.WorkspaceID]time.Time{},
		closed:        map[ids.WorkspaceID]bool{},
		policies:      map[wsm.LeaseID]wsm.LeasePolicy{},
		progress:      map[ids.WorkspaceID]wsm.MergeProgress{},
		held:          map[wsm.TurnID]wsm.HeldPrompt{},
	}
}

func (f *fakeDB) PutMergeProgress(_ context.Context, p wsm.MergeProgress) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.shut {
		return errStateClientClosed
	}
	if f.progressErr != nil {
		return f.progressErr
	}
	var doc struct {
		Step string `json:"step"`
	}
	if err := json.Unmarshal(p.Document, &doc); err != nil {
		return err
	}
	f.progress[p.Workspace] = p
	f.progressSteps = append(f.progressSteps, doc.Step)
	return nil
}

func (f *fakeDB) MergeProgressOf(_ context.Context, id ids.WorkspaceID) (wsm.MergeProgress, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	p, ok := f.progress[id]
	return p, ok, nil
}

func (f *fakeDB) DropMergeProgress(_ context.Context, lease wsm.LeaseID) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.shut {
		return false, errStateClientClosed
	}
	for id, p := range f.progress {
		if p.Lease == lease {
			delete(f.progress, id)
			return true, nil
		}
	}
	return false, nil
}

func (f *fakeDB) AdoptMergeLease(_ context.Context, id ids.WorkspaceID, lease wsm.LeaseID) (wsm.Lease, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	held, ok := f.leases[id]
	if !ok || held.ID != lease || held.Holder != wsm.HolderMerge {
		return wsm.Lease{}, fmt.Errorf("no merge lease %s is held on %s", lease, id)
	}
	f.adopted = append(f.adopted, lease)
	return held, nil
}

func (f *fakeDB) TurnCloses(_ context.Context, id ids.WorkspaceID, turns []wsm.TurnID) (map[wsm.TurnID]wsm.RecordedClose, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := map[wsm.TurnID]wsm.RecordedClose{}
	for _, want := range turns {
		for _, t := range f.turns[id] {
			if t.ID == want && t.Close != nil {
				out[want] = wsm.RecordedClose{How: *t.Close, At: *t.ClosedAt}
			}
		}
	}
	return out, nil
}

func (f *fakeDB) RecordedTurns(_ context.Context, id ids.WorkspaceID, turns []wsm.TurnID) (map[wsm.TurnID]bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := map[wsm.TurnID]bool{}
	for _, want := range turns {
		for _, t := range f.turns[id] {
			if t.ID == want {
				out[want] = true
			}
		}
	}
	return out, nil
}

func (f *fakeDB) HeldPromptByTurn(_ context.Context, turn wsm.TurnID) (wsm.HeldPrompt, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	h, ok := f.held[turn]
	return h, ok, nil
}

func (f *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	ws, ok := f.workspaces[id]
	if !ok {
		return wsm.Workspace{}, fmt.Errorf("no workspace %s", id)
	}
	return ws, nil
}

func (f *fakeDB) CreationJob(_ context.Context, id ids.WorkspaceID) (wsm.CreationJob, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err, corrupt := f.jobDecodeErrs[id]; corrupt {
		return wsm.CreationJob{}, false, err
	}
	job, ok := f.jobs[id]
	return job, ok, nil
}

func (f *fakeDB) Session(_ context.Context, id ids.WorkspaceID) (wsm.Session, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	s, ok := f.sessions[id]
	return s, ok, nil
}

func (f *fakeDB) AcquireLease(ctx context.Context, id ids.WorkspaceID, holder wsm.LeaseHolder, policy wsm.LeasePolicy) (wsm.Lease, error) {
	return f.AcquireLeaseAs(ctx, id, wsm.NewLeaseID(), holder, policy)
}

func (f *fakeDB) AcquireLeaseAs(_ context.Context, id ids.WorkspaceID, lease wsm.LeaseID, holder wsm.LeaseHolder, policy wsm.LeasePolicy) (wsm.Lease, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if existing, held := f.leases[id]; held {
		return wsm.Lease{}, &wsm.LeaseHeldError{Workspace: id, Lease: existing.ID, Holder: existing.Holder, Policy: existing.Policy}
	}
	held := wsm.Lease{ID: lease, Workspace: id, Holder: holder, Policy: policy}
	f.leases[id] = held
	f.policies[held.ID] = policy
	return held, nil
}

func (f *fakeDB) ReleaseLease(_ context.Context, lease wsm.LeaseID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.shut {
		return errStateClientClosed
	}
	if f.beforeRelease != nil {
		f.beforeRelease(f)
	}
	for id, held := range f.leases {
		if held.ID == lease {
			delete(f.leases, id)
		}
	}
	f.releasedLeases = append(f.releasedLeases, lease)
	return nil
}

func (f *fakeDB) Lease(_ context.Context, id ids.WorkspaceID) (wsm.Lease, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	lease, ok := f.leases[id]
	return lease, ok, nil
}

func (f *fakeDB) SetLeasePolicy(_ context.Context, lease wsm.LeaseID, p wsm.LeasePolicy) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.policies[lease] = p
	return nil
}

func (f *fakeDB) SetMergedAt(_ context.Context, id ids.WorkspaceID, at time.Time) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.shut {
		return errStateClientClosed
	}
	f.mergedAt[id] = at
	return nil
}

func (f *fakeDB) SetClosed(_ context.Context, id ids.WorkspaceID, closed bool) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.shut {
		return errStateClientClosed
	}
	f.closed[id] = closed
	return nil
}

func (f *fakeDB) OpenTurns(_ context.Context, id ids.WorkspaceID) ([]wsm.Turn, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]wsm.Turn(nil), f.turns[id]...), nil
}

func (f *fakeDB) CloseTurn(_ context.Context, turn wsm.TurnID, at time.Time, how wsm.TurnClose) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	for id, list := range f.turns {
		kept := list[:0]
		for _, t := range list {
			if t.ID != turn {
				kept = append(kept, t)
			}
		}
		f.turns[id] = kept
	}
	return nil
}

// AllDisplacedTurns answers the staged turns still carrying the mark, which is
// the boot recovery's whole input.
func (f *fakeDB) AllDisplacedTurns(_ context.Context) ([]wsm.Turn, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []wsm.Turn
	for _, list := range f.turns {
		for _, t := range list {
			if t.Displaced {
				out = append(out, t)
			}
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].ID < out[j].ID })
	return out, nil
}

// ClaimDisplacedTurn takes the record exclusively, exactly as the store's one
// conditional statement does: only a turn still marked can be claimed.
func (f *fakeDB) ClaimDisplacedTurn(_ context.Context, turn wsm.TurnID, _ time.Time) (wsm.DisplacedClaim, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	claimed := false
	for id, list := range f.turns {
		kept := list[:0]
		for _, t := range list {
			if t.ID == turn && t.Displaced {
				claimed = true
				continue
			}
			kept = append(kept, t)
		}
		f.turns[id] = kept
	}
	if claimed {
		f.retired = append(f.retired, turn)
	}
	return wsm.DisplacedClaim{Claimed: claimed}, nil
}

func (f *fakeDB) OpenMergeLedger(_ context.Context, id ids.WorkspaceID, lease wsm.LeaseID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ledger[id] = append(f.ledger[id], wsm.MergeLedgerEntry{Workspace: id, Lease: lease})
	return nil
}

func (f *fakeDB) RecordTabInterval(_ context.Context, lease wsm.LeaseID, interval wsm.TabInterval) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	for id, entries := range f.ledger {
		for i := range entries {
			if entries[i].Lease == lease {
				entries[i].Intervals = append(entries[i].Intervals, interval)
				f.ledger[id] = entries
				return nil
			}
		}
	}
	return fmt.Errorf("no ledger for lease %s", lease)
}

func (f *fakeDB) MergeLedger(_ context.Context, id ids.WorkspaceID) ([]wsm.MergeLedgerEntry, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]wsm.MergeLedgerEntry(nil), f.ledger[id]...), nil
}

func (f *fakeDB) RequestMerge(_ context.Context, repo wsm.RepoKey, id ids.WorkspaceID, source wsm.MergeSource, at time.Time) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.enqueueErr != nil {
		err := f.enqueueErr
		f.enqueueErr = nil
		return err
	}
	for i, entry := range f.queues[repo] {
		if entry.Workspace == id {
			return &wsm.MergeQueuedError{Repo: repo, Workspace: id, Position: i + 1, State: entry.State}
		}
	}
	f.seq++
	f.queues[repo] = append(f.queues[repo], wsm.MergeQueueEntry{
		Repo: repo, Workspace: id, State: wsm.MergeRequested, Source: source, EnqueuedAt: at,
	})
	f.renumber(repo)
	return nil
}

// QueueMerge moves a requested entry to the back of the line, exactly as the
// durable queue re-sequences it.
func (f *fakeDB) QueueMerge(_ context.Context, repo wsm.RepoKey, id ids.WorkspaceID) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	for i, entry := range f.queues[repo] {
		if entry.Workspace != id {
			continue
		}
		if entry.State != wsm.MergeRequested {
			return 0, fmt.Errorf("the merge of %s is %s, not requested", id, entry.State)
		}
		f.queues[repo] = append(f.queues[repo][:i], f.queues[repo][i+1:]...)
		entry.State = wsm.MergeQueued
		f.queues[repo] = append(f.queues[repo], entry)
		f.renumber(repo)
		place := 0
		for _, e := range f.queues[repo] {
			if e.State != wsm.MergeRequested {
				place++
			}
		}
		return place, nil
	}
	return 0, fmt.Errorf("no queue entry for %s", id)
}

// renumber restates the positions, which are derived from order rather than
// stored — exactly as the durable queue derives them.
func (f *fakeDB) renumber(repo wsm.RepoKey) {
	for i := range f.queues[repo] {
		f.queues[repo][i].Position = i + 1
	}
}

func (f *fakeDB) AdmitMerge(_ context.Context, repo wsm.RepoKey, id ids.WorkspaceID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	for i, entry := range f.queues[repo] {
		if entry.Workspace == id {
			f.queues[repo][i].State = wsm.MergeAdmitted
			return nil
		}
	}
	return fmt.Errorf("no queue entry for %s", id)
}

func (f *fakeDB) RemoveMergeQueueEntry(_ context.Context, repo wsm.RepoKey, id ids.WorkspaceID, cause string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.shut {
		return errStateClientClosed
	}
	for i, entry := range f.queues[repo] {
		if entry.Workspace == id {
			f.queues[repo] = append(f.queues[repo][:i], f.queues[repo][i+1:]...)
			f.renumber(repo)
			f.dropped = append(f.dropped, fmt.Sprintf("%s:%s", id, cause))
			return nil
		}
	}
	return fmt.Errorf("no queue entry for %s", id)
}

func (f *fakeDB) MergeQueue(_ context.Context, repo wsm.RepoKey) ([]wsm.MergeQueueEntry, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.queueReadErr != nil {
		return nil, f.queueReadErr
	}
	return append([]wsm.MergeQueueEntry(nil), f.queues[repo]...), nil
}

func (f *fakeDB) AllMergeQueues(_ context.Context) (map[wsm.RepoKey][]wsm.MergeQueueEntry, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := map[wsm.RepoKey][]wsm.MergeQueueEntry{}
	for repo, entries := range f.queues {
		out[repo] = append([]wsm.MergeQueueEntry(nil), entries...)
	}
	return out, nil
}

func (f *fakeDB) SetMergeQueuePaused(_ context.Context, repo wsm.RepoKey, paused bool) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.paused[repo] = paused
	return nil
}

func (f *fakeDB) MergeQueuePaused(_ context.Context, repo wsm.RepoKey) (bool, error) {
	f.mu.Lock()
	hook := f.onPausedRead
	f.pausedReads++
	f.mu.Unlock()
	if hook != nil {
		hook()
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.paused[repo], nil
}

// fakeGit answers every git fact from a script. It embeds nothing: the whole
// interface is declared, so a call the orchestrator adds later is a compile
// error rather than a surprise at run time.
type fakeGit struct {
	mu sync.Mutex
	// standing answers RebaseInProgress: a rebase stands in the worktree.
	standing bool
	// onTip answers CommitsBetween(_, _, "HEAD"): what a standing rebase has
	// replayed onto its tip.
	onTip []gitclient.Commit
	// holds parks one named call until its release closes, announcing the
	// call on entered: a git command still running when the daemon exits.
	holds map[string]gitHold
	// seq stamps every call with the harness's shared sequence, so an ordering
	// between a git call and a feed push is asserted on the real order rather
	// than inferred.
	seq func() int
	at  map[string]int

	defaultBranch string
	commonDirs    map[string]string
	sameRepo      bool
	sameRepoErr   error
	refs          map[string]string
	// refSeqs are ResolveRef answers scripted per ref, consumed in order with
	// the last one standing.
	refSeqs map[string][]string
	// outcomes are the MergeNoFF answers, consumed in order.
	outcomes []gitclient.MergeOutcome
	// conflicted are the ConflictedFiles answers, consumed in order.
	conflicted [][]string
	// commits are the Commit shas, consumed in order; an exhausted script
	// answers a standing sha.
	commits []string
	// commitErr fails every Commit when set.
	commitErr error
	// commitMessages records what each Commit was asked to record.
	commitMessages []string
	landed         []gitclient.Commit
	landedErr      error
	changed        []string
	changedErr     error
	// changedIn answers ChangedPaths for one range, consumed in order with the
	// last one standing; a range it does not name answers `changed`.
	changedIn map[string][][]string
	// rangesAsked records every range ChangedPaths was asked.
	rangesAsked []string
	clean       bool
	cleanErr    error

	// queueTrees and queueBases record every scratch tree the queue made and
	// the commit each was made at, in order.
	queueTrees []string
	queueBases []string
	// fastForwards records every landing, as "<dir>@<commit>".
	fastForwards   []string
	fastForwardErr error
	// contained answers IsAncestor for the source branch: it is already in the
	// target. ancestry answers every other IsAncestor, keyed "<a>><b>", false
	// when unscripted.
	contained bool
	ancestry  map[string]bool
	// branches answers CurrentBranch by directory, "master" when unscripted.
	branches map[string]string
	// between answers CommitsBetween; nil answers one commit of work.
	between    []gitclient.Commit
	betweenErr error
	// rebase is the scripted rebase: the commits it replays, the replay index
	// (1-based) whose first replay conflicts with the files named, and a
	// failure every rebase command answers.
	rebaseTotal     int
	rebaseReplayed  int
	rebaseConflicts map[int][]string
	rebaseMet       map[int]bool
	rebaseErr       error
	rebaseCommands  []string
	// worktrees answers ListWorktrees; addedWorktrees records AddWorktree.
	worktrees      []gitclient.Worktree
	addedWorktrees []string
	// fetches records every Fetch; fetchErr fails them.
	fetches  []string
	fetchErr error

	// calls records what was asked of git, in order.
	calls []string
	// removedWorktrees records the teardown's removals of a workspace's
	// worktree; removedQueueTrees the queue's scratch trees it removed.
	removedWorktrees  []string
	removedQueueTrees []string
	// mergeDirs records where every merge was made; mergeErr fails them all.
	mergeDirs []string
	mergeErr  error
}

func newFakeGit(seq func() int) *fakeGit {
	return &fakeGit{
		seq:             seq,
		at:              map[string]int{},
		defaultBranch:   "master",
		commonDirs:      map[string]string{},
		refs:            map[string]string{},
		refSeqs:         map[string][]string{},
		changedIn:       map[string][][]string{},
		clean:           true,
		ancestry:        map[string]bool{},
		branches:        map[string]string{},
		rebaseConflicts: map[int][]string{},
		rebaseMet:       map[int]bool{},
	}
}

func (g *fakeGit) record(call string) {
	at := g.seq()
	g.mu.Lock()
	g.calls = append(g.calls, call)
	if _, seen := g.at[call]; !seen {
		g.at[call] = at
	}
	hold, held := g.holds[call]
	delete(g.holds, call)
	g.mu.Unlock()
	if held {
		close(hold.entered)
		<-hold.release
	}
}

// gitHold is one parked git call.
type gitHold struct {
	entered chan struct{}
	release chan struct{}
}

// hold parks the next call of one name until release closes; entered closes
// once the call is in flight.
func (g *fakeGit) hold(call string) (entered, release chan struct{}) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.holds == nil {
		g.holds = map[string]gitHold{}
	}
	h := gitHold{entered: make(chan struct{}), release: make(chan struct{})}
	g.holds[call] = h
	return h.entered, h.release
}

func (g *fakeGit) DefaultBranch(context.Context, string) (string, error) {
	g.record("default_branch")
	return g.defaultBranch, nil
}

func (g *fakeGit) BranchExists(_ context.Context, _, branch string) (bool, error) {
	g.record("branch_exists")
	_, ok := g.refs["refs/heads/"+branch]
	return ok, nil
}

func (g *fakeGit) ResolveRef(_ context.Context, _, ref string) (string, error) {
	g.record("resolve_ref")
	g.mu.Lock()
	defer g.mu.Unlock()
	// A SCRIPTED SEQUENCE answers in order and then keeps answering its last
	// value, so a ref that moves once (a repair commit, a target that moved
	// under the gate) is scripted as its two values.
	if seq := g.refSeqs[ref]; len(seq) > 0 {
		if len(seq) > 1 {
			g.refSeqs[ref] = seq[1:]
		}
		return seq[0], nil
	}
	if sha, ok := g.refs[ref]; ok {
		return sha, nil
	}
	return "0000000000000000000000000000000000000000", nil
}

func (g *fakeGit) CreateWorktree(context.Context, string, string, string, string) error {
	g.record("create_worktree")
	return nil
}

// RemoveWorktree records a removal of one of the queue's own scratch trees
// apart from the removal of a workspace's worktree, because the two are
// different acts with different ordering rules.
func (g *fakeGit) RemoveWorktree(_ context.Context, _, worktreeDir string) error {
	g.mu.Lock()
	queueTree := slices.Contains(g.queueTrees, worktreeDir)
	g.mu.Unlock()
	if queueTree {
		g.record("remove_queue_tree")
		g.mu.Lock()
		g.removedQueueTrees = append(g.removedQueueTrees, worktreeDir)
		g.mu.Unlock()
		return nil
	}
	g.record("remove_worktree")
	g.mu.Lock()
	g.removedWorktrees = append(g.removedWorktrees, worktreeDir)
	g.mu.Unlock()
	return nil
}

func (g *fakeGit) AddDetachedWorktree(_ context.Context, _, worktreeDir, commit string) error {
	g.record("add_detached_worktree")
	g.mu.Lock()
	g.queueTrees = append(g.queueTrees, worktreeDir)
	g.queueBases = append(g.queueBases, commit)
	g.mu.Unlock()
	return nil
}

func (g *fakeGit) FastForward(_ context.Context, dir, commit string) error {
	g.record("fast_forward")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.fastForwards = append(g.fastForwards, dir+"@"+commit)
	return g.fastForwardErr
}

func (g *fakeGit) IsAncestor(_ context.Context, _, ancestor, descendant string) (bool, error) {
	g.record("is_ancestor")
	g.mu.Lock()
	defer g.mu.Unlock()
	if ancestor == "feature" {
		return g.contained, nil
	}
	return g.ancestry[ancestor+">"+descendant], nil
}

func (g *fakeGit) Nuke(context.Context, string, string, string) error {
	g.record("nuke")
	return nil
}

func (g *fakeGit) CommonDir(_ context.Context, dir string) (string, error) {
	g.record("common_dir")
	if common, ok := g.commonDirs[dir]; ok {
		return common, nil
	}
	return dir + "/.git", nil
}

func (g *fakeGit) MainWorktree(_ context.Context, dir string) (string, error) {
	g.record("main_worktree")
	return dir, nil
}

// RepositoryOf is MainWorktree's probe half. The merge suite never asks it;
// it exists so this fake still satisfies the whole interface.
func (g *fakeGit) RepositoryOf(_ context.Context, dir string) (string, bool, error) {
	g.record("repository_of")
	return dir, true, nil
}

func (g *fakeGit) SameRepo(context.Context, string, string) (bool, error) {
	g.record("same_repo")
	return g.sameRepo, g.sameRepoErr
}

func (g *fakeGit) MergeNoFF(_ context.Context, dir, _, _ string) (gitclient.MergeOutcome, error) {
	g.record("merge_no_ff")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.mergeDirs = append(g.mergeDirs, dir)
	// The outcomes answer in order and the LAST ONE STANDS: every attempt
	// makes the merge again, and a branch nobody changed merges the same way.
	var out gitclient.MergeOutcome
	if len(g.outcomes) > 0 {
		out = g.outcomes[0]
		if len(g.outcomes) > 1 {
			g.outcomes = g.outcomes[1:]
		}
	}
	if g.mergeErr != nil {
		return gitclient.MergeOutcome{}, g.mergeErr
	}
	return out, nil
}

func (g *fakeGit) ConflictedFiles(context.Context, string) ([]string, error) {
	g.record("conflicted_files")
	g.mu.Lock()
	var files []string
	if len(g.conflicted) > 0 {
		files = g.conflicted[0]
		g.conflicted = g.conflicted[1:]
	}
	g.mu.Unlock()
	return files, nil
}

func (g *fakeGit) Commit(_ context.Context, _, message string) (string, error) {
	g.record("commit")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.commitMessages = append(g.commitMessages, message)
	if g.commitErr != nil {
		return "", g.commitErr
	}
	if len(g.commits) == 0 {
		return "concluded000000", nil
	}
	sha := g.commits[0]
	g.commits = g.commits[1:]
	return sha, nil
}

func (g *fakeGit) AbortMerge(context.Context, string) error {
	g.record("abort_merge")
	return nil
}

func (g *fakeGit) RevertMerge(context.Context, string, string) error {
	g.record("revert_merge")
	return nil
}

func (g *fakeGit) LandedRange(context.Context, string, string) ([]gitclient.Commit, error) {
	g.record("landed_range")
	return g.landed, g.landedErr
}

func (g *fakeGit) ChangedPaths(_ context.Context, _, rangeSpec string) ([]string, error) {
	g.record("changed_paths")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.rangesAsked = append(g.rangesAsked, rangeSpec)
	if seq := g.changedIn[rangeSpec]; len(seq) > 0 {
		if len(seq) > 1 {
			g.changedIn[rangeSpec] = seq[1:]
		}
		return seq[0], nil
	}
	return g.changed, g.changedErr
}

func (g *fakeGit) IsClean(context.Context, string) (bool, error) {
	g.record("is_clean")
	return g.clean, g.cleanErr
}

func (g *fakeGit) CurrentBranch(_ context.Context, dir string) (string, error) {
	g.record("current_branch")
	g.mu.Lock()
	defer g.mu.Unlock()
	if branch, ok := g.branches[dir]; ok {
		return branch, nil
	}
	return "master", nil
}

func (g *fakeGit) CommitsBetween(_ context.Context, _, _, tip string) ([]gitclient.Commit, error) {
	g.record("commits_between")
	g.mu.Lock()
	defer g.mu.Unlock()
	if tip == "HEAD" {
		return g.onTip, nil
	}
	if g.between == nil {
		return []gitclient.Commit{{SHA: "c0ffee0000001", Subject: "the work"}}, g.betweenErr
	}
	return g.between, g.betweenErr
}

func (g *fakeGit) StartRebase(_ context.Context, _, onto string, commits []string) (gitclient.RebaseStep, error) {
	g.record("start_rebase")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.rebaseTotal = len(commits)
	g.rebaseReplayed = 0
	g.rebaseCommands = append(g.rebaseCommands, "start "+onto)
	return g.replayLocked()
}

func (g *fakeGit) ContinueRebase(context.Context, string) (gitclient.RebaseStep, error) {
	g.record("continue_rebase")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.rebaseCommands = append(g.rebaseCommands, "continue")
	return g.replayLocked()
}

// replayLocked answers one rebase command: the next commit replayed, or its
// scripted conflict the first time it is met.
func (g *fakeGit) replayLocked() (gitclient.RebaseStep, error) {
	if g.rebaseErr != nil {
		return gitclient.RebaseStep{}, g.rebaseErr
	}
	k := g.rebaseReplayed + 1
	if files, scripted := g.rebaseConflicts[k]; scripted && !g.rebaseMet[k] {
		g.rebaseMet[k] = true
		return gitclient.RebaseStep{Conflicted: files}, nil
	}
	g.rebaseReplayed = k
	return gitclient.RebaseStep{Done: g.rebaseReplayed >= g.rebaseTotal}, nil
}

func (g *fakeGit) RebaseInProgress(context.Context, string) (bool, error) {
	g.record("rebase_in_progress")
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.standing, nil
}

func (g *fakeGit) AddWorktree(_ context.Context, _, worktreeDir, branch string) error {
	g.record("add_worktree")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.addedWorktrees = append(g.addedWorktrees, worktreeDir)
	g.branches[worktreeDir] = branch
	return nil
}

// RestoreWorktree is never the merge's to call: a restore belongs to
// OpenWorkspace. Reaching it here is a test failure, not an answer.
func (g *fakeGit) RestoreWorktree(context.Context, string, string, string) error {
	g.record("restore_worktree")
	return errors.New("fakeGit: the merge never restores a workspace's worktree")
}

// UnregisterMissingWorktree is never the merge's to call either.
func (g *fakeGit) UnregisterMissingWorktree(context.Context, string, string) error {
	g.record("unregister_missing_worktree")
	return errors.New("fakeGit: the merge never unregisters a missing worktree")
}

func (g *fakeGit) Fetch(_ context.Context, dir, remote string) error {
	g.record("fetch")
	g.mu.Lock()
	defer g.mu.Unlock()
	g.fetches = append(g.fetches, dir+"<"+remote)
	return g.fetchErr
}

// THE REAPER'S OPERATIONS are no part of a merge. Each one fails loudly, so a
// merge that ever reached for one would fail its test rather than pass on a
// zero answer.
func (g *fakeGit) ListWorktrees(context.Context, string) ([]gitclient.Worktree, error) {
	g.record("list_worktrees")
	g.mu.Lock()
	defer g.mu.Unlock()
	return append([]gitclient.Worktree(nil), g.worktrees...), nil
}

func (g *fakeGit) PruneWorktrees(context.Context, string) error {
	return errors.New("merge fake git: PruneWorktrees is not a merge operation")
}

func (g *fakeGit) RemoveCleanWorktree(context.Context, string, string) error {
	return errors.New("merge fake git: RemoveCleanWorktree is not a merge operation")
}

func (g *fakeGit) AdminDir(context.Context, string) (string, error) {
	return "", errors.New("merge fake git: AdminDir is not a merge operation")
}

func (g *fakeGit) CommitterTime(context.Context, string, string) (time.Time, error) {
	return time.Time{}, errors.New("merge fake git: CommitterTime is not a merge operation")
}

func (g *fakeGit) TreeOf(context.Context, string, string) (string, error) {
	return "", errors.New("merge fake git: TreeOf is not a merge operation")
}

func (g *fakeGit) MergeTree(context.Context, string, string, string) (gitclient.MergeTreeOutcome, error) {
	return gitclient.MergeTreeOutcome{}, errors.New("merge fake git: MergeTree is not a merge operation")
}

func (g *fakeGit) DeleteBranchAt(context.Context, string, string, string) error {
	return errors.New("merge fake git: DeleteBranchAt is not a merge operation")
}

// seen reports whether git was asked for something.
func (g *fakeGit) seen(call string) bool {
	g.mu.Lock()
	defer g.mu.Unlock()
	for _, c := range g.calls {
		if c == call {
			return true
		}
	}
	return false
}

// fakeQueue records what the merge submitted down the one delivery path.
type fakeQueue struct {
	mu sync.Mutex
	// db is the harness's store, which the door's displaced claim is taken on.
	db *fakeDB

	submissions []promptqueue.Submission
	disposition promptqueue.Disposition
	err         error
	// leaseEvents records, per OnLeaseChanged, whether the workspace's merge
	// lease stood when the queue was told.
	leaseEvents []bool
}

// ReleaseReconnectHolds is a no-op: no merge scenario brings a session up.
func (q *fakeQueue) ReleaseReconnectHolds(ids.WorkspaceID) {}

// OnVendorServes is a no-op: no merge scenario stands a vendor block.
func (q *fakeQueue) OnVendorServes(ids.WorkspaceID) {}

func (q *fakeQueue) Submit(_ context.Context, sub promptqueue.Submission) (promptqueue.Disposition, error) {
	q.mu.Lock()
	q.submissions = append(q.submissions, sub)
	q.mu.Unlock()
	return q.disposition, q.err
}

func (q *fakeQueue) Release(context.Context, ids.WorkspaceID, ids.TurnID) error { return nil }
func (q *fakeQueue) Drop(context.Context, ids.WorkspaceID, ids.TurnID) error    { return nil }
func (q *fakeQueue) Accept(context.Context, ids.WorkspaceID, ids.TurnID) error  { return nil }
func (q *fakeQueue) BeginEdit(context.Context, ids.WorkspaceID, ids.TurnID, promptqueue.EditorProbe) error {
	return nil
}
func (q *fakeQueue) CommitEdit(context.Context, ids.WorkspaceID, ids.TurnID, *conversationv1.UserSaid) error {
	return nil
}
func (q *fakeQueue) CancelEdit(context.Context, ids.WorkspaceID, ids.TurnID) error { return nil }
func (q *fakeQueue) EditorGone(ids.WorkspaceID)                                    {}
func (q *fakeQueue) Editing(ids.WorkspaceID) (promptqueue.Edit, bool) {
	return promptqueue.Edit{}, false
}
func (q *fakeQueue) Fold(context.Context, ids.WorkspaceID, ids.TurnID, ids.TurnID) error {
	return nil
}
func (q *fakeQueue) SubmitSessionAct(context.Context, ids.WorkspaceID, promptqueue.Act) error {
	return nil
}
func (q *fakeQueue) OnTurnEnded(ids.WorkspaceID, ids.TurnID, wsm.TurnClose) {}
func (q *fakeQueue) OnTurnAdopted(ids.WorkspaceID, ids.TurnID)              {}
func (q *fakeQueue) OnTurnsEndedUnobserved(ids.WorkspaceID, []ids.TurnID)   {}
func (q *fakeQueue) Reviving(ids.WorkspaceID) bool                          { return false }
func (q *fakeQueue) OnLeaseChanged(ws ids.WorkspaceID) {
	q.db.mu.Lock()
	lease, held := q.db.leases[ws]
	q.db.mu.Unlock()
	q.mu.Lock()
	q.leaseEvents = append(q.leaseEvents, held && lease.Holder == wsm.HolderMerge)
	q.mu.Unlock()
}
func (q *fakeQueue) RestoreHolds(context.Context) error { return nil }

// HeldSince is never reached by the merge orchestrator; answering it loudly
// keeps a new caller from passing silently.
func (q *fakeQueue) HeldSince(context.Context, ids.WorkspaceID, time.Time) ([]ids.TurnID, error) {
	return nil, errors.New("fakeQueue: the merge orchestrator never asks for holds since a time")
}

// RollBack is never reached by the merge orchestrator; answering it loudly
// keeps a new caller from passing silently.
func (q *fakeQueue) RollBack(context.Context, ids.WorkspaceID, time.Time, []ids.TurnID, func(context.Context) error) error {
	return errors.New("fakeQueue: the merge orchestrator never rolls back")
}

// ClaimDisplacedTurn is the queue's door claim, taken on the harness's store
// exactly as the real queue takes it on the durable one.
func (q *fakeQueue) ClaimDisplacedTurn(ctx context.Context, _ ids.WorkspaceID, turn ids.TurnID) (bool, error) {
	claim, err := q.db.ClaimDisplacedTurn(ctx, turn, time.Time{})
	return claim.Claimed, err
}

// CloseOrphans is never reached by the merge orchestrator; answering it
// loudly keeps a new caller from passing silently.
func (q *fakeQueue) CloseOrphans(context.Context, ids.WorkspaceID, time.Time) (wsm.OrphanReport, error) {
	return wsm.OrphanReport{}, errors.New("fakeQueue: the merge orchestrator never closes orphans")
}

// RequestBounce is never reached by the merge orchestrator; answering it
// loudly keeps a new caller from passing silently.
func (q *fakeQueue) RequestBounce(context.Context, ids.WorkspaceID, bounce.Request) (bounce.Decision, error) {
	return bounce.Decision{}, errors.New("fakeQueue: the merge orchestrator never asks for a bounce")
}

// EndKeptDrain is never asked by the merge orchestrator; a handover's reclaim
// is the rollout's.
func (q *fakeQueue) EndKeptDrain(ids.WorkspaceID) {}

// The handover's queue half is never reached by the merge orchestrator;
// answering it loudly keeps a new caller from passing silently.
func (q *fakeQueue) SealMove(context.Context, ids.WorkspaceID) (bounce.Handoff, []bounce.Request, error) {
	return bounce.Handoff{}, nil, errors.New("fakeQueue: the merge orchestrator never seals a move")
}
func (q *fakeQueue) UnsealMove(context.Context, ids.WorkspaceID, bounce.Handoff) error {
	return errors.New("fakeQueue: the merge orchestrator never unseals a move")
}
func (q *fakeQueue) AdoptHandoff(context.Context, ids.WorkspaceID, bounce.Handoff) error {
	return errors.New("fakeQueue: the merge orchestrator never adopts a handoff")
}
func (q *fakeQueue) RejudgeHeld(context.Context, ids.WorkspaceID) error {
	return errors.New("fakeQueue: the merge orchestrator never re-judges holds")
}

func (q *fakeQueue) OnFree(ids.WorkspaceID) {}
func (q *fakeQueue) OnDeparted(ids.WorkspaceID, promptqueue.Watcher, sessionwatcher.Departure) {
}

// Drain: this fake runs nothing in the background, so its work is always
// already done.
func (q *fakeQueue) Drain(time.Duration) bool { return true }

// origins reports the origins the merge submitted under, in order. The origin
// is the merge's whole attribution, so it is what the phase tests assert.
func (q *fakeQueue) origins() []conversationv1.PromptOrigin {
	q.mu.Lock()
	defer q.mu.Unlock()
	var out []conversationv1.PromptOrigin
	for _, sub := range q.submissions {
		out = append(out, sub.Origin)
	}
	return out
}

// countOrigin reports how many submissions carried one origin, which is how
// "exactly once" is asserted.
func (q *fakeQueue) countOrigin(origin conversationv1.PromptOrigin) int {
	n := 0
	for _, got := range q.origins() {
		if got == origin {
			n++
		}
	}
	return n
}

// fakeFeed records the synthesized rows and the output addresses. Like every
// resolver fake here it embeds its interface, so it declares only what the
// orchestrator calls and any other call panics loudly rather than answering a
// zero value.
type fakeFeed struct {
	feed.Resolver
	// seq stamps every push with the harness's shared sequence.
	seq func() int
	mu  sync.Mutex

	rows      []synthesized
	addresses []*wsm.OutputAddress
	// onUpsert, when set, runs on every push before it is recorded, so a test
	// can read the harness's state at the instant a row is published.
	onUpsert func(row *frontendv1.FeedRow)
}

// synthesized is one published row with the feed it landed on and when it was
// pushed, in the harness's shared sequence.
type synthesized struct {
	WS   ids.WorkspaceID
	Feed feedid.Feed
	Row  *frontendv1.FeedRow
	At   int
}

// UpsertDurable is the ONE way the orchestrator publishes a row: every merge
// row is drawn again by a new daemon. The fake declares no UpsertSynthesized,
// so a merge row published any other way panics on the embedded nil
// interface rather than passing unrecorded.
func (f *fakeFeed) UpsertDurable(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow) {
	at := f.seq()
	if f.onUpsert != nil {
		f.onUpsert(row)
	}
	f.mu.Lock()
	f.rows = append(f.rows, synthesized{WS: ws, Feed: feed, Row: row, At: at})
	f.mu.Unlock()
}

func (f *fakeFeed) SetOutputAddress(_ ids.WorkspaceID, addr *wsm.OutputAddress) {
	f.mu.Lock()
	f.addresses = append(f.addresses, addr)
	f.mu.Unlock()
}

// tabs reports the tab kinds published, in order, one entry per push.
func (f *fakeFeed) tabs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, row := range f.rows {
		tab := row.Row.GetMergeTab()
		if tab == nil {
			continue
		}
		out = append(out, tabKindOf(tab))
	}
	return out
}

// tabSequence reports the tab kinds in the order they were FIRST opened, which
// is the sequence a method's test asserts.
func (f *fakeFeed) tabSequence() []string {
	var out []string
	seen := map[string]bool{}
	for _, kind := range f.tabs() {
		if seen[kind] {
			continue
		}
		seen[kind] = true
		out = append(out, kind)
	}
	return out
}

// lastTabOfKind reports the last push of one tab kind.
func (f *fakeFeed) lastTabOfKind(kind string) *frontendv1.FeedMergeTab {
	f.mu.Lock()
	defer f.mu.Unlock()
	var found *frontendv1.FeedMergeTab
	for _, row := range f.rows {
		tab := row.Row.GetMergeTab()
		if tab != nil && tabKindOf(tab) == kind {
			found = tab
		}
	}
	return found
}

// heads reports every head-row push's result arm name.
func (f *fakeFeed) heads() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, row := range f.rows {
		activity := row.Row.GetActivity()
		if activity == nil || activity.GetMerge() == nil {
			continue
		}
		switch activity.GetMerge().GetResult().(type) {
		case *frontendv1.FeedMerge_Success:
			out = append(out, "success")
		case *frontendv1.FeedMerge_Error:
			out = append(out, "error")
		default:
			out = append(out, "update")
		}
	}
	return out
}

// lastMergeErrorArm names the error arm of the last head row that carried one.
func (f *fakeFeed) lastMergeErrorArm() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	arm := ""
	for _, row := range f.rows {
		err := row.Row.GetActivity().GetMerge().GetError()
		if err == nil {
			continue
		}
		switch err.GetReason().(type) {
		case *frontendv1.FeedMergeError_Failed:
			arm = "failed"
		case *frontendv1.FeedMergeError_Abandoned:
			arm = "abandoned"
		default:
			arm = "unset"
		}
	}
	return arm
}

// lastAbandonedSummary answers the summary the abandoned terminal carries.
func (f *fakeFeed) lastAbandonedSummary() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	summary := ""
	for _, row := range f.rows {
		if abandoned := row.Row.GetActivity().GetMerge().GetError().GetAbandoned(); abandoned != nil {
			summary = abandoned.GetSummary()
		}
	}
	return summary
}

// tabKindOf names a tab row's kind arm.
func tabKindOf(tab *frontendv1.FeedMergeTab) string {
	switch tab.GetKind().(type) {
	case *frontendv1.FeedMergeTab_Queue:
		return TabQueue
	case *frontendv1.FeedMergeTab_PrePrompt:
		return TabPrePrompt
	case *frontendv1.FeedMergeTab_Rebasing:
		return TabRebasing
	case *frontendv1.FeedMergeTab_Committing:
		return TabCommitting
	case *frontendv1.FeedMergeTab_UpdatingMain:
		return TabUpdatingMain
	case *frontendv1.FeedMergeTab_Conflicts:
		return TabConflicts
	case *frontendv1.FeedMergeTab_Tests:
		return TabTests
	case *frontendv1.FeedMergeTab_Fixes:
		return TabFixes
	case *frontendv1.FeedMergeTab_PostPrompt:
		return TabPostPrompt
	}
	return ""
}

// fakeFooter and fakeSidebar record the facts each surface was told.
type fakeFooter struct {
	footer.Resolver
	mu    sync.Mutex
	facts []footer.MergeFacts
}

func (f *fakeFooter) SetMerge(_ ids.WorkspaceID, facts footer.MergeFacts) {
	f.mu.Lock()
	f.facts = append(f.facts, facts)
	f.mu.Unlock()
}

// last reports the footer's most recent facts.
func (f *fakeFooter) last() footer.MergeFacts {
	f.mu.Lock()
	defer f.mu.Unlock()
	if len(f.facts) == 0 {
		return footer.MergeFacts{}
	}
	return f.facts[len(f.facts)-1]
}

type fakeSidebar struct {
	sidebar.Resolver
	mu    sync.Mutex
	facts []footer.MergeFacts
}

func (s *fakeSidebar) SetMerge(_ ids.WorkspaceID, facts footer.MergeFacts) {
	s.mu.Lock()
	s.facts = append(s.facts, facts)
	s.mu.Unlock()
}

// fakeHolds records the tray's offers.
type fakeHolds struct {
	holds.Resolver
	mu     sync.Mutex
	offers []*frontendv1.HeldOffer
}

func (h *fakeHolds) SetOffer(_ ids.WorkspaceID, offer *frontendv1.HeldOffer) {
	h.mu.Lock()
	h.offers = append(h.offers, offer)
	h.mu.Unlock()
}

// standing reports the offer currently standing, nil when none is.
func (h *fakeHolds) standing() *frontendv1.HeldOffer {
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.offers) == 0 {
		return nil
	}
	return h.offers[len(h.offers)-1]
}

// fakePainter turns text into one plain span, which is enough for the gate's
// tests: what the painter does with an escape is the paint package's own
// subject, not the merge's.
type fakePainter struct{}

func (fakePainter) ParseANSI(text string) (paint.Spans, error) {
	return paint.Spans{{Text: text}}, nil
}
func (fakePainter) Highlight(_, code string) (paint.Spans, error) {
	return paint.Spans{{Text: code}}, nil
}

// errStateClientClosed is what a write against a closed state client answers.
var errStateClientClosed = errors.New("wsm: begin transaction: sql: database is closed")

// fakeRunner is the scripted test-all script. It answers from a queue of runs,
// so a fixes loop's second round can differ from its first.
type fakeRunner struct {
	mu sync.Mutex

	runs []scriptedRun
	// before, when set, is called before an invocation answers. It is the seam
	// a test holds a run inside its LONG phase with, so the drain can be
	// exercised against a merge that is mid-gate.
	before func()
	// argv records every invocation, which is what the --suites contract is
	// asserted against.
	argv [][]string
	dirs []string
}

// scriptedRun is one answer from the fake script.
type scriptedRun struct {
	Output string
	Code   int
	Err    error
}

func (r *fakeRunner) RunLines(ctx context.Context, dir string, argv []string, onLine func(string)) (string, int, error) {
	r.mu.Lock()
	before := r.before
	r.mu.Unlock()
	if before != nil {
		// THE FAKE IS CUT BY ITS CONTEXT, as the real runner is (it kills the
		// script's process group): a test holding a run inside its gate sees
		// the drain cut it.
		held := make(chan struct{})
		go func() {
			before()
			close(held)
		}()
		select {
		case <-held:
		case <-ctx.Done():
			return "", 0, fmt.Errorf("fake script: %w", ctx.Err())
		}
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	r.argv = append(r.argv, argv)
	r.dirs = append(r.dirs, dir)
	if len(r.runs) == 0 {
		return "", 0, nil
	}
	run := r.runs[0]
	r.runs = r.runs[1:]
	if run.Err == nil && onLine != nil {
		for _, line := range strings.SplitAfter(run.Output, "\n") {
			if line != "" {
				onLine(strings.TrimSuffix(line, "\n"))
			}
		}
	}
	return run.Output, run.Code, run.Err
}

// harness is one orchestrator with every fake it was built from.
type harness struct {
	t *testing.T

	o       *orchestrator
	db      *fakeDB
	git     *fakeGit
	queue   *fakeQueue
	feed    *fakeFeed
	footer  *fakeFooter
	sidebar *fakeSidebar
	holds   *fakeHolds
	runner  *fakeRunner
	logs    *dlog.TestSurfaces
	// pauseInTerminal is the terminal seam the drain's tests wire.
	pauseInTerminal AdmissionPause
	rollout         *fakeRollout
	stateDir        string
	targetD         string
	sourceD         string

	// briefs answers the brief loader; a name absent from it is a LOUD failure,
	// exactly as a missing file is.
	briefs map[string][]string
	// policy is what the repository's own `.agent-repl/prompts` holds, by
	// brief name; policyErr fails one brief's read even though it is present.
	policy    map[string]string
	policyErr map[string]error
	// repoRoot is the harness repository's main checkout root, which is where
	// its policy directory would live.
	repoRoot string
	// pauseAfterCapture stands in for the production-nil admission seam.
	pauseAfterCapture AdmissionPause
	// briefValues records the values each brief was spliced with, which is how
	// the escalation constants are asserted to reach the agent.
	briefValues []map[string]string
	// turnCloses answers AwaitTurnEnd, consumed in order.
	turnCloses []wsm.TurnClose
	// startedSessions records the revivals a configured prompt caused.
	startedSessions []ids.WorkspaceID
	// stoppedSessions records the landed workspaces whose sessions were reaped
	// before their worktrees were removed.
	stoppedSessions []ids.WorkspaceID
	stopForces      []bool
	stopAt          int
	stopErr         error
	// occupancyReleases counts the occupancy guards dropped.
	occupancyReleases int
	// displaced is what CaptureDisplaced answers with, nil for none.
	displaced *Displaced
	// captures counts the CaptureDisplaced calls: a merge an agent asked for
	// must make none.
	captures int
	// freeness answers the admission's freeness wait; free by default.
	freeness *fakeFreeness
	// awaitedTurns records every turn AwaitTurnEnd waited on.
	awaitedTurns []ids.TurnID
	// awaitGate, when set, holds every AwaitTurnEnd until it is closed or the
	// wait's context ends: the requesting turn still running.
	awaitGate chan struct{}
	// inFlight is the turn TurnInFlight answers, empty for none.
	inFlight ids.TurnID
	// script is the gate's script, a real file so the gate's preflight finds
	// it; gateTrees records the tree each gate command was resolved for.
	script    string
	gateTrees []string
	// seq is the shared monotonic sequence both the feed and the git fakes
	// stamp their calls with, so an ordering between two subsystems is asserted
	// on the real order.
	seq int

	mu  sync.Mutex
	now time.Time
}

// fakeFreeness is the fleet's freeness. A busy workspace's AwaitFree signals
// `waiting` on entry and then blocks until `release` is closed or ctx ends,
// answering awaitErr, so a test observes the held merge on a channel rather
// than on elapsed time.
type fakeFreeness struct {
	busy     bool
	awaitErr error
	waiting  chan struct{}
	release  chan struct{}

	mu      sync.Mutex
	awaited int
}

func (f *fakeFreeness) Free(ids.WorkspaceID) bool { return !f.busy }

func (f *fakeFreeness) AwaitFree(ctx context.Context, _ ids.WorkspaceID) error {
	f.mu.Lock()
	f.awaited++
	f.mu.Unlock()
	if f.waiting != nil {
		close(f.waiting)
	}
	if f.release != nil {
		select {
		case <-f.release:
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return f.awaitErr
}

func (f *fakeFreeness) awaits() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.awaited
}

// fakeRollout records the self-reload trigger and when it fired.
type fakeRollout struct {
	mu sync.Mutex

	fired  int
	landed []gitclient.Commit
	// leasesAtFire records how many leases had been released when it fired,
	// which is how "only after release" is asserted.
	leasesAtFire int
	db           *fakeDB
}

func (t *fakeRollout) Landed(_ context.Context, landed []gitclient.Commit) {
	t.mu.Lock()
	t.fired++
	t.landed = landed
	t.db.mu.Lock()
	t.leasesAtFire = len(t.db.releasedLeases)
	t.db.mu.Unlock()
	t.mu.Unlock()
}

// theWorkspace is the workspace every harness merges.
const theWorkspace = ids.WorkspaceID("ws-1")

// theRepository is the repository every harness workspace belongs to. It is
// registered because a merge resolves its repository's own merge policy, and
// a workspace naming a repository the registry does not hold is a fault.
const theRepository = ids.RepoID("repo-1")

// harnessPolicy is the harness's policy probe: a repository states exactly the
// briefs the harness put in `policy`, and nothing touches a disk.
type harnessPolicy struct{ h *harness }

func (p harnessPolicy) Missing(_ string, names []string) []string {
	p.h.mu.Lock()
	defer p.h.mu.Unlock()
	var missing []string
	for _, name := range names {
		if _, ok := p.h.policy[name]; !ok {
			missing = append(missing, name+prompts.Suffix)
		}
	}
	return missing
}

func (p harnessPolicy) Text(_ string, name string) (string, error) {
	p.h.mu.Lock()
	defer p.h.mu.Unlock()
	text, ok := p.h.policy[name]
	if !ok {
		return "", fmt.Errorf("no policy brief %q", name)
	}
	if err := p.h.policyErr[name]; err != nil {
		return "", err
	}
	return text, nil
}

// newHarness builds an orchestrator whose admission is driven a step at a time,
// so a queue's ordering is asserted without waiting on a goroutine.
func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{
		t:         t,
		db:        newFakeDB(),
		footer:    &fakeFooter{},
		sidebar:   &fakeSidebar{},
		holds:     &fakeHolds{},
		runner:    &fakeRunner{},
		logs:      dlog.NewTestSurfaces(),
		stateDir:  t.TempDir(),
		now:       time.Unix(1700000000, 0).UTC(),
		briefs:    map[string][]string{},
		policy:    map[string]string{},
		policyErr: map[string]error{},
		freeness:  &fakeFreeness{},
	}
	h.queue = &fakeQueue{db: h.db}
	h.git = newFakeGit(h.next)
	h.feed = &fakeFeed{seq: h.next}
	h.rollout = &fakeRollout{db: h.db}
	h.targetD = t.TempDir()
	h.sourceD = t.TempDir()
	h.briefs[BriefConflictResolve] = []string{"conflict_commit", "source_branch", "worktree_dir", "target_branch", "conflicted_files"}
	h.briefs[BriefTestFailureResolve] = []string{"source_branch", "worktree_dir", "target_branch", "failing_suites", "archive_path", "failure_tail", "attempt", "max_attempts", "escalation_file", "escalation_marker"}

	h.repoRoot = t.TempDir()
	h.script = filepath.Join(h.stateDir, "test-all.sh")
	if err := os.WriteFile(h.script, []byte("#!/usr/bin/env bash\n"), 0o644); err != nil {
		t.Fatalf("writing the gate's script: %v", err)
	}
	h.registerRepo(theRepository, h.repoRoot)
	h.register(theWorkspace, "ws-one")
	o, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the orchestrator: %v", err)
	}
	h.o = o
	// NOTHING THE ORCHESTRATOR STARTED OUTLIVES ITS TEST; this waits, bounded,
	// for every goroutine it counted to be gone.
	t.Cleanup(func() {
		gone := make(chan struct{})
		go func() { o.live.Wait(); close(gone) }()
		select {
		case <-gone:
		case <-time.After(5 * time.Second):
			t.Error("a merge run outlived its test")
		}
	})
	return h
}

// deps assembles the dependency set from the harness's fakes.
func (h *harness) deps() Deps {
	return Deps{
		DB: h.db, Git: h.git, Queue: h.queue, Feed: h.feed, Footer: h.footer,
		Sidebar: h.sidebar, Holds: h.holds, PromptsDir: "prompts",
		Policy:      harnessPolicy{h: h},
		Briefs:      h.loadBrief,
		SelfRepoDir: "/self/checkout",
		StateDir:    h.stateDir,
		TestCommand: func(tree string) []string {
			h.mu.Lock()
			defer h.mu.Unlock()
			h.gateTrees = append(h.gateTrees, tree)
			return []string{"bash", h.script}
		},
		TestRunner: h.runner,
		Painter:    fakePainter{},
		StartSession: func(_ context.Context, ws ids.WorkspaceID) error {
			h.mu.Lock()
			h.startedSessions = append(h.startedSessions, ws)
			h.mu.Unlock()
			h.db.mu.Lock()
			h.db.sessions[ws] = wsm.Session{Workspace: ws}
			h.db.mu.Unlock()
			return nil
		},
		StopSession: func(_ context.Context, ws ids.WorkspaceID, force bool) error {
			at := h.next()
			h.mu.Lock()
			h.stoppedSessions = append(h.stoppedSessions, ws)
			h.stopForces = append(h.stopForces, force)
			if h.stopAt == 0 {
				h.stopAt = at
			}
			err := h.stopErr
			h.mu.Unlock()
			return err
		},
		Occupy: func(ids.WorkspaceID, string) (func(), bool, error) {
			return func() {
				h.mu.Lock()
				h.occupancyReleases++
				h.mu.Unlock()
			}, true, nil
		},
		AwaitTurnEnd: func(ctx context.Context, _ ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error) {
			h.mu.Lock()
			gate := h.awaitGate
			h.mu.Unlock()
			if gate != nil {
				select {
				case <-gate:
				case <-ctx.Done():
					return 0, ctx.Err()
				}
			}
			h.mu.Lock()
			defer h.mu.Unlock()
			h.awaitedTurns = append(h.awaitedTurns, turn)
			if len(h.turnCloses) == 0 {
				return wsm.CloseCompleted, nil
			}
			close := h.turnCloses[0]
			h.turnCloses = h.turnCloses[1:]
			return close, nil
		},
		CaptureDisplaced: func(context.Context, ids.WorkspaceID) (Displaced, bool, error) {
			h.mu.Lock()
			h.captures++
			h.mu.Unlock()
			if h.displaced == nil {
				return Displaced{}, false, nil
			}
			return *h.displaced, true, nil
		},
		Freeness:          h.freeness,
		PauseAfterCapture: h.pauseAfterCapture,
		PauseInTerminal:   h.pauseInTerminal,
		TurnInFlight: func(ids.WorkspaceID) (ids.TurnID, bool) {
			h.mu.Lock()
			defer h.mu.Unlock()
			return h.inFlight, h.inFlight != ""
		},
		Home:    "/home/tester",
		Rollout: h.rollout,
		Now:     h.clock,
		Log:     h.logs,
	}
}

// next stamps the shared sequence.
func (h *harness) next() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.seq++
	return h.seq
}

// clock advances a millisecond per read, so two stamps in one run are ordered
// without any test ever sleeping.
func (h *harness) clock() time.Time {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.now = h.now.Add(time.Millisecond)
	return h.now
}

// loadBrief answers the brief loader. A brief the harness does not hold fails
// LOUDLY, which is what a missing file does; a placeholder the values do not
// cover fails the same way, which is what a bad splice does.
func (h *harness) loadBrief(name string, values map[string]string) (string, error) {
	placeholders, ok := h.briefs[name]
	if !ok {
		return "", fmt.Errorf("no brief %q", name)
	}
	text := name
	for _, key := range placeholders {
		value, covered := values[key]
		if !covered {
			return "", fmt.Errorf("brief %q has no value for %q", name, key)
		}
		text += " " + value
	}
	h.mu.Lock()
	h.briefValues = append(h.briefValues, values)
	h.mu.Unlock()
	return text, nil
}

// register records a workspace with the geometry a merge needs.
func (h *harness) register(ws ids.WorkspaceID, name string) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.workspaces[ws] = wsm.Workspace{ID: ws, Name: name, Dir: h.sourceD, Branch: "feature", Repo: theRepository}
	h.db.jobs[ws] = wsm.CreationJob{
		Workspace: ws,
		Layout: wsm.MergeLayout{
			SourceBranch: "feature", SourceDir: h.sourceD, TargetDir: h.targetD, Origin: "create",
		},
	}
	h.db.sessions[ws] = wsm.Session{Workspace: ws}
	h.git.mu.Lock()
	h.git.branches[h.sourceD] = "feature"
	// ONE REPOSITORY: every worktree of it reports the target's common dir.
	h.git.commonDirs[h.sourceD] = string(h.repoKey())
	h.git.mu.Unlock()
}

// registerRepo records one repository in the fake registry, which is what a
// scoped pause resolves its repository ref against.
func (h *harness) registerRepo(id ids.RepoID, dir string) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.repos = append(h.db.repos, wsm.Repository{ID: id, Dir: dir, Name: string(id), DefaultBranch: "master"})
}

// repoKey is the queue key the harness's target resolves to.
func (h *harness) repoKey() wsm.RepoKey { return wsm.RepoKey(h.targetD + "/.git") }

// landsCleanly scripts a clean no-ff merge producing one commit.
func (h *harness) landsCleanly(sha string) {
	h.git.outcomes = append(h.git.outcomes, gitclient.MergeOutcome{Landed: &gitclient.Commit{SHA: sha}})
	h.git.landed = []gitclient.Commit{{SHA: sha, Subject: "the work"}}
}

// gatePasses scripts one passing gate run.
func (h *harness) gatePasses(suites ...string) {
	out := ""
	for _, suite := range suites {
		out += fmt.Sprintf("[agent-repl-tests] %s: passed in 3s\n", suite)
	}
	h.runner.runs = append(h.runner.runs, scriptedRun{Output: out, Code: 0})
}

// gateFails scripts one failing gate run.
func (h *harness) gateFails(suite string) {
	h.runner.runs = append(h.runner.runs, scriptedRun{
		Output: fmt.Sprintf("[agent-repl-tests] ERROR: %s failed after 4s with exit code 1\n", suite),
		Code:   1,
	})
}

// admit runs the pump once for the harness's repository.
func (h *harness) admit(ctx context.Context) error {
	_, err := h.o.pumpOnce(ctx, h.repoKey())
	return err
}

// emacsRepo makes the harness's target this daemon's OWN repository, which is
// what selects the merge-commit method.
func (h *harness) emacsRepo() {
	h.git.sameRepo = true
	h.git.commonDirs[h.targetD] = string(h.repoKey())
}

// ownCheckout makes the target the daemon's own checkout rather than a sibling
// worktree of it, which is the extra condition the self-reload requires.
func (h *harness) ownCheckout() {
	h.emacsRepo()
	h.o.deps.SelfRepoDir = h.targetD
}

// mergeConflicted scripts a no-ff merge that stopped on conflicts.
func mergeConflicted(files ...string) gitclient.MergeOutcome {
	return gitclient.MergeOutcome{Conflicted: files}
}

// configureActions records the workspace's configured before/after prompts.
func (h *harness) configureActions(before, after []string) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	job := h.db.jobs[theWorkspace]
	job.Actions = wsm.MergeActions{Before: before, After: after}
	h.db.jobs[theWorkspace] = job
}

// escalate writes the record the fixes agent uses to end its loop without a
// passing suite, in its own worktree.
func (h *harness) escalate(why string) {
	h.t.Helper()
	body := EscalationMarker + "\n" + why + "\n"
	if err := os.WriteFile(filepath.Join(h.sourceD, EscalationFile), []byte(body), 0o644); err != nil {
		h.t.Fatalf("writing the escalation record: %v", err)
	}
}

// leaseID reports the lease the harness's merge holds.
func (h *harness) leaseID(t *testing.T) wsm.LeaseID {
	t.Helper()
	lease, held, err := h.db.Lease(context.Background(), theWorkspace)
	if err != nil || !held {
		t.Fatalf("no lease is held: %v", err)
	}
	return lease.ID
}

// enqueue queues the harness's merge as the USER asks for one, failing the
// test if it is refused.
func enqueue(t *testing.T, h *harness) {
	t.Helper()
	if err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, By: RequestedByUser}); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
}

// admitAsync admits the harness's merge on its own goroutine, for the tests whose
// merge parks and therefore never returns on its own.
func admitAsync(h *harness, ctx context.Context) <-chan error {
	done := make(chan error, 1)
	go func() { _, err := h.o.pumpOnce(ctx, h.repoKey()); done <- err }()
	return done
}

// equal reports whether two string slices match.
func equal(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// names renders a directory listing for a failure message.
func names(entries []os.DirEntry) []string {
	var out []string
	for _, e := range entries {
		out = append(out, e.Name())
	}
	return out
}

// roundsOfKind reports the rounds one tab kind was drawn with, in order, once
// per round.
func (f *fakeFeed) roundsOfKind(kind string) []int {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []int
	seen := map[uint32]bool{}
	for _, row := range f.rows {
		tab := row.Row.GetMergeTab()
		if tab == nil || tabKindOf(tab) != kind {
			continue
		}
		round := tab.GetLabel().GetRound()
		if seen[round] {
			continue
		}
		seen[round] = true
		out = append(out, int(round))
	}
	return out
}

// ListRepositories answers the fake registry, in registration order.
func (d *fakeDB) ListRepositories(ctx context.Context) ([]wsm.Repository, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	out := make([]wsm.Repository, len(d.repos))
	copy(out, d.repos)
	return out, nil
}

// gateBroken scripts n gate runs that exit 127: the shell could not find the
// gate's command, so the gate never ran.
func (h *harness) gateBroken(n int) {
	for i := 0; i < n; i++ {
		h.runner.runs = append(h.runner.runs, scriptedRun{
			Output: "bash: modules/app/agent-repl/bin/test-all.sh: No such file or directory\n", Code: 127})
	}
}

// openIntervals names every ledger interval of the harness's merge that was
// opened and never closed, as "<kind>/<round>".
func (h *harness) openIntervals() []string {
	entries, _ := h.db.MergeLedger(context.Background(), theWorkspace)
	open := map[string]bool{}
	for _, entry := range entries {
		for _, interval := range entry.Intervals {
			key := roundKey(interval.Kind, interval.Round)
			if interval.EndedAt == nil {
				open[key] = true
			} else {
				delete(open, key)
			}
		}
	}
	var out []string
	for key := range open {
		out = append(out, key)
	}
	sort.Strings(out)
	return out
}

// leaseHeld reports whether the harness's workspace still holds a lease.
func (h *harness) leaseHeld() bool {
	_, held, _ := h.db.Lease(context.Background(), theWorkspace)
	return held
}

// lockFree reports whether the harness repository's kernel lock is free, by
// taking it and giving it straight back.
func (h *harness) lockFree(t *testing.T) bool {
	t.Helper()
	lock, taken, err := acquireRepoLock(h.o.lockDir, string(h.repoKey()))
	if err != nil {
		t.Fatalf("probing the repository lock: %v", err)
	}
	if taken {
		if err := lock.Release(); err != nil {
			t.Fatalf("releasing the probe's lock: %v", err)
		}
	}
	return taken
}

// queuedEntries names the workspaces on the harness repository's durable
// queue, in order.
func (h *harness) queuedEntries() []string {
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	var out []string
	for _, entry := range entries {
		out = append(out, string(entry.Workspace))
	}
	return out
}

// otherWorkspace is the second workspace a source-workspace merge lands.
const otherWorkspace = ids.WorkspaceID("ws-2")

// registerOther records a second workspace of the harness's repository, with
// its own worktree on its own branch, and answers its worktree.
func (h *harness) registerOther(ws ids.WorkspaceID, name, branch string) string {
	h.t.Helper()
	dir := h.t.TempDir()
	h.db.mu.Lock()
	h.db.workspaces[ws] = wsm.Workspace{ID: ws, Name: name, Dir: dir, Branch: branch, Repo: theRepository}
	h.db.jobs[ws] = wsm.CreationJob{Workspace: ws, Layout: wsm.MergeLayout{
		SourceBranch: branch, SourceDir: dir, TargetDir: h.targetD, Origin: "create"}}
	h.db.sessions[ws] = wsm.Session{Workspace: ws}
	h.db.mu.Unlock()
	h.git.mu.Lock()
	h.git.branches[dir] = branch
	h.git.commonDirs[dir] = string(h.repoKey())
	h.git.mu.Unlock()
	return dir
}

// request records the harness workspace's merge of one source, as asked by.
func (h *harness) request(t *testing.T, source wsm.MergeSource, by Requester) error {
	t.Helper()
	return h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: source, By: by})
}

// runWait runs the harness workspace's standing request wait to its end, as
// its goroutine would: the requesting turn's end, then its place in line.
func (h *harness) runWait(t *testing.T) {
	t.Helper()
	h.o.mu.Lock()
	wait := h.o.requested[theWorkspace]
	h.o.mu.Unlock()
	if wait == nil {
		t.Fatal("no request wait stands for the harness workspace")
	}
	h.o.waitThenQueue(wait)
}

// entryState answers the harness workspace's queue entry's state, and false
// when it has none.
func (h *harness) entryState() (wsm.MergeQueueState, bool) {
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	for _, entry := range entries {
		if entry.Workspace == theWorkspace {
			return entry.State, true
		}
	}
	return 0, false
}

// steps answers every step the footer was told, in order, one per change.
func (f *fakeFooter) steps() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, facts := range f.facts {
		step := string(facts.Step)
		if facts.State == StateFailed || facts.State == StateMerged {
			step = facts.State
		}
		if len(out) == 0 || out[len(out)-1] != step {
			out = append(out, step)
		}
	}
	return out
}

// all answers every facts value the footer was told.
func (f *fakeFooter) all() []footer.MergeFacts {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]footer.MergeFacts(nil), f.facts...)
}

// recordFor answers the first record with one level and operation.
func (h *harness) recordFor(level, operation string) (dlog.Record, bool) {
	for _, r := range h.logs.Records() {
		if r.Level == level && r.Operation == operation {
			return r, true
		}
	}
	return dlog.Record{}, false
}

// commits answers n commits of work, the rebase's replay.
func commits(n int) []gitclient.Commit {
	out := make([]gitclient.Commit, n)
	for i := range out {
		out[i] = gitclient.Commit{SHA: fmt.Sprintf("c%012d", i+1), Subject: fmt.Sprintf("commit %d", i+1)}
	}
	return out
}

// addresses answers every output address the merge installed.
func (f *fakeFeed) installed() []*wsm.OutputAddress {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]*wsm.OutputAddress(nil), f.addresses...)
}
