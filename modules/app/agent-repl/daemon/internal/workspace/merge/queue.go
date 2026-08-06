package merge

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"claude-repld/internal/dlog"
)

// DurableQueue is merge.Queue plus the completion acknowledgement the frozen
// contract omits.
//
// merge.Queue's Subscribe hands out a plain `<-chan Request`, so a receiver has
// no way to say "this one is finished". Without that word the queue must drop
// its durable record at DELIVERY time, and a daemon bounce mid-cherry-pick
// would lose the request entirely — exactly the durability the queue exists to
// provide. Complete closes the loop: an entry's record lives until the merge
// reaches a terminal outcome, so a bounce replays it instead of forgetting it.
type DurableQueue interface {
	Queue
	// Complete drops repo's HEAD entry, which MUST be req. Only one owner
	// drains a repo and it processes one entry at a time, so the head is
	// unambiguously the entry just delivered; a mismatch is a violated
	// invariant and returns a hard error rather than guessing which entry was
	// meant.
	Complete(repo string, req Request) error

	// RecordStatusWatermark persists runID's last published updated_at_ms onto
	// its durable entry, so the run resumed from that entry after a bounce can
	// seed its clock ABOVE everything the pre-bounce process published.
	//
	// It is called by merge.RunStatus and by nothing else. Two writers of one
	// watermark could lower it, and a lowered watermark is exactly the
	// regression it exists to prevent.
	RecordStatusWatermark(repo, runID string, updatedAtMs int64) error

	// MarkTerminal durably records term on repo's HEAD entry, which MUST be
	// req, and leaves the entry OUTSTANDING.
	//
	// It is the other half of Complete, and it exists for exactly one moment:
	// a run that reached a terminal outcome whose terminal STATUS could not be
	// published. The merge itself is durable (the commits are on the target and
	// the entry is still on the queue), so the terminal WORD is made durable
	// beside it rather than dropped — the entry is kept, marked, and the next
	// boot's Drain re-publishes it instead of replaying a merge that already
	// happened.
	MarkTerminal(repo string, req Request, term TerminalStatus) error
	// PendingTerminal reports the terminal status recorded on repo's HEAD
	// entry, which MUST be req. The bool is false for the ordinary entry, whose
	// run has not reached a terminal at all.
	PendingTerminal(repo string, req Request) (TerminalStatus, bool, error)

	// EvictWaiting drops every entry on repo's queue for workspace that is NOT
	// the head, and returns the requests it dropped in queue order.
	//
	// THE HEAD IS NEVER EVICTED, which is why the verb is "waiting" rather than
	// "cancel". The head is the entry the repository's one drain goroutine has
	// already been handed: it holds the shim lease and may be mid-cherry-pick,
	// so dropping its record would leave a merge running with nothing left to
	// Complete. Every entry behind it has been delivered to nobody, so removing
	// it is a pure queue operation with no second party to race.
	//
	// A remove failure LEAVES THAT ENTRY EXACTLY WHERE IT WAS — memory follows
	// disk here as everywhere else in this file — and travels back beside the
	// entries that did go, so a caller says the terminal word for precisely the
	// merges that really left the queue.
	EvictWaiting(repo, workspace string) ([]Request, error)
}

// TerminalStatus is a run's TERMINAL word, recorded durably when publishing it
// FAILED.
//
// It carries exactly what re-publishing the status needs and nothing else: the
// merged/failed classification, the cause a frontend renders, and — for a merged
// run only — the after-action failure that rides the merged status. A run's
// PROGRESS is deliberately absent for the same reason it is absent from the rest
// of the entry: the commit cursor of a dead process describes work the resumed
// one has not done.
type TerminalStatus struct {
	// Outcome is the terminal classification, either OutcomeMerged or
	// OutcomeFailed. The two parking outcomes are not terminal and are refused.
	Outcome Outcome
	// Cause is the terminal status's cause text, the sentence a user reads.
	Cause string
	// AfterActionError is the after-action's failure carried on a merged
	// status. It is empty for a failed run, which never reached an after-action.
	AfterActionError string
}

// validate refuses a terminal record that could not be re-published faithfully.
// A record written wrong is worse than none: the boot replay would put an
// invented terminal word on the wire under a run id a user is watching.
func (t TerminalStatus) validate() error {
	switch t.Outcome {
	case OutcomeMerged, OutcomeFailed:
	default:
		return fmt.Errorf("merge: terminal status outcome %q is not terminal", t.Outcome)
	}
	if t.Cause == "" {
		return fmt.Errorf("merge: terminal status for outcome %q needs a cause", t.Outcome)
	}
	if t.Outcome == OutcomeFailed && t.AfterActionError != "" {
		return fmt.Errorf("merge: a failed terminal status carries no after-action error, got %q", t.AfterActionError)
	}
	return nil
}

var _ DurableQueue = (*FileQueue)(nil)

// queueFilePrefix names the durable entry files. The dot-temp write below
// relies on the prefix so a half-written file can never be read as an entry.
const queueFilePrefix = "merge_request_"

// FileQueue is the durable merge.Queue substrate: one JSON file per
// outstanding request, under a per-repository directory, written with the same
// atomic dot-temp-then-rename discipline as the workspacecmd claim-file inbox.
//
// Durability is the point. A workspace whose merge lands on the daemon's own
// repository bounces the daemon mid-queue; the entries that outlive the bounce
// are what merge.Coordinator.Drain reconstructs at the next boot.
//
// Delivery is strictly one-at-a-time per repository: the serve loop hands out
// the head entry and then waits for its Complete before looking at the next.
// That is what makes "index 1 is the merge currently cherry-picking" true by
// construction rather than by convention.
type FileQueue struct {
	dir  string
	logf dlog.Logf

	mu    sync.Mutex
	repos map[string]*repoQueue
}

// repoQueue is one repository's in-memory view of its durable entries. The
// slice is the authority while the process lives; the files are the authority
// across a bounce, and hydration reconciles the two exactly once per repo.
type repoQueue struct {
	entries []*queueEntry
	// wake carries a publish notification to a serve loop parked on an empty
	// queue. Buffered by one because the loop rechecks the slice on wake, so a
	// coalesced pair of notifications loses nothing.
	wake chan struct{}
	// subscribed marks that a serve loop owns this repo. A second Subscribe is
	// a violated single-ownership invariant, not a supported topology.
	subscribed bool
}

// queueEntry is one outstanding request plus its durable file name and the
// latch its Complete closes.
type queueEntry struct {
	id   string
	req  Request
	done chan struct{}
	// watermarkMs mirrors the entry file's status_watermark_ms.
	//
	// IT IS DELIBERATELY NOT req.StatusWatermarkMs. Complete identifies the head
	// by comparing whole Request values, so every field of req has to stay
	// exactly as it was delivered; a field that moved while the merge ran would
	// make the entry fail to match itself. req carries the watermark the entry
	// was HYDRATED with (what a resume seeds from), this carries the one the
	// live run has reached.
	watermarkMs int64
	// terminal is the terminal word recorded by MarkTerminal, nil for the
	// ordinary entry. It mirrors what is on disk, and it is set only AFTER the
	// durable write lands — memory must never claim a terminal the next boot
	// cannot read.
	terminal *TerminalStatus
}

// entryFile is the on-disk form of a queue entry. The request's fields are
// mirrored explicitly (rather than embedding Request) so the durable format is
// a stated contract that a later field rename cannot silently break.
//
// Request.Run is absent from it BY CONSTRUCTION, and that is the point of
// mirroring rather than embedding: a *RunStatus is a live publisher bound to
// this process's sink and clock, and nothing about it can be written down.
// RunID is the half that can, and it is written down deliberately — an entry
// replayed by the next boot resumes the run the user has been watching instead
// of appearing as a fresh merge nobody asked for.
type entryFile struct {
	Repo         string `json:"repo"`
	Workspace    string `json:"workspace"`
	Name         string `json:"name"`
	SourceBranch string `json:"source_branch"`
	SourceDir    string `json:"source_dir"`
	TargetDir    string `json:"target_dir"`
	// RunID is the identity of the run that owns this entry. Empty only for an
	// entry written before the field existed, which the drain reports and
	// replays under a freshly minted id.
	RunID string `json:"run_id,omitempty"`
	// StatusWatermarkMs is the HIGHEST updated_at_ms this entry's run has
	// published. It is the one piece of a run's status a bounce must remember:
	// the resumed publisher seeds its clock from max(now, watermark+1), so a
	// backwards wall-clock step across the bounce cannot make a resumed status
	// sort BELOW the ones the dead process already put on the wire.
	//
	// Zero for a run that has published nothing yet, and for an entry written
	// before the field existed. Both mean the same thing to a resume — there is
	// no floor to respect — and both are served correctly by now().
	StatusWatermarkMs int64 `json:"status_watermark_ms,omitempty"`
	// PendingTerminal is the run's TERMINAL word, present ONLY on an entry whose
	// terminal status could not be published. It is a pointer with omitempty
	// deliberately: an ordinary entry's durable form is byte-for-byte what it
	// always was, so this field is additive to the format rather than a new
	// version of it.
	//
	// An entry carrying it is NOT a merge to replay. The merge already reached
	// its outcome — the commits are on the target, or the run already failed —
	// and re-running it would land a second time or fail a second time. What the
	// next boot owes the user is the WORD, which is why the word is what is
	// written down.
	PendingTerminal *entryTerminal `json:"pending_terminal,omitempty"`
}

// entryTerminal is the on-disk form of a TerminalStatus, mirrored field by
// field for the same reason the request is: a stated durable contract that a
// later field rename cannot silently break.
type entryTerminal struct {
	Outcome          string `json:"outcome"`
	Cause            string `json:"cause"`
	AfterActionError string `json:"after_action_error,omitempty"`
}

// terminal decodes the recorded terminal word, reporting absence for an
// ordinary entry.
func (f entryFile) terminal() (TerminalStatus, bool) {
	if f.PendingTerminal == nil {
		return TerminalStatus{}, false
	}
	return TerminalStatus{
		Outcome:          Outcome(f.PendingTerminal.Outcome),
		Cause:            f.PendingTerminal.Cause,
		AfterActionError: f.PendingTerminal.AfterActionError,
	}, true
}

func (f entryFile) request() Request {
	return Request{
		Workspace:         f.Workspace,
		Name:              f.Name,
		SourceBranch:      f.SourceBranch,
		SourceDir:         f.SourceDir,
		TargetDir:         f.TargetDir,
		RunID:             f.RunID,
		StatusWatermarkMs: f.StatusWatermarkMs,
	}
}

// newEntryFile is the durable form of a request that has NOT reached a terminal
// outcome, which is every request at publish time. PendingTerminal is therefore
// left nil here and set only by MarkTerminal.
func newEntryFile(repo string, req Request) entryFile {
	return entryFile{
		Repo:              repo,
		Workspace:         req.Workspace,
		Name:              req.Name,
		SourceBranch:      req.SourceBranch,
		SourceDir:         req.SourceDir,
		TargetDir:         req.TargetDir,
		RunID:             req.runIdentity(),
		StatusWatermarkMs: req.StatusWatermarkMs,
	}
}

// NewFileQueue validates its dependencies and returns the queue rooted at dir.
// Both are required: a queue with no directory has no durable record, and a
// queue with no logger cannot explain a stalled drain from the shared log.
func NewFileQueue(dir string, logf dlog.Logf) (*FileQueue, error) {
	if dir == "" {
		return nil, fmt.Errorf("merge: FileQueue needs a directory")
	}
	if logf == nil {
		return nil, fmt.Errorf("merge: FileQueue needs a Logf")
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("merge: create queue dir %s: %w", dir, err)
	}
	return &FileQueue{dir: dir, logf: logf, repos: map[string]*repoQueue{}}, nil
}

// Publish implements merge.Queue: it writes req's durable record and returns
// the position it landed at.
//
// The record is written BEFORE the in-memory append, so a returned position
// always names an entry that survives a bounce. A write failure leaves the
// queue exactly as it was.
func (q *FileQueue) Publish(_ context.Context, repo string, req Request) (Position, error) {
	if repo == "" {
		q.logf("merge: queue publish REFUSED empty repo key {ws=%s}", req.Workspace)
		return Position{}, fmt.Errorf("merge: queue Publish needs a repo key")
	}
	if err := req.validate(); err != nil {
		q.logf("merge: queue publish REFUSED invalid request {repo=%s ws=%s}: %v", repo, req.Workspace, err)
		return Position{}, err
	}

	q.mu.Lock()
	defer q.mu.Unlock()
	if err := q.hydrateLocked(repo); err != nil {
		return Position{}, err
	}
	id, err := newEntryID()
	if err != nil {
		q.logf("merge: queue publish id FAILED {repo=%s ws=%s}: %v", repo, req.Workspace, err)
		return Position{}, err
	}
	if err := q.writeEntry(repo, id, req); err != nil {
		q.logf("merge: queue publish write FAILED {repo=%s ws=%s id=%s}: %v", repo, req.Workspace, id, err)
		return Position{}, err
	}
	rq := q.repos[repo]
	rq.entries = append(rq.entries, &queueEntry{
		id:          id,
		req:         req,
		done:        make(chan struct{}),
		watermarkMs: req.StatusWatermarkMs,
	})
	pos := Position{Index: len(rq.entries), Depth: len(rq.entries), Repo: repo}
	// Non-blocking: the serve loop rechecks the slice after every wake, so a
	// notification that finds the buffer full is redundant rather than lost.
	select {
	case rq.wake <- struct{}{}:
	default:
	}
	q.logf("merge: queue publish {repo=%s ws=%s name=%s id=%s run=%s index=%d depth=%d}",
		repo, req.Workspace, req.Name, id, req.runIdentity(), pos.Index, pos.Depth)
	return pos, nil
}

// Subscribe implements merge.Queue: it returns repo's request stream and the
// cancel that tears the stream down.
//
// A second live Subscribe for one repo would put two drainers on one target
// worktree, which is the exact race single ownership exists to prevent. It is a
// programming error, so it panics rather than returning a second stream that
// works most of the time.
func (q *FileQueue) Subscribe(repo string) (<-chan Request, func()) {
	if repo == "" {
		panic("merge: FileQueue.Subscribe needs a repo key")
	}
	q.mu.Lock()
	if err := q.hydrateLocked(repo); err != nil {
		q.mu.Unlock()
		panic(fmt.Sprintf("merge: FileQueue.Subscribe hydrate %s: %v", repo, err))
	}
	rq := q.repos[repo]
	if rq.subscribed {
		q.mu.Unlock()
		panic(fmt.Sprintf("merge: FileQueue.Subscribe called twice for repo %s — a repository has exactly one merge.Coordinator drain", repo))
	}
	rq.subscribed = true
	q.mu.Unlock()

	ch := make(chan Request)
	cancel := make(chan struct{})
	var once sync.Once
	go q.serve(repo, rq, ch, cancel)
	return ch, func() {
		once.Do(func() {
			close(cancel)
			q.mu.Lock()
			rq.subscribed = false
			q.mu.Unlock()
			q.logf("merge: queue unsubscribed {repo=%s}", repo)
		})
	}
}

// serve is the per-repository delivery loop. It delivers the head entry, waits
// for that entry's Complete, and only then looks at the next one.
func (q *FileQueue) serve(repo string, rq *repoQueue, ch chan Request, cancel chan struct{}) {
	defer close(ch)
	for {
		head := q.head(repo)
		if head == nil {
			select {
			case <-rq.wake:
				continue
			case <-cancel:
				return
			}
		}
		select {
		case ch <- head.req:
		case <-cancel:
			return
		}
		select {
		case <-head.done:
		case <-cancel:
			return
		}
	}
}

// head returns repo's oldest outstanding entry, or nil when the queue is empty.
func (q *FileQueue) head(repo string) *queueEntry {
	q.mu.Lock()
	defer q.mu.Unlock()
	rq := q.repos[repo]
	if rq == nil || len(rq.entries) == 0 {
		return nil
	}
	return rq.entries[0]
}

// Complete implements DurableQueue: it drops repo's head entry after its merge
// reached a terminal outcome.
//
// The durable file is removed BEFORE the entry leaves memory, so a remove
// failure stalls the queue loudly instead of advancing past a record that would
// replay the same merge at the next boot.
func (q *FileQueue) Complete(repo string, req Request) error {
	q.mu.Lock()
	defer q.mu.Unlock()
	rq := q.repos[repo]
	if rq == nil || len(rq.entries) == 0 {
		q.logf("merge: queue complete on EMPTY queue {repo=%s ws=%s}", repo, req.Workspace)
		return fmt.Errorf("merge: queue Complete for %q: repo %s has no outstanding entry", req.Workspace, repo)
	}
	head := rq.entries[0]
	if head.req != req {
		q.logf("merge: queue complete MISMATCH {repo=%s head_ws=%s got_ws=%s}", repo, head.req.Workspace, req.Workspace)
		return fmt.Errorf("merge: queue Complete for %q: repo %s head is %q", req.Workspace, repo, head.req.Workspace)
	}
	path := q.entryPath(repo, head.id)
	if err := os.Remove(path); err != nil {
		q.logf("merge: queue complete remove FAILED {repo=%s ws=%s path=%s}: %v", repo, req.Workspace, path, err)
		return fmt.Errorf("merge: remove queue entry %s: %w", path, err)
	}
	rq.entries = rq.entries[1:]
	close(head.done)
	q.logf("merge: queue complete {repo=%s ws=%s id=%s remaining=%d}", repo, req.Workspace, head.id, len(rq.entries))
	return nil
}

// RecordStatusWatermark implements DurableQueue: it raises runID's entry's
// status_watermark_ms to updatedAtMs and rewrites the durable record.
//
// THE WATERMARK ONLY EVER RISES. A lower value is refused rather than written,
// because the whole point of the field is to be a floor no resume can publish
// beneath; accepting a lower one would hand the next boot a floor below statuses
// already on the wire. merge.RunStatus is the sole writer and its own clock is
// strictly increasing, so a lower value reaching here is a violated invariant,
// not a race to absorb.
//
// A RETIRED ENTRY IS A NO-OP, NOT A FAILURE. The watermark exists to seed a
// resume, and a resume is built from a durable entry; once Complete has dropped
// the record there is nothing left to resume and therefore nothing to protect.
// The terminal `merged` status the post-merge hook republishes after the entry
// is retired lands in exactly this case, and refusing it would deny a merge that
// demonstrably happened over a file that is deliberately gone. It is logged so
// the case is visible rather than silent.
func (q *FileQueue) RecordStatusWatermark(repo, runID string, updatedAtMs int64) error {
	if repo == "" {
		return fmt.Errorf("merge: queue RecordStatusWatermark needs a repo key")
	}
	if runID == "" {
		return fmt.Errorf("merge: queue RecordStatusWatermark for repo %s needs a run id", repo)
	}
	q.mu.Lock()
	defer q.mu.Unlock()
	rq := q.repos[repo]
	if rq == nil {
		q.logf("merge: queue watermark for a repo with no entries {repo=%s run=%s updated_at_ms=%d} — the run is retired, so there is nothing left to resume",
			repo, runID, updatedAtMs)
		return nil
	}
	for _, e := range rq.entries {
		if e.req.runIdentity() != runID {
			continue
		}
		if updatedAtMs <= e.watermarkMs {
			q.logf("merge: queue watermark REFUSING a regression {repo=%s ws=%s run=%s have=%d got=%d}",
				repo, e.req.Workspace, runID, e.watermarkMs, updatedAtMs)
			return fmt.Errorf("merge: queue watermark for run %s on repo %s is %d, refusing %d", runID, repo, e.watermarkMs, updatedAtMs)
		}
		f := newEntryFile(repo, e.req)
		f.StatusWatermarkMs = updatedAtMs
		if err := q.writeEntryFile(repo, e.id, f); err != nil {
			q.logf("merge: queue watermark write FAILED {repo=%s ws=%s run=%s id=%s}: %v", repo, e.req.Workspace, runID, e.id, err)
			return fmt.Errorf("merge: record status watermark for run %s: %w", runID, err)
		}
		e.watermarkMs = updatedAtMs
		return nil
	}
	q.logf("merge: queue watermark for an entry that is gone {repo=%s run=%s updated_at_ms=%d} — the run is retired, so there is nothing left to resume",
		repo, runID, updatedAtMs)
	return nil
}

// MarkTerminal implements DurableQueue: it records term on repo's head entry and
// leaves the entry outstanding.
//
// THE ENTRY IS NOT ACKED. Complete is the ack, and only a terminal status that
// actually reached a frontend earns it. Marking is the opposite word: this run
// is over, its outcome stands, and the record is kept precisely because the
// outcome has not been SAID yet.
//
// It is IDEMPOTENT. A second mark of the same head overwrites the first record
// with the same discipline the first used, so a retry — or a caller that marks
// twice for one terminal — leaves exactly one entry carrying exactly one word.
func (q *FileQueue) MarkTerminal(repo string, req Request, term TerminalStatus) error {
	if err := term.validate(); err != nil {
		q.logf("merge: queue terminal mark REFUSED {repo=%s ws=%s outcome=%s}: %v", repo, req.Workspace, term.Outcome, err)
		return err
	}
	q.mu.Lock()
	defer q.mu.Unlock()
	head, err := q.headForLocked(repo, req, "terminal mark")
	if err != nil {
		return err
	}
	f := newEntryFile(repo, head.req)
	f.PendingTerminal = &entryTerminal{
		Outcome:          string(term.Outcome),
		Cause:            term.Cause,
		AfterActionError: term.AfterActionError,
	}
	if err := q.writeEntryFile(repo, head.id, f); err != nil {
		q.logf("merge: queue terminal mark write FAILED {repo=%s ws=%s id=%s outcome=%s}: %v", repo, req.Workspace, head.id, term.Outcome, err)
		return fmt.Errorf("merge: mark queue entry %s terminal: %w", q.entryPath(repo, head.id), err)
	}
	// Memory follows the disk, never leads it.
	marked := term
	head.terminal = &marked
	q.logf("merge: queue terminal MARKED {repo=%s ws=%s id=%s run=%s outcome=%s cause=%s after_action_error=%s} — the entry is KEPT so the next boot re-publishes this terminal status",
		repo, req.Workspace, head.id, head.req.runIdentity(), term.Outcome, term.Cause, term.AfterActionError)
	return nil
}

// PendingTerminal implements DurableQueue: it reports the terminal word recorded
// on repo's head entry, if any.
func (q *FileQueue) PendingTerminal(repo string, req Request) (TerminalStatus, bool, error) {
	q.mu.Lock()
	defer q.mu.Unlock()
	head, err := q.headForLocked(repo, req, "terminal lookup")
	if err != nil {
		return TerminalStatus{}, false, err
	}
	if head.terminal == nil {
		return TerminalStatus{}, false, nil
	}
	return *head.terminal, true, nil
}

// EvictWaiting implements DurableQueue: it drops repo's non-head entries for
// workspace and returns the requests it dropped.
//
// THE DURABLE FILE GOES FIRST, ENTRY BY ENTRY, exactly as Complete does it: an
// entry that left memory but not disk would be replayed by the next boot as a
// merge the user had already taken off the queue. So a remove failure keeps its
// entry in the slice, is loud-logged, and is returned — that merge is still
// queued and still runs when its turn comes, which is the honest outcome of an
// eviction that did not happen.
//
// THE HEAD IS RETAINED AND SAID SO. It is not a failure and not a silent skip:
// the head is the merge in flight, and a user who interrupted a workspace whose
// merge is already cherry-picking needs the log to show that its entry stayed.
func (q *FileQueue) EvictWaiting(repo, workspace string) ([]Request, error) {
	if repo == "" {
		return nil, fmt.Errorf("merge: queue EvictWaiting needs a repo key")
	}
	if workspace == "" {
		return nil, fmt.Errorf("merge: queue EvictWaiting on repo %s needs a workspace", repo)
	}
	q.mu.Lock()
	defer q.mu.Unlock()
	if err := q.hydrateLocked(repo); err != nil {
		return nil, err
	}
	rq := q.repos[repo]
	var (
		evicted []Request
		kept    []*queueEntry
		failed  error
	)
	for i, e := range rq.entries {
		switch {
		case e.req.Workspace != workspace:
			kept = append(kept, e)
		case i == 0:
			q.logf("merge: queue evict RETAINING THE HEAD {repo=%s ws=%s id=%s run=%s} — the head is the merge in flight, and its drain owns the entry until the merge reaches a terminal outcome",
				repo, workspace, e.id, e.req.runIdentity())
			kept = append(kept, e)
		default:
			path := q.entryPath(repo, e.id)
			if err := os.Remove(path); err != nil {
				q.logf("merge: queue evict remove FAILED {repo=%s ws=%s id=%s path=%s}: %v — the entry is KEPT, so the merge still runs when its turn comes rather than leaving the queue in memory only",
					repo, workspace, e.id, path, err)
				failed = errors.Join(failed, fmt.Errorf("merge: remove queue entry %s: %w", path, err))
				kept = append(kept, e)
				continue
			}
			evicted = append(evicted, e.req)
			q.logf("merge: queue EVICTED {repo=%s ws=%s id=%s run=%s index=%d}", repo, workspace, e.id, e.req.runIdentity(), i+1)
		}
	}
	rq.entries = kept
	return evicted, failed
}

// headForLocked resolves repo's head entry and asserts it IS req, which is the
// same single-ownership invariant Complete asserts and for the same reason: one
// owner drains a repo one entry at a time, so a head that is not the entry the
// caller was handed is a violated invariant rather than an entry to guess at.
func (q *FileQueue) headForLocked(repo string, req Request, op string) (*queueEntry, error) {
	rq := q.repos[repo]
	if rq == nil || len(rq.entries) == 0 {
		q.logf("merge: queue %s on EMPTY queue {repo=%s ws=%s}", op, repo, req.Workspace)
		return nil, fmt.Errorf("merge: queue %s for %q: repo %s has no outstanding entry", op, req.Workspace, repo)
	}
	head := rq.entries[0]
	if head.req != req {
		q.logf("merge: queue %s MISMATCH {repo=%s head_ws=%s got_ws=%s}", op, repo, head.req.Workspace, req.Workspace)
		return nil, fmt.Errorf("merge: queue %s for %q: repo %s head is %q", op, req.Workspace, repo, head.req.Workspace)
	}
	return head, nil
}

// Snapshot implements merge.Queue: every repository's outstanding entries, in
// delivery order. It hydrates every repository directory on disk first, which
// is what makes it usable as merge.Coordinator.Drain's boot-time reconstruction.
func (q *FileQueue) Snapshot() map[string][]Request {
	q.mu.Lock()
	defer q.mu.Unlock()
	if err := q.hydrateAllLocked(); err != nil {
		// Snapshot has no error channel in the frozen contract. A hydration
		// failure means the durable record is unreadable, which would silently
		// present a queue as empty and lose every pending merge — the loudest
		// available response is the correct one.
		panic(fmt.Sprintf("merge: FileQueue.Snapshot hydrate: %v", err))
	}
	out := map[string][]Request{}
	for repo, rq := range q.repos {
		if len(rq.entries) == 0 {
			continue
		}
		reqs := make([]Request, len(rq.entries))
		for i, e := range rq.entries {
			reqs[i] = e.req
		}
		out[repo] = reqs
	}
	return out
}

// hydrateLocked loads repo's durable entries the first time the repo is
// touched. Once loaded, memory is the authority for that repo.
func (q *FileQueue) hydrateLocked(repo string) error {
	if _, ok := q.repos[repo]; ok {
		return nil
	}
	entries, err := q.readRepoDir(q.repoDir(repo))
	if err != nil {
		return err
	}
	q.repos[repo] = &repoQueue{entries: entries, wake: make(chan struct{}, 1)}
	if len(entries) > 0 {
		q.logf("merge: queue hydrated {repo=%s depth=%d}", repo, len(entries))
	}
	return nil
}

// hydrateAllLocked loads every repository directory under the queue root that
// has not been hydrated yet. The repo key is read from the entry files rather
// than from the directory name, which is a hash of the key (an absolute path
// makes a poor directory component).
func (q *FileQueue) hydrateAllLocked() error {
	dirents, err := os.ReadDir(q.dir)
	if err != nil {
		return fmt.Errorf("merge: read queue dir %s: %w", q.dir, err)
	}
	for _, de := range dirents {
		if !de.IsDir() {
			continue
		}
		path := filepath.Join(q.dir, de.Name())
		entries, err := q.readRepoDir(path)
		if err != nil {
			return err
		}
		if len(entries) == 0 {
			continue
		}
		repo, err := q.repoOf(path)
		if err != nil {
			return err
		}
		if _, ok := q.repos[repo]; ok {
			continue
		}
		q.repos[repo] = &repoQueue{entries: entries, wake: make(chan struct{}, 1)}
		q.logf("merge: queue hydrated {repo=%s depth=%d}", repo, len(entries))
	}
	return nil
}

// repoOf reads the repo key recorded inside a repository directory's oldest
// entry. Every entry in one directory carries the same key by construction.
func (q *FileQueue) repoOf(dir string) (string, error) {
	names, err := q.entryNames(dir)
	if err != nil {
		return "", err
	}
	if len(names) == 0 {
		return "", fmt.Errorf("merge: queue dir %s has no entries to key from", dir)
	}
	f, err := readEntryFile(filepath.Join(dir, names[0]))
	if err != nil {
		return "", err
	}
	if f.Repo == "" {
		return "", fmt.Errorf("merge: queue entry %s carries no repo key", filepath.Join(dir, names[0]))
	}
	return f.Repo, nil
}

// entryNames lists a repository directory's entry files in delivery order. The
// id's leading nanosecond timestamp makes the lexical order chronological, and
// its random suffix makes the order total even for two publishes in one
// nanosecond.
func (q *FileQueue) entryNames(dir string) ([]string, error) {
	dirents, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("merge: read queue dir %s: %w", dir, err)
	}
	var names []string
	for _, de := range dirents {
		name := de.Name()
		if de.IsDir() || !strings.HasPrefix(name, queueFilePrefix) || !strings.HasSuffix(name, ".json") {
			continue
		}
		names = append(names, name)
	}
	sort.Strings(names)
	return names, nil
}

// readRepoDir decodes a repository directory's entries in delivery order.
func (q *FileQueue) readRepoDir(dir string) ([]*queueEntry, error) {
	names, err := q.entryNames(dir)
	if err != nil {
		return nil, err
	}
	var entries []*queueEntry
	for _, name := range names {
		f, err := readEntryFile(filepath.Join(dir, name))
		if err != nil {
			return nil, err
		}
		req := f.request()
		if err := req.validate(); err != nil {
			return nil, fmt.Errorf("merge: queue entry %s: %w", filepath.Join(dir, name), err)
		}
		id := strings.TrimSuffix(strings.TrimPrefix(name, queueFilePrefix), ".json")
		entry := &queueEntry{
			id:          id,
			req:         req,
			done:        make(chan struct{}),
			watermarkMs: f.StatusWatermarkMs,
		}
		if term, ok := f.terminal(); ok {
			if err := term.validate(); err != nil {
				return nil, fmt.Errorf("merge: queue entry %s terminal record: %w", filepath.Join(dir, name), err)
			}
			entry.terminal = &term
		}
		entries = append(entries, entry)
	}
	return entries, nil
}

func readEntryFile(path string) (entryFile, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return entryFile{}, fmt.Errorf("merge: read queue entry %s: %w", path, err)
	}
	var f entryFile
	if err := json.Unmarshal(raw, &f); err != nil {
		return entryFile{}, fmt.Errorf("merge: decode queue entry %s: %w", path, err)
	}
	return f, nil
}

// writeEntry lands one durable record atomically: the payload goes to a
// dot-prefixed temp name the entry glob cannot match, then renames into place.
// A reader therefore never sees a partial entry.
func (q *FileQueue) writeEntry(repo, id string, req Request) error {
	return q.writeEntryFile(repo, id, newEntryFile(repo, req))
}

// writeEntryFile lands one durable record atomically, whatever its contents.
// MarkTerminal and RecordStatusWatermark rewrite an EXISTING entry through it,
// and the dot-temp rename is what makes those rewrites atomic too: a reader
// either sees the entry as it was or sees it carrying its new word, never a
// half-written mixture — a truncated watermark would read as zero, no floor at
// all.
func (q *FileQueue) writeEntryFile(repo, id string, f entryFile) error {
	dir := q.repoDir(repo)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("merge: create queue dir %s: %w", dir, err)
	}
	payload, err := json.Marshal(f)
	if err != nil {
		return fmt.Errorf("merge: marshal queue entry for %q: %w", f.Workspace, err)
	}
	tmp, err := os.CreateTemp(dir, "."+queueFilePrefix+"*.json")
	if err != nil {
		return fmt.Errorf("merge: create temp queue entry in %s: %w", dir, err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName) // No-op once the rename below succeeds.
	if _, err := tmp.Write(payload); err != nil {
		tmp.Close()
		return fmt.Errorf("merge: write %s: %w", tmpName, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("merge: close %s: %w", tmpName, err)
	}
	path := q.entryPath(repo, id)
	if err := os.Rename(tmpName, path); err != nil {
		return fmt.Errorf("merge: rename queue entry to %s: %w", path, err)
	}
	return nil
}

// repoDir is the repository's queue directory: a hash of the key, because the
// key is an absolute path and cannot be a path component itself.
func (q *FileQueue) repoDir(repo string) string {
	sum := sha256.Sum256([]byte(repo))
	return filepath.Join(q.dir, hex.EncodeToString(sum[:]))
}

func (q *FileQueue) entryPath(repo, id string) string {
	return filepath.Join(q.repoDir(repo), queueFilePrefix+id+".json")
}

// newEntryID mints a lexically-chronological entry id: zero-padded nanoseconds
// for the order, random bytes so two publishes in one nanosecond still get a
// stable total order and distinct file names.
func newEntryID() (string, error) {
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("merge: mint queue entry id: %w", err)
	}
	return fmt.Sprintf("%020d-%s", time.Now().UnixNano(), hex.EncodeToString(b[:])), nil
}
