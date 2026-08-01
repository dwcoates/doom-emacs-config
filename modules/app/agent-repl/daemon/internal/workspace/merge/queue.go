package merge

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
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
}

// entryFile is the on-disk form of a queue entry. The request's fields are
// mirrored explicitly (rather than embedding Request) so the durable format is
// a stated contract that a later field rename cannot silently break.
type entryFile struct {
	Repo         string `json:"repo"`
	Workspace    string `json:"workspace"`
	Name         string `json:"name"`
	SourceBranch string `json:"source_branch"`
	SourceDir    string `json:"source_dir"`
	TargetDir    string `json:"target_dir"`
}

func (f entryFile) request() Request {
	return Request{
		Workspace:    f.Workspace,
		Name:         f.Name,
		SourceBranch: f.SourceBranch,
		SourceDir:    f.SourceDir,
		TargetDir:    f.TargetDir,
	}
}

func newEntryFile(repo string, req Request) entryFile {
	return entryFile{
		Repo:         repo,
		Workspace:    req.Workspace,
		Name:         req.Name,
		SourceBranch: req.SourceBranch,
		SourceDir:    req.SourceDir,
		TargetDir:    req.TargetDir,
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
	rq.entries = append(rq.entries, &queueEntry{id: id, req: req, done: make(chan struct{})})
	pos := Position{Index: len(rq.entries), Depth: len(rq.entries), Repo: repo}
	// Non-blocking: the serve loop rechecks the slice after every wake, so a
	// notification that finds the buffer full is redundant rather than lost.
	select {
	case rq.wake <- struct{}{}:
	default:
	}
	q.logf("merge: queue publish {repo=%s ws=%s name=%s id=%s index=%d depth=%d}",
		repo, req.Workspace, req.Name, id, pos.Index, pos.Depth)
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
		entries = append(entries, &queueEntry{id: id, req: req, done: make(chan struct{})})
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
	dir := q.repoDir(repo)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("merge: create queue dir %s: %w", dir, err)
	}
	payload, err := json.Marshal(newEntryFile(repo, req))
	if err != nil {
		return fmt.Errorf("merge: marshal queue entry for %q: %w", req.Workspace, err)
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
