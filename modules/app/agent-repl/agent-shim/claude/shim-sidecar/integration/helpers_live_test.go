package integration

// helpers_live_test.go — LIVE agent-repl SESSIONS, the way a real shim makes one.
//
// The sidecar watches only the files of ACTIVE workspaces (active.go): a
// workspace is active while a live shim holds `<lock dir>/workspace-<key>.lock`,
// and its conversation is the one `<state>/shim/<key>/agent-id.json` names. So a
// subject that wants a transcript read makes its session live exactly as a shim
// does — it writes the identity record and holds a REAL kernel flock on the
// workspace lock — and a subject about an inactive or external session simply
// does not. Nothing here tells the sidecar anything it would not read off a real
// machine.

import (
	"crypto/md5"
	"encoding/hex"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"testing"
)

// liveRoot is one state root and the sessions a subject made live in it.
type liveRoot struct {
	state string
	mu    sync.Mutex
	// recorded maps each vendor session id an identity record names (an
	// original, or a linked rotation) to the record's workspace key.
	recorded map[string]string
	// held is every workspace lock this subject holds, by key.
	held map[string]*os.File
}

var (
	liveRootsMu sync.Mutex
	liveRoots   = map[*testing.T]*liveRoot{}
)

// liveRootFor answers the subject's own state root, creating it once per test.
// Every tree the subject builds shares it, because one sidecar reads one state
// root.
func liveRootFor(t *testing.T) *liveRoot {
	t.Helper()
	liveRootsMu.Lock()
	defer liveRootsMu.Unlock()
	if root, ok := liveRoots[t]; ok {
		return root
	}
	root := newLiveRoot(t, filepath.Join(t.TempDir(), "state"))
	liveRoots[t] = root
	t.Cleanup(func() {
		liveRootsMu.Lock()
		delete(liveRoots, t)
		liveRootsMu.Unlock()
	})
	return root
}

// newLiveRoot builds a live root over an existing or new state directory. Its
// locks are released when the subject ends.
func newLiveRoot(t *testing.T, state string) *liveRoot {
	t.Helper()
	mustMkdirAll(t, filepath.Join(state, "shim"))
	mustMkdirAll(t, liveLockDir(state))
	root := &liveRoot{state: state, recorded: map[string]string{}, held: map[string]*os.File{}}
	t.Cleanup(func() {
		root.mu.Lock()
		defer root.mu.Unlock()
		for key, file := range root.held {
			if err := file.Close(); err != nil {
				t.Errorf("releasing workspace %s's lock: %v", key, err)
			}
		}
		root.held = map[string]*os.File{}
	})
	return root
}

// liveLockDir is where a sidecar started over state looks for workspace locks:
// startSidecar points AGENT_REPL_LOCK_DIR there.
func liveLockDir(state string) string { return filepath.Join(state, "lock") }

// liveKeyOf is a session's fixture workspace key: eight hex digits, the shape
// of the shim's md5(cwd)[:8]. It only has to join a record to its lock by name.
func liveKeyOf(session string) string {
	sum := md5.Sum([]byte(session))
	return hex.EncodeToString(sum[:])[:8]
}

// activate makes session live: a session no record names yet is minted as its
// own workspace's conversation, and the workspace's lock is held.
func (r *liveRoot) activate(t *testing.T, session string) {
	t.Helper()
	r.mu.Lock()
	key, recorded := r.recorded[session]
	r.mu.Unlock()
	if !recorded {
		key = liveKeyOf(session)
		r.mint(t, key, session)
		return
	}
	r.hold(t, key)
}

// mint writes a workspace's agent-id.json by tmp-and-rename, as
// engine/identity.ts does, and holds the workspace's lock: a shim that minted
// an identity is a live one.
func (r *liveRoot) mint(t *testing.T, key, original string) {
	t.Helper()
	dir := filepath.Join(r.state, "shim", key)
	mustMkdirAll(t, dir)
	temporary := filepath.Join(dir, "agent-id.json.tmp")
	mustWriteFile(t, temporary, `{
  "original_vendor_session_id": "`+original+`",
  "workspace_key": "`+key+`",
  "minted_at_ms": 1735689600000
}
`)
	if err := os.Rename(temporary, filepath.Join(dir, "agent-id.json")); err != nil {
		t.Fatalf("renaming %s into place: %v", temporary, err)
	}
	r.mu.Lock()
	r.recorded[original] = key
	r.mu.Unlock()
	r.hold(t, key)
}

// link writes the pointer file a rotation leaves behind, by tmp-and-rename.
func (r *liveRoot) link(t *testing.T, key, vendorID, original string) {
	t.Helper()
	dir := filepath.Join(r.state, "shim", key, "vendor-id")
	mustMkdirAll(t, dir)
	temporary := filepath.Join(dir, vendorID+".json.tmp")
	mustWriteFile(t, temporary, `{
  "vendor_session_id": "`+vendorID+`",
  "original_vendor_session_id": "`+original+`",
  "linked_at_ms": 1735689700000
}
`)
	if err := os.Rename(temporary, filepath.Join(dir, vendorID+".json")); err != nil {
		t.Fatalf("renaming %s into place: %v", temporary, err)
	}
	r.mu.Lock()
	r.recorded[vendorID] = key
	r.mu.Unlock()
}

// hold takes a workspace's lock, unless this subject already holds it.
func (r *liveRoot) hold(t *testing.T, key string) {
	t.Helper()
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, ok := r.held[key]; ok {
		return
	}
	path := filepath.Join(liveLockDir(r.state), "workspace-"+key+".lock")
	file, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		t.Fatalf("creating the workspace lock %s: %v", path, err)
	}
	if err := syscall.Flock(int(file.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("taking the workspace lock %s: %v", path, err)
	}
	r.held[key] = file
}

// release drops a workspace's lock, which is what a shim's death looks like.
func (r *liveRoot) release(t *testing.T, key string) {
	t.Helper()
	r.mu.Lock()
	defer r.mu.Unlock()
	file, ok := r.held[key]
	if !ok {
		t.Fatalf("workspace %s's lock is not held", key)
	}
	delete(r.held, key)
	if err := file.Close(); err != nil {
		t.Fatalf("releasing workspace %s's lock: %v", key, err)
	}
}

// holdEveryRecordedWorkspace holds the lock of every workspace a shim left an
// agent-id.json for under this root — what a tree the REAL mocked vendor wrote
// needs, since that shim is gone by the time the sidecar reads it.
func (r *liveRoot) holdEveryRecordedWorkspace(t *testing.T) {
	t.Helper()
	records, err := filepath.Glob(filepath.Join(r.state, "shim", "*", "agent-id.json"))
	if err != nil {
		t.Fatalf("listing the identity records under %s: %v", r.state, err)
	}
	if len(records) == 0 {
		t.Fatalf("the mocked vendor left no identity record under %s, so no workspace of it can be live", r.state)
	}
	for _, record := range records {
		r.hold(t, filepath.Base(filepath.Dir(record)))
	}
}
