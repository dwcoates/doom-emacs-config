package handler

// testsupport_test.go — the suite's shared arrangement.
//
// NOTHING HERE TOUCHES A VENDOR. Every test drives decoded records through the
// pure conversion layer; AGENT_REPL_FORBID_VENDOR_CALLS is exported so a
// regression that reached for one would fail loudly rather than run.

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	// The verbose helper is exercised too: a verbose-only log statement that
	// panics would otherwise stay invisible until production turned it on.
	os.Exit(m.Run())
}

// testLogger builds a logger writing nowhere. The sidecar's logger REQUIRES both
// sinks, so they are supplied rather than nil-ed: a test that silently disabled
// half the logging contract would not be testing the production path.
func testLogger(t *testing.T) *logging.Bound {
	t.Helper()
	return logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
}

// framesFrom decodes JSONL text into the frames a tailer would deliver, with the
// real byte offsets, so write ids computed in a test are the ids production
// mints for the same file.
func framesFrom(t *testing.T, text string) []tail.Frame {
	t.Helper()
	var frames []tail.Frame
	offset := int64(0)
	for _, line := range strings.Split(text, "\n") {
		raw := line
		line = strings.TrimSpace(line)
		if line == "" {
			offset += int64(len(raw)) + 1
			continue
		}
		frame := tail.Frame{Raw: []byte(raw), Offset: offset}
		var record map[string]any
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			frame.ParseErr = err
		} else {
			frame.Obj = record
		}
		frames = append(frames, frame)
		offset += int64(len(raw)) + 1
	}
	return frames
}

// sessionContext is the attribution a session transcript is read under.
func sessionContext(path, session string) *Context {
	return &Context{
		Path: path, SessionID: session, MainAgentID: session,
		AgentID: session, FileID: testFileID(path), Kind: tail.KindSessionTranscript,
	}
}

// corpusRoot locates testdata/corpus from this package.
func corpusRoot(t *testing.T) string {
	t.Helper()
	root := repoModuleRoot(t)
	return filepath.Join(root, "testdata", "corpus")
}

// projectsRoot locates the real captured transcript tree.
func projectsRoot(t *testing.T) string {
	t.Helper()
	return filepath.Join(repoModuleRoot(t), "projects")
}

// repoModuleRoot walks up to modules/app/agent-repl, which holds both testdata
// and projects.
func repoModuleRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for i := 0; i < 12; i++ {
		if _, err := os.Stat(filepath.Join(dir, "testdata", "corpus", "MANIFEST.md")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatalf("could not locate modules/app/agent-repl from %s", dir)
	return ""
}

// ---- readers over produced entries, so assertions read as claims ----

func pageLine(e *storev1.StoreEntry) *storev1.StorePageLine {
	return e.GetAgentUpdate().GetServeableFrame()
}

func frameOf(e *storev1.StoreEntry) *conversationv1.AgentFrame {
	return pageLine(e).GetAgentItem().GetAgentFrame()
}

func activityOf(e *storev1.StoreEntry) *conversationv1.AgentActivity {
	return frameOf(e).GetUpdate().GetActivity()
}

// entryByKey finds the one entry carrying an upsert key, failing when the count
// is not exactly one: a duplicated key is the bug most of these tests exist to
// catch, so "the first match" would hide it.
func entryByKey(t *testing.T, entries []*storev1.StoreEntry, key string) *storev1.StoreEntry {
	t.Helper()
	var found *storev1.StoreEntry
	count := 0
	for _, e := range entries {
		if e.GetUpsertKey() == key {
			found = e
			count++
		}
	}
	if count != 1 {
		t.Fatalf("upsert_key %q: got %d entries, want exactly 1 (keys: %v)", key, count, allKeys(entries))
	}
	return found
}

func allKeys(entries []*storev1.StoreEntry) []string {
	keys := make([]string, 0, len(entries))
	for _, e := range entries {
		keys = append(keys, e.GetUpsertKey())
	}
	return keys
}

// vendorKinds collects the vendor_specific kinds a run produced.
func vendorKinds(entries []*storev1.StoreEntry) []string {
	var kinds []string
	for _, e := range entries {
		if v := e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific(); v != nil {
			kinds = append(kinds, v.GetKind())
		}
	}
	return kinds
}

// testFileID spells a DISTINCT "dev:inode" per fixture path, the way the kernel
// hands the reader one. The write identity is digested from the file id rather
// than the path (R-S1), so a harness that gave every fixture the same id would
// make two files' first records collide — which is a defect in the harness, not
// in the rule.
func testFileID(path string) string {
	sum := sha256.Sum256([]byte(path))
	return "16777232:" + hex.EncodeToString(sum[:4])
}
