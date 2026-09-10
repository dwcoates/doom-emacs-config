package account_test

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"claude-repld/internal/account"
)

func TestEncodeCWD(t *testing.T) {
	tests := []struct {
		name string
		cwd  string
		want string
	}{
		{name: "slashes become dashes", cwd: "/Users/me/proj", want: "-Users-me-proj"},
		{name: "a dot becomes a dash too", cwd: "/Users/me/.config/doom", want: "-Users-me--config-doom"},
		{name: "an underscore becomes a dash", cwd: "/private/var/folders/_m/x", want: "-private-var-folders--m-x"},
		{name: "existing dashes survive", cwd: "/tmp/a-b-c", want: "-tmp-a-b-c"},
		{name: "case is preserved", cwd: "/Users/Me/ProjA", want: "-Users-Me-ProjA"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := account.EncodeCWD(tc.cwd)

			// Assert.
			if got != tc.want {
				t.Fatalf("EncodeCWD(%q) = %q, want %q", tc.cwd, got, tc.want)
			}
		})
	}
}

func TestTranscriptPath(t *testing.T) {
	// Arrange, Act.
	got := account.TranscriptPath("/root", "/Users/me/proj", "uuid-1")

	// Assert.
	want := filepath.Join("/root", "projects", "-Users-me-proj", "uuid-1.jsonl")
	if got != want {
		t.Fatalf("TranscriptPath() = %q, want %q", got, want)
	}
}

// identitylessRecord is a well-formed vendor record with NO identity in it, so
// a fork's re-minting pass carries it through byte for byte and a test about
// WHERE a transcript lands can still assert on its exact bytes.
const identitylessRecord = `{"type":"summary"}`

// transcriptFixture is one test's pair of config roots plus a workspace dir.
type transcriptFixture struct {
	def   string
	multi string
	ws    string
	r     account.Resolver
}

// newTranscriptFixture builds two roots and a workspace routed to the DEFAULT
// account (the workspace dir is outside the multi-repo root).
func newTranscriptFixture(t *testing.T) transcriptFixture {
	t.Helper()
	base := t.TempDir()
	f := transcriptFixture{
		def:   filepath.Join(base, "default-root"),
		multi: filepath.Join(base, "multi-root"),
		ws:    filepath.Join(base, "workspaces", "ws1"),
	}
	if err := os.MkdirAll(f.ws, 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	f.r = newResolver(t, account.Roots{
		Default:       f.def,
		MultiRepo:     f.multi,
		MultiRepoRoot: filepath.Join(base, "multi-repos"),
	})
	return f
}

// plantTranscript writes a transcript under one root for one workspace.
func plantTranscript(t *testing.T, configDir, ws, sessionID, body string) string {
	t.Helper()
	path := account.TranscriptPath(configDir, ws, sessionID)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}
	return path
}

// plantSidecar writes one file inside the transcript's sidecar directory.
func plantSidecar(t *testing.T, transcriptPath, name, body string) string {
	t.Helper()
	dir := transcriptPath[:len(transcriptPath)-len(".jsonl")]
	path := filepath.Join(dir, name)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}
	return dir
}

func TestFindTranscriptInTheRoutedRoot(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	want := plantTranscript(t, f.def, f.ws, "uuid-1", "{}")

	// Act.
	got, err := f.r.FindTranscript(context.Background(), f.ws, "uuid-1")

	// Assert.
	if err != nil {
		t.Fatalf("FindTranscript() = %v, want nil", err)
	}
	if got.Path != want || got.ConfigDir != f.def {
		t.Fatalf("FindTranscript() = %+v, want %s under %s", got, want, f.def)
	}
}

func TestFindTranscriptFallsBackToTheOtherRoot(t *testing.T) {
	// Arrange: only the NON-routed root holds it — the account-switch signal.
	f := newTranscriptFixture(t)
	want := plantTranscript(t, f.multi, f.ws, "uuid-1", "{}")

	// Act.
	got, err := f.r.FindTranscript(context.Background(), f.ws, "uuid-1")

	// Assert.
	if err != nil {
		t.Fatalf("FindTranscript() = %v, want nil", err)
	}
	if got.Path != want || got.ConfigDir != f.multi {
		t.Fatalf("FindTranscript() = %+v, want %s under %s", got, want, f.multi)
	}
}

func TestFindTranscriptPrefersTheRoutedRootWhenBothHoldTheUUID(t *testing.T) {
	// Arrange: the same vendor uuid under both roots is disambiguated by
	// routing, never by picking one.
	f := newTranscriptFixture(t)
	routed := plantTranscript(t, f.def, f.ws, "uuid-1", "routed")
	plantTranscript(t, f.multi, f.ws, "uuid-1", "other")

	// Act.
	got, err := f.r.FindTranscript(context.Background(), f.ws, "uuid-1")

	// Assert.
	if err != nil {
		t.Fatalf("FindTranscript() = %v, want nil", err)
	}
	if got.Path != routed {
		t.Fatalf("FindTranscript() = %q, want the routed root's %q", got.Path, routed)
	}
}

func TestFindTranscriptReportsTheSidecarDirectory(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	path := plantTranscript(t, f.def, f.ws, "uuid-1", "{}")
	want := plantSidecar(t, path, "note.json", "{}")

	// Act.
	got, err := f.r.FindTranscript(context.Background(), f.ws, "uuid-1")

	// Assert.
	if err != nil {
		t.Fatalf("FindTranscript() = %v, want nil", err)
	}
	if got.SidecarDir != want {
		t.Fatalf("SidecarDir = %q, want %q", got.SidecarDir, want)
	}
}

func TestFindTranscriptMissIsTyped(t *testing.T) {
	// Arrange: the RESUME GUARD's input — a missing transcript must be a typed
	// refusal, not an untyped failure the caller has to string-match.
	f := newTranscriptFixture(t)

	// Act.
	_, err := f.r.FindTranscript(context.Background(), f.ws, "uuid-gone")

	// Assert.
	var miss *account.NotFoundError
	if !errors.As(err, &miss) {
		t.Fatalf("FindTranscript() = %v, want *account.NotFoundError", err)
	}
	if len(miss.Probed) != 2 {
		t.Fatalf("Probed = %v, want both roots probed", miss.Probed)
	}
}

func TestFindTranscriptRejectsEmptySessionID(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)

	// Act.
	_, err := f.r.FindTranscript(context.Background(), f.ws, "")

	// Assert.
	if err == nil {
		t.Fatal("FindTranscript() = nil error, want a refusal")
	}
}

func TestFindTranscriptRejectsEmptyWorkspaceDir(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)

	// Act.
	_, err := f.r.FindTranscript(context.Background(), "", "uuid-1")

	// Assert.
	if err == nil {
		t.Fatal("FindTranscript() = nil error, want a refusal")
	}
}

func TestPortTranscriptCopiesAndLeavesTheSourceInPlace(t *testing.T) {
	// Arrange: a fork — the parent keeps its own conversation.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", identitylessRecord)
	child := filepath.Join(t.TempDir(), "child-ws")

	// Act.
	err := f.r.PortTranscript(context.Background(), src, f.multi, child, "uuid-2")

	// Assert.
	if err != nil {
		t.Fatalf("PortTranscript() = %v, want nil", err)
	}
	assertFileBody(t, account.TranscriptPath(f.multi, child, "uuid-2"), identitylessRecord)
	assertFileBody(t, src, identitylessRecord)
}

// TestPortTranscriptFilesTheCopyUnderTheChildsOwnVendorSessionId covers the
// fork's whole reason for renaming: a vendor session id is single-occupancy
// under the shim's session lock, so the child's copy is filed under an id of
// its own and the parent's file is left exactly where it was.
func TestPortTranscriptFilesTheCopyUnderTheChildsOwnVendorSessionId(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "parent-uuid", identitylessRecord)
	child := filepath.Join(t.TempDir(), "child-ws")

	// Act.
	err := f.r.PortTranscript(context.Background(), src, f.multi, child, "child-uuid")

	// Assert.
	if err != nil {
		t.Fatalf("PortTranscript() = %v, want nil", err)
	}
	assertFileBody(t, account.TranscriptPath(f.multi, child, "child-uuid"), identitylessRecord)
	if _, err := os.Stat(account.TranscriptPath(f.multi, child, "parent-uuid")); err == nil {
		t.Fatal("the copy was also filed under the parent's id, want only the child's")
	}
	assertFileBody(t, src, identitylessRecord)
}

func TestPortTranscriptCarriesTheSidecarDirectory(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", identitylessRecord)
	plantSidecar(t, src, "note.json", `{"note":"sidecar"}`)
	child := filepath.Join(t.TempDir(), "child-ws")

	// Act.
	err := f.r.PortTranscript(context.Background(), src, f.multi, child, "uuid-2")

	// Assert.
	if err != nil {
		t.Fatalf("PortTranscript() = %v, want nil", err)
	}
	dest := account.TranscriptPath(f.multi, child, "uuid-2")
	assertFileBody(t, filepath.Join(dest[:len(dest)-len(".jsonl")], "note.json"), `{"note":"sidecar"}`)
}

// TestPortTranscriptRemintsTheSidecarsSubagentRecordsUnderTheSameMapping covers
// the half of a fork that lives beside the transcript: a subagent's AgentId is
// the tool_use_id of the call that spawned it, stated in the parent's transcript
// AND in the sidecar's `agent-<id>.meta.json`, with the `agent-<id>` file name
// and the sidechain records' `agentId` naming the same agent. One mapping has to
// move all of them, or the child's history points at agents it does not have.
func TestPortTranscriptRemintsTheSidecarsSubagentRecordsUnderTheSameMapping(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "parent-uuid",
		`{"type":"assistant","uuid":"u1","sessionId":"parent-uuid","message":{"id":"msg_1","content":[{"type":"tool_use","id":"toolu_1","name":"Agent","input":{"subagent_type":"Explore"}}]}}`+"\n")
	plantSidecar(t, src, filepath.Join("subagents", "agent-loc1.meta.json"),
		`{"agentType":"Explore","description":"look","toolUseId":"toolu_1","spawnDepth":1}`)
	plantSidecar(t, src, filepath.Join("subagents", "agent-loc1.jsonl"),
		`{"type":"user","uuid":"s1","agentId":"loc1","isSidechain":true,"sessionId":"parent-uuid"}`+"\n")
	child := filepath.Join(t.TempDir(), "child-ws")

	// Act.
	err := f.r.PortTranscript(context.Background(), src, f.multi, child, "child-uuid")

	// Assert: the locator moved with the records that name it, and the meta's
	// toolUseId is the very id the ported transcript's spawning call now carries.
	if err != nil {
		t.Fatalf("PortTranscript() = %v, want nil", err)
	}
	dest := account.TranscriptPath(f.multi, child, "child-uuid")
	subagents := filepath.Join(strings.TrimSuffix(dest, ".jsonl"), "subagents")
	entries, err := os.ReadDir(subagents)
	if err != nil {
		t.Fatalf("ReadDir(%s) = %v", subagents, err)
	}
	locator := ""
	for _, entry := range entries {
		if strings.HasSuffix(entry.Name(), ".jsonl") {
			locator = strings.TrimSuffix(strings.TrimPrefix(entry.Name(), "agent-"), ".jsonl")
		}
	}
	if locator == "" || locator == "loc1" {
		t.Fatalf("the ported sidecar holds %v, want a subagent transcript under a re-minted locator", entries)
	}
	transcriptBody := readBody(t, dest)
	metaBody := readBody(t, filepath.Join(subagents, "agent-"+locator+".meta.json"))
	subagentBody := readBody(t, filepath.Join(subagents, "agent-"+locator+".jsonl"))
	spawned := jsonField(t, transcriptBody, "message", "content", "0", "id")
	if got := jsonField(t, metaBody, "toolUseId"); got != spawned {
		t.Fatalf("the meta names %q as the spawning call, want the transcript's own %q", got, spawned)
	}
	if got := jsonField(t, subagentBody, "agentId"); got != locator {
		t.Fatalf("the subagent record states agentId %q, want the file name's %q", got, locator)
	}
	if strings.Contains(transcriptBody+metaBody+subagentBody, "toolu_1") {
		t.Fatal("the ported conversation still carries the parent's tool_use id")
	}
}

// readBody reads one ported file.
func readBody(t *testing.T, path string) string {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile(%s) = %v", path, err)
	}
	return string(raw)
}

// jsonField walks one JSON document to a string leaf; a numeric step indexes an
// array.
func jsonField(t *testing.T, body string, steps ...string) string {
	t.Helper()
	var value any
	if err := json.Unmarshal([]byte(strings.TrimSpace(body)), &value); err != nil {
		t.Fatalf("Unmarshal(%q) = %v", body, err)
	}
	for _, step := range steps {
		switch typed := value.(type) {
		case map[string]any:
			value = typed[step]
		case []any:
			index, err := strconv.Atoi(step)
			if err != nil || index >= len(typed) {
				t.Fatalf("step %q does not index %v", step, typed)
			}
			value = typed[index]
		default:
			t.Fatalf("step %q has nothing to walk in %v", step, typed)
		}
	}
	leaf, ok := value.(string)
	if !ok {
		t.Fatalf("%v is not a string leaf", value)
	}
	return leaf
}

func TestMoveTranscriptWithinOneFilesystemRemovesTheSource(t *testing.T) {
	// Arrange: an account switch — two roots holding the same conversation is
	// exactly what makes a resume ambiguous.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", "body")

	// Act.
	err := f.r.MoveTranscript(context.Background(), src, f.multi, f.ws)

	// Assert.
	if err != nil {
		t.Fatalf("MoveTranscript() = %v, want nil", err)
	}
	assertFileBody(t, account.TranscriptPath(f.multi, f.ws, "uuid-1"), "body")
	if _, err := os.Stat(src); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("Stat(source) = %v, want it gone", err)
	}
}

func TestMoveTranscriptCarriesTheSidecarDirectory(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", "body")
	srcSidecar := plantSidecar(t, src, "note.json", "sidecar")

	// Act.
	err := f.r.MoveTranscript(context.Background(), src, f.multi, f.ws)

	// Assert.
	if err != nil {
		t.Fatalf("MoveTranscript() = %v, want nil", err)
	}
	dest := account.TranscriptPath(f.multi, f.ws, "uuid-1")
	assertFileBody(t, filepath.Join(dest[:len(dest)-len(".jsonl")], "note.json"), "sidecar")
	if _, err := os.Stat(srcSidecar); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("Stat(source sidecar) = %v, want it gone", err)
	}
}

func TestMoveTranscriptRefusesWhenTheDestinationExists(t *testing.T) {
	// Arrange: overwriting one conversation with another is never recovery.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", "source")
	plantTranscript(t, f.multi, f.ws, "uuid-1", "already there")

	// Act.
	err := f.r.MoveTranscript(context.Background(), src, f.multi, f.ws)

	// Assert.
	if err == nil {
		t.Fatal("MoveTranscript() = nil error, want a refusal")
	}
	assertFileBody(t, account.TranscriptPath(f.multi, f.ws, "uuid-1"), "already there")
}

func TestPortTranscriptRefusesWhenTheSidecarDestinationExists(t *testing.T) {
	// Arrange: the sidecar is half the conversation; its destination is guarded
	// exactly as the transcript's is.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", "source")
	plantSidecar(t, src, "note.json", "sidecar")
	destExisting := account.TranscriptPath(f.multi, f.ws, "uuid-1")
	plantSidecar(t, destExisting, "note.json", "already there")

	// Act.
	err := f.r.PortTranscript(context.Background(), src, f.multi, f.ws, "uuid-1")

	// Assert.
	if err == nil {
		t.Fatal("PortTranscript() = nil error, want a refusal")
	}
}

func TestPortTranscriptRefusesAMissingSource(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)

	// Act.
	err := f.r.PortTranscript(context.Background(), filepath.Join(f.def, "nope.jsonl"), f.multi, f.ws, "uuid-2")

	// Assert.
	if err == nil {
		t.Fatal("PortTranscript() = nil error, want a refusal")
	}
}

func TestPortTranscriptRejectsMissingInputs(t *testing.T) {
	tests := []struct {
		name      string
		source    string
		destRoot  string
		destWSDir string
	}{
		{name: "no source", source: "", destRoot: "/m", destWSDir: "/ws"},
		{name: "no destination root", source: "/s.jsonl", destRoot: "", destWSDir: "/ws"},
		{name: "no workspace dir", source: "/s.jsonl", destRoot: "/m", destWSDir: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newTranscriptFixture(t)

			// Act.
			err := f.r.PortTranscript(context.Background(), tc.source, tc.destRoot, tc.destWSDir, "uuid-2")

			// Assert.
			if err == nil {
				t.Fatal("PortTranscript() = nil error, want a refusal")
			}
		})
	}
}

func TestMoveTranscriptHonorsCancelledContext(t *testing.T) {
	// Arrange.
	f := newTranscriptFixture(t)
	src := plantTranscript(t, f.def, f.ws, "uuid-1", "body")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := f.r.MoveTranscript(ctx, src, f.multi, f.ws)

	// Assert.
	if err == nil {
		t.Fatal("MoveTranscript() = nil error, want the context's error")
	}
}

// assertFileBody fails unless path holds exactly want.
func assertFileBody(t *testing.T, path, want string) {
	t.Helper()
	got, err := os.ReadFile(path) //nolint:gosec // test-owned path
	if err != nil {
		t.Fatalf("ReadFile(%s) = %v", path, err)
	}
	if string(got) != want {
		t.Fatalf("ReadFile(%s) = %q, want %q", path, got, want)
	}
}
