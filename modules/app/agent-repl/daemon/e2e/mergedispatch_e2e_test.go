// THE DISPATCH-JSON ROUTE INTO THE MERGE QUEUE — PART OF THE ACCEPTANCE GATE.
//
// ============================================================================
// THIS FILE IS WRITTEN AGAINST THE SPEC, NOT AGAINST THE IMPLEMENTATION.
//
// It is SPEC ITEM 7 of the merge-pipeline acceptance gate whose helper home is
// mergepipeline_e2e_test.go: a `merge' entry in a workspace-command file is a
// FIRST-CLASS REQUEST TO MERGE, and an entry that cannot name a real repository
// is REFUSED rather than accepted-and-forgotten. The producer is being built on
// sibling branches, so everything here is EXPECTED TO FAIL until that work
// lands, and it is deliberately neither skipped nor build-tagged: the gate runs
// as
//
//	go test ./e2e/ -count=1 -run TestE2EMergePipeline
//
// and a gate that passes vacuously is not a gate.
// ============================================================================
//
// WHY THIS ROUTE NEEDS ITS OWN FILE. Every other merge suite beside it drives
// the merge over the frontend WebSocket, where a MergeWorkspaceCmd arrives with
// a request id, is validated by the frontend command surface, and is answered
// with a CommandAck the test can read. The dispatch-JSON route has NONE of
// that. A skill writes a JSON file into the inbox directory, the file is
// claimed, parsed, persisted, and deleted, and the emitter is long gone. There
// is no ack to nack, no connection to close, and no human watching. That is
// precisely why the refusal half matters more here than anywhere else: on this
// route, "silently accepted and never merged" and "merged" are indistinguishable
// to the thing that asked.
//
// The entry shape under test is the one Emacs' own merge dispatcher reads
// (worktree.el `agent-repl--handle-merge-command'):
//
//	{"type":"merge","workspace":"<workspace key>","project_dir":"<abs repo dir>"}
//
// WHAT THIS FILE COVERS
//
//   - a well-formed merge entry reaches the repository's merge queue and RUNS,
//     observable as the workspace's MergeStatus phase stream reaching `merged';
//   - an entry whose project_dir is unusable — absent, relative, or naming
//     something that is not a repository — is REJECTED LOUDLY: the command file
//     is quarantined beside the inbox as durable evidence, the rejection is
//     logged, and NOTHING is enqueued.
//
// WHAT THIS FILE DELIBERATELY DOES NOT COVER
//
//   - the phase ORDER and the per-pick cursor (mergepipeline), the before/after
//     ACTIONS (mergeactions), the CONFLICT payload (mergestatusconflict), and
//     the daemon BOUNCE (mergestatusdurability). This file asks only whether the
//     dispatch route reaches the queue at all, and whether a bad entry is
//     stopped at the door;
//   - the OTHER command types the inbox ingests. `create' has its own durable
//     suite in internal/workspace/create, and the UI-only types are opaque to
//     the daemon by design;
//   - the `workspace' field's own resolution. Which key a merge is FILED under
//     is mergegeometry's question; this file only asks which repository it RUNS
//     against, which is what project_dir names.
//
// WHY THE INBOX IS BUILT HERE RATHER THAN TAKEN FROM THE HARNESS. newUDSHarness
// wires no create.Inbox — its workspace-creation seam is emptyWorkspaceCreation,
// which fails loudly on every creation call — so this file stands one up against
// the same daemon: a real FileJobStore on disk, a real create.Manager, and the
// real Inbox.ScanAndDrain. The creation collaborators are stubs in the spirit of
// emptyWorkspaceCreation: a merge-only command file must never plan a worktree,
// create a session, or publish a workspace, so every one of those stubs FAILS
// rather than quietly succeeding if the route ever wanders into it.
package e2e

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/workspace/create"
)

// --- the captured inbox log ---------------------------------------------------

// dispatchLog captures the lines the Inbox writes through its Logf.
//
// Capturing them is not decoration: "rejected LOUDLY" is a two-part claim, and
// the quarantine file only evidences the first part. A daemon that moved the
// file aside and said nothing has hidden the refusal from the operator, who has
// no other channel here — the emitter is a fire-and-forget skill and there is no
// ack to carry the complaint back on.
//
// The mutex is not theoretical. Inbox.Run polls on a ticker, and the manager
// advances jobs on their own goroutines; both reach the same Logf.
type dispatchLog struct {
	mu    sync.Mutex
	lines []string
}

func (l *dispatchLog) logf(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.lines = append(l.lines, fmt.Sprintf(format, args...))
}

func (l *dispatchLog) snapshot() []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	out := make([]string, len(l.lines))
	copy(out, l.lines)
	return out
}

// mentions reports whether any captured line contains every one of parts. It
// matches on SUBSTRINGS rather than on a whole line because the gate constrains
// what a rejection must SAY, never its exact wording.
func (l *dispatchLog) mentions(parts ...string) bool {
	for _, line := range l.snapshot() {
		matched := true
		for _, part := range parts {
			if !strings.Contains(line, part) {
				matched = false
				break
			}
		}
		if matched {
			return true
		}
	}
	return false
}

// --- the strict creation stubs ------------------------------------------------
//
// A merge command file materializes nothing. Every stub below therefore stands
// on a path this file does not model, and each one fails loudly when called, so
// a route that quietly turned a merge entry into a workspace creation reports
// itself as a named failure instead of as a mysterious extra worktree.

type dispatchStubWorktrees struct{ t *testing.T }

func (s dispatchStubWorktrees) PlanWorktree(context.Context, create.Job) (create.WorktreeResult, error) {
	s.t.Error("a merge command file asked the daemon to PLAN A WORKTREE: a merge entry names an existing repository and must never enter workspace materialization")
	return create.WorktreeResult{}, fmt.Errorf("e2e: worktree planning is not part of the merge dispatch route")
}

func (s dispatchStubWorktrees) EnsureWorktree(context.Context, create.Job) error {
	s.t.Error("a merge command file asked the daemon to CREATE A WORKTREE: a merge entry names an existing repository and must never enter workspace materialization")
	return fmt.Errorf("e2e: worktree creation is not part of the merge dispatch route")
}

type dispatchStubGeometry struct{ t *testing.T }

func (s dispatchStubGeometry) RecordWorkspaceGeometry(context.Context, create.Job) error {
	s.t.Error("a merge command file asked the daemon to RECORD CREATION GEOMETRY: the merge route READS the geometry map, it never writes it")
	return fmt.Errorf("e2e: geometry recording is not part of the merge dispatch route")
}

type dispatchStubSessions struct{ t *testing.T }

func (s dispatchStubSessions) EnsureSession(context.Context, create.Job) (string, error) {
	s.t.Error("a merge command file asked the daemon to CREATE A SESSION: nothing on the merge dispatch route brings a session up")
	return "", fmt.Errorf("e2e: session creation is not part of the merge dispatch route")
}

type dispatchStubHealth struct{ t *testing.T }

func (s dispatchStubHealth) AwaitHealthy(context.Context, create.Job) error {
	s.t.Error("a merge command file asked the daemon to AWAIT SESSION HEALTH: nothing on the merge dispatch route brings a session up")
	return fmt.Errorf("e2e: session health is not part of the merge dispatch route")
}

type dispatchStubPrompts struct{ t *testing.T }

func (s dispatchStubPrompts) SubmitInitialPrompt(context.Context, create.Job) error {
	s.t.Error("a merge command file asked the daemon to SUBMIT AN INITIAL PROMPT: a merge entry carries no prompt")
	return fmt.Errorf("e2e: initial prompts are not part of the merge dispatch route")
}

type dispatchStubAvailable struct{ t *testing.T }

func (s dispatchStubAvailable) PublishWorkspaceAvailable(context.Context, create.Available) error {
	s.t.Error("a merge command file asked the daemon to PUBLISH A WORKSPACE AS AVAILABLE: a merge entry creates no workspace to render")
	return fmt.Errorf("e2e: workspace availability is not part of the merge dispatch route")
}

type dispatchStubPublication struct{ t *testing.T }

func (s dispatchStubPublication) ReleaseSessionPublication(context.Context, create.PublicationDecision) error {
	s.t.Error("a merge command file asked the daemon to RELEASE SESSION PUBLICATION: a merge entry creates no session publication gate")
	return fmt.Errorf("e2e: session publication release is not part of the merge dispatch route")
}

func (s dispatchStubPublication) PrepareSessionPublication(context.Context, create.Job) error {
	s.t.Error("a merge command file asked the daemon to PREPARE SESSION PUBLICATION: a merge entry creates no session publication gate")
	return fmt.Errorf("e2e: session publication preparation is not part of the merge dispatch route")
}

// dispatchHostActions RECORDS rather than refuses.
//
// It is the one collaborator a merge entry may legitimately reach today: the
// inbox's current behavior is to persist every non-create type as an opaque
// HostAction and hand it to whoever is listening. Refusing here would pin the
// gate to one particular future wiring, and the gate's claim is about the
// OBSERVABLE OUTCOME — the merge runs — not about which internal seam carries
// it. What the recording buys is the negative assertion: after a rejected file,
// nothing reached the host either.
type dispatchHostActions struct {
	mu      sync.Mutex
	actions []create.HostAction
}

func (h *dispatchHostActions) PublishHostAction(_ context.Context, action create.HostAction) error {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.actions = append(h.actions, action)
	return nil
}

func (h *dispatchHostActions) published() []create.HostAction {
	h.mu.Lock()
	defer h.mu.Unlock()
	out := make([]create.HostAction, len(h.actions))
	copy(out, h.actions)
	return out
}

// --- the inbox fixture --------------------------------------------------------

// dispatchInbox is one real command-file ingress rooted in scratch space: the
// directory a skill writes into, the durable job store, the captured log, and
// the sink that records whatever the ingress hands onward.
type dispatchInbox struct {
	t     *testing.T
	dir   string
	store *create.FileJobStore
	log   *dispatchLog
	sink  *dispatchHostActions
	inbox *create.Inbox
}

// newDispatchInbox builds the ingress with the production Inbox and Manager and
// the strict stubs above.
//
// It takes the harness because the merge route is NOT a stub: h.merges is the
// same *server.MergeDispatchBinding main.go binds into the daemon's own inbox,
// already pointed at the *server.MergeDispatch that harness's WireAgentShim
// built. Routing through it rather than through a local fake is what makes this
// file a test of the dispatch-JSON route instead of a test of a fixture — the
// rejection-sentinel translation the quarantine assertions depend on lives in
// that binding and exists in exactly one place.
func newDispatchInbox(t *testing.T, h *e2eHarness) *dispatchInbox {
	t.Helper()
	root := t.TempDir()
	d := &dispatchInbox{
		t:    t,
		dir:  filepath.Join(root, "inbox"),
		log:  &dispatchLog{},
		sink: &dispatchHostActions{},
	}
	store, err := create.OpenJobStore(filepath.Join(root, "state", "workspace-create.json"), d.log.logf)
	if err != nil {
		t.Fatalf("open the workspace-create job store: %v", err)
	}
	d.store = store
	manager, err := create.NewManager(create.Config{
		Store:       store,
		Planner:     dispatchStubWorktrees{t: t},
		Worktrees:   dispatchStubWorktrees{t: t},
		Geometry:    dispatchStubGeometry{t: t},
		Sessions:    dispatchStubSessions{t: t},
		Health:      dispatchStubHealth{t: t},
		Prompts:     dispatchStubPrompts{t: t},
		Available:   dispatchStubAvailable{t: t},
		Releases:    dispatchStubPublication{t: t},
		Publication: dispatchStubPublication{t: t},
		HostActions: d.sink,
		Logf:        d.log.logf,
	})
	if err != nil {
		t.Fatalf("build the workspace-create manager: %v", err)
	}
	d.inbox = &create.Inbox{Dir: d.dir, Store: store, Manager: manager, Merges: h.merges, Logf: d.log.logf, Interval: 1}
	if err := os.MkdirAll(d.dir, 0o755); err != nil {
		t.Fatalf("make the command-file inbox directory: %v", err)
	}
	return d
}

// write drops one command file into the inbox under the name the ingress
// recognizes (the `workspace_commands_' prefix and the `.json' suffix) and
// returns its path.
func (d *dispatchInbox) write(name, body string) string {
	d.t.Helper()
	path := filepath.Join(d.dir, "workspace_commands_"+name+".json")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		d.t.Fatalf("write the command file %s: %v", path, err)
	}
	return path
}

// drain runs one full ingest pass. It is SYNCHRONOUS by contract
// (Inbox.ScanAndDrain is the daemon's startup drain), which is what lets every
// quarantine assertion below be a plain filesystem read with no waiting at all.
func (d *dispatchInbox) drain() {
	d.t.Helper()
	if err := d.inbox.ScanAndDrain(context.Background()); err != nil {
		d.t.Fatalf("draining the inbox failed structurally: %v. A command file the daemon cannot understand is that FILE's failure and must be quarantined and stepped over, never propagated as an ingress failure that stops the remaining files", err)
	}
}

// quarantined lists the rejected-file residue in the inbox directory. A claim
// the ingress refuses is renamed to `<claim>.rejected' and never deleted: it is
// the only surviving record of what the emitter asked for.
func (d *dispatchInbox) quarantined() []string {
	d.t.Helper()
	entries, err := os.ReadDir(d.dir)
	if err != nil {
		d.t.Fatalf("read the inbox directory %s: %v", d.dir, err)
	}
	var out []string
	for _, entry := range entries {
		if !entry.IsDir() && strings.HasSuffix(entry.Name(), ".rejected") {
			out = append(out, entry.Name())
		}
	}
	return out
}

// residue lists every remaining name in the inbox directory, for failure
// messages: against an unbuilt producer the useful question is never "was it
// quarantined" but "what, if anything, is left on disk at all".
func (d *dispatchInbox) residue() []string {
	d.t.Helper()
	entries, err := os.ReadDir(d.dir)
	if err != nil {
		d.t.Fatalf("read the inbox directory %s: %v", d.dir, err)
	}
	names := make([]string, 0, len(entries))
	for _, entry := range entries {
		names = append(names, entry.Name())
	}
	return names
}

// mergeDispatchEntry renders one command file containing a single merge entry.
// It builds the JSON by hand rather than through a marshaller so a test row can
// OMIT project_dir entirely, which is one of the three spellings under test and
// is not the same wire shape as an empty string.
func mergeDispatchEntry(workspace, projectDir string) string {
	if projectDir == "" {
		return fmt.Sprintf(`[{"type":"merge","workspace":%q}]`, workspace)
	}
	return fmt.Sprintf(`[{"type":"merge","workspace":%q,"project_dir":%q}]`, workspace, projectDir)
}

// --- the tests ----------------------------------------------------------------

// TestE2EMergePipelineDispatchJSONMergeReachesTheQueue is SPEC ITEM 7's
// POSITIVE half: a well-formed merge entry dropped into the command-file inbox
// reaches the repository's merge queue and RUNS TO COMPLETION.
//
// It asserts on the frontend's MergeStatus stream rather than on anything the
// ingress returns, and that choice is the point. A merge that was persisted,
// published, and acknowledged internally but never picked a commit is exactly
// the failure this route is prone to, and only the workspace's own structured
// status can tell the two apart. `merged' on the frontend means the pipeline
// genuinely ran; a store row means only that someone wrote a store row.
//
// The geometry is RECORDED, not sent, for the same reason it is in every merge
// suite here: MergeWorkspaceCmd carries no geometry any more and THE daemon owns
// the workspace->worktree map, so a fixture repository the daemon never created
// becomes mergeable by writing the record creation would have written.
// mergeCmdFor is reused for that recording alone — the command value it returns
// is deliberately discarded, because the whole question here is whether the
// JSON route can do what the wire route does.
func TestE2EMergePipelineDispatchJSONMergeReachesTheQueue(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a target declaring a passing suite, a sibling with commits that
	// collide with nothing, its geometry recorded, and a frontend watching before
	// anything is dropped into the inbox.
	const (
		branch  = "feature-dispatch-json"
		commits = 2
	)
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.multiCommitWorktree(branch, commits)
	_ = mergeCmdFor(t, h.geometry, repo, wsDir, branch)
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)
	d := newDispatchInbox(t, h)

	// Act — a skill drops the merge entry and the daemon drains it.
	d.write("merge-reaches-queue", mergeDispatchEntry(wsDir, wsDir))
	d.drain()

	// Assert — the run reached the queue, ran, and finished, and the status
	// stream it published is well-formed.
	w.awaitStatusArm(wsDir, armMerged)
	assertStatusStreamWellFormed(t, w, wsDir)
	assertNoArm(t, w, wsDir, armFailed,
		"a dispatch-JSON merge of non-colliding commits through a passing suite must land exactly as the same merge sent over the wire does")
	assertNoArm(t, w, wsDir, armConflict,
		"the fixture branch's commits touch files nothing else touches, so nothing can conflict")

	// The work is genuinely on the target, not merely reported as such.
	for i := 1; i <= commits; i++ {
		name := fmt.Sprintf("%s-%d.txt", branch, i)
		if !strings.Contains(readMergeFile(t, repo.target, name), "hello from "+branch) {
			t.Errorf("the dispatch-JSON merge of %s reported merged but %s is not in the target %s: the route reported work it did not do",
				wsDir, name, repo.target)
		}
	}
	// A well-formed entry is CONSUMED: the ingress deletes the claim only after
	// the entry is durably taken, so leftover quarantine residue would mean the
	// route refused an entry it then also ran.
	if rejected := d.quarantined(); len(rejected) != 0 {
		t.Errorf("a well-formed merge entry left quarantine residue %v in %s: the spec quarantines only entries the daemon REFUSES, and this one was required to run",
			rejected, d.dir)
	}
}

// TestE2EMergePipelineDispatchJSONUnusableProjectDirIsRejected is SPEC ITEM 7's
// NEGATIVE half, and it is ONE test because its three rows are three spellings
// of one edge: an entry whose project_dir cannot name a repository never
// enqueues.
//
// Absent, relative, and unknown are not three different rules — they are the
// three ways the SAME rule is broken, and a producer that handled any one of
// them by a special case rather than by validating project_dir would fail the
// other two. Splitting them into three tests would assert one edge three times;
// keeping them as rows asserts one edge against every spelling of it.
//
// WHY THE ASSERTIONS NEED NO WAITING. ScanAndDrain is synchronous, so by the
// time drain returns the ingress has finished with the file: the quarantine
// residue is on disk, the log line is captured, and the store either has a row
// or does not. Every check below is therefore a direct read, and a failure means
// the behavior is absent rather than merely late.
//
// WHY THE NEGATIVE IS CHECKED ON THE STORE RATHER THAN ON THE FRONTEND. Absence
// is not observable as a timeout — waiting for a merge that never comes proves
// only that it did not come YET. The durable store is where an accepted entry
// would have to exist, so an empty store is a POSITIVE statement that nothing
// was accepted, checked at a moment when acceptance would already have happened.
func TestE2EMergePipelineDispatchJSONUnusableProjectDirIsRejected(t *testing.T) {
	acceptanceGate(t)

	tests := []struct {
		name string
		// projectDir is the entry's project_dir field. Empty OMITS the field
		// entirely rather than sending "", because a missing key and an empty
		// string are different wire shapes and the missing key is the one a
		// hand-written or older emitter actually produces.
		projectDir string
		// why states the rule the row breaks, and is quoted verbatim into every
		// failure message so a red run names the requirement rather than the
		// fixture.
		why string
	}{
		{
			name:       "project_dir is absent",
			projectDir: "",
			why:        "an entry with NO project_dir names no repository at all, so there is no queue it could be admitted to",
		},
		{
			name:       "project_dir is relative",
			projectDir: filepath.Join("relative", "to", "nothing"),
			why:        "a RELATIVE project_dir resolves against whatever directory the daemon happens to be running in, which is not a repository the emitter chose",
		},
		{
			name:       "project_dir is unknown",
			projectDir: filepath.Join(string(filepath.Separator), "nonexistent-e2e-repo", "not-a-git-repository"),
			why:        "an ABSOLUTE project_dir that is not a repository names a merge target that does not exist, and inventing one is worse than refusing",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			acceptanceGate(t)

			// Arrange — a real repository whose worktree is genuinely mergeable,
			// so the ONLY thing wrong with the entry is its project_dir. A fixture
			// that was unmergeable for some other reason could not distinguish a
			// project_dir rejection from an incidental refusal.
			const branch = "feature-dispatch-bad-project-dir"
			h := newUDSHarness(t)
			repo := newMergeRepo(t)
			wsDir := repo.cleanWorktree(branch)
			_ = mergeCmdFor(t, h.geometry, repo, wsDir, branch)
			d := newDispatchInbox(t, h)

			// Act.
			path := d.write("merge-bad-project-dir", mergeDispatchEntry(wsDir, tc.projectDir))
			d.drain()

			// Assert — quarantined, logged, and nothing enqueued.
			rejected := d.quarantined()
			if len(rejected) == 0 {
				t.Errorf("the merge entry in %s was not quarantined: %s, so the spec requires the command file to be renamed aside as a *.rejected file. The inbox directory %s instead holds %v",
					path, tc.why, d.dir, d.residue())
			}
			// The log must NAME the file it refused. A generic "rejected a
			// command" line the operator cannot trace back to a file is not the
			// loud half of "rejected loudly".
			if !d.log.mentions(filepath.Base(path)) {
				t.Errorf("nothing in the inbox's log named the refused command file %s: %s, and quarantining a file without saying so hides the refusal from the only channel this route has (the emitter is a fire-and-forget skill with no ack). Captured log lines: %v",
					filepath.Base(path), tc.why, d.log.snapshot())
			}
			jobs, err := d.store.List()
			if err != nil {
				t.Fatalf("list the durable creation jobs: %v", err)
			}
			if len(jobs) != 0 {
				t.Errorf("the refused merge entry in %s produced %d durable job(s): %s, so NOTHING may be persisted for it",
					path, len(jobs), tc.why)
			}
			pending, err := d.store.PendingHostActions()
			if err != nil {
				t.Fatalf("list the pending host actions: %v", err)
			}
			if len(pending) != 0 {
				t.Errorf("the refused merge entry in %s was persisted as %d host action(s) %v: %s, so it must be refused at the door rather than durably accepted and handed onward",
					path, len(pending), hostActionTypes(pending), tc.why)
			}
			if published := d.sink.published(); len(published) != 0 {
				t.Errorf("the refused merge entry in %s was published onward as %d action(s) %v: %s, and a refused entry that still reaches a consumer is a refusal in name only",
					path, len(published), hostActionTypes(published), tc.why)
			}
		})
	}
}

// hostActionTypes names the actions in a failure message. The IDs carry the
// source file and array index, so a reader can find the offending entry without
// re-running anything.
func hostActionTypes(actions []create.HostAction) []string {
	out := make([]string, 0, len(actions))
	for _, action := range actions {
		out = append(out, fmt.Sprintf("%s(%s)", action.Type, action.ID))
	}
	return out
}
