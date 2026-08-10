// THE MERGE QUEUE, end to end over the REAL processes: two workspaces of the
// same repository contending for one target worktree, driven as real
// MergeWorkspaceCmd frames over the frontend WebSocket, landing in real git
// repositories on disk, and observed as the resolved RenderState plus the
// MergeStatus facts on the pushed WorkspaceState.
//
// WHY THE HEAD OF THE QUEUE IS A CONFLICT. Serialization is only observable if
// the head of the queue is still holding it when the second request arrives. A
// clean cherry-pick of two commits finishes in milliseconds, so a test that
// fired both and waited would prove nothing but that both eventually landed —
// exactly the "eventual success" assertion this file is required NOT to make.
// A conflicting pick parks in the target tree awaiting a human (merge.Driver
// never aborts one), so the head is provably still held when the second
// request is admitted, and the second request's merge_queued position is a
// FACT rather than a race.
//
// WHY THE GEOMETRY IS RECORDED RATHER THAN SENT. MergeWorkspaceCmd carries no
// geometry at all any more (source_branch/source_dir/target_dir are reserved in
// frontend.proto): THE daemon owns the workspace->worktree map, so a fixture
// repository the daemon never created becomes mergeable by writing the record
// the daemon would have written at creation time (mergeCmdFor). The worktrees
// are still real `git worktree add` siblings rather than fixtures pretending to
// be siblings, because the whole repo-key question this file covers is about
// what sibling worktrees resolve to.
//
// This file is the HELPER HOME for the merge e2e suite. mergeconflict,
// mergelease, mergeleaserelease and mergedurability reuse its fixtures and its
// mergeWatch READ-ONLY, the way the other e2e files reuse clearcompact's
// liveSession and interrupt's awaitAll.
//
// NOT COVERED HERE — the shim lease and its release (mergelease /
// mergeleaserelease), the conflict's in-tree residue (mergeconflict), and the
// bounce (mergedurability).
package e2e

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"

	"claude-repld/internal/workspace/geometry"
)

// --- git fixtures -----------------------------------------------------------

// mergeGitEnv strips the repository bindings a parent Git hook exports. Without
// it a fixture `git commit` can rewrite the CALLER's index instead of the
// temporary repo's — the same boundary internal/workspace/merge's own tests
// draw (merge_test.go gitFixtureEnv).
func mergeGitEnv() []string {
	env := make([]string, 0, len(os.Environ()))
	for _, entry := range os.Environ() {
		switch {
		case strings.HasPrefix(entry, "GIT_DIR="),
			strings.HasPrefix(entry, "GIT_WORK_TREE="),
			strings.HasPrefix(entry, "GIT_INDEX_FILE="),
			strings.HasPrefix(entry, "GIT_PREFIX="):
			continue
		default:
			env = append(env, entry)
		}
	}
	return env
}

// mergeGit runs one fixture git command, failing the test on a non-zero exit.
// The PRODUCTION git is merge.Driver's own; nothing here runs on its behalf.
func mergeGit(t *testing.T, dir string, args ...string) string {
	t.Helper()
	full := append([]string{"-c", "core.hooksPath=/dev/null", "-C", dir}, args...)
	cmd := exec.Command("git", full...)
	cmd.Env = mergeGitEnv()
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
	return string(out)
}

// mergeGitExit runs one fixture git command and returns its exit code plus
// combined output WITHOUT failing: it is how a test asks a yes/no question of
// the repository (is a cherry-pick still in progress?).
func mergeGitExit(t *testing.T, dir string, args ...string) (int, string) {
	t.Helper()
	full := append([]string{"-c", "core.hooksPath=/dev/null", "-C", dir}, args...)
	cmd := exec.Command("git", full...)
	cmd.Env = mergeGitEnv()
	out, err := cmd.CombinedOutput()
	if err == nil {
		return 0, string(out)
	}
	var exit *exec.ExitError
	if !errors.As(err, &exit) {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
	return exit.ExitCode(), string(out)
}

// canonicalDir resolves symlinks, because the workspace KEY a merge's state is
// filed under is a canonical directory (dlog.WorkspaceFromDirectory), and
// t.TempDir hands back /var/... on macOS where the canonical form is
// /private/var/.... A test that sent the uncanonical path would file its
// assertions under a workspace the daemon never wrote.
func canonicalDir(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatalf("canonicalize %s: %v", dir, err)
	}
	return resolved
}

// requireGit skips loudly when git is absent, matching the harness's posture on
// every other external prerequisite (node, the go toolchain): a missing tool is
// an operator message, never a failure.
func requireGit(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not found in PATH")
	}
}

// mergeRepo is one fixture repository: the TARGET worktree everything
// cherry-picks into, plus the sibling worktrees cut from it.
type mergeRepo struct {
	t      *testing.T
	target string
}

// newMergeRepo creates a repository on `main` with one commit carrying
// shared.txt, and repo-local identity + hook isolation so merge.Driver's own
// cherry-pick can commit hermetically.
func newMergeRepo(t *testing.T) *mergeRepo {
	t.Helper()
	requireGit(t)
	dir := canonicalDir(t, t.TempDir())
	mergeGit(t, dir, "init", "-q", "-b", "main")
	mergeGit(t, dir, "config", "user.email", "e2e@example.com")
	mergeGit(t, dir, "config", "user.name", "E2E")
	mergeGit(t, dir, "config", "commit.gpgsign", "false")
	mergeGit(t, dir, "config", "core.hooksPath", "/dev/null")
	writeMergeFile(t, dir, "shared.txt", "base\n")
	mergeGit(t, dir, "add", ".")
	mergeGit(t, dir, "commit", "-q", "-m", "base")
	return &mergeRepo{t: t, target: dir}
}

// worktree adds a linked worktree on a new branch cut from main — a genuine
// sibling (shared object store and refs), which is the ONLY fixture that can
// answer the repo-key question this file asks.
func (r *mergeRepo) worktree(branch string) string {
	r.t.Helper()
	dir := canonicalDir(r.t, r.t.TempDir())
	// `git worktree add` insists on creating the leaf itself.
	leaf := filepath.Join(dir, branch)
	mergeGit(r.t, r.target, "worktree", "add", "-q", "-b", branch, leaf, "main")
	return canonicalDir(r.t, leaf)
}

// divergeTarget moves the target's own main forward, so a worktree that also
// touched shared.txt will genuinely conflict rather than fast-forward.
func (r *mergeRepo) divergeTarget(content string) {
	r.t.Helper()
	writeMergeFile(r.t, r.target, "shared.txt", content)
	mergeGit(r.t, r.target, "add", ".")
	mergeGit(r.t, r.target, "commit", "-q", "-m", "target moves shared.txt")
}

func readMergeFile(t *testing.T, dir, name string) string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		t.Fatalf("read %s in %s: %v", name, dir, err)
	}
	return string(data)
}

func writeMergeFile(t *testing.T, dir, name, content string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(content), 0o644); err != nil {
		t.Fatalf("write %s in %s: %v", name, dir, err)
	}
}

// commitIn writes a file in a worktree and commits it, returning nothing: what
// matters downstream is the branch, not the sha.
func commitIn(t *testing.T, dir, name, content, message string) {
	t.Helper()
	writeMergeFile(t, dir, name, content)
	mergeGit(t, dir, "add", ".")
	mergeGit(t, dir, "commit", "-q", "-m", message)
}

// conflictingWorktree is the standard PARKED HEAD of these tests: a sibling
// worktree whose single commit rewrites the same file the target has since
// rewritten, so its cherry-pick conflicts and stays parked in the target tree.
func (r *mergeRepo) conflictingWorktree(branch string) string {
	r.t.Helper()
	dir := r.worktree(branch)
	commitIn(r.t, dir, "shared.txt", "from "+branch+"\n", branch+" rewrites shared.txt")
	r.divergeTarget("from main\n")
	return dir
}

// cleanWorktree is a sibling whose commit touches a file nothing else does, so
// its cherry-pick can only be held up by the QUEUE, never by a conflict. It is
// the probe every serialization assertion in this file rests on.
func (r *mergeRepo) cleanWorktree(branch string) string {
	r.t.Helper()
	dir := r.worktree(branch)
	commitIn(r.t, dir, branch+".txt", "hello from "+branch+"\n", branch+" adds its own file")
	return dir
}

// blockedWorktree is the HARD-FAILURE fixture: the target already carries an
// UNTRACKED file the branch's commit adds as a tracked one, so git refuses the
// pick outright rather than pausing on it. The refusal exits non-zero with
// nothing in CHERRY_PICK_HEAD to resolve and with the range genuinely
// un-incorporated, which is exactly merge.Driver's merge_failed sentinel
// ("cherry-pick exited %d with no conflict to resolve").
//
// A conflict would NOT do here: the two outcomes are distinct by design, and a
// test that provoked merge_failed with a conflict would be asserting the wrong
// classification.
func (r *mergeRepo) blockedWorktree(branch string) string {
	r.t.Helper()
	dir := r.worktree(branch)
	commitIn(r.t, dir, "blocked.txt", "from "+branch+"\n", branch+" adds blocked.txt")
	writeMergeFile(r.t, r.target, "blocked.txt", "an untracked squatter in the target\n")
	return dir
}

// --- the merge command surface ----------------------------------------------

// mergeCmd is one MergeWorkspaceCmd's worth of identity.
//
// `workspace` is the daemon's workspace KEY (the session cwd / the source
// worktree), and `name` is the DISPLAY name the merge/<name> completion tag is
// written from. They are separate fields precisely because conflating them
// filed merge rows under a key nothing else used (frontend.proto
// MergeWorkspaceCmd.workspace_name).
//
// IT CARRIES NO GEOMETRY, because the command no longer can: source_branch,
// source_dir and target_dir are reserved in frontend.proto and the daemon
// answers all three from the map it owns. The geometry a fixture repository
// needs is RECORDED into that map by mergeCmdFor.
type mergeCmd struct {
	workspace string
	name      string
}

// mergeGeometryRecorder is the harness-side half of THE daemon's geometry map:
// what a test records is what the daemon's own resolution will read back.
// Satisfied by *geometry.Store.
type mergeGeometryRecorder interface {
	Record(context.Context, geometry.Record) error
}

// mergeCmdFor records the geometry the daemon would have recorded when it
// created worktreeDir, and returns the bare command that merges it into repo's
// target. The workspace key and the source dir are the same directory, exactly
// as they are in production (the session's cwd IS its worktree).
//
// RECORDING IS PART OF BUILDING THE COMMAND rather than a separate step a test
// could forget: a merge whose geometry was never recorded is refused by the
// daemon, and a suite that discovered that as a nack deadline instead of a
// missing setup line would be debugging the wrong thing.
func mergeCmdFor(t *testing.T, geo mergeGeometryRecorder, repo *mergeRepo, worktreeDir, branch string) mergeCmd {
	t.Helper()
	if err := geo.Record(context.Background(), geometry.Record{
		Workspace:    worktreeDir,
		SourceBranch: branch,
		SourceDir:    worktreeDir,
		TargetDir:    repo.target,
		Origin:       geometry.OriginCreated,
	}); err != nil {
		t.Fatalf("record merge geometry for %s: %v", worktreeDir, err)
	}
	return mergeCmd{workspace: worktreeDir, name: branch}
}

func sendMerge(t *testing.T, conn *websocket.Conn, requestID string, m mergeCmd) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"workspace":%q,"mergeWorkspace":{"workspaceName":%q}}`,
		requestID, m.workspace, m.name))
}

// sendMergeResume is the resolve-and-continue handoff: the same workspace with
// conflict_resolved_continue set, which is how a human-resolved conflict is
// handed back to the coordinator.
func sendMergeResume(t *testing.T, conn *websocket.Conn, requestID string, m mergeCmd) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"workspace":%q,"mergeWorkspace":{"conflictResolvedContinue":true,"workspaceName":%q}}`,
		requestID, m.workspace, m.name))
}

// --- observation -------------------------------------------------------------

// The merge phase vocabulary. These used to be the strings that appeared on
// WorkspaceState.merge_phase; that field is RETIRED, so the suite names the
// merge axis's own resolved RenderState instead -- the same axis, read off the
// surface that still carries it. merge.Phase's values were always "the
// snake_case render-state vocabulary the SSM and the frontend.v1 RenderState
// enum share" (merge.go), so this is a change of surface and not of meaning.
const (
	phaseMergeEnqueuing = frontendv1.RenderState_RENDER_STATE_MERGE_ENQUEUING
	phaseMerging        = frontendv1.RenderState_RENDER_STATE_MERGING
	phaseMergeQueued    = frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED
	phaseMergeConflict  = frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT
	phaseMergeFailed    = frontendv1.RenderState_RENDER_STATE_MERGE_FAILED
	phaseMerged         = frontendv1.RenderState_RENDER_STATE_MERGED
)

// mergeWatch is an accumulating reader over ONE frontend connection.
//
// Accumulating rather than matching-and-discarding, because every assertion
// this suite makes is about ORDER between frames produced by different
// producers: a merge_queued pushed by the coordinator's enqueue, a CommandAck
// enqueued by the frontend server after the dispatch returns, and a merged
// pushed when the cherry-pick lands. A helper that read until the first match
// and dropped the rest could not answer "was it queued BEFORE it landed",
// which is the whole serialization question.
type mergeWatch struct {
	t      *testing.T
	conn   *websocket.Conn
	states []*frontendv1.WorkspaceState
	acks   map[string]*frontendv1.CommandAck
	items  map[string][]*frontendv1.ConversationItem
}

func newMergeWatch(t *testing.T, conn *websocket.Conn) *mergeWatch {
	t.Helper()
	return &mergeWatch{
		t:     t,
		conn:  conn,
		acks:  map[string]*frontendv1.CommandAck{},
		items: map[string][]*frontendv1.ConversationItem{},
	}
}

func (w *mergeWatch) record(frame *frontendv1.FrontendFrame) {
	switch f := frame.GetFrame().(type) {
	case *frontendv1.FrontendFrame_WorkspaceState:
		w.states = append(w.states, f.WorkspaceState)
	case *frontendv1.FrontendFrame_CommandAck:
		w.acks[f.CommandAck.GetRequestId()] = f.CommandAck
	case *frontendv1.FrontendFrame_ConversationDelta:
		ws := f.ConversationDelta.GetWorkspace()
		w.items[ws] = append(w.items[ws], f.ConversationDelta.GetItems()...)
	}
}

// mergeRunTimeout is the wait budget for a frame a MERGE RUN has to produce,
// and it is deliberately not the suite's frameTimeout.
//
// frameTimeout bounds a frame the daemon composes from state it already holds,
// and it says so: five seconds against a measured ~0.65s, where a wait needing
// tens of seconds would be a test synchronizing by generosity. The awaits here
// are a different animal. A merge run's frames are published either side of
// GIT PROCESSES — a rebase replay per commit, a suite selection, a test gate,
// and the landing sequence of update-ref, worktree re-sync and merge commit —
// and the run publishes nothing in between, because the merge status vocabulary
// has no arm for "landing on the target". The bound is therefore set by how
// long a handful of git invocations take on the machine, which on the gate is a
// machine running every other suite of this repository at the same time; the
// suite that exercises this same pipeline in-process takes 108s of it.
//
// The named bound is one merge run's git work under that contention. It is NOT
// a hang backstop — `go test -timeout` is — and it is not a bigger number for a
// flake: a merge that publishes nothing for a minute IS a broken merge.
var mergeRunTimeout = 60 * time.Second

// until reads and records frames until cond holds, failing at the deadline. The
// condition is evaluated over EVERYTHING recorded so far, so a fact that
// arrived before until was called still satisfies it.
//
// Both budgets are mergeRunTimeout: the total, and each individual read. The
// read matters as much as the total here, because the silence this suite has to
// tolerate is one uninterrupted gap between two publications rather than a slow
// accumulation of frames.
func (w *mergeWatch) until(what string, cond func() bool) {
	w.t.Helper()
	deadline := time.Now().Add(mergeRunTimeout)
	for {
		if cond() {
			return
		}
		if !time.Now().Before(deadline) {
			w.t.Fatalf("%s never arrived before the deadline", what)
		}
		w.record(readFrameWithin(w.t, w.conn, mergeRunTimeout))
	}
}

// stateInPhase reports whether state PRESENTS phase.
//
// TWO SURFACES ARE READ, because the retired flat merge_phase was a third that
// neither reproduces alone.
//
//   - The resolved RenderState is the merge axis's own output, but `state` is a
//     COMPOSITE: connectivity and session status contribute to it too. A merged
//     workspace is hibernated moments after it lands, so its render state moves
//     off `merged` while the run's terminal account is still on the frame.
//   - MergeStatus is the run's own arm, and it survives that move -- but it
//     cannot name every axis row either: `enqueued` rides merge_enqueuing for a
//     head admission and merge_queued for a deferred one, so it pins neither.
//
// The union is what the coarse field used to stand in for, and reading both is
// what keeps these assertions about the MERGE rather than about whichever
// surface happened to still be carrying it.
func stateInPhase(state *frontendv1.WorkspaceState, phase frontendv1.RenderState) bool {
	if state.GetState() == phase {
		return true
	}
	return phase != frontendv1.RenderState_RENDER_STATE_UNSPECIFIED &&
		mergeStatusPhase(state.GetMergeStatus()) == phase
}

// mergeStatusPhase maps a run's status arm onto the render state it rides, or
// UNSPECIFIED for an arm that pins none.
func mergeStatusPhase(status *frontendv1.MergeStatus) frontendv1.RenderState {
	switch status.GetPhase().(type) {
	case *frontendv1.MergeStatus_BeforeAction, *frontendv1.MergeStatus_CherryPicking,
		*frontendv1.MergeStatus_Testing, *frontendv1.MergeStatus_AfterAction:
		return frontendv1.RenderState_RENDER_STATE_MERGING
	case *frontendv1.MergeStatus_Conflict:
		return frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT
	case *frontendv1.MergeStatus_Merged:
		return frontendv1.RenderState_RENDER_STATE_MERGED
	case *frontendv1.MergeStatus_Failed:
		return frontendv1.RenderState_RENDER_STATE_MERGE_FAILED
	}
	return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED
}

// firstPhase returns the first recorded WorkspaceState for ws presenting phase,
// or nil.
func (w *mergeWatch) firstPhase(ws string, phase frontendv1.RenderState) *frontendv1.WorkspaceState {
	for _, state := range w.states {
		if state.GetWorkspace() == ws && stateInPhase(state, phase) {
			return state
		}
	}
	return nil
}

func (w *mergeWatch) sawPhase(ws string, phase frontendv1.RenderState) bool {
	return w.firstPhase(ws, phase) != nil
}

// awaitPhase blocks until ws has been seen in phase and returns the FIRST such
// state — the one whose queue facts describe the transition rather than some
// later republication of it.
func (w *mergeWatch) awaitPhase(ws string, phase frontendv1.RenderState) *frontendv1.WorkspaceState {
	w.t.Helper()
	w.until(fmt.Sprintf("a WorkspaceState for %s presenting %s", ws, phase), func() bool {
		return w.sawPhase(ws, phase)
	})
	return w.firstPhase(ws, phase)
}

// latest returns the most recently recorded WorkspaceState for ws, or nil.
func (w *mergeWatch) latest(ws string) *frontendv1.WorkspaceState {
	for i := len(w.states) - 1; i >= 0; i-- {
		if w.states[i].GetWorkspace() == ws {
			return w.states[i]
		}
	}
	return nil
}

func (w *mergeWatch) awaitAck(requestID string) *frontendv1.CommandAck {
	w.t.Helper()
	w.until("the CommandAck for "+requestID, func() bool { return w.acks[requestID] != nil })
	return w.acks[requestID]
}

// awaitOKAck is awaitAck plus the assertion that the command was ACCEPTED. A
// merge command's ack means ADMITTED TO THE QUEUE, never MERGED:
// merge.Coordinator.Enqueue "returns as soon as the request is DURABLY
// enqueued, not when the merge completes".
func (w *mergeWatch) awaitOKAck(requestID string) *frontendv1.CommandAck {
	w.t.Helper()
	ack := w.awaitAck(requestID)
	if !ack.GetOk() {
		w.t.Fatalf("command %s was nacked: %s", requestID, ack.GetError())
	}
	return ack
}

// awaitItem blocks until a conversation item for ws satisfies match.
func (w *mergeWatch) awaitItem(ws, what string, match func(*frontendv1.ConversationItem) bool) *frontendv1.ConversationItem {
	w.t.Helper()
	var found *frontendv1.ConversationItem
	w.until(what, func() bool {
		for _, item := range w.items[ws] {
			if match(item) {
				found = item
				return true
			}
		}
		return false
	})
	return found
}

// firstEnqueued returns the first recorded WorkspaceState for ws carrying a
// merge_status on its `enqueued` arm, or nil.
func (w *mergeWatch) firstEnqueued(ws string) *frontendv1.WorkspaceState {
	for _, state := range w.states {
		if state.GetWorkspace() == ws && state.GetMergeStatus().GetEnqueued() != nil {
			return state
		}
	}
	return nil
}

// assertAdmittedQueuePosition is the shared queue-fact assertion: the 1-based
// index and the queue depth the run was ADMITTED at.
//
// REWRITTEN off the retired flat merge_queue_position / merge_queue_depth pair,
// which was a queue snapshot re-read at push time and therefore readable on any
// frame. MergeStatus reports the place ONCE, on the `enqueued` arm the
// coordinator publishes under the repository's advance gate, so the assertion
// is scoped to that admission rather than to whatever frame happened to be
// latest. That is the stronger reading: it pins where the run entered the
// queue, which no later re-read can contradict.
func assertAdmittedQueuePosition(t *testing.T, w *mergeWatch, ws string, wantIndex, wantDepth int32) {
	t.Helper()
	w.until("a WorkspaceState for "+ws+" carrying an enqueued merge_status", func() bool {
		return w.firstEnqueued(ws) != nil
	})
	enqueued := w.firstEnqueued(ws).GetMergeStatus().GetEnqueued()
	if got := enqueued.GetPosition(); got != wantIndex {
		t.Errorf("admitted queue position for %s = %d, want %d (1-based; the merge cherry-picking now is 1)",
			ws, got, wantIndex)
	}
	if got := enqueued.GetDepth(); got != wantDepth {
		t.Errorf("admitted queue depth for %s = %d, want %d", ws, got, wantDepth)
	}
}

// parkTheQueueHead merges a conflicting sibling and returns once its conflict
// is PARKED — i.e. the repository's queue head is provably still held. Every
// serialization test in this suite starts here, because a queue with nothing at
// its head cannot demonstrate a queue.
func parkTheQueueHead(t *testing.T, geo mergeGeometryRecorder, w *mergeWatch, repo *mergeRepo, worktreeDir, branch, requestID string) mergeCmd {
	t.Helper()
	cmd := mergeCmdFor(t, geo, repo, worktreeDir, branch)
	sendMerge(t, w.conn, requestID, cmd)
	w.awaitPhase(worktreeDir, phaseMergeConflict)
	assertAdmittedQueuePosition(t, w, worktreeDir, 1, 1)
	return cmd
}

// --- tests -------------------------------------------------------------------

// TestE2EASecondMergeOnTheSameRepoIsQueuedBehindTheFirst is THE serialization
// assertion: with one merge holding the repository's queue head, a second
// merge of the same repository is admitted as merge_queued at position 2 of a
// 2-deep queue, and it is queued BEFORE it lands rather than merely landing
// afterwards.
//
// The "before it lands" half is what makes this a serialization test rather
// than an eventual-success test: the assertion is checked at the moment the
// merge_queued is observed, and it fails if the second merge had already
// reported merged by then.
func TestE2EASecondMergeOnTheSameRepoIsQueuedBehindTheFirst(t *testing.T) {
	// Arrange — one repository, two sibling worktrees, the first parked at a
	// conflict so the queue head is genuinely held.
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	first := repo.conflictingWorktree("feature-first")
	second := repo.cleanWorktree("feature-second")
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)
	parkTheQueueHead(t, h.geometry, w, repo, first, "feature-first", "r-merge-first")

	// Act — the second workspace of the SAME repository asks to merge.
	sendMerge(t, conn, "r-merge-second", mergeCmdFor(t, h.geometry, repo, second, "feature-second"))

	// Assert — admitted to the queue, behind the parked head, and not landed.
	w.awaitOKAck("r-merge-second")
	w.awaitPhase(second, phaseMergeQueued)
	assertAdmittedQueuePosition(t, w, second, 2, 2)
	if w.sawPhase(second, phaseMerged) {
		t.Error("the second merge reported merged before it was ever seen queued: the two merges were not serialized")
	}
	if w.sawPhase(second, phaseMerging) {
		t.Error("the second merge began cherry-picking while the first still held the queue head")
	}
}

// TestE2EQueueDepthIsKeyedByRepositoryNotByWorktree covers the REPO KEY: what
// shares a queue and what does not.
//
// The repo key itself never reaches the wire (Position.Repo is daemon-internal
// and WorkspaceState carries no repo field), so the key's identity is asserted
// through its ONE observable consequence — the queue a newly admitted merge
// lands on. A sibling worktree of the parked repository is admitted BEHIND the
// parked head on a 2-deep queue; a worktree of a genuinely different repository
// is admitted at the head of its own 1-deep queue and drains while the other
// repository is still parked. Any repo key that resolved siblings differently,
// or distinct repositories identically, breaks exactly one of these two rows.
//
// Both rows assert on the ADMISSION state rather than on a later one, because a
// merge that has reached a terminal phase is off its queue (Position: "Zero
// means not enqueued") and its queue facts no longer describe an admission.
func TestE2EQueueDepthIsKeyedByRepositoryNotByWorktree(t *testing.T) {
	tests := []struct {
		name string
		// sameRepo selects whether the enqueued worktree is a sibling of the
		// parked repository or a worktree of an independent one.
		sameRepo bool
		// admissionPhase is the phase the merge is ADMITTED in: deferred behind
		// the parked head on a shared queue, cherry-picking straight away on
		// its own.
		admissionPhase frontendv1.RenderState
		// wantIndex / wantDepth are that admission's queue facts.
		wantIndex int32
		wantDepth int32
		// wantDrains is whether the merge may run to completion while the other
		// repository's head is still parked — true only when the two queues are
		// genuinely independent.
		wantDrains bool
	}{
		{
			name:           "a sibling worktree joins the parked repository's queue",
			sameRepo:       true,
			admissionPhase: phaseMergeQueued,
			wantIndex:      2,
			wantDepth:      2,
			wantDrains:     false,
		},
		{
			name:           "a different repository queues independently",
			sameRepo:       false,
			admissionPhase: phaseMerging,
			wantIndex:      1,
			wantDepth:      1,
			wantDrains:     true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a parked head on repo one, plus the worktree under test.
			h := newUDSHarness(t)
			parked := newMergeRepo(t)
			parkedDir := parked.conflictingWorktree("feature-parked")
			conn := h.dialFrontend(t)
			defer conn.Close()
			w := newMergeWatch(t, conn)
			parkTheQueueHead(t, h.geometry, w, parked, parkedDir, "feature-parked", "r-merge-parked")

			subject := parked
			if !tc.sameRepo {
				subject = newMergeRepo(t)
			}
			subjectDir := subject.cleanWorktree("feature-subject")

			// Act.
			sendMerge(t, conn, "r-merge-subject", mergeCmdFor(t, h.geometry, subject, subjectDir, "feature-subject"))

			// Assert — the queue the merge was admitted to is the repository's,
			// not the worktree's.
			w.awaitOKAck("r-merge-subject")
			w.awaitPhase(subjectDir, tc.admissionPhase)
			assertAdmittedQueuePosition(t, w, subjectDir, tc.wantIndex, tc.wantDepth)
			if tc.wantDrains {
				w.awaitPhase(subjectDir, phaseMerged)
			} else if w.sawPhase(subjectDir, phaseMerged) {
				t.Error("a sibling worktree merged while its repository's queue head was still parked")
			}
		})
	}
}
