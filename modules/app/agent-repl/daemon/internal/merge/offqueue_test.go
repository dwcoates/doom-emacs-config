package merge

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"strings"
	"reflect"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/semver"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// evicted queues the harness's own merge and evicts it, so its request
// stands off the queue, and makes its branch exist.
func evicted(t *testing.T, h *harness) wsm.OffQueueMerge {
	t.Helper()
	enqueue(t, h)
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	rec, kept, err := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
	if err != nil || !kept {
		t.Fatalf("OffQueueMergeOf = %+v, %v, %v; want the evicted request", rec, kept, err)
	}
	branchExists(h, requestBranch(h.db.workspaces[theWorkspace], rec.Source))
	return rec
}

// branchExists makes BRANCH exist in the harness's repository.
func branchExists(h *harness, branch string) {
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	h.git.refs["refs/heads/"+branch] = "b4anch"
}

// refusedArm answers ERR's refusal arm, "" for an error that is none.
func refusedArm(err error) string {
	if refusal, ok := Refused(err); ok {
		return refusal.Arm
	}
	return ""
}

func TestRequeuePutsAnEvictedMergeBackInLineAsItself(t *testing.T) {
	// Arrange
	h := newHarness(t)
	rec := evicted(t, h)

	// Act
	requeued, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser})

	// Assert: in line, as itself, the record gone, and the requeue recorded.
	if err != nil || !requeued.InLine || requeued.Position != 1 {
		t.Fatalf("Requeue = %+v, %v; want in line at 1", requeued, err)
	}
	entry, ok := h.entryOf(theWorkspace)
	if !ok || entry.State != wsm.MergeQueued || !reflect.DeepEqual(entry.Source, rec.Source) {
		t.Fatalf("queue entry = %+v (%v), want the same request queued", entry, ok)
	}
	if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); kept {
		t.Fatal("the requeue left the record standing")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.merge.requeue",
		Message: "put a merge that left its queue back on it, as itself"}); !ok {
		t.Fatalf("the requeue was not recorded: %v", h.logs.Records())
	}
}

func TestRequeuePutsAnAsyncMergeBackOnItsOwnMergeWorkspace(t *testing.T) {
	// Arrange: an async merge handed off and evicted.
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	branchExists(h, h.db.workspaces[theMergeWorkspace].Branch)
	forks := len(h.detached.forks)

	// Act
	_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theMergeWorkspace, By: RequestedByUser})

	// Assert: the same merge workspace stands in line again, and no other
	// was forked.
	if err != nil {
		t.Fatalf("Requeue: %v", err)
	}
	if _, ok := h.entryOf(theMergeWorkspace); !ok {
		t.Fatal("the merge workspace is not back in line")
	}
	if len(h.detached.forks) != forks {
		t.Fatalf("the requeue forked %d new workspaces, want none", len(h.detached.forks)-forks)
	}
}

func TestRequeueRecordsTheModelTheSessionResumesUnder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	evicted(t, h)

	// Act
	_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser, Model: "claude-opus-5-5"})

	// Assert
	if err != nil {
		t.Fatalf("Requeue: %v", err)
	}
	if got := h.db.sessions[theWorkspace].Model; got != "claude-opus-5-5" {
		t.Fatalf("the session's recorded model = %q, want the requeue's", got)
	}
}

func TestRequeueRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		req     Requeue
		want    string
	}{
		{name: "a workspace whose merge holds no request", arrange: func(*testing.T, *harness) {}, want: ArmUnknownMerge},
		{name: "a merge standing in line", arrange: func(t *testing.T, h *harness) { enqueue(t, h) }, want: ArmStillQueued},
		{name: "a branch that is gone", arrange: func(t *testing.T, h *harness) {
			rec := evicted(t, h)
			h.git.mu.Lock()
			delete(h.git.refs, "refs/heads/"+requestBranch(h.db.workspaces[theWorkspace], rec.Source))
			h.git.mu.Unlock()
		}, want: ArmBranchGone},
		{name: "a merge whose landing closed its workspace", arrange: func(t *testing.T, h *harness) {
			closeWith(h, wsm.CloseByMerge)
		}, want: ArmAlreadyLanded},
		{name: "a closed workspace", arrange: func(t *testing.T, h *harness) {
			closeWith(h, wsm.CloseByUser)
		}, want: ArmWorkspaceClosed},
		{name: "a version for a request that recorded its own", arrange: func(t *testing.T, h *harness) { evicted(t, h) },
			req: Requeue{Version: &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "another"}}, want: ArmVersionAlreadyRecorded},
		{name: "a placement behind a merge not in line", arrange: func(t *testing.T, h *harness) { evicted(t, h) },
			req: Requeue{PlaceBehind: "nobody"}, want: ArmPlacementBehindNotInLine},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)
			before, beforeKept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
			req := tt.req
			req.Workspace, req.By = theWorkspace, RequestedByUser

			// Act
			_, err := h.o.Requeue(context.Background(), req)

			// Assert: refused under its arm, the record unchanged, recorded.
			if refusedArm(err) != tt.want {
				t.Fatalf("Requeue = %v, want the %s refusal", err, tt.want)
			}
			after, afterKept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
			if beforeKept != afterKept || !reflect.DeepEqual(before, after) {
				t.Fatalf("the refusal changed the record: %+v -> %+v", before, after)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.merge.requeue"}); !ok {
				t.Fatalf("the refusal was not recorded: %v", h.logs.Records())
			}
		})
	}
}

// closeWith closes the harness workspace as BY closes it.
func closeWith(h *harness, by wsm.CloseCause) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	w := h.db.workspaces[theWorkspace]
	w.Closed, w.Closure = true, &wsm.Closure{By: by}
	h.db.workspaces[theWorkspace] = w
}

func TestRequeueOfAnAdoptedMergeOntoAVersionedMainBranch(t *testing.T) {
	tests := []struct {
		name    string
		version *wsm.VersionIncrement
		want    string
	}{
		{name: "without an increment it is refused", want: ArmVersionIncrementRequired},
		{name: "with one it lands as that increment", version: &wsm.VersionIncrement{Part: semver.Minor, ChangelogLine: "Adopted merges land"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: an adopted record of the harness's branch on agent-repl's
			// versioned master, asked by an agent.
			h := newHarness(t)
			h.emacsRepo()
			branch := h.db.workspaces[theWorkspace].Branch
			branchExists(h, branch)
			h.db.mu.Lock()
			h.db.offQueue[theWorkspace] = wsm.OffQueueMerge{Workspace: theWorkspace, Cause: wsm.MergeLeftAdopted, Adopted: true,
				Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Branch: branch}}
			h.db.mu.Unlock()

			// Act
			_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByAgent, Asker: theWorkspace, Version: tt.version})

			// Assert
			if refusedArm(err) != tt.want {
				t.Fatalf("Requeue = %v, want arm %q", err, tt.want)
			}
			if tt.want != "" {
				return
			}
			entry, ok := h.entryOf(theWorkspace)
			if !ok || !reflect.DeepEqual(entry.Source.Version, tt.version) {
				t.Fatalf("queue entry = %+v (%v), want the supplied increment", entry, ok)
			}
		})
	}
}

func TestRequeueWhoseRecordCannotBeReadIsAnErrorAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.offQueueErr = errors.New("the state database is unreadable")

	// Act
	_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser})

	// Assert
	if err == nil || refusedArm(err) != "" {
		t.Fatalf("Requeue = %v, want the read's error", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.requeue",
		Message: "could not read the merge's request off its queue"}); !ok {
		t.Fatalf("no ERROR record: %v", h.logs.Records())
	}
}

func TestDiscardOfAWorkspacesOwnMergeDropsOnlyTheRequest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	evicted(t, h)
	closed := false

	// Act
	discarded, err := h.o.Discard(context.Background(), theWorkspace, func(context.Context) error { closed = true; return nil })

	// Assert
	if err != nil || discarded.Async {
		t.Fatalf("Discard = %+v, %v; want a workspace's own merge discarded", discarded, err)
	}
	if closed || h.db.closed[theWorkspace] {
		t.Fatal("discarding a workspace's own merge closed the workspace")
	}
	if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); kept {
		t.Fatal("the discard left the request standing")
	}
}

func TestDiscardOfAnAsyncMergeClosesItsMergeWorkspace(t *testing.T) {
	tests := []struct {
		name        string
		clean       bool
		wantRemoved bool
	}{
		{name: "its clean cleanup worktree is removed", clean: true, wantRemoved: true},
		{name: "its dirty cleanup worktree is kept", clean: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: an evicted async merge whose request cleans up a worktree.
			h := newHarness(t)
			if err := requestDetached(t, h, RequestedByUser); err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
				t.Fatalf("Evict: %v", err)
			}
			cleanup := t.TempDir()
			h.db.mu.Lock()
			w := h.db.workspaces[theMergeWorkspace]
			w.MergeOf.CleanupWorktree, w.MergeOf.CleanupDir = true, cleanup
			h.db.workspaces[theMergeWorkspace] = w
			h.db.mu.Unlock()
			h.git.mu.Lock()
			h.git.cleanByDir = map[string]bool{cleanup: tt.clean}
			h.git.mu.Unlock()
			closes := 0
			close := func(ctx context.Context) error {
				closes++
				return h.db.MarkClosed(ctx, theMergeWorkspace, wsm.Closure{By: wsm.CloseByUser})
			}

			// Act
			discarded, err := h.o.Discard(context.Background(), theMergeWorkspace, close)

			// Assert
			if err != nil || !discarded.Async || discarded.CleanupWorktreeRemoved != tt.wantRemoved {
				t.Fatalf("Discard = %+v, %v; want async, removed %v", discarded, err, tt.wantRemoved)
			}
			if closes != 1 {
				t.Fatalf("the merge workspace was closed %d times, want once", closes)
			}
			if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theMergeWorkspace); kept {
				t.Fatal("the close left the request standing")
			}
			if removed := slices.Contains(h.git.cleanedWorktrees, cleanup); removed != tt.wantRemoved {
				t.Fatalf("cleanup worktree removed %v, want %v", removed, tt.wantRemoved)
			}
		})
	}
}

func TestDiscardRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    string
	}{
		{name: "a workspace whose merge holds no request", arrange: func(*testing.T, *harness) {}, want: ArmUnknownMerge},
		{name: "a merge standing in line", arrange: func(t *testing.T, h *harness) { enqueue(t, h) }, want: ArmStillQueued},
		{name: "a closed workspace", arrange: func(t *testing.T, h *harness) { closeWith(h, wsm.CloseByUser) }, want: ArmUnknownMerge},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)

			// Act
			_, err := h.o.Discard(context.Background(), theWorkspace, func(context.Context) error {
				t.Fatal("a refused discard closed the workspace")
				return nil
			})

			// Assert
			if refusedArm(err) != tt.want {
				t.Fatalf("Discard = %v, want the %s refusal", err, tt.want)
			}
		})
	}
}

func TestADiscardWhoseCloseFailsKeepsTheRequest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Act
	_, err := h.o.Discard(context.Background(), theMergeWorkspace, func(context.Context) error { return errors.New("the shim would not stand down") })

	// Assert
	if err == nil {
		t.Fatal("a discard whose close failed succeeded")
	}
	if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theMergeWorkspace); !kept {
		t.Fatal("a failed discard lost the request")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.discard",
		Message: "could not close the discarded async merge's merge workspace; the request stands"}); !ok {
		t.Fatalf("no ERROR record: %v", h.logs.Records())
	}
}

func TestAnAsyncMergeUnderANameAMergeOffItsQueueHoldsIsRefused(t *testing.T) {
	// Arrange: the async merge evicted, its merge workspace open under its name.
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	name := h.db.workspaces[theMergeWorkspace].Name

	// Act
	_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser,
		Detached: &DetachedRun{Maker: theWorkspace, Name: name}})

	// Assert: refused naming the merge, so the caller requeues it.
	refusal, ok := Refused(err)
	if !ok || refusal.Arm != ArmNameOffQueue || refusal.Fields["name"] != name || refusal.Fields["workspace"] == nil || refusal.Fields["cause"] == nil {
		t.Fatalf("Enqueue = %v, want the %s refusal naming %q", err, ArmNameOffQueue, name)
	}
}

func TestStatusListsTheMergesOffTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	evicted(t, h)

	// Act
	statuses, err := h.o.Status(context.Background(), nil)

	// Assert
	if err != nil || len(statuses) != 1 || len(statuses[0].OffQueue) != 1 {
		t.Fatalf("Status = %+v, %v; want the one merge off the queue", statuses, err)
	}
	off := statuses[0].OffQueue[0]
	if off.Workspace != theWorkspace || off.Cause != string(CauseUserDrop) || off.Requester != "" {
		t.Fatalf("off-queue member = %+v, want the evicted own merge", off)
	}
}

func TestTheSidebarIsToldTheMergesOffTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert: the async merge credited to its requester, in the sidebar's words.
	want := []sidebar.OffQueueEntry{{Workspace: theMergeWorkspace, Credited: theWorkspace, Async: true, Cause: "evicted"}}
	if got := h.sidebar.offQueueTold(); !reflect.DeepEqual(got, want) {
		t.Fatalf("sidebar told %+v, want %+v", got, want)
	}
}

// TestEveryOffQueueCauseHasItsArmAndItsWords pins the two maps to one set of
// causes, so no cause is listed on the wire without words for the sidebar,
// or the reverse.
func TestEveryOffQueueCauseHasItsArmAndItsWords(t *testing.T) {
	// Arrange
	var arms, words []string
	for cause := range offQueueCauses {
		arms = append(arms, cause)
	}
	for cause := range offQueueWords {
		words = append(words, cause)
	}
	slices.Sort(arms)
	slices.Sort(words)

	// Act and Assert
	if !slices.Equal(arms, words) {
		t.Fatalf("causes with an arm %v, causes with words %v; want the same set", arms, words)
	}
	for _, cause := range arms {
		if OffQueueCause(cause).GetCause() == nil {
			t.Errorf("cause %q maps to no arm", cause)
		}
	}
}

func TestAnUndeclaredOffQueueCausePanics(t *testing.T) {
	for _, tt := range []struct {
		name string
		call func()
	}{
		{name: "its arm", call: func() { OffQueueCause("no_such_cause") }},
		{name: "its words", call: func() { OffQueueWords("no_such_cause") }},
	} {
		t.Run(tt.name, func(t *testing.T) {
			defer func() {
				if recover() == nil {
					t.Fatal("an undeclared cause did not panic")
				}
			}()
			tt.call()
		})
	}
}


// exitCauses names every cause constant this package declares, by its Go
// name, with its value: the table TestEveryExitCauseKeepsOrEndsItsRequest
// classifies.
var exitCauses = map[string]string{
	"CauseUserDrop":           string(CauseUserDrop),
	"CauseUserDequeue":        string(CauseUserDequeue),
	"CauseWorkspaceClosed":    string(CauseWorkspaceClosed),
	"CauseDaemonShutdown":     string(CauseDaemonShutdown),
	"CauseAheadDidNotLand":    string(CauseAheadDidNotLand),
	"causeMerged":             causeMerged,
	"causeFailed":             causeFailed,
	"causeRestartFailed":      causeRestartFailed,
	"causeRestartResume":      causeRestartResume,
	"causePickFailed":         causePickFailed,
	"causeDetachedForkFailed": causeDetachedForkFailed,
	"causeFinishFailed":       causeFinishFailed,
	"causeHiddenAtColdGate":   causeHiddenAtColdGate,
}

// notQueueExits are the causes that never take a queue row off: an async
// request dropped before it was handed to a merge workspace
// (wsm.DropAsyncMergeRequest) stood in no queue.
var notQueueExits = map[string]bool{causeDetachedForkFailed: true}

// declaredCauseNames reads, from the package's production sources, the name
// of every constant that is a cause: typed AbandonCause, or named cause*.
func declaredCauseNames(t *testing.T) []string {
	t.Helper()
	var names []string
	for _, file := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(token.NewFileSet(), file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			gen, ok := decl.(*ast.GenDecl)
			if !ok || gen.Tok != token.CONST {
				continue
			}
			for _, spec := range gen.Specs {
				value := spec.(*ast.ValueSpec)
				typed := false
				if ident, ok := value.Type.(*ast.Ident); ok && ident.Name == "AbandonCause" {
					typed = true
				}
				for _, name := range value.Names {
					if typed || (strings.HasPrefix(name.Name, "cause") && len(name.Name) > len("cause")) {
						names = append(names, name.Name)
					}
				}
			}
		}
	}
	slices.Sort(names)
	return names
}

// TestEveryExitCauseKeepsOrEndsItsRequest holds every cause a merge leaves its
// queue for to exactly one fate: it ends the request (wsm.EndsRequest), or it
// keeps it off the queue under a declared arm and words (OffQueueCause,
// OffQueueWords), or it is no queue exit at all. A cause with no fate would
// reach OffQueueWords undeclared and panic the daemon.
func TestEveryExitCauseKeepsOrEndsItsRequest(t *testing.T) {
	// Arrange
	var tabled []string
	for name := range exitCauses {
		tabled = append(tabled, name)
	}
	slices.Sort(tabled)

	// Act
	declared := declaredCauseNames(t)

	// Assert: the table names every declared cause, and each has one fate.
	if !slices.Equal(declared, tabled) {
		t.Fatalf("declared causes %v, classified causes %v; want the same set", declared, tabled)
	}
	for name, cause := range exitCauses {
		_, kept := offQueueCauses[cause]
		fates := 0
		for _, fate := range []bool{wsm.EndsRequest(cause), kept, notQueueExits[cause]} {
			if fate {
				fates++
			}
		}
		if fates != 1 {
			t.Errorf("%s (%q): ends %v, kept %v, no exit %v; want exactly one fate", name, cause, wsm.EndsRequest(cause), kept, notQueueExits[cause])
		}
	}
}

func TestARunningMergeWhoseWorkspaceClosedKeepsNoRequestAndPanicsNothing(t *testing.T) {
	// Arrange and Act: the merge, held inside its gate, abandoned because its
	// workspace closed, with its workspace not yet marked closed.
	h := ended(t, phaseRunning, resolutions[2])

	// Assert
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("the abandoned merge is still queued")
	}
	if rec, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); kept {
		t.Fatalf("a merge whose workspace closed kept a request: %+v", rec)
	}
}
