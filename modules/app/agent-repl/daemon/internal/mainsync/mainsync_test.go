package mainsync

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/wsm"
)

// GIT IS NEVER CALLED: fakeGit answers from a script and records each
// fast-forward.

const repoID ids.RepoID = "repo-1"

var t0 = time.Date(2026, 10, 30, 12, 0, 0, 0, time.UTC)

type fakeStore struct {
	main wsm.Workspace
	told map[ids.RepoID]wsm.MainSyncTold
	noWS bool
}

func (f *fakeStore) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return []wsm.Repository{{ID: repoID, Dir: "/repos/main"}}, nil
}

func (f *fakeStore) WorkspaceByDir(context.Context, string) (wsm.Workspace, error) {
	if f.noWS {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return f.main, nil
}

func (f *fakeStore) MainSyncTold(_ context.Context, repo ids.RepoID) (wsm.MainSyncTold, bool, error) {
	t, ok := f.told[repo]
	return t, ok, nil
}

func (f *fakeStore) RecordMainSyncTold(_ context.Context, repo ids.RepoID, t wsm.MainSyncTold) error {
	f.told[repo] = t
	return nil
}

type fakeGit struct {
	ffErr error
	ffs   []string
}

func (g *fakeGit) ResolveRef(_ context.Context, _, ref string) (string, error) {
	return "sha-of-" + ref, nil
}

func (g *fakeGit) FastForward(_ context.Context, dir, commit string) error {
	g.ffs = append(g.ffs, dir+" "+commit)
	return g.ffErr
}

type fakeTarget struct {
	busy     bool
	released int
}

func (t *fakeTarget) HoldTargetForSync(context.Context, string) (func(), bool, error) {
	if t.busy {
		return nil, false, nil
	}
	return func() { t.released++ }, true, nil
}

type fakeTurns struct{ held []string }

func (f *fakeTurns) Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error {
	f.held = append(f.held, string(ws)+" "+what)
	return perform(ctx)
}

type fakePrompts struct {
	got    []string
	origin conversationv1.PromptOrigin
}

func (p *fakePrompts) Submit(_ context.Context, _ ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, _ wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	p.got = append(p.got, said.GetContent().GetBlocks()[0].GetText().GetText())
	p.origin = origin
	return prompthandler.Outcome{Turn: "turn-1"}, nil
}

type rig struct {
	s       *Syncer
	store   *fakeStore
	git     *fakeGit
	target  *fakeTarget
	turns   *fakeTurns
	prompts *fakePrompts
	log     *dlog.TestLogger
	running bool
	opened  []ids.WorkspaceID
}

func newRig(t *testing.T) *rig {
	t.Helper()
	r := &rig{store: &fakeStore{main: wsm.Workspace{ID: "main-ws", Dir: "/repos/main"}, told: map[ids.RepoID]wsm.MainSyncTold{}},
		git: &fakeGit{}, target: &fakeTarget{}, turns: &fakeTurns{}, prompts: &fakePrompts{}, log: dlog.NewTestLogger()}
	var err error
	r.s, err = New(Deps{Store: r.store, Git: r.git, Target: r.target, Turns: r.turns,
		Running: func(context.Context, ids.WorkspaceID) (bool, error) { return r.running, nil },
		Open:    func(_ context.Context, ws ids.WorkspaceID) error { r.opened = append(r.opened, ws); return nil },
		Briefs: func(name string, v map[string]string) (string, error) {
			return name + "|" + v["branch"] + "|" + v["upstream_sha"] + "|" + v["cause"], nil
		},
		MintKey: func(p string) (string, error) { return p, nil }, Serves: func() bool { return true }, Clock: clocktest.New(t0), Log: r.log})
	if err != nil {
		t.Fatal(err)
	}
	return r
}

func behind(ahead, behind int) repodivergence.Counted {
	return repodivergence.Counted{Branch: "master", Upstream: "origin/master", Ahead: ahead, Behind: behind}
}

func (r *rig) sync(m repodivergence.Counted) { r.s.Sync(context.Background(), r.prompts, repoID, m) }

func TestABranchBehindAndNotAheadIsFastForwardedUnderBothHolds(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	r.sync(behind(0, 3))

	// Assert
	if len(r.git.ffs) != 1 || r.git.ffs[0] != "/repos/main origin/master" {
		t.Fatalf("fast-forwards = %v, want one to the upstream", r.git.ffs)
	}
	if len(r.turns.held) != 1 || r.turns.held[0] != "main-ws main sync" || r.target.released != 1 {
		t.Fatalf("turns held %v, target released %d; want both holds taken and released", r.turns.held, r.target.released)
	}
	if len(r.prompts.got) != 0 {
		t.Fatalf("told %q, want no workspace involved", r.prompts.got)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opSync, Message: "fast-forwarded the main checkout to its upstream"}); !ok {
		t.Fatalf("no record of the fast-forward: %+v", r.log.Records())
	}
}

func TestASyncWaitsForTheNextSweep(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(r *rig)
		message string
	}{
		{name: "a merge of the repository is in flight", arrange: func(r *rig) { r.target.busy = true },
			message: "a merge of the repository is in line or running; the sync waits for the next sweep"},
		{name: "the main workspace is running a turn", arrange: func(r *rig) { r.running = true },
			message: "the main workspace is running a turn; the sync waits for the next sweep"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			tt.arrange(r)

			// Act
			r.sync(behind(0, 3))

			// Assert
			if len(r.git.ffs) != 0 || len(r.prompts.got) != 0 {
				t.Fatalf("fast-forwards %v, told %q; want neither", r.git.ffs, r.prompts.got)
			}
			if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "debug", Operation: opSync, Message: tt.message}); !ok {
				t.Fatalf("no record %q: %+v", tt.message, r.log.Records())
			}
		})
	}
}

func TestAClosedMainWorkspaceIsFastForwardedWithoutItsTurnHold(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.store.main.Closed = true

	// Act
	r.sync(behind(0, 1))

	// Assert
	if len(r.git.ffs) != 1 || len(r.turns.held) != 0 {
		t.Fatalf("fast-forwards %v, turns held %v; want the fast-forward with no turn to hold", r.git.ffs, r.turns.held)
	}
}

func TestAFailedFastForwardIsToldToTheMainWorkspaceOnce(t *testing.T) {
	localChanges := &gitclient.Error{Args: []string{"merge", "--ff-only", "origin/master"}, ExitCode: 1,
		Stderr: "error: Your local changes to the following files would be overwritten by merge:\n\tconfig.el\n\tlisp/a.el\nPlease commit your changes or stash them before you merge.\nAborting\n"}
	tests := []struct {
		name  string
		m     repodivergence.Counted
		ffErr error
		want  string
	}{
		{name: "a diverged branch", m: behind(2, 3), want: "holds 2 commit(s) the upstream lacks, and lacks 3"},
		{name: "local changes in the way", m: behind(0, 3), ffErr: localChanges, want: "- `config.el`\n- `lisp/a.el`"},
		{name: "git failing otherwise", m: behind(0, 3), ffErr: &gitclient.Error{Args: []string{"merge", "--ff-only", "origin/master"}, ExitCode: 128, Stderr: "fatal: index.lock exists"},
			want: "`git merge --ff-only origin/master` exited 128:\n~~~text\nfatal: index.lock exists\n~~~"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.git.ffErr = tt.ffErr

			// Act
			r.sync(tt.m)
			r.sync(tt.m)

			// Assert
			if len(r.prompts.got) != 1 || !strings.Contains(r.prompts.got[0], tt.want) || !strings.Contains(r.prompts.got[0], "sha-of-origin/master") {
				t.Fatalf("told %q, want one repair prompt saying %q", r.prompts.got, tt.want)
			}
			if r.prompts.origin != Origin {
				t.Fatalf("origin = %v, want %v", r.prompts.origin, Origin)
			}
			if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opSync, Message: "the main checkout still cannot be fast-forwarded, as its workspace was already told"}); !ok {
				t.Fatalf("no record of the repeat left untold: %+v", r.log.Records())
			}
		})
	}
}

func TestANewUpstreamCommitIsToldAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.store.told[repoID] = wsm.MainSyncTold{UpstreamSHA: "an-older-commit", Cause: "diverged"}

	// Act
	r.sync(behind(1, 1))

	// Assert
	if len(r.prompts.got) != 1 || r.store.told[repoID].UpstreamSHA != "sha-of-origin/master" {
		t.Fatalf("told %q, record %+v; want the new commit told and recorded", r.prompts.got, r.store.told[repoID])
	}
}

func TestAClosedMainWorkspaceIsReopenedToBeTold(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.store.main.Closed = true

	// Act
	r.sync(behind(1, 1))

	// Assert
	if len(r.opened) != 1 || r.opened[0] != "main-ws" || len(r.prompts.got) != 1 {
		t.Fatalf("opened %v, told %q; want the workspace reopened and told", r.opened, r.prompts.got)
	}
}

func TestNothingIsDoneWithoutTheWorkspaceOrTheServingRole(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, r *rig)
	}{
		{name: "no workspace for the main checkout", arrange: func(_ *testing.T, r *rig) { r.store.noWS = true }},
		{name: "a daemon that does not serve", arrange: func(t *testing.T, r *rig) {
			s, err := New(Deps{Store: r.store, Git: r.git, Target: r.target, Turns: r.turns, Running: func(context.Context, ids.WorkspaceID) (bool, error) { return false, nil },
				Open: func(context.Context, ids.WorkspaceID) error { return nil }, Briefs: func(string, map[string]string) (string, error) { return "", nil },
				MintKey: func(string) (string, error) { return "", nil }, Serves: func() bool { return false }, Clock: clocktest.New(t0), Log: r.log})
			if err != nil {
				t.Fatal(err)
			}
			r.s = s
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			tt.arrange(t, r)

			// Act
			r.sync(behind(0, 2))

			// Assert
			if len(r.git.ffs) != 0 || len(r.prompts.got) != 0 {
				t.Fatalf("fast-forwards %v, told %q; want nothing", r.git.ffs, r.prompts.got)
			}
		})
	}
}

func TestObserveKeepsOnlyEachRepositorysLatestBehindMeasure(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	r.s.Observe(repoID, behind(0, 1))
	r.s.Observe(repoID, behind(0, 4))
	r.s.Observe("repo-2", behind(0, 0))
	r.s.Observe("repo-3", repodivergence.Failed{Reason: "offline"})
	got := r.s.take()

	// Assert
	if len(got) != 1 || got[0].id != repoID || got[0].measure.Behind != 4 {
		t.Fatalf("take = %+v, want only repo-1's latest", got)
	}
}

func TestOverwrittenPathsReadsBothOfGitsRefusals(t *testing.T) {
	// Act
	got := overwrittenPaths("error: The following untracked working tree files would be overwritten by merge:\n\tnew.go\nPlease move or remove them before you merge.\n")

	// Assert
	if fmt.Sprint(got) != "[new.go]" {
		t.Fatalf("overwrittenPaths = %v", got)
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	// Act
	_, err := New(Deps{})

	// Assert
	if err == nil {
		t.Fatal("New(Deps{}) = nil, want refused")
	}
}
