package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/pullrefs"
	"agent-repld/internal/wsm"
)

func TestBindPullTarget(t *testing.T) {
	gitRecord := wsm.Workspace{ID: "w1", Repo: "repo-1", Dir: "/work/repos/explanation-engine-feature"}
	cases := []struct {
		name      string
		owners    pullrefs.Owners
		origin    string
		originErr error
		record    wsm.Workspace
		want      *pullrefs.Target
		wantLog   string
	}{
		{name: "an owned account links into the repository its origin names",
			owners: pullrefs.Owners{"/config": "ChessCom"}, origin: "git@github.com:ChessCom/explanation-engine.git", record: gitRecord,
			want: &pullrefs.Target{Owner: "ChessCom", Repo: "explanation-engine"}},
		{name: "the origin's name wins over the checkout directory's",
			owners: pullrefs.Owners{"/config": "dwcoates"}, origin: "https://github.com/dwcoates/doom-emacs-config.git", record: gitRecord,
			want: &pullrefs.Target{Owner: "dwcoates", Repo: "doom-emacs-config"}},
		{name: "an origin off github.com links nowhere",
			owners: pullrefs.Owners{"/config": "ChessCom"}, origin: "git@gitlab.com:x/y.git", record: gitRecord,
			wantLog: "the repository's origin is not on github.com; its references link nowhere"},
		{name: "a repository with no origin links nowhere",
			owners: pullrefs.Owners{"/config": "ChessCom"}, origin: "", record: gitRecord,
			wantLog: "the repository has no origin remote; its references link nowhere"},
		{name: "an unreadable origin links nowhere",
			owners: pullrefs.Owners{"/config": "ChessCom"}, originErr: errors.New("No such remote 'origin'"), record: gitRecord,
			wantLog: "could not read the repository's origin remote; its references link nowhere"},
		{name: "an account with no owner links nowhere", owners: pullrefs.Owners{}, record: gitRecord},
		{name: "a thread links nowhere", owners: pullrefs.Owners{"/config": "ChessCom"},
			record: wsm.Workspace{ID: "w1", Kind: wsm.WorkspaceKindThread, Dir: "/work/zettel/thread"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			for dir, owner := range tc.owners {
				f.pullOwners[dir] = owner
			}
			f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Dir: "/work/repos/explanation-engine"})
			f.git.origins = map[string]string{"/work/repos/explanation-engine": tc.origin}
			f.git.originErr = tc.originErr

			// Act.
			err := f.verbs.(*verbs).bindPullTarget(context.Background(), f.log.Global(), tc.record)

			// Assert.
			if err != nil {
				t.Fatalf("bindPullTarget: %v", err)
			}
			got, installed := f.feed.pullTargets["w1"]
			if !installed {
				t.Fatal("no pull request target was installed")
			}
			if (got == nil) != (tc.want == nil) || got != nil && *got != *tc.want {
				t.Fatalf("installed target = %v, want %v", got, tc.want)
			}
			if tc.wantLog != "" && !loggedPullTarget(f, tc.wantLog) {
				t.Fatalf("no %s record %q; records = %+v", opPullTarget, tc.wantLog, f.log.logger.Records())
			}
		})
	}
}

// loggedPullTarget reports whether the canonical log holds the bind's record
// MESSAGE naming the workspace.
func loggedPullTarget(f *fixture, message string) bool {
	for _, r := range f.log.logger.Records() {
		if r.Operation == opPullTarget && r.Message == message && r.Context["workspace"] == "w1" {
			return true
		}
	}
	return false
}

func TestBindPullTargetFailsOnAnUnknownRepository(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	record := wsm.Workspace{ID: "w1", Repo: "repo-missing", Dir: "/work/repos/x"}

	// Act.
	err := f.verbs.(*verbs).bindPullTarget(context.Background(), f.log.Global(), record)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "is not in the registry") {
		t.Fatalf("err = %v, want the missing repository named", err)
	}
	assertPullTargetError(t, f, "the workspace's repository is not in the registry", "repository", "repo-missing")
	if _, installed := f.feed.pullTargets["w1"]; installed {
		t.Fatal("a target was installed for a workspace whose repository is unknown")
	}
}

func TestBindPullTargetFailsWhenTheRepositoriesCannotBeListed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.listRepositoriesErr = errors.New("disk on fire")
	record := wsm.Workspace{ID: "w1", Repo: "repo-1", Dir: "/work/repos/x"}

	// Act.
	err := f.verbs.(*verbs).bindPullTarget(context.Background(), f.log.Global(), record)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "disk on fire") {
		t.Fatalf("err = %v, want the listing's cause", err)
	}
	assertPullTargetError(t, f, "could not list the repositories to bind the workspace's pull request target", "cause", "disk on fire")
	if _, installed := f.feed.pullTargets["w1"]; installed {
		t.Fatal("a target was installed though the repositories could not be listed")
	}
}

// assertPullTargetError fails unless the canonical log holds the bind's error
// record MESSAGE, naming the workspace and KEY as VALUE.
func assertPullTargetError(t *testing.T, f *fixture, message, key, value string) {
	t.Helper()
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" && r.Operation == opPullTarget && r.Message == message &&
			r.Context["workspace"] == "w1" && r.Context[key] == value {
			return
		}
	}
	t.Fatalf("no %s error record %q with %s=%s; records = %+v", opPullTarget, message, key, value, f.log.logger.Records())
}

func TestRegisterBindsThePullTarget(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.pullOwners["/config"] = "ChessCom"

	// Act.
	record, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	got := f.feed.pullTargets[record.ID]
	if got == nil || got.Owner != "ChessCom" {
		t.Fatalf("installed target = %v, want one under ChessCom", got)
	}
}
