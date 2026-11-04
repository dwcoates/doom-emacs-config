package landedbranch

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
)

const (
	testOp   = "daemon.test.retire"
	testRepo = "/repo"
	testHead = "c0ffee"
)

// fakeGit answers Retire's questions from its fields and records deletions.
// No real git runs in these tests.
type fakeGit struct {
	exists      bool
	existsErr   error
	worktrees   []gitclient.Worktree
	listErr     error
	resolveErr  error
	ancestor    bool
	ancestorErr error
	deleteErr   error

	deleted       []string
	ancestorAsked [2]string
	ancestorCalls int
}

func (g *fakeGit) BranchExists(context.Context, string, string) (bool, error) {
	return g.exists, g.existsErr
}

func (g *fakeGit) ResolveRef(context.Context, string, string) (string, error) {
	return testHead, g.resolveErr
}

func (g *fakeGit) IsAncestor(_ context.Context, _, ancestor, descendant string) (bool, error) {
	g.ancestorAsked = [2]string{ancestor, descendant}
	g.ancestorCalls++
	return g.ancestor, g.ancestorErr
}

func (g *fakeGit) ListWorktrees(context.Context, string) ([]gitclient.Worktree, error) {
	return g.worktrees, g.listErr
}

func (g *fakeGit) DeleteBranchAt(_ context.Context, _, branch, head string) error {
	if g.deleteErr != nil {
		return g.deleteErr
	}
	g.deleted = append(g.deleted, branch+"@"+head)
	return nil
}

func TestRetire(t *testing.T) {
	boom := errors.New("boom")
	cases := []struct {
		name        string
		git         fakeGit
		branch      string
		landing     Landing
		want        Outcome
		wantErr     bool
		wantDeleted []string
		wantLevel   string
		wantMessage string
		// wantJudgedBy is the deleted record's judged_by, empty for a kept
		// branch or an error.
		wantJudgedBy string
		// wantAncestryAsked is whether git was asked the ancestry question.
		wantAncestryAsked bool
	}{
		{
			name:              "a merged branch with no recorded head is deleted at its judged tip by ancestry",
			git:               fakeGit{exists: true, ancestor: true},
			branch:            "ramdisk-leak-investigation",
			landing:           Landing{Into: "master"},
			want:              Deleted,
			wantDeleted:       []string{"ramdisk-leak-investigation@" + testHead},
			wantLevel:         "info",
			wantMessage:       "deleted the landed branch",
			wantJudgedBy:      "ancestry",
			wantAncestryAsked: true,
		},
		{
			name:         "a rebased landing whose tip is the recorded landed head is deleted though no tip is an ancestor",
			git:          fakeGit{exists: true, ancestor: false},
			branch:       "ramdisk-leak-investigation",
			landing:      Landing{Into: "master", Head: testHead},
			want:         Deleted,
			wantDeleted:  []string{"ramdisk-leak-investigation@" + testHead},
			wantLevel:    "info",
			wantMessage:  "deleted the landed branch",
			wantJudgedBy: "recorded_head",
		},
		{
			name:              "a tip that moved past the recorded landed head and did not land is kept",
			git:               fakeGit{exists: true, ancestor: false},
			branch:            "moved-on",
			landing:           Landing{Into: "master", Head: "0ldhead"},
			want:              KeptNotLanded,
			wantLevel:         "info",
			wantMessage:       "kept the branch: its tip is neither the head its landing recorded nor inside the ref it landed into",
			wantAncestryAsked: true,
		},
		{
			name:              "a tip that moved past the recorded landed head but is inside the target is deleted by ancestry",
			git:               fakeGit{exists: true, ancestor: true},
			branch:            "moved-and-landed",
			landing:           Landing{Into: "master", Head: "0ldhead"},
			want:              Deleted,
			wantDeleted:       []string{"moved-and-landed@" + testHead},
			wantLevel:         "info",
			wantMessage:       "deleted the landed branch",
			wantJudgedBy:      "ancestry",
			wantAncestryAsked: true,
		},
		{
			name:        "a checked-out branch whose tip is the recorded landed head is kept",
			git:         fakeGit{exists: true, worktrees: []gitclient.Worktree{{Dir: "/wt", Branch: "busy-landed"}}},
			branch:      "busy-landed",
			landing:     Landing{Into: "master", Head: testHead},
			want:        KeptCheckedOut,
			wantLevel:   "info",
			wantMessage: "kept the landed branch: a worktree has it checked out",
		},
		{
			name:        "an absent branch with a recorded landed head is left alone",
			git:         fakeGit{exists: false},
			branch:      "gone-landed",
			landing:     Landing{Into: "master", Head: testHead},
			want:        KeptAbsent,
			wantLevel:   "debug",
			wantMessage: "the landed branch is already gone",
		},
		{
			name:        "a branch that is already gone is left alone",
			git:         fakeGit{exists: false},
			branch:      "gone",
			landing:     Landing{Into: "master"},
			want:        KeptAbsent,
			wantLevel:   "debug",
			wantMessage: "the landed branch is already gone",
		},
		{
			name:        "a checked-out branch is kept",
			git:         fakeGit{exists: true, ancestor: true, worktrees: []gitclient.Worktree{{Dir: "/wt", Branch: "busy"}}},
			branch:      "busy",
			landing:     Landing{Into: "master"},
			want:        KeptCheckedOut,
			wantLevel:   "info",
			wantMessage: "kept the landed branch: a worktree has it checked out",
		},
		{
			name:              "an unmerged branch with no recorded head is kept",
			git:               fakeGit{exists: true, ancestor: false},
			branch:            "ramdisk-leak-investigation-vdy",
			landing:           Landing{Into: "master"},
			want:              KeptNotLanded,
			wantLevel:         "info",
			wantMessage:       "kept the branch: its tip is neither the head its landing recorded nor inside the ref it landed into",
			wantAncestryAsked: true,
		},
		{
			name:        "a probe that fails is an error and keeps the branch",
			git:         fakeGit{existsErr: boom},
			branch:      "b",
			landing:     Landing{Into: "master"},
			wantErr:     true,
			wantLevel:   "error",
			wantMessage: "could not retire the landed branch: probe the branch; the branch is kept",
		},
		{
			name:        "a worktree list that fails is an error and keeps the branch",
			git:         fakeGit{exists: true, listErr: boom},
			branch:      "b",
			landing:     Landing{Into: "master"},
			wantErr:     true,
			wantLevel:   "error",
			wantMessage: "could not retire the landed branch: list the worktrees; the branch is kept",
		},
		{
			name:        "a resolve that fails is an error and keeps the branch",
			git:         fakeGit{exists: true, resolveErr: boom},
			branch:      "b",
			landing:     Landing{Into: "master"},
			wantErr:     true,
			wantLevel:   "error",
			wantMessage: "could not retire the landed branch: resolve the branch; the branch is kept",
		},
		{
			name:              "an ancestry read that fails is an error and keeps the branch",
			wantAncestryAsked: true,
			git:               fakeGit{exists: true, ancestorErr: boom},
			branch:            "b",
			landing:           Landing{Into: "master"},
			wantErr:           true,
			wantLevel:         "error",
			wantMessage:       "could not retire the landed branch: read whether the branch landed; the branch is kept",
		},
		{
			name:              "a compare-and-delete git refuses is an error and keeps the branch",
			wantAncestryAsked: true,
			git:               fakeGit{exists: true, ancestor: true, deleteErr: boom},
			branch:            "moved",
			landing:           Landing{Into: "master"},
			wantErr:           true,
			wantLevel:         "error",
			wantMessage:       "could not retire the landed branch: delete the branch at the judged head; the branch is kept",
		},
		{
			name:        "a request without a ref to land into is refused",
			git:         fakeGit{exists: true, ancestor: true},
			branch:      "b",
			landing:     Landing{},
			wantErr:     true,
			wantLevel:   "error",
			wantMessage: "could not retire the landed branch: check the request; the branch is kept",
		},
		{
			name:        "a branch that is its own landing ref is refused",
			git:         fakeGit{exists: true, ancestor: true},
			branch:      "master",
			landing:     Landing{Into: "master", Head: testHead},
			wantErr:     true,
			wantLevel:   "error",
			wantMessage: "could not retire the landed branch: check the request; the branch is kept",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			git := tc.git
			log := dlog.NewTestLogger()

			// Act
			got, err := Retire(context.Background(), &git, log, testOp, testRepo, tc.branch, tc.landing, dlog.Context{"workspace": "w1"})

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("err = %v, wantErr %v", err, tc.wantErr)
			}
			if !tc.wantErr && got != tc.want {
				t.Fatalf("outcome = %q, want %q", got, tc.want)
			}
			if len(git.deleted) != len(tc.wantDeleted) {
				t.Fatalf("deleted = %v, want %v", git.deleted, tc.wantDeleted)
			}
			for i := range tc.wantDeleted {
				if git.deleted[i] != tc.wantDeleted[i] {
					t.Fatalf("deleted = %v, want %v", git.deleted, tc.wantDeleted)
				}
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: tc.wantLevel, Operation: testOp, Message: tc.wantMessage})
			if !ok {
				t.Fatalf("no %s record %q; records: %+v", tc.wantLevel, tc.wantMessage, log.Records())
			}
			if rec.Context["branch"] != tc.branch || rec.Context["workspace"] != "w1" || rec.Context["repo"] != testRepo || rec.Context["landed_head"] != tc.landing.Head {
				t.Fatalf("record context = %v, want branch %q, workspace w1, repo %q, landed_head %q", rec.Context, tc.branch, testRepo, tc.landing.Head)
			}
			if tc.wantJudgedBy != "" && rec.Context["judged_by"] != tc.wantJudgedBy {
				t.Fatalf("judged_by = %v, want %q", rec.Context["judged_by"], tc.wantJudgedBy)
			}
			if asked := git.ancestorCalls > 0; asked != tc.wantAncestryAsked {
				t.Fatalf("ancestry asked = %v, want %v", asked, tc.wantAncestryAsked)
			}
		})
	}
}

func TestRetireAsksWhetherTheTipIsInsideTheLandingRef(t *testing.T) {
	// Arrange
	git := fakeGit{exists: true, ancestor: true}

	// Act
	if _, err := Retire(context.Background(), &git, dlog.NewTestLogger(), testOp, testRepo, "b", Landing{Into: "parent-branch"}, nil); err != nil {
		t.Fatalf("Retire: %v", err)
	}

	// Assert
	if git.ancestorAsked != [2]string{testHead, "parent-branch"} {
		t.Fatalf("IsAncestor asked %v, want [%s parent-branch]", git.ancestorAsked, testHead)
	}
}
