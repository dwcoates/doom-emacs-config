package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/gitclient"
	"agent-repld/internal/wsm"
)

var (
	testIdentity    = gitclient.Identity{Name: "Test", Email: "test@example.com"}
	outsideIdentity = gitclient.Identity{Name: "Outside Contributor", Email: "oc@example.org"}
)

// identified is one commit with that author and committer.
func identified(sha string, author, committer gitclient.Identity) gitclient.CommitIdentity {
	return gitclient.CommitIdentity{SHA: sha, Subject: "work " + sha, Author: author, Committer: committer}
}

func TestIdentityRefusal(t *testing.T) {
	tests := []struct {
		name          string
		global        gitclient.Identity
		commits       []gitclient.CommitIdentity
		wantSummary   string
		wantOffending []string
	}{
		{
			name:    "every commit under the global identity lands",
			global:  fakeIdentity,
			commits: []gitclient.CommitIdentity{identified("aaa", fakeIdentity, fakeIdentity)},
		},
		{
			name:          "a wrong author is refused",
			global:        fakeIdentity,
			commits:       []gitclient.CommitIdentity{identified("aaa", testIdentity, fakeIdentity)},
			wantSummary:   `1 of 1 commits are not under the global git identity Pat Example <pat@example.com> (first: aaa "work aaa": author Test <test@example.com>)`,
			wantOffending: []string{`aaa "work aaa": author Test <test@example.com>`},
		},
		{
			name:          "a wrong committer is refused",
			global:        fakeIdentity,
			commits:       []gitclient.CommitIdentity{identified("aaa", fakeIdentity, testIdentity)},
			wantSummary:   `1 of 1 commits are not under the global git identity Pat Example <pat@example.com> (first: aaa "work aaa": committer Test <test@example.com>)`,
			wantOffending: []string{`aaa "work aaa": committer Test <test@example.com>`},
		},
		{
			name:          "a name alone differing is refused",
			global:        fakeIdentity,
			commits:       []gitclient.CommitIdentity{identified("aaa", gitclient.Identity{Name: "Claude", Email: fakeIdentity.Email}, fakeIdentity)},
			wantSummary:   `1 of 1 commits are not under the global git identity Pat Example <pat@example.com> (first: aaa "work aaa": author Claude <pat@example.com>)`,
			wantOffending: []string{`aaa "work aaa": author Claude <pat@example.com>`},
		},
		{
			// The decision: the rebase-and-merge path lands local work, so an
			// outside contributor's commit is refused like any other author.
			name:          "an outside contributor's commit is refused",
			global:        fakeIdentity,
			commits:       []gitclient.CommitIdentity{identified("aaa", outsideIdentity, fakeIdentity)},
			wantSummary:   `1 of 1 commits are not under the global git identity Pat Example <pat@example.com> (first: aaa "work aaa": author Outside Contributor <oc@example.org>)`,
			wantOffending: []string{`aaa "work aaa": author Outside Contributor <oc@example.org>`},
		},
		{
			name:   "every offending commit is listed, the first in the summary",
			global: fakeIdentity,
			commits: []gitclient.CommitIdentity{
				identified("aaa", fakeIdentity, fakeIdentity),
				identified("bbb", testIdentity, testIdentity),
				identified("ccc", fakeIdentity, testIdentity),
			},
			wantSummary: `2 of 3 commits are not under the global git identity Pat Example <pat@example.com> (first: bbb "work bbb": author Test <test@example.com>, committer Test <test@example.com>)`,
			wantOffending: []string{
				`bbb "work bbb": author Test <test@example.com>, committer Test <test@example.com>`,
				`ccc "work ccc": committer Test <test@example.com>`,
			},
		},
		{
			name:        "no global name is refused",
			global:      gitclient.Identity{Email: fakeIdentity.Email},
			commits:     []gitclient.CommitIdentity{identified("aaa", fakeIdentity, fakeIdentity)},
			wantSummary: noGlobalIdentity,
		},
		{
			name:        "no global email is refused",
			global:      gitclient.Identity{Name: fakeIdentity.Name},
			commits:     []gitclient.CommitIdentity{identified("aaa", fakeIdentity, fakeIdentity)},
			wantSummary: noGlobalIdentity,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			summary, offending := identityRefusal(tc.global, tc.commits)

			// Assert.
			if summary != tc.wantSummary || !slices.Equal(offending, tc.wantOffending) {
				t.Fatalf("identityRefusal = %q, %q; want %q, %q", summary, offending, tc.wantSummary, tc.wantOffending)
			}
		})
	}
}

func TestALandingUnderTheGlobalIdentityChecksEveryCommitToTheMergeCommit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)

	// Act.
	admitted(t, h)

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.identityRanges) != 1 || !strings.HasSuffix(h.git.identityRanges[0], "..merge0000commit") {
		t.Fatalf("identity ranges = %v, want one, from the tip to the merge commit", h.git.identityRanges)
	}
	if !slices.Equal(h.git.fastForwards, []string{h.targetD + "@merge0000commit"}) {
		t.Fatalf("fast-forwards = %v, want the target to the merge commit", h.git.fastForwards)
	}
	if h.git.at["commit_identities"] > h.git.at["fast_forward"] && h.git.at["fast_forward"] != 0 {
		t.Fatalf("the identities were read after the fast-forward: %v", h.git.calls)
	}
}

// assertRefusedLanding asserts the merge failed in area other with wantDetail
// in its account, and the target never moved.
func assertRefusedLanding(t *testing.T, h *harness, wantDetail string) {
	t.Helper()
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" || !strings.Contains(got.Detail, wantDetail) {
		t.Fatalf("footer = %+v, want failed in area other carrying %q", got, wantDetail)
	}
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.fastForwards) != 0 {
		t.Fatalf("a refused landing moved the target: %v", h.git.fastForwards)
	}
}

func TestALandingCarryingAnotherIdentityFailsAndLeavesTheTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.identities = []gitclient.CommitIdentity{
		identified("aaa0000000000", testIdentity, fakeIdentity),
		identified("merge0000commit", fakeIdentity, fakeIdentity),
	}

	// Act.
	admitted(t, h)

	// Assert.
	assertRefusedLanding(t, h, "1 of 2 commits are not under the global git identity Pat Example <pat@example.com>")
	record, logged := h.recordFor("error", "daemon.merge.identity")
	if !logged || record.Context["global_identity"] != "Pat Example <pat@example.com>" ||
		!slices.Equal(record.Context["offending"].([]string), []string{`aaa000000000 "work aaa0000000000": author Test <test@example.com>`}) ||
		record.Context["commit"] != "merge0000commit" {
		t.Fatalf("refusal record = %+v (logged %v), want an ERROR naming the global identity, the offending commit and the merge commit", record, logged)
	}
}

func TestALandingWithNoGlobalIdentityFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.globalIdentity = gitclient.Identity{}

	// Act.
	admitted(t, h)

	// Assert.
	assertRefusedLanding(t, h, noGlobalIdentity)
	if _, logged := h.recordFor("error", "daemon.merge.identity"); !logged {
		t.Fatalf("no ERROR record of the refusal")
	}
}

func TestALandingWhoseGlobalIdentityCannotBeReadFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.globalIdentityErr = errors.New("fatal: bad config line 1")

	// Act.
	admitted(t, h)

	// Assert.
	assertRefusedLanding(t, h, "fatal: bad config line 1")
}

func TestALandingWhoseCommitIdentitiesCannotBeReadFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.identitiesErr = errors.New("fatal: bad revision")

	// Act.
	admitted(t, h)

	// Assert.
	assertRefusedLanding(t, h, "fatal: bad revision")
}

func TestALandingWhoseMergeCommitBringsInNothingFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.identities = []gitclient.CommitIdentity{}

	// Act.
	admitted(t, h)

	// Assert.
	assertRefusedLanding(t, h, "brings in no commit")
	if _, logged := h.recordFor("error", "daemon.merge.identity"); !logged {
		t.Fatalf("no ERROR record of the empty range")
	}
}

func TestABranchMergedUpstreamIsNotIdentityChecked(t *testing.T) {
	// Arrange: the remote recorded these identities, not this machine.
	h := newHarness(t)
	mergedUpstream(h)
	h.git.globalIdentity = gitclient.Identity{}

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert.
	if h.git.seen("global_identity") || h.git.seen("commit_identities") {
		t.Fatalf("the upstream path read identities: %v", h.git.calls)
	}
	if !slices.Equal(h.git.fastForwards, []string{h.sourceD + "@upstream00000"}) {
		t.Fatalf("fast-forwards = %v, want the default branch to upstream", h.git.fastForwards)
	}
}
