package topbar

import (
	"testing"

	"agent-repld/internal/ids"
)

// requestOf is the sign-in request the workspace's last view carries, "" for
// none.
func requestOf(t *testing.T, h *harness, ws ids.WorkspaceID) string {
	t.Helper()
	got, ok := h.r.Topic(ws).Latest()
	if !ok {
		t.Fatalf("no view published for %s", ws)
	}
	return got.GetAccount().GetSignInRequest().GetRequestId()
}

func TestASignInRequestRidesTheStripOfTheAccountThatNeedsIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.RequestSignIn(testWS)

	// Assert.
	if requestOf(t, h, testWS) == "" {
		t.Fatal("no sign-in request on the strip, want one")
	}
}

func TestASignInRequestRidesEveryStripOfTheSameAccountAndNoOther(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	const sibling, stranger ids.WorkspaceID = "sibling", "stranger"
	for _, ws := range []ids.WorkspaceID{sibling, stranger} {
		if err := h.r.SetWorkspaceDir(ws, t.TempDir()); err != nil {
			t.Fatalf("SetWorkspaceDir: %v", err)
		}
	}
	h.r.SetAccount(sibling, testAccount("dev@example.com"))
	other := testAccount("other@example.com")
	other.ConfigDir = "/Users/dev/.claude-other"
	h.r.SetAccount(stranger, other)

	// Act.
	h.r.RequestSignIn(testWS)

	// Assert.
	h.r.mu.Lock()
	sib, str := h.r.account(h.r.states[sibling]), h.r.account(h.r.states[stranger])
	h.r.mu.Unlock()
	if sib.GetSignInRequest() == nil || str.GetSignInRequest() != nil {
		t.Fatalf("sibling request %v, stranger request %v, want the sibling's alone", sib.GetSignInRequest(), str.GetSignInRequest())
	}
}

func TestEachDeterminationIsANewSignInRequest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.RequestSignIn(testWS)
	first := requestOf(t, h, testWS)

	// Act.
	h.r.RequestSignIn(testWS)

	// Assert.
	if second := requestOf(t, h, testWS); second == "" || second == first {
		t.Fatalf("requests %q then %q, want two different ones", first, second)
	}
}

func TestAnAnsweredSignInRequestLeavesTheStrip(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.RequestSignIn(testWS)

	// Act.
	h.r.SignInAnswered("/Users/dev/.claude")

	// Assert.
	if got := requestOf(t, h, testWS); got != "" {
		t.Fatalf("request %q, want none once answered", got)
	}
}

func TestAnAnswerForAnotherAccountLeavesTheRequest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.RequestSignIn(testWS)

	// Act.
	h.r.SignInAnswered("/Users/dev/.claude-other")

	// Assert.
	if requestOf(t, h, testWS) == "" {
		t.Fatal("no request, want it standing for its own account")
	}
}

func TestASignInRequestForAWorkspaceWithNoAccountIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.r.RequestSignIn(testWS)

	// Assert.
	for _, r := range h.log.Records() {
		if r.Level == "error" && r.Operation == "daemon.topbar.request_sign_in" {
			return
		}
	}
	t.Fatalf("no ERROR record for an account-less request: %v", h.log.Records())
}
