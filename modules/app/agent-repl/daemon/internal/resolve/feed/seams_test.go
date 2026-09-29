package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// The five Landing-3 shapes, each asserted at its ONE seam function, so a wire
// change lands in one place and is caught here.

func TestApplyHeadlineAlwaysSetsTheDaemonsSentence(t *testing.T) {
	// Arrange, Act.
	errored := &frontendv1.FeedTurnEndedErrored{}
	applyHeadline(errored, headline{Text: "rate limited by the vendor"}, "")

	// Assert: REQUIRED — the client holds no per-arm sentence table.
	if errored.GetHeadline().GetText() != "rate limited by the vendor" {
		t.Fatalf("headline = %q", errored.GetHeadline().GetText())
	}
}

func TestApplyHeadlineKeepsTheVendorsSentenceBesideOurs(t *testing.T) {
	// Arrange, Act.
	errored := &frontendv1.FeedTurnEndedErrored{}
	applyHeadline(errored, headline{Text: "ours"}, "theirs")

	// Assert: only one of the two is ours, so neither is folded into the other.
	if errored.GetHeadline().GetText() != "ours" {
		t.Fatalf("headline = %q", errored.GetHeadline().GetText())
	}
	if errored.GetMessage().GetText() != "theirs" {
		t.Fatalf("message = %q", errored.GetMessage().GetText())
	}
}

func TestApplyHeadlineLeavesTheMessageUnsetWhenTheVendorSaidNothing(t *testing.T) {
	// Arrange, Act: a query death has no vendor wording.
	errored := &frontendv1.FeedTurnEndedErrored{}
	applyHeadline(errored, headline{Text: "the query died"}, "")

	// Assert.
	if errored.GetMessage() != nil {
		t.Fatalf("message = %+v, want unset", errored.GetMessage())
	}
}

func TestANilFormIsTheNoneArmAndNeverAnEmptyText(t *testing.T) {
	// Arrange, Act.
	returned := &frontendv1.FeedToolCallReturned{}
	applyReturnedForm(returned, nil)

	// Assert: presence, not a sentinel.
	if returned.GetNone() == nil {
		t.Fatalf("form = %T, want none", returned.GetForm())
	}
	if returned.GetText() != nil {
		t.Fatal("a nil form produced an empty text output")
	}
}

func TestASetFormIsApplied(t *testing.T) {
	// Arrange, Act.
	returned := &frontendv1.FeedToolCallReturned{}
	applyReturnedForm(returned, textForm("hello"))

	// Assert.
	if returned.GetText().GetText() != "hello" {
		t.Fatalf("form = %+v, want the text output", returned.GetForm())
	}
}

func TestApplyInputForm(t *testing.T) {
	tests := []struct {
		name string
		form inputForm
		want string
	}{
		{name: "a shell line", form: inputFormCommand, want: "command"},
		{name: "a muted path", form: inputFormPath, want: "path"},
		{name: "a query", form: inputFormQuery, want: "query"},
		{name: "no treatment named", form: inputFormNone, want: "none"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			input := &frontendv1.FeedToolCallInput{Text: "x"}
			applyInputForm(input, tc.form)

			// Assert.
			if got := inputFormWord(input); got != tc.want {
				t.Fatalf("form = %q, want %q", got, tc.want)
			}
			if tc.form.String() != tc.want {
				t.Fatalf("String() = %q, want %q", tc.form.String(), tc.want)
			}
		})
	}
}

func TestLostCauseOf(t *testing.T) {
	tests := []struct {
		name string
		how  any
		want detachedLostCause
	}{
		{name: "the file disappeared", how: &conversationv1.DetachedLostFileVanished{}, want: lostFileVanished},
		{name: "it went silent", how: &conversationv1.DetachedLostWentSilent{}, want: lostWentSilent},
		{name: "a boot sweep found it", how: &conversationv1.DetachedLostSweptUp{}, want: lostSweptUp},
		{name: "no arm set", how: nil, want: lostNone},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			lost := &conversationv1.DetachedLost{}
			switch h := tc.how.(type) {
			case *conversationv1.DetachedLostFileVanished:
				lost.How = &conversationv1.DetachedLost_FileVanished{FileVanished: h}
			case *conversationv1.DetachedLostWentSilent:
				lost.How = &conversationv1.DetachedLost_WentSilent{WentSilent: h}
			case *conversationv1.DetachedLostSweptUp:
				lost.How = &conversationv1.DetachedLost_SweptUp{SweptUp: h}
			}

			// Act.
			got := lostCauseOf(lost)

			// Assert.
			if got != tc.want {
				t.Fatalf("lostCauseOf = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAnOrdinaryFailureNamesNoLostCause(t *testing.T) {
	// Arrange, Act, Assert: LOST is not FAILED, so a plain failure must not
	// answer a lost cause.
	if got := lostCauseOfSubagent(&conversationv1.AgentSubagentFailure{}); got != lostNone {
		t.Fatalf("lostCauseOfSubagent = %v, want lostNone", got)
	}
	if got := lostCauseOfBash(&conversationv1.AgentBashInterrupted{}); got != lostNone {
		t.Fatalf("lostCauseOfBash = %v, want lostNone", got)
	}
	if got := lostCauseOfAgentFailure(&conversationv1.AgentFailure{}); got != lostNone {
		t.Fatalf("lostCauseOfAgentFailure = %v, want lostNone", got)
	}
}

func TestLostSentenceNeverClaimsAFailure(t *testing.T) {
	tests := []struct {
		name  string
		cause detachedLostCause
	}{
		{name: "file vanished", cause: lostFileVanished},
		{name: "went silent", cause: lostWentSilent},
		{name: "swept up", cause: lostSweptUp},
		{name: "none", cause: lostNone},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := lostSentence(tc.cause)

			// Assert.
			if !contains(got, "we lost sight of this work") {
				t.Fatalf("sentence = %q, want the lost wording", got)
			}
			if contains(got, "failed") {
				t.Fatalf("sentence = %q, want no claim of failure", got)
			}
		})
	}
}

func TestDetachedLostCauseString(t *testing.T) {
	tests := []struct {
		name  string
		cause detachedLostCause
		want  string
	}{
		{name: "file vanished", cause: lostFileVanished, want: "file_vanished"},
		{name: "went silent", cause: lostWentSilent, want: "went_silent"},
		{name: "swept up", cause: lostSweptUp, want: "swept_up"},
		{name: "none", cause: lostNone, want: "none"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act, Assert.
			if got := tc.cause.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

// Landing 11: the DetachedLost arm is relayed BY NAME onto both feed lost
// rows, and a cause this build does not carry sets no arm at all.

func TestApplySubagentLostHowRelaysEachArmByName(t *testing.T) {
	tests := []struct {
		name  string
		cause detachedLostCause
		want  func(*frontendv1.FeedSubagentLost) bool
	}{
		{
			name:  "file vanished",
			cause: lostFileVanished,
			want:  func(l *frontendv1.FeedSubagentLost) bool { return l.GetFileVanished() != nil },
		},
		{
			name:  "went silent",
			cause: lostWentSilent,
			want:  func(l *frontendv1.FeedSubagentLost) bool { return l.GetWentSilent() != nil },
		},
		{
			name:  "swept up",
			cause: lostSweptUp,
			want:  func(l *frontendv1.FeedSubagentLost) bool { return l.GetSweptUp() != nil },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			lost := &frontendv1.FeedSubagentLost{}

			// Act.
			ok := applySubagentLostHow(lost, tc.cause)

			// Assert.
			if !ok {
				t.Fatalf("applySubagentLostHow(%v) reported the arm unlanded", tc.cause)
			}
			if !tc.want(lost) {
				t.Fatalf("how = %T, want the %s arm", lost.GetHow(), tc.name)
			}
		})
	}
}

func TestApplySubagentLostHowLeavesAnUnnamedCauseUnset(t *testing.T) {
	// Arrange.
	lost := &frontendv1.FeedSubagentLost{}

	// Act.
	ok := applySubagentLostHow(lost, lostNone)

	// Assert: NEVER SILENTLY DEFAULTED — no arm, and the caller is told.
	if ok {
		t.Fatal("an unnamed cause reported a landed arm")
	}
	if lost.GetHow() != nil {
		t.Fatalf("how = %T, want unset", lost.GetHow())
	}
}

func TestApplyShellLostHowRelaysEachArmByName(t *testing.T) {
	tests := []struct {
		name  string
		cause detachedLostCause
		want  func(*frontendv1.FeedShellLost) bool
	}{
		{
			name:  "file vanished",
			cause: lostFileVanished,
			want:  func(l *frontendv1.FeedShellLost) bool { return l.GetFileVanished() != nil },
		},
		{
			name:  "went silent",
			cause: lostWentSilent,
			want:  func(l *frontendv1.FeedShellLost) bool { return l.GetWentSilent() != nil },
		},
		{
			name:  "swept up",
			cause: lostSweptUp,
			want:  func(l *frontendv1.FeedShellLost) bool { return l.GetSweptUp() != nil },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			lost := &frontendv1.FeedShellLost{}

			// Act.
			ok := applyShellLostHow(lost, tc.cause)

			// Assert.
			if !ok {
				t.Fatalf("applyShellLostHow(%v) reported the arm unlanded", tc.cause)
			}
			if !tc.want(lost) {
				t.Fatalf("how = %T, want the %s arm", lost.GetHow(), tc.name)
			}
		})
	}
}

func TestApplyShellLostHowLeavesAnUnnamedCauseUnset(t *testing.T) {
	// Arrange.
	lost := &frontendv1.FeedShellLost{}

	// Act.
	ok := applyShellLostHow(lost, lostNone)

	// Assert.
	if ok {
		t.Fatal("an unnamed cause reported a landed arm")
	}
	if lost.GetHow() != nil {
		t.Fatalf("how = %T, want unset", lost.GetHow())
	}
}
