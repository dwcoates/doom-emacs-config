package feed

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prose"
	"agent-repld/internal/pullrefs"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

var chessComTarget = &pullrefs.Target{Owner: "ChessCom", Repo: "explanation-engine"}

const linked12 = "[#12](https://github.com/ChessCom/explanation-engine/pull/12)"

// settleResponse draws one settled response bubble saying MARKDOWN and
// answers its prose.
func settleResponse(h *harness, markdown string) *frontendv1.ParsedProse {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), nil, nil)
	return h.response().GetSuccess().GetProse().GetProse()
}

// plainMarkdown answers the markdown of PARSED's only plain block.
func plainMarkdown(t *testing.T, parsed *frontendv1.ParsedProse) string {
	t.Helper()
	if len(parsed.GetBlocks()) != 1 || parsed.GetBlocks()[0].GetPlain() == nil {
		t.Fatalf("blocks = %+v, want one plain block", parsed.GetBlocks())
	}
	return parsed.GetBlocks()[0].GetPlain().GetMarkdown()
}

func TestAResponseLinksItsPullReferences(t *testing.T) {
	cases := []struct {
		name   string
		target *pullrefs.Target
		want   string
	}{
		{name: "a workspace with a target links them", target: chessComTarget, want: "landed in " + linked12},
		{name: "a workspace with no target leaves them", want: "landed in #12"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.SetPullTarget(testWorkspace, tc.target)

			// Act.
			parsed := settleResponse(h, "landed in #12")

			// Assert.
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn markdown = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAResponsesSourceStaysAsWritten(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	parsed := settleResponse(h, "landed in #12")

	// Assert: a reply quotes and a search reads what the agent wrote.
	if got := parsed.GetSource().GetMarkdown(); got != "landed in #12" {
		t.Fatalf("source = %q, want the text as written", got)
	}
}

func TestAnArrivingResponseLeavesItsTrailingReference(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act: the text so far ends on a reference more digits may extend.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "#12 then #3"}, nil), nil, nil)

	// Assert.
	if got := plainMarkdown(t, h.response().GetUpdate().GetProse().GetProse()); got != linked12+" then #3" {
		t.Fatalf("drawn markdown = %q, want only the earlier reference linked", got)
	}
}

func TestAWithdrawnTargetLinksNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	h.resolver.SetPullTarget(testWorkspace, nil)
	parsed := settleResponse(h, "landed in #12")

	// Assert.
	if got := plainMarkdown(t, parsed); got != "landed in #12" {
		t.Fatalf("drawn markdown = %q, want nothing linked", got)
	}
	if h.resolver.pullTarget(testWorkspace) != nil {
		t.Fatal("pullTarget answers a target after it was withdrawn")
	}
}

func TestAResetKeepsThePullTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	h.resolver.ResetWorkspace(testWorkspace, "test")

	// Assert.
	if got := h.resolver.pullTarget(testWorkspace); got == nil || *got != *chessComTarget {
		t.Fatalf("target after a reset = %v, want %v", got, chessComTarget)
	}
}

func TestSetPullTargetRecordsTheTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Assert.
	if !h.hasRecordWith("debug", "daemon.feed.pull_target", "owner", "ChessCom") {
		t.Fatalf("records = %+v, want the installed target recorded", h.records())
	}
}

func TestLinkPullsLinksEveryDrawnMarkdown(t *testing.T) {
	cases := []struct {
		name  string
		block *frontendv1.ProseBlock
		read  func(*frontendv1.ProseBlock) string
	}{
		{
			name: "a tree node's text",
			block: &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Tree{Tree: &frontendv1.ProseTree{
				Nodes: []*frontendv1.ProseTreeNode{{Text: &frontendv1.ProseTreeNodeText{Markdown: "see #12"}}},
			}}},
			read: func(b *frontendv1.ProseBlock) string { return b.GetTree().GetNodes()[0].GetText().GetMarkdown() },
		},
		{
			name: "an entry's answer",
			block: &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Entries{Entries: &frontendv1.ProseEntryTree{
				Nodes: []*frontendv1.ProseEntryTreeNode{{Kind: &frontendv1.ProseEntryTreeNode_Answer{
					Answer: &frontendv1.ProseEntryTreeNodeAnswer{Text: &frontendv1.ProseEntryText{Markdown: "see #12"}},
				}}},
			}}},
			read: func(b *frontendv1.ProseBlock) string {
				return b.GetEntries().GetNodes()[0].GetAnswer().GetText().GetMarkdown()
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.SetPullTarget(testWorkspace, chessComTarget)
			parsed := &frontendv1.ParsedProse{Blocks: []*frontendv1.ProseBlock{tc.block}}

			// Act.
			h.resolver.linkPulls(testWorkspace, parsed, false)

			// Assert.
			if got := tc.read(tc.block); got != "see "+linked12 {
				t.Fatalf("drawn markdown = %q, want the reference linked", got)
			}
		})
	}
}

func TestDrawUserBlocksLinksAPromptsReferences(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)
	content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "review #12"}}},
	}}
	link := func(parsed *frontendv1.ParsedProse) { h.resolver.DrawLinks(testWorkspace, parsed) }

	// Act.
	blocks := DrawUserBlocks(content, func(s string) string { return s }, refusingResolver, link, nil, dlog.NewTestLogger())

	// Assert.
	parsed := blocks[0].GetText().GetProse()
	if got := plainMarkdown(t, parsed); got != "review "+linked12 {
		t.Fatalf("drawn prompt = %q, want the reference linked", got)
	}
	if got := parsed.GetSource().GetMarkdown(); got != "review #12" {
		t.Fatalf("prompt source = %q, want the text as written", got)
	}
}

func TestDrawUserBlocksMarksUpRecentCodeSpans(t *testing.T) {
	tests := []struct {
		name  string
		text  string
		spans []string
		want  string
	}{
		{"plain prompt", "check foo() please", []string{"foo()"}, "check `foo()` please"},
		{"no spans", "check foo() please", nil, "check foo() please"},
		{"already code", "check `foo()` please", []string{"foo()"}, "check `foo()` please"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
				{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: tc.text}}},
			}}

			// Act.
			blocks := DrawUserBlocks(content, func(s string) string { return s }, refusingResolver, linksNoPulls, tc.spans, dlog.NewTestLogger())

			// Assert.
			parsed := blocks[0].GetText().GetProse()
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn prompt = %q, want %q", got, tc.want)
			}
			if got := parsed.GetSource().GetMarkdown(); got != tc.text {
				t.Fatalf("prompt source = %q, want the text as written", got)
			}
		})
	}
}

func TestMarkCodeSpansMarksUpEntryAnswers(t *testing.T) {
	// Arrange.
	parsed := prose.ParsePrompt(dlog.NewTestLogger(), "1 - run foo() now")

	// Act.
	markCodeSpans(parsed, []string{"foo()"})

	// Assert.
	var got string
	for _, b := range parsed.GetBlocks() {
		for _, n := range b.GetEntries().GetNodes() {
			if t := n.GetAnswer().GetText(); t != nil {
				got = t.GetMarkdown()
			}
		}
	}
	if got != "run `foo()` now" {
		t.Fatalf("entry answer = %q, want the span marked up", got)
	}
}

func TestRecentCodeSpansReadsOnlyTheLastFinalResponses(t *testing.T) {
	// Arrange.
	ids3 := []*frontendv1.FeedId{{Value: "a"}, {Value: "b"}, {Value: "c"}}
	s := &wsState{
		finalAnswers:   ids3,
		answerMarkdown: map[string]string{"a": "old `oldspan`", "b": "mid `midspan`", "c": "new `newspan` `midspan`"},
	}
	r := &resolver{}

	// Act.
	got := r.recentCodeSpans(s)

	// Assert.
	want := []string{"newspan", "midspan"}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("recentCodeSpans = %q, want %q", got, want)
	}
}

func TestRecentCodeSpansWithoutFinalResponsesIsEmpty(t *testing.T) {
	if got := (&resolver{}).recentCodeSpans(&wsState{}); len(got) != 0 {
		t.Fatalf("recentCodeSpans = %q, want none", got)
	}
}

// gns613 is a pull request of a repository other than the workspace's own.
var gns613 = pullrefs.Ref{Target: pullrefs.Target{Owner: "ChessCom", Repo: "gns-server"}, Number: "613"}

const gns613URL = "https://github.com/ChessCom/gns-server/pull/613"

const linkedGns613 = "[#613](" + gns613URL + ")"

// fakePullReferences is an in-memory PullReferenceStore; two harnesses
// sharing one are a daemon and its restarted successor.
type fakePullReferences struct {
	refs      map[ids.WorkspaceID][]pullrefs.Ref
	at        []time.Time
	loadErr   error
	recordErr error
	loads     int
	// inLoad, when set, runs at every load, before it answers.
	inLoad func()
}

func newFakePullReferences() *fakePullReferences {
	return &fakePullReferences{refs: map[ids.WorkspaceID][]pullrefs.Ref{}}
}

func (f *fakePullReferences) RecordPullReference(_ context.Context, ws ids.WorkspaceID, ref pullrefs.Ref, at time.Time) error {
	if f.recordErr != nil {
		return f.recordErr
	}
	f.refs[ws] = append(f.refs[ws], ref)
	f.at = append(f.at, at)
	return nil
}

func (f *fakePullReferences) PullReferences(_ context.Context, ws ids.WorkspaceID) ([]pullrefs.Ref, error) {
	f.loads++
	if f.inLoad != nil {
		f.inLoad()
	}
	if f.loadErr != nil {
		return nil, f.loadErr
	}
	return f.refs[ws], nil
}

// settleResponseAs draws one settled response bubble of UNIT saying MARKDOWN
// and answers its drawn markdown: the newest response row's.
func settleResponseAs(h *harness, unit, markdown string) string {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame(unit, &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), nil, nil)
	rows := h.responseRows()
	if len(rows) == 0 {
		h.t.Fatalf("no response row of %s", unit)
	}
	return plainMarkdown(h.t, rows[len(rows)-1].GetActivity().GetResponse().GetSuccess().GetProse().GetProse())
}

func TestABareReferenceLinksToThePullRequestAnEarlierTextNamed(t *testing.T) {
	cases := []struct {
		name   string
		target *pullrefs.Target
		first  string
		second string
		want   string
	}{
		{name: "a full address named earlier", target: chessComTarget,
			first: "opened " + gns613URL, second: "#613 merged", want: linkedGns613 + " merged"},
		{name: "a markdown link named earlier", target: chessComTarget,
			first: "opened [the fix](" + gns613URL + ")", second: "#613 merged", want: linkedGns613 + " merged"},
		{name: "a repo reference named earlier", target: chessComTarget,
			first: "opened gns-server#613", second: "#613 merged", want: linkedGns613 + " merged"},
		{name: "an unnamed number links into the workspace's repository", target: chessComTarget,
			first: "opened " + gns613URL, second: "#12 merged", want: linked12 + " merged"},
		{name: "a workspace with no target links a named number", first: "opened " + gns613URL,
			second: "#613 merged", want: linkedGns613 + " merged"},
		{name: "a workspace with no target leaves an unnamed number", first: "opened " + gns613URL,
			second: "#12 merged", want: "#12 merged"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.SetPullTarget(testWorkspace, tc.target)
			settleResponseAs(h, "unit-1", tc.first)

			// Act.
			got := settleResponseAs(h, "unit-2", tc.second)

			// Assert.
			if got != tc.want {
				t.Fatalf("drawn markdown = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestABareReferenceLinksToThePullRequestItsOwnTextNamedBefore(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	got := settleResponseAs(h, "unit-1", "see #613\n\n"+gns613URL)

	// Assert: the whole text's addresses are learned before any is linked.
	if want := "see " + linkedGns613 + "\n\n" + gns613URL; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
}

func TestAnArrivingTextDoesNotLearnItsTrailingAddress(t *testing.T) {
	// Arrange.
	store := newFakePullReferences()
	h := newDurableHarness(t, durableStores{pullReferences: store})
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act: the address so far may still grow digits.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "opened https://github.com/ChessCom/gns-server/pull/61"}, nil), nil, nil)

	// Assert.
	if got := store.refs[testWorkspace]; len(got) != 0 {
		t.Fatalf("recorded %+v, want nothing learned from a growing address", got)
	}
}

func TestALearnedPullRequestIsRecordedOnce(t *testing.T) {
	// Arrange.
	store := newFakePullReferences()
	h := newDurableHarness(t, durableStores{pullReferences: store})

	// Act.
	settleResponseAs(h, "unit-1", "opened "+gns613URL)
	settleResponseAs(h, "unit-2", "again "+gns613URL)

	// Assert.
	if got := store.refs[testWorkspace]; len(got) != 1 || got[0] != gns613 {
		t.Fatalf("recorded %+v, want %+v once", got, gns613)
	}
	if want := time.UnixMilli(h.nowMs); !store.at[0].Equal(want) {
		t.Fatalf("recorded at %v, want %v", store.at[0], want)
	}
	if !h.hasRecordWith("debug", opPullReferences, "repo", "gns-server") {
		t.Fatalf("records = %+v, want the learning recorded", h.records())
	}
}

func TestARestartedDaemonLinksThePullRequestsTheOneBeforeLearned(t *testing.T) {
	// Arrange: one daemon learns the pull request, its successor shares the
	// store.
	store := newFakePullReferences()
	before := newDurableHarness(t, durableStores{pullReferences: store})
	settleResponseAs(before, "unit-1", "opened "+gns613URL)
	after := newDurableHarness(t, durableStores{pullReferences: store})
	after.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	got := settleResponseAs(after, "unit-2", "#613 merged")

	// Assert.
	if want := linkedGns613 + " merged"; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
	if !after.hasRecordWith("info", opPullReferencesRestore, "pull_requests", 1) {
		t.Fatalf("records = %+v, want the load recorded", after.records())
	}
}

func TestTheKnownPullRequestsLoadRecordsHowLongItsReadTook(t *testing.T) {
	tests := []struct {
		name  string
		level string
		known bool
	}{
		{name: "a load that found pull requests records its read at INFO", level: "info", known: true},
		{name: "a load that found none records its read at DEBUG", level: "debug"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the read, made as a workspace is primed (prime.go),
			// takes 300ms on the harness clock.
			store := newFakePullReferences()
			if tt.known {
				store.refs[otherWorkspace] = []pullrefs.Ref{gns613}
			}
			h := newDurableHarness(t, durableStores{pullReferences: store})
			store.inLoad = func() { h.nowMs += 300 }

			// Act: the workspace is primed, and its known pull requests are
			// installed under the lock.
			h.resolver.prime(otherWorkspace)
			h.resolver.mu.Lock()
			h.resolver.knownPulls(otherWorkspace)
			h.resolver.mu.Unlock()

			// Assert.
			if !h.hasRecordWith(tt.level, opPullReferencesRestore, "read_ms", int64(300)) {
				t.Fatalf("records = %+v, want the load's 300ms read recorded at %s", h.records(), tt.level)
			}
		})
	}
}

func TestTheKnownPullRequestsAreLoadedOnce(t *testing.T) {
	// Arrange.
	store := newFakePullReferences()
	h := newDurableHarness(t, durableStores{pullReferences: store})

	// Act.
	settleResponseAs(h, "unit-1", "#1")
	settleResponseAs(h, "unit-2", "#2")

	// Assert.
	if store.loads != 1 {
		t.Fatalf("loads = %d, want 1", store.loads)
	}
}

func TestAResetKeepsTheKnownPullRequests(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)
	settleResponseAs(h, "unit-1", "opened "+gns613URL)

	// Act.
	h.resolver.ResetWorkspace(testWorkspace, "test")
	h.resolver.OnMainAgent(testWorkspace, mainAgent())
	got := settleResponseAs(h, "unit-2", "#613 merged")

	// Assert.
	if want := linkedGns613 + " merged"; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
}

func TestAKnownPullRequestsLoadFailureIsLogged(t *testing.T) {
	// Arrange.
	store := newFakePullReferences()
	store.loadErr = errors.New("disk on fire")
	h := newDurableHarness(t, durableStores{pullReferences: store})
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	got := settleResponseAs(h, "unit-1", "#12 merged")

	// Assert: the workspace links into its own repository, loudly.
	if want := linked12 + " merged"; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
	if !h.hasRecordWith("error", opPullReferencesRestore, "error", "disk on fire") {
		t.Fatalf("records = %+v, want the failed load at error", h.records())
	}
}

func TestAKnownPullRequestsRecordFailureIsLogged(t *testing.T) {
	// Arrange.
	store := newFakePullReferences()
	store.recordErr = errors.New("disk on fire")
	h := newDurableHarness(t, durableStores{pullReferences: store})
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)
	settleResponseAs(h, "unit-1", "opened "+gns613URL)

	// Act.
	got := settleResponseAs(h, "unit-2", "#613 merged")

	// Assert: this process still knows it, and says it could not keep it.
	if want := linkedGns613 + " merged"; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
	if !h.hasRecordWith("error", opPullReferences, "error", "disk on fire") {
		t.Fatalf("records = %+v, want the failed recording at error", h.records())
	}
}

func TestLinkPullsLinksAPromptMirroredOutsideTheResolver(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	settleResponseAs(h, "unit-1", "opened "+gns613URL)
	parsed := prose.ParsePrompt(dlog.NewTestLogger(), "review #613")

	// Act.
	h.resolver.DrawLinks(testWorkspace, parsed)

	// Assert.
	if got := plainMarkdown(t, parsed); got != "review "+linkedGns613 {
		t.Fatalf("drawn prompt = %q, want the known pull request linked", got)
	}
}

// linksNoPulls is the pull request linker of a DrawUserBlocks case that is
// not about pull request references: DrawUserBlocks links every text block
// it draws, so a case hands it one that links nothing.
func linksNoPulls(*frontendv1.ParsedProse) {}
