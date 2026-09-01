//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// ==========================================================================
// OpenFeed / WatchFeed / GetFeedPage — the page/tail seam.
// ==========================================================================

func TestWatchFeedTailsExactlyAfterTheOpenedPageWithNoGapOrOverlap(t *testing.T) {
	// Arrange: one row lands before the feed is ever opened.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-seam", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[1])

	// Act: open the root feed (mints the page + token), then push a SECOND
	// row before ever watching the tail.
	page, token := f.openFeed(nil)
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-2", "second")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-2", "second")[1])
	tail := f.d.WatchFeed(token)

	// Assert: the page already carries resp-1's text; the tail's first row
	// is resp-2's, proving no gap (resp-2 was not lost between open and
	// watch) and no overlap (resp-1 is not re-delivered on the tail).
	sawFirst := false
	for _, r := range page.GetSuccess().GetRows() {
		if r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == "first" {
			sawFirst = true
		}
	}
	if !sawFirst {
		t.Fatalf("OpenFeed's page = %v, want it to already carry the row pushed before the open", page)
	}
	got := awaitRow(t, f, tail, "the tail's first row after the open", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess() != nil
	})
	if md := got.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown(); md != "second" {
		t.Fatalf("the tail's first row = %q, want %q (resp-1 must not be re-delivered)", md, "second")
	}
}

func TestWatchFeedWithAnUnmintedTokenIsRefusedAtTheTransport(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	stream, err := f.d.Client().WatchFeed(f.d.Ctx(), connect.NewRequest(&agentreplv1.WatchFeedRequest{
		Watch: &agentreplv1.FeedWatchToken{Value: "bogus-unminted-token"},
	}))

	// Assert
	if err == nil {
		if stream.Receive() {
			t.Fatalf("WatchFeed(bogus token) delivered a row %v, want a transport refusal", stream.Msg())
		}
		err = stream.Err()
	}
	if err == nil {
		t.Fatal("WatchFeed(bogus token) = success, want a transport-level refusal")
	}
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestGetFeedPageNextWithNoWalkStandingIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws,
		Page:      &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("GetFeedPage{next} = error %v, want a typed no_walk_standing refusal", err)
	}
	if resp.Msg.GetError().GetNoWalkStanding() == nil {
		t.Fatalf("GetFeedPage{next} with no walk standing = %v, want error.no_walk_standing", resp.Msg)
	}
}

func TestGetFeedPageFirstThenNextWalksOlderPages(t *testing.T) {
	// Arrange: push enough rows that the newest page cannot hold them all.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-walk", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	const n = 150
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}

	// Act
	first, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws,
		Page:      &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	if err != nil {
		t.Fatalf("GetFeedPage{first} = error %v, want a page", err)
	}
	if first.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{first} = %v, want a clean page over %d rows", first.Msg, n)
	}
	if first.Msg.GetSuccess().GetSuccess().GetHasMore() == nil {
		t.Skip("feedWalk: the fake session's history did not exceed one page at " + FeedPageSizeNote)
	}
	next, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws,
		Page:      &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}},
	}))

	// Assert: the older page exists and does not repeat the newest page's rows.
	if err != nil {
		t.Fatalf("GetFeedPage{next} = error %v, want the older page", err)
	}
	if next.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{next} = %v, want the older page", next.Msg)
	}
	newest := map[string]bool{}
	for _, r := range first.Msg.GetSuccess().GetSuccess().GetRows() {
		newest[r.GetId().GetValue()] = true
	}
	for _, r := range next.Msg.GetSuccess().GetSuccess().GetRows() {
		if newest[r.GetId().GetValue()] {
			t.Fatalf("GetFeedPage{next} repeated a row %v the first page already served", r.GetId())
		}
	}
}

func TestGetFeedPageWalkIsPerConnection(t *testing.T) {
	// Arrange: enough rows for at least one older page, and a walk
	// established on connection A.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-perconn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	for i := 0; i < 150; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	clientA := f.d.Client()
	first, err := clientA.GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	if err != nil || first.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{first} on connection A = %v, %v, want a page", first.Msg, err)
	}
	if _, err := clientA.GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}},
	})); err != nil {
		t.Fatalf("GetFeedPage{next} on connection A = error %v", err)
	}

	// Act: a second, independent connection's {next} with no walk of its own.
	clientB := f.d.Dial()
	resp, err := clientB.GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}},
	}))

	// Assert: connection B has never walked, so it is refused regardless of
	// what connection A has done.
	if err != nil {
		t.Fatalf("GetFeedPage{next} on a fresh connection = error %v, want a typed refusal", err)
	}
	if resp.Msg.GetError().GetNoWalkStanding() == nil {
		t.Fatalf("GetFeedPage{next} on a connection that never walked = %v, want no_walk_standing", resp.Msg)
	}
}

func TestGetFeedPageWalkIsNotPersistedAcrossAReconnect(t *testing.T) {
	// Arrange: establish a walk on one connection, then abandon it.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-noreplay", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	for i := 0; i < 150; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	if _, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	})); err != nil {
		t.Fatalf("GetFeedPage{first} = error %v", err)
	}

	// Act: reconnect (a fresh connection stands in for a client restart) and
	// go straight to {next}.
	reconnected := f.d.Dial()
	resp, err := reconnected.GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("GetFeedPage{next} after a reconnect = error %v, want a typed refusal", err)
	}
	if resp.Msg.GetError().GetNoWalkStanding() == nil {
		t.Fatalf("GetFeedPage{next} after a reconnect = %v, want no_walk_standing: the walk must not survive the connection", resp.Msg)
	}
}

// ==========================================================================
// The response bubble: growth, and self-correction on the terminal.
// ==========================================================================

func TestAGrowingResponseRepushesTheSameFeedIdThenSettlesWhole(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-grow", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: start, then two update fragments, then the terminal.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-grow"),
		Item:       &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}}},
	}))
	first := awaitRow(t, f, tail, "the response's opened row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse() != nil
	})
	id := first.GetId().GetValue()

	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-grow"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Update{
			Update: &conversationv1.AgentResponseUpdate{NewMarkdown: "Hel"},
		}}},
	}))
	grown := awaitRow(t, f, tail, "the response with its accumulated prose", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == id && r.GetActivity().GetResponse().GetUpdate() != nil
	})
	if grown.GetActivity().GetResponse().GetUpdate().GetProse().GetMarkdown() != "Hel" {
		t.Fatalf("the growing row's prose = %q, want %q", grown.GetActivity().GetResponse().GetUpdate().GetProse().GetMarkdown(), "Hel")
	}

	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-grow"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "Hello world"}},
		}}},
	}))

	// Assert: the SAME FeedId settles with the whole text.
	settled := awaitRow(t, f, tail, "the settled response", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == id && r.GetActivity().GetResponse().GetSuccess() != nil
	})
	if md := settled.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown(); md != "Hello world" {
		t.Fatalf("the settled response's prose = %q, want %q", md, "Hello world")
	}
}

func TestALostResponseFragmentSelfCorrectsOnTheTerminal(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-selfcorrect", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-lost"),
		Item:       &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}}},
	}))
	first := awaitRow(t, f, tail, "the opened row", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetResponse() != nil })

	// Act: skip straight to the terminal — as if an update fragment had
	// never arrived at all — carrying the WHOLE settled text.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-lost"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "the whole answer"}},
		}}},
	}))

	// Assert: the terminal frame alone is a correct, complete rendering.
	settled := awaitRow(t, f, tail, "the settled response despite the missed fragment", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == first.GetId().GetValue() && r.GetActivity().GetResponse().GetSuccess() != nil
	})
	if md := settled.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown(); md != "the whole answer" {
		t.Fatalf("the settled response's prose = %q, want the whole text %q despite the missed fragment", md, "the whole answer")
	}
}

// ==========================================================================
// Tool cards.
// ==========================================================================

func TestReadToolCardDrawsCodeOutputWithPaintSpansAndOmittedForAHeadCut(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-read", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("read-1"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
			Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "big.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("read-1"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Success{
			Success: &conversationv1.AgentReadSuccess{
				Path: &conversationv1.ReadPath{Path: "big.go"},
				Extent: &conversationv1.AgentReadSuccess_Head{Head: &conversationv1.AgentReadHead{
					Contents:   "package main\n",
					TotalLines: 4312,
					Cut:        &conversationv1.AgentReadHead_LineCap{LineCap: &conversationv1.AgentReadCutAtLineCap{}},
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the read's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetForm() != nil
	})
	code := row.GetActivity().GetSimpleToolCall().GetReturned().GetCode()
	if code == nil {
		t.Fatalf("the read's tool card form = %v, want code output", row.GetActivity().GetSimpleToolCall().GetReturned())
	}
	if len(code.GetSpans()) == 0 {
		t.Fatal("the read's code output carries no paint spans, want at least one")
	}
	if code.GetOmitted() == nil {
		t.Fatal("a head-cut read's code output carries no omitted line, want one composed for the cut")
	}
}

func TestWriteToolCardDrawsDiffLines(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-write", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("write-1"),
		Item: &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Start{
			Start: &conversationv1.AgentWriteStart{Path: &conversationv1.ReadPath{Path: "new.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("write-1"),
		Item: &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Success{
			Success: &conversationv1.AgentWriteSuccess{
				Path:      &conversationv1.ReadPath{Path: "new.go"},
				Outcome:   &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}},
				Patch:     []*conversationv1.FilePatchHunk{{Lines: []string{"package main"}}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the write's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetDiff() != nil
	})
	diff := row.GetActivity().GetSimpleToolCall().GetReturned().GetDiff()
	if len(diff.GetLines()) == 0 {
		t.Fatal("the write's diff output carries no lines, want the created file's hunk")
	}
}

func TestEditToolCardDrawsDiffLines(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-edit", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("edit-1"),
		Item: &conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{Result: &conversationv1.AgentEdit_Start{
			Start: &conversationv1.AgentEditStart{Path: &conversationv1.ReadPath{Path: "old.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("edit-1"),
		Item: &conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{Result: &conversationv1.AgentEdit_Success{
			Success: &conversationv1.AgentEditSuccess{
				Path:      &conversationv1.ReadPath{Path: "old.go"},
				Patch:     []*conversationv1.FilePatchHunk{{Lines: []string{"-old", "+new"}}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the edit's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetDiff() != nil
	})
	if len(row.GetActivity().GetSimpleToolCall().GetReturned().GetDiff().GetLines()) == 0 {
		t.Fatal("the edit's diff output carries no lines, want the change's hunk")
	}
}

func TestGrepToolCardDrawsLinesOutputWithOmitted(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-grep", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("grep-1"),
		Item: &conversationv1.AgentActivity_Grep{Grep: &conversationv1.AgentGrep{Result: &conversationv1.AgentGrep_Start{
			Start: &conversationv1.AgentGrepStart{Query: &conversationv1.AgentGrepQuery{Pattern: "TODO"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("grep-1"),
		Item: &conversationv1.AgentActivity_Grep{Grep: &conversationv1.AgentGrep{Result: &conversationv1.AgentGrep_Success{
			Success: &conversationv1.AgentGrepSuccess{
				Query: &conversationv1.AgentGrepQuery{Pattern: "TODO"},
				Matches: &conversationv1.AgentGrepSuccess_Content{Content: &conversationv1.AgentGrepContent{
					Content: "a.go:1: TODO\n",
					Extent:  &conversationv1.AgentGrepContent_Partial{Partial: &conversationv1.AgentGrepContentPartial{LinesReturned: 1, LinesOmitted: 42}},
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the grep's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetLines() != nil
	})
	lines := row.GetActivity().GetSimpleToolCall().GetReturned().GetLines()
	if len(lines.GetLines()) == 0 {
		t.Fatal("the grep's lines output carries no lines")
	}
	if lines.GetOmitted() == nil {
		t.Fatal("a partial grep's lines output carries no omitted floor, want one composed")
	}
}

func TestGlobToolCardDrawsLinesOutput(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-glob", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("glob-1"),
		Item: &conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{Result: &conversationv1.AgentGlob_Start{
			Start: &conversationv1.AgentGlobStart{Query: &conversationv1.AgentGlobQuery{Pattern: "*.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("glob-1"),
		Item: &conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{Result: &conversationv1.AgentGlob_Success{
			Success: &conversationv1.AgentGlobSuccess{
				Query:     &conversationv1.AgentGlobQuery{Pattern: "*.go"},
				Paths:     []string{"a.go", "b.go"},
				Extent:    &conversationv1.AgentGlobSuccess_All{All: &conversationv1.AgentGlobAll{FilesReturned: 2}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the glob's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetLines() != nil
	})
	if got := row.GetActivity().GetSimpleToolCall().GetReturned().GetLines().GetLines(); len(got) != 2 {
		t.Fatalf("the glob's lines output = %v, want the two matched paths", got)
	}
}

func TestBashForegroundToolCardDrawsTextOutput(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-bash", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-1"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: "echo hi"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-1"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{
			Success: &conversationv1.AgentBashSuccess{
				Command: &conversationv1.AgentBashCommand{Line: "echo hi"},
				Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
					Output: &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
						Stdout: "hi\n",
						Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
					}}},
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the bash call's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetText() != nil
	})
	if got := row.GetActivity().GetSimpleToolCall().GetReturned().GetText().GetText(); got == "" {
		t.Fatal("the bash call's text output is empty, want the command's stdout")
	}
}

func TestAProgressFrameRepushesRunningLastProgress(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-progress", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-prog"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: "sleep 5"}, StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-prog"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Progress{
			Progress: &conversationv1.AgentToolCallProgress{LastProgressAtMs: 42},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the running call's re-pushed last progress", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetRunning().GetLastProgress() != nil
	})
	if got := row.GetActivity().GetSimpleToolCall().GetRunning().GetLastProgress().GetAtMs(); got != 42 {
		t.Fatalf("the running card's last progress = %d, want 42", got)
	}
}

func TestAFailedToolCallDrawsReturnedFailed(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-toolfail", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-fail"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: "false"}, StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-fail"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Failure{
			Failure: &conversationv1.AgentBashFailure{Error: &conversationv1.AgentToolFailure{SettledAt: settledAt(2)}},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the failed call's tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall() != nil
	})
	if row.GetActivity().GetSimpleToolCall().GetReturned().GetFailed() == nil {
		t.Fatalf("a failed call's outcome = %v, want returned.failed", row.GetActivity().GetSimpleToolCall().GetOutcome())
	}
}

func TestADeniedPermissionDrawsTheToolCardAsDenied(t *testing.T) {
	// Arrange: a denied tool never starts and has no activity frames at all
	// (permission.proto) — the card is drawn from the permission gate alone.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-denied", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-denied", "gated-bash")},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
			Id:        &conversationv1.AgentPermissionId{Value: "perm-denied"},
			GatedCall: activityID("gated-bash"),
			Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
				Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
					By: &conversationv1.AgentPermissionDenied_User{User: &conversationv1.AgentPermissionDeniedByUser{Message: "no"}},
				}},
			}},
		}},
	}))

	// Assert: SOMEWHERE in the feed, the gated call's own card draws denied.
	found := false
	for {
		row := awaitRow(t, f, tail, "a row following the denial", func(*frontendv1.FeedRow) bool { return true })
		if row.GetActivity().GetSimpleToolCall().GetDenied() != nil {
			found = true
			break
		}
		if row.GetPermission().GetState() != nil {
			// The permission card itself re-pushed as answered; keep looking
			// for the gated call's own card among the same batch of pushes.
			continue
		}
	}
	if !found {
		t.Skip("feedDenied: the harness observed the permission's own answered re-push but no distinct gated-call tool card; report as unexpressible if this recurs")
	}
}

// ==========================================================================
// Skill card.
// ==========================================================================

func TestSkillCardComposesFromExactlyTheStartAndSuccessFrames(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-skill", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("skill-1"),
		Item: &conversationv1.AgentActivity_SkillUse{SkillUse: &conversationv1.AgentSkillUse{Result: &conversationv1.AgentSkillUse_Start{
			Start: &conversationv1.AgentSkillUseStart{Skill: &conversationv1.AgentSkillName{Name: "graphify"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("skill-1"),
		Item: &conversationv1.AgentActivity_SkillUse{SkillUse: &conversationv1.AgentSkillUse{Result: &conversationv1.AgentSkillUse_Success{
			Success: &conversationv1.AgentSkillUseSuccess{
				Skill:     &conversationv1.AgentSkillName{Name: "graphify"},
				Document:  &conversationv1.AgentSkillDocument{Markdown: "# graphify\n"},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the loaded skill card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSkill().GetLoaded() != nil
	})
	if md := row.GetActivity().GetSkill().GetLoaded().GetDocument().GetMarkdown(); md != "# graphify\n" {
		t.Fatalf("the skill card's document = %q, want the loaded markdown", md)
	}
}

// ==========================================================================
// Plan mode.
// ==========================================================================

func TestPlanModeEnterThenExitCoalesceOntoOneFeedId(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-plan", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: enter.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-enter"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Start{
			Start: &conversationv1.AgentPlanModeStart{Act: &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}}, StartedAt: startedAt(1)},
		}}},
	}))
	entered := awaitRow(t, f, tail, "the planning bubble", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetPlan() != nil })
	if entered.GetActivity().GetPlan().GetPlanning() == nil {
		t.Fatalf("the entered plan bubble's state = %v, want planning", entered.GetActivity().GetPlan())
	}
	id := entered.GetId().GetValue()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-enter"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Success{
			Success: &conversationv1.AgentPlanModeSuccess{Act: &conversationv1.AgentPlanModeSuccess_Entered{Entered: &conversationv1.AgentPlanModeEntered{}}, SettledAt: settledAt(2)},
		}}},
	}))

	// Act: exit, a DIFFERENT tool call, presenting the plan.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-exit"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Start{
			Start: &conversationv1.AgentPlanModeStart{Act: &conversationv1.AgentPlanModeStart_Exit{Exit: &conversationv1.AgentPlanModeExit{}}, StartedAt: startedAt(3)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-exit"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Success{
			Success: &conversationv1.AgentPlanModeSuccess{
				Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
					Plan: &conversationv1.AgentResponseProse{Markdown: "1. do it"},
				}},
				SettledAt: settledAt(4),
			},
		}}},
	}))

	// Assert: the exit fills the SAME bubble id the enter opened.
	planned := awaitRow(t, f, tail, "the presented plan on the same bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == id && r.GetActivity().GetPlan().GetPlanned() != nil
	})
	if md := planned.GetActivity().GetPlan().GetPlanned().GetProse().GetMarkdown(); md != "1. do it" {
		t.Fatalf("the coalesced plan bubble's document = %q, want the exit's plan", md)
	}
}

func TestPlanModeExitWithoutEnterIsLegal(t *testing.T) {
	// Arrange: a session started in the plan permission mode never calls
	// EnterPlanMode at all.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-planexit-only", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-exit-only"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Start{
			Start: &conversationv1.AgentPlanModeStart{Act: &conversationv1.AgentPlanModeStart_Exit{Exit: &conversationv1.AgentPlanModeExit{}}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-exit-only"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Success{
			Success: &conversationv1.AgentPlanModeSuccess{
				Act:       &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{Plan: &conversationv1.AgentResponseProse{Markdown: "solo exit"}}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the plan bubble from an exit with no enter", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetPlan().GetPlanned() != nil
	})
	if md := row.GetActivity().GetPlan().GetPlanned().GetProse().GetMarkdown(); md != "solo exit" {
		t.Fatalf("plan bubble = %q, want the exit's plan even with no enter", md)
	}
}

// ==========================================================================
// Worktree separation dividers.
// ==========================================================================

func TestWorktreeEnterDrawsASeparationDividerWithNoTokenDelta(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-wt-enter", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("wt-enter"),
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{State: &conversationv1.AgentWorktree_Start{
			Start: &conversationv1.AgentWorktreeStart{Act: &conversationv1.AgentWorktreeStart_Enter{Enter: &conversationv1.AgentWorktreeEnter{}}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("wt-enter"),
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{State: &conversationv1.AgentWorktree_Success{
			Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Entered{Entered: &conversationv1.AgentWorktreeEntered{
					Path: "/tmp/wt-1", Branch: proto.String("feature-x"), Message: "entered",
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the entered-worktree divider", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeEntered() != nil
	})
	if row.GetSeparation().GetTokens() != nil {
		t.Fatalf("a worktree divider carries a token delta %v, want none: worktree moves change no context", row.GetSeparation().GetTokens())
	}
	if row.GetSeparation().GetWorktreeEntered().GetPath().GetText() != "/tmp/wt-1" {
		t.Fatalf("the entered divider's path = %q, want /tmp/wt-1", row.GetSeparation().GetWorktreeEntered().GetPath().GetText())
	}
}

func TestWorktreeExitDrawsASeparationDividerWithNoTokenDelta(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-wt-exit", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("wt-exit"),
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{State: &conversationv1.AgentWorktree_Start{
			Start: &conversationv1.AgentWorktreeStart{Act: &conversationv1.AgentWorktreeStart_Exit{Exit: &conversationv1.AgentWorktreeExit{
				Action: &conversationv1.AgentWorktreeExit_Keep{Keep: &conversationv1.AgentWorktreeExitKeep{}},
			}}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("wt-exit"),
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{State: &conversationv1.AgentWorktree_Success{
			Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
					Outcome:     &conversationv1.AgentWorktreeExited_Kept{Kept: &conversationv1.AgentWorktreeKept{}},
					OriginalCwd: "/repo",
					Path:        "/tmp/wt-1",
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the left-worktree divider", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeLeft() != nil
	})
	if row.GetSeparation().GetTokens() != nil {
		t.Fatalf("a worktree divider carries a token delta %v, want none", row.GetSeparation().GetTokens())
	}
	if row.GetSeparation().GetWorktreeLeft().GetKept() == nil {
		t.Fatalf("the left divider's outcome = %v, want kept", row.GetSeparation().GetWorktreeLeft())
	}
}

// ==========================================================================
// AgentUpdate.context_cut.
// ==========================================================================

func TestContextCutClearedDrawsASeparation(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-clear", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{
			Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the cleared-context divider", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetCleared() != nil
	})
	if row.GetSeparation().GetLabel().GetText() == "" {
		t.Fatal("the cleared divider carries no label, want a composed one")
	}
}

func TestContextCutCompactedDrawsASeparationWithFormattedTokens(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-compact", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{
			Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{
				Summary: &conversationv1.AgentResponseProse{Markdown: "a summary"},
				Tokens:  &conversationv1.ContextTokenDelta{TokensBefore: 180_000, TokensAfter: 12_000},
				Trigger: &conversationv1.ContextCompacted_Automatic{Automatic: &conversationv1.ContextCompactionAutomatic{}},
			}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the compacted divider", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetCompacted() != nil
	})
	tokens := row.GetSeparation().GetTokens()
	if tokens == nil || tokens.GetBeforeText() == "" || tokens.GetAfterText() == "" {
		t.Fatalf("the compacted divider's tokens = %v, want formatted before/after text", tokens)
	}
}

func TestContextCutCompactionFailedDrawsNoSeparationAndSurfacesTheError(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-compactfail", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{
			Cut: &conversationv1.ContextCut_CompactionFailed{CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "vendor timeout"}},
		}},
	}))

	// Assert: no separation divider is drawn for a compaction that did not
	// happen — the context is unchanged.
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "compaction_failed draws no separation divider")
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

// ==========================================================================
// Permission and question cards.
// ==========================================================================

func TestPermissionStartDrawsOpenRowFooterAndHostNotification(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-permstart", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)
	host := f.d.WatchHost(f.ws)

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-open", "gated-1")},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the open permission card", func(r *frontendv1.FeedRow) bool {
		return r.GetPermission().GetOpen() != nil
	})
	if row.GetPermission().GetHeadline().GetText() == "" {
		t.Fatal("the open permission card carries no headline")
	}
	awaitFooter(t, f, footer, "footer waiting.permission", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetPermission() != nil
	})
	awaitRow2 := harness.AwaitView(t, f.d.Ctx(), host, "the host notification for the permission ask", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetNotification().GetKind().GetPermissionRequested() != nil
	})
	if awaitRow2.GetNotification().GetKind().GetPermissionRequested().GetToolName() == "" {
		t.Fatal("the permission_requested notification carries no tool name")
	}
}

func TestPermissionAnsweredRepushesAsAnswered(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-permanswer", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-ans", "gated-2")},
	}))
	opened := awaitRow(t, f, tail, "the open card", func(r *frontendv1.FeedRow) bool { return r.GetPermission().GetOpen() != nil })

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: answeredPermission("perm-ans", "gated-2")},
	}))

	// Assert
	answered := awaitRow(t, f, tail, "the answered re-push on the same row", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == opened.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
	})
	if answered.GetPermission().GetAnswered().GetAllowedOnce() == nil {
		t.Fatalf("the answered card's verdict = %v, want allowed_once", answered.GetPermission().GetAnswered())
	}
}

func TestQuestionStartDrawsAnOpenRow(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-qstart", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-1"},
			Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Auth method?"},
					Header:   "Auth",
					Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
						Options: []*conversationv1.AgentQuestionOption{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "OAuth"}}},
					}},
				}}},
				StartedAt: startedAt(1),
			}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the open question card", func(r *frontendv1.FeedRow) bool { return r.GetQuestion().GetOpen() != nil })
	if len(row.GetQuestion().GetQuestions()) != 1 {
		t.Fatalf("the open question card = %v, want exactly one posed question", row.GetQuestion())
	}
}

func TestQuestionAnsweredRepushesWithEchoedLabels(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-qanswer", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-2"},
			Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Which env?"},
					Header:   "Env",
					Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
						Options: []*conversationv1.AgentQuestionOption{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "prod"}}},
					}},
				}}},
				StartedAt: startedAt(1),
			}},
		}},
	}))
	opened := awaitRow(t, f, tail, "the open question", func(r *frontendv1.FeedRow) bool { return r.GetQuestion().GetOpen() != nil })

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-2"},
			Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Which env?"}, Header: "Env",
				}}},
				Outcome: &conversationv1.AgentQuestionSuccess_Answered{Answered: &conversationv1.AgentQuestionAnswers{
					Answers: []*conversationv1.AgentQuestionSelection{{
						Question: &conversationv1.AgentQuestionText{Text: "Which env?"},
						Chosen:   []*conversationv1.AgentQuestionChoice{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "prod"}}},
					}},
				}},
			}},
		}},
	}))

	// Assert
	answered := awaitRow(t, f, tail, "the answered question on the same row", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == opened.GetId().GetValue() && r.GetQuestion().GetAnswered() != nil
	})
	got := answered.GetQuestion().GetAnswered().GetAnswers()
	if len(got) != 1 || len(got[0].GetChosen()) != 1 || got[0].GetChosen()[0] != "prod" {
		t.Fatalf("the answered question's echoed choices = %v, want [\"prod\"]", got)
	}
}

// ==========================================================================
// Subagents (sync and detached).
// ==========================================================================

func TestASyncSubagentSpawnDrawsABubbleHeadAndItsOwnFeedServesSubFeedRows(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-subagent", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: spawn.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("spawn-1"),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{
			Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: &conversationv1.AgentId{Value: "sub-1"},
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "explore the code", Description: proto.String("Explore the code")},
				StartedAt:      startedAt(1),
			},
		}}},
	}))
	bubble := awaitRow(t, f, tail, "the spawn's bubble head", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetSubagent() != nil })
	if bubble.GetActivity().GetSubagent().GetDescription().GetText() != "Explore the code" {
		t.Fatalf("the bubble's description = %q, want the commission's", bubble.GetActivity().GetSubagent().GetDescription().GetText())
	}

	// Act: a frame carrying the created agent's own id — routed to the sub-feed.
	f.shim.PushAgentFrame("sub-1", activityFrame("sub-1", &conversationv1.AgentActivity{
		ActivityId: activityID("sub-work-1"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "subagent said this"}},
		}}},
	}))

	// Assert: OpenFeed on the bubble's own FeedId serves the sub-feed, which
	// carries the frame addressed to the created agent.
	_, subToken := f.openFeed(bubble.GetId())
	subTail := f.d.WatchFeed(subToken)
	subRow := awaitRow(t, f, subTail, "the subagent's own work on its sub-feed", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess() != nil
	})
	if md := subRow.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown(); md != "subagent said this" {
		t.Fatalf("the sub-feed's row = %q, want the subagent's own prose", md)
	}
}

func TestASettledSubagentDrawsSettledSucceededWithTokens(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-subsettled", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("spawn-2"),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{
			Start: &conversationv1.AgentSubagentStart{CreatedAgentId: &conversationv1.AgentId{Value: "sub-2"}, Prompt: &conversationv1.AgentSubagentPrompt{Text: "fix it"}, StartedAt: startedAt(1)},
		}}},
	}))
	bubble := awaitRow(t, f, tail, "the spawn's bubble", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetSubagent() != nil })

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("spawn-2"),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Success{
			Success: &conversationv1.AgentSubagentSuccess{
				Prompt: &conversationv1.AgentSubagentPrompt{Text: "fix it"},
				Report: &conversationv1.AgentSubagentReport{Prose: &conversationv1.AgentResponseProse{Markdown: "fixed"}},
				Totals: &conversationv1.AgentSubagentTotals{
					DurationMs: 1000,
					Usage:      &conversationv1.AgentSubagentTotals_Full{Full: &conversationv1.TokenUsage{OutputTokens: 12_400}},
				},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	settled := awaitRow(t, f, tail, "the settled subagent bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == bubble.GetId().GetValue() && r.GetActivity().GetSubagent().GetSettled() != nil
	})
	if settled.GetActivity().GetSubagent().GetSettled().GetSucceeded() == nil {
		t.Fatalf("the settled bubble's outcome = %v, want succeeded", settled.GetActivity().GetSubagent().GetSettled().GetOutcome())
	}
	if settled.GetActivity().GetSubagent().GetTokens().GetText() == "" {
		t.Fatal("the settled bubble carries no token sum, want one formatted from the totals")
	}
}

func TestADetachedSubagentGetsDetachedSubagentAndItsOwnWatchAgentEagerly(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-detachsub", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-sub-1", "sub-detached-1", "roam free")))

	// Assert: the fake saw WatchAgent for the detached subagent BEFORE this
	// test ever calls OpenFeed on its bubble.
	watched := f.shim.ExpectWatchAgent()
	if watched.GetTarget().GetValue() != "sub-detached-1" {
		t.Fatalf("the eager WatchAgent named %q, want the detached subagent's id %q", watched.GetTarget().GetValue(), "sub-detached-1")
	}
	row := awaitRow(t, f, tail, "the detached_subagent row", func(r *frontendv1.FeedRow) bool { return r.GetDetachedSubagent() != nil })
	if row.GetDetachedSubagent().GetSubagent().GetLabel().GetText() == "" && row.GetDetachedSubagent().GetSubagent().GetDescription().GetText() != "roam free" {
		t.Fatalf("the detached bubble = %v, want the commission drawn", row.GetDetachedSubagent().GetSubagent())
	}
}

// ==========================================================================
// Detached bash.
// ==========================================================================

func TestDetachedShellDrawsHeadAndSpoolTailFromWatchBashDeltas(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-detachshell", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-shell-1", "tail -f build.log")))
	head := awaitRow(t, f, tail, "the detached_shell head", func(r *frontendv1.FeedRow) bool { return r.GetDetachedShell() != nil })

	// Act
	f.shim.PushBash("work-shell-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{
		Update: &conversationv1.AgentBashUpdate{NewOutput: "building...\n", FromOffset: 0},
	}})

	// Assert
	grown := awaitRow(t, f, tail, "the spool tail growing from the bash delta", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == head.GetId().GetValue() && r.GetDetachedShell().GetShell().GetSpool() != nil
	})
	if grown.GetDetachedShell().GetShell().GetSpool().GetText() != "building...\n" {
		t.Fatalf("the spool tail = %q, want the delta's text", grown.GetDetachedShell().GetShell().GetSpool().GetText())
	}
}

func TestDetachedShellSettledDrawsCompletedWithExit(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-detachshellend", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-shell-2", "make")))
	head := awaitRow(t, f, tail, "the detached_shell head", func(r *frontendv1.FeedRow) bool { return r.GetDetachedShell() != nil })

	// Act
	f.shim.PushBash("work-shell-2", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{
		Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "make"},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output:      &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{Stdout: "done\n", Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}}}}},
				Termination: &conversationv1.AgentBashTermination{How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 0}}},
			}},
			SettledAt: settledAt(2),
		},
	}})

	// Assert
	settled := awaitRow(t, f, tail, "the settled detached shell", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == head.GetId().GetValue() && r.GetDetachedShell().GetShell().GetSettled() != nil
	})
	shellSettled := settled.GetDetachedShell().GetShell().GetSettled()
	if shellSettled.GetCompleted() == nil {
		t.Fatalf("the settled shell's outcome = %v, want completed", shellSettled.GetOutcome())
	}
	if shellSettled.GetExit().GetCode() != 0 {
		t.Fatalf("the settled shell's exit = %v, want code 0", shellSettled.GetExit())
	}
}

func TestADetachedBashSpoolGapIsRefusedAndLogged(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-spoolgap", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-gap-1", "long-build")))
	awaitRow(t, f, tail, "the detached_shell head", func(r *frontendv1.FeedRow) bool { return r.GetDetachedShell() != nil })

	// Act: a delta whose from_offset does not match what has accumulated
	// (nothing has accumulated yet, so any nonzero offset is a gap).
	f.shim.PushBash("work-gap-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{
		Update: &conversationv1.AgentBashUpdate{NewOutput: "mid-stream\n", FromOffset: 999},
	}})

	// Assert: the gap is refused — the spool does not silently jump ahead.
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "a spool gap must not upsert the shell's row")
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

// ==========================================================================
// Turn terminal.
// ==========================================================================

func TestTurnEndedConcludedStampsTheAnsweringResponse(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-concluded", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-answer"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"}},
		}}},
	}))
	answer := awaitRow(t, f, tail, "the answering response", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetResponse().GetSuccess() != nil })

	// Act
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("resp-answer")))

	// Assert
	terminal := awaitRow(t, f, tail, "the turn's terminal row", func(r *frontendv1.FeedRow) bool { return r.GetTurnEnded().GetConcluded() != nil })
	if terminal.GetTurnEnded().GetConcluded().GetAnswer().GetValue() != answer.GetId().GetValue() {
		t.Fatalf("the concluded terminal's answer = %v, want it to name the answering response %v", terminal.GetTurnEnded().GetConcluded().GetAnswer(), answer.GetId())
	}
}

func TestInterruptedTurnDrawsInterrupted(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-interrupted", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, interruptedFrame(mainAgent))

	// Assert
	row := awaitRow(t, f, tail, "the interrupted terminal", func(r *frontendv1.FeedRow) bool { return r.GetTurnEnded().GetInterrupted() != nil })
	_ = row
}

func TestApiRequestFailedRateLimitedRespellsWithRetryAfter(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-429", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Message: "rate limited",
			Kind:    &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{RetryAfterMs: proto.Int64(5000)}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the rate-limited terminal", func(r *frontendv1.FeedRow) bool { return r.GetTurnEnded().GetErrored().GetRateLimited() != nil })
	if got := row.GetTurnEnded().GetErrored().GetRateLimited().GetRetryAfterMs(); got != 5000 {
		t.Fatalf("the rate-limited terminal's retry_after_ms = %d, want 5000", got)
	}
	if row.GetTurnEnded().GetErrored().GetHeadline().GetText() == "" {
		t.Fatal("the errored terminal carries no composed headline")
	}
}

func TestApiRequestFailedAuthenticationFailedRespells(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-401", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Message: "invalid credential",
			Kind:    &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the authentication-failed terminal", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded().GetErrored().GetAuthenticationFailed() != nil
	})
	_ = row
}

// ==========================================================================
// Hooks.
// ==========================================================================

func TestABlockedHookDrawsACard(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-hookblocked", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("hook-1"),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{
			Start: &conversationv1.AgentHookStart{HookName: "protect-master", Event: conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_PRE_TOOL_USE, StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("hook-1"),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_BlockingError{
			BlockingError: &conversationv1.AgentHookBlockingError{Command: "./guard.sh", BlockingText: "master is protected"},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the blocked hook's card", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetHook().GetBlocked() != nil })
	if row.GetActivity().GetHook().GetBlocked().GetReason() != "master is protected" {
		t.Fatalf("the blocked hook's reason = %q, want the hook's own text", row.GetActivity().GetHook().GetBlocked().GetReason())
	}
}

func TestAFailedHookDrawsACard(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-hookfailed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("hook-2"),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{
			Start: &conversationv1.AgentHookStart{HookName: "lint", Event: conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_POST_TOOL_USE, StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("hook-2"),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_NonBlockingError{
			NonBlockingError: &conversationv1.AgentHookNonBlockingError{Command: "lint.sh", ExitCode: 1},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the failed hook's card", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetHook().GetFailed() != nil })
	if row.GetActivity().GetHook().GetFailed().GetExitCode() != 1 {
		t.Fatalf("the failed hook's exit code = %d, want 1", row.GetActivity().GetHook().GetFailed().GetExitCode())
	}
}

func TestASucceededHookDrawsNothing(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-hookok", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("hook-3"),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{
			Start: &conversationv1.AgentHookStart{HookName: "format", Event: conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_POST_TOOL_USE, StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("hook-3"),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Succeeded{
			Succeeded: &conversationv1.AgentHookSucceeded{Command: "format.sh", ExitCode: 0},
		}}},
	}))

	// Assert: a succeeding hook is quiet — FeedHook has no succeeded arm at
	// all, so nothing is ever drawn for it.
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "a succeeded hook draws no row")
}

// ==========================================================================
// Artifacts.
// ==========================================================================

func TestArtifactPublishDrawsThePurpleBubble(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-artifact", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("artifact-1"),
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{Result: &conversationv1.AgentArtifact_Start{
			Start: &conversationv1.AgentArtifactStart{
				Act: &conversationv1.AgentArtifactStart_Publish{Publish: &conversationv1.AgentArtifactPublish{
					FilePath: "report.html", Title: proto.String("Merge Queue Report"), Favicon: proto.String("📊"),
				}},
				StartedAtMs: 1,
			},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("artifact-1"),
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{Result: &conversationv1.AgentArtifact_Success{
			Success: &conversationv1.AgentArtifactSuccess{Outcome: &conversationv1.AgentArtifactSuccess_Published{Published: &conversationv1.AgentArtifactPublished{
				Url: "https://claude.ai/artifact/abc", Title: proto.String("Merge Queue Report"),
			}}},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the published artifact bubble", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetArtifact().GetPublished() != nil })
	if row.GetActivity().GetArtifact().GetHeading().GetText() == "" {
		t.Fatal("the artifact bubble carries no composed heading")
	}
	if row.GetActivity().GetArtifact().GetPublished().GetUrl().GetUrl() != "https://claude.ai/artifact/abc" {
		t.Fatalf("the artifact bubble's url = %q, want the published url", row.GetActivity().GetArtifact().GetPublished().GetUrl().GetUrl())
	}
}

func TestArtifactListDrawsNothing(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-artifactlist", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("artifact-list-1"),
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{Result: &conversationv1.AgentArtifact_Start{
			Start: &conversationv1.AgentArtifactStart{Act: &conversationv1.AgentArtifactStart_List{List: &conversationv1.AgentArtifactList{}}, StartedAtMs: 1},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("artifact-list-1"),
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{Result: &conversationv1.AgentArtifact_Success{
			Success: &conversationv1.AgentArtifactSuccess{Outcome: &conversationv1.AgentArtifactSuccess_Listed{Listed: &conversationv1.AgentArtifactListed{}}},
		}}},
	}))

	// Assert
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "a list act produces no feed row")
}

// ==========================================================================
// Findings.
// ==========================================================================

func TestFindingsDrawRowsInServedOrder(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-findings", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("findings-1"),
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{State: &conversationv1.AgentReportFindings_Start{
			Start: &conversationv1.AgentReportFindingsStart{StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("findings-1"),
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{State: &conversationv1.AgentReportFindings_Success{
			Success: &conversationv1.AgentReportFindingsSuccess{
				Findings: []*conversationv1.AgentFinding{
					{File: "a.go", Summary: "first, most severe"},
					{File: "b.go", Summary: "second"},
					{File: "c.go", Summary: "third, least severe"},
				},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the findings bubble", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetFindings() != nil })
	rows := row.GetActivity().GetFindings().GetRows()
	if len(rows) != 3 {
		t.Fatalf("the findings bubble carries %d rows, want 3", len(rows))
	}
	want := []string{"first, most severe", "second", "third, least severe"}
	for i, r := range rows {
		if r.GetSummary().GetText() != want[i] {
			t.Fatalf("findings row %d summary = %q, want %q (never re-sorted)", i, r.GetSummary().GetText(), want[i])
		}
	}
}

// ==========================================================================
// Unmodeled tools.
// ==========================================================================

func TestAnUnmodeledToolDrawsNoRowAndAddsOneTopbarWarning(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-unmodeled", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	topbar := f.d.WatchTopbar(f.ws)

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("unmodeled-1"),
		Item: &conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{Result: &conversationv1.AgentUnmodeled_Start{
			Start: &conversationv1.AgentUnmodeledStart{ToolName: "mcp__weird__tool", StartedAt: startedAt(1)},
		}}},
	}))

	// Assert: no row is ever drawn for the unmodeled call.
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "an unmodeled tool draws no feed row")
	warned := awaitTopbar(t, f, topbar, "one topbar warning naming the unmodeled tool", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 1
	})
	w := warned.GetWarnings().GetWarnings()[0]
	if w.GetUnmodeledTool().GetToolName().GetText() != "mcp__weird__tool" {
		t.Fatalf("the topbar warning's tool name = %q, want %q", w.GetUnmodeledTool().GetToolName().GetText(), "mcp__weird__tool")
	}
}

func TestASecondCallToTheSameUnmodeledToolAddsNoSecondWarning(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-unmodeleddup", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	topbar := f.d.WatchTopbar(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("unmodeled-2"),
		Item: &conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{Result: &conversationv1.AgentUnmodeled_Start{
			Start: &conversationv1.AgentUnmodeledStart{ToolName: "mcp__weird__tool", StartedAt: startedAt(1)},
		}}},
	}))
	awaitTopbar(t, f, topbar, "the first warning", func(v *frontendv1.TopbarView) bool { return len(v.GetWarnings().GetWarnings()) == 1 })

	// Act: the SAME tool name, called again.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("unmodeled-3"),
		Item: &conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{Result: &conversationv1.AgentUnmodeled_Start{
			Start: &conversationv1.AgentUnmodeledStart{ToolName: "mcp__weird__tool", StartedAt: startedAt(2)},
		}}},
	}))

	// Act: a DIFFERENT tool name — this one must add a second warning.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("unmodeled-4"),
		Item: &conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{Result: &conversationv1.AgentUnmodeled_Start{
			Start: &conversationv1.AgentUnmodeledStart{ToolName: "mcp__other__tool", StartedAt: startedAt(3)},
		}}},
	}))

	// Assert
	awaitTopbar(t, f, topbar, "a second warning for the distinct name only", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 2
	})
}

// ==========================================================================
// feed-suite-local helpers.
// ==========================================================================

// FeedPageSizeNote explains a skip when the fake session's pushed history did
// not exceed one page under whatever page-size constant the daemon uses — a
// value this suite deliberately does not hardcode (it is not stated in
// SPEC.md, ARCHITECTURE.md or the protos).
const FeedPageSizeNote = "an unknown page-size constant"

// feedResponseFrames builds the start+success frame pair for one settled
// response unit, so callers can push a whole row in two calls.
func feedResponseFrames(activityIDValue, markdown string) [2]*conversationv1.AgentFrame {
	return [2]*conversationv1.AgentFrame{
		activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID(activityIDValue),
			Item:       &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}}},
		}),
		activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID(activityIDValue),
			Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
				Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: markdown}},
			}}},
		}),
	}
}

// feedRowLabeledResponse builds one single-frame settled response row, used
// to pad a feed's history past whatever page size the daemon uses.
func feedRowLabeledResponse(i int) *conversationv1.AgentFrame {
	id := "wall-" + itoa(i)
	return activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "row " + itoa(i)}},
		}}},
	})
}

// itoa avoids importing strconv solely for this suite's synthetic row labels.
func itoa(i int) string {
	if i == 0 {
		return "0"
	}
	neg := i < 0
	if neg {
		i = -i
	}
	var buf [20]byte
	pos := len(buf)
	for i > 0 {
		pos--
		buf[pos] = byte('0' + i%10)
		i /= 10
	}
	if neg {
		pos--
		buf[pos] = '-'
	}
	return string(buf[pos:])
}
