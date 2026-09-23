package feed

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/paint"
	"claude-repld/internal/sessionwatcher"
)

// ---- the harness ----

// testWorkspace is the workspace every test in this package resolves against.
const testWorkspace ids.WorkspaceID = "ws-1"

// fakeSurfaces is a dlog.Surfaces whose every sink is one capturing logger, so
// a test can assert the canonical record of a branch without a real log file.
type fakeSurfaces struct {
	log *dlog.TestLogger
	// dirErr, when set, makes Workspace refuse — the invariant-violation path.
	dirErr error
}

// Global implements dlog.Surfaces.
func (f *fakeSurfaces) Global() dlog.Logger { return f.log }

// Workspace implements dlog.Surfaces.
func (f *fakeSurfaces) Workspace(dir string) (dlog.Logger, error) {
	if f.dirErr != nil {
		return nil, f.dirErr
	}
	return f.log, nil
}

// WorkspaceOrCentral implements dlog.Surfaces: the workspace's logger when it
// resolves, and the global one when it does not.
func (f *fakeSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	log, err := f.Workspace(dir)
	if err != nil {
		return f.Global()
	}
	return log
}

// ShimSink implements dlog.Surfaces.
func (f *fakeSurfaces) ShimSink(dir string) (dlog.Borrowed, error) { return nil, nil }

// BindWorkspaceIDs implements dlog.Surfaces. This double answers its own
// workspace ids, so there is no lookup to install.
func (f *fakeSurfaces) BindWorkspaceIDs(dlog.WorkspaceIDLookup) {}

func (f *fakeSurfaces) ShimRollRequests() <-chan dlog.ShimRollRequest { return nil }

// ClientLog implements dlog.Surfaces.
func (f *fakeSurfaces) ClientLog(dir string, record dlog.ClientRecord) error { return nil }

// Evict implements dlog.Surfaces.
func (f *fakeSurfaces) Evict(dir string) error { return nil }

// Close implements dlog.Surfaces.
func (f *fakeSurfaces) Close() error { return nil }

// fakePainter emits one span per call, tagged so a test can prove the read
// card went through the painter rather than around it.
type fakePainter struct {
	// err, when set, makes Highlight refuse.
	err error
	// lastLanguage records the grammar the resolver chose.
	lastLanguage string
}

// ParseANSI implements paint.Painter.
func (p *fakePainter) ParseANSI(text string) (paint.Spans, error) {
	return paint.Spans{{Text: text}}, nil
}

// Highlight implements paint.Painter.
func (p *fakePainter) Highlight(language, code string) (paint.Spans, error) {
	p.lastLanguage = language
	if p.err != nil {
		return nil, p.err
	}
	return paint.Spans{{Text: code, Class: "keyword"}}, nil
}

// harness is one resolver under test plus what a test needs to inspect it.
type harness struct {
	t        *testing.T
	resolver *resolver
	log      *dlog.TestLogger
	painter  *fakePainter
	// nowMs is the injected clock, advanced explicitly rather than slept on.
	nowMs int64
	// cutSeq mints a distinct store position per context cut for the helper
	// that does not name one, so two ordinary cuts in one test are two
	// entries rather than the same entry twice.
	cutSeq int
	// ported is the fork's ported parent conversation the resolver reads at
	// the opening history page; portedErr fails that read.
	ported    []PortedPrompt
	portedErr error
	// faults is the fake fault record every raise in this resolver lands in.
	faults *fakeFaults
	// clock is the fake AfterFunc: stall windows are armed into it and fired
	// by the test, never waited on.
	clock *fakeStallClock
	// placed are the entry placements Deps.EntryPlaced was told, in order.
	placed []placedEntry
}

// placedEntry is one Deps.EntryPlaced call.
type placedEntry struct {
	unit string
	row  string
}

// newHarness builds a resolver with deterministic dependencies: a fixed clock,
// a test-local FeedId encoder (feedid is a leaf landing in parallel, so the
// resolver must be provable without it), and a painter that tags its spans.
func newHarness(t *testing.T) *harness {
	t.Helper()
	log := dlog.NewTestLogger()
	painter := &fakePainter{}
	h := &harness{
		t: t, log: log, painter: painter, nowMs: 1_700_000_000_000,
		faults: &fakeFaults{}, clock: &fakeStallClock{},
	}

	resolver, err := newResolver(Deps{
		Log:          &fakeSurfaces{log: log},
		WorkspaceDir: func(ids.WorkspaceID) (string, error) { return "/tmp/ws", nil },
		Encode:       testEncode,
		EncodeFeed:   testEncodeFeed,
		Painter:      painter,
		ResolveImage: func(block *conversationv1.ImageBlock) (string, string, error) {
			return "https://host/img", "screenshot.png", nil
		},
		PortedPrompts: func(context.Context, ids.WorkspaceID) ([]PortedPrompt, error) {
			return h.ported, h.portedErr
		},
		Now:       func() time.Time { return time.UnixMilli(h.nowMs) },
		AfterFunc: h.clock.AfterFunc,
		Faults:    h.faults,
		PageSize:  3,
		EntryPlaced: func(_ ids.WorkspaceID, unit string, row *frontendv1.FeedId) {
			h.placed = append(h.placed, placedEntry{unit: unit, row: row.GetValue()})
		},
	})
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}
	h.resolver = resolver
	return h
}

func TestFeedDecisionsRecordTheirSelectedBranches(t *testing.T) {
	tests := []struct {
		name      string
		condition string
		act       func(*resolver)
	}{
		{
			name: "a cached workspace logger is selected", condition: "cached logger",
			act: func(r *resolver) { r.logger(testWorkspace); r.logger(testWorkspace) },
		},
		{
			name: "an existing workspace state is selected", condition: "ok",
			act: func(r *resolver) { r.state(testWorkspace); r.state(testWorkspace) },
		},
		{
			name: "a nonempty feed identity is selected", condition: "id.GetValue() != \"\"",
			act: func(r *resolver) { r.feedKey(testWorkspace, rootFeed()) },
		},
		{
			name: "an unset output address selects the root", condition: "s.address == nil",
			act: func(r *resolver) { r.outputPlacement(r.state(testWorkspace)) },
		},
		{
			name: "a missing row selects the no-retirement path", condition: "_, ok := f.rows[id]; !ok",
			act: func(r *resolver) { r.retire(r.state(testWorkspace), rootFeed(), "missing") },
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			tt.act(h.resolver)

			// Assert.
			for _, record := range h.records() {
				if record.Level != "debug" || record.Operation != "daemon.feed.row_decision" {
					continue
				}
				if tt.condition == "cached logger" && record.Context["cached"] == true {
					return
				}
				if record.Context["condition"] == tt.condition {
					return
				}
			}
			t.Fatalf("records = %+v, want feed decision %q", h.records(), tt.condition)
		})
	}
}

// testEncode is a deterministic, delimiter-safe stand-in for feedid.Encode:
// the same Ref always yields the same value, which is the only property this
// package's tests depend on.
func testEncode(ref feedid.Ref) *frontendv1.FeedId {
	return &frontendv1.FeedId{Value: fmt.Sprintf("row|%s|%s|%s|%s|%s",
		ref.WS, testFeedValue(ref.Feed), ref.Row.Kind, ref.Row.ID, ref.Row.Sub)}
}

// testEncodeFeed is the same for feedid.EncodeFeed.
func testEncodeFeed(ws ids.WorkspaceID, feed feedid.Feed) *frontendv1.FeedId {
	return &frontendv1.FeedId{Value: fmt.Sprintf("feed|%s|%s", ws, testFeedValue(feed))}
}

// testFeedValue renders a feed address.
func testFeedValue(feed feedid.Feed) string {
	switch {
	case feed.Merge != nil:
		return "merge:" + string(*feed.Merge)
	case feed.Agent != nil:
		return "agent:" + feed.Agent.GetValue()
	case feed.Shell != nil:
		return "shell:" + string(*feed.Shell)
	default:
		return "root"
	}
}

// rootFeed is the workspace's top-level feed.
func rootFeed() feedid.Feed { return feedid.Feed{Root: true} }

// noAddress is the empty output address every sink call carries when no lease
// holder has installed one.
func noAddress() sessionwatcher.OutputAddress {
	return sessionwatcher.OutputAddress{Feed: rootFeed()}
}

// mainAgent is the agent id the harness treats as the main thread.
func mainAgent() *conversationv1.AgentId { return &conversationv1.AgentId{Value: "agent-main"} }

// rows returns one feed's rows in order, for assertions.
func (h *harness) rows(feed feedid.Feed) []*frontendv1.FeedRow {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)
	f := h.resolver.feed(s, feed)
	out := make([]*frontendv1.FeedRow, 0, len(f.order))
	for _, id := range f.order {
		out = append(out, f.rows[id])
	}
	return out
}

// only returns the single row a feed holds, failing when there is not exactly
// one — a family test wants to assert a row, not find one.
func (h *harness) only(feed feedid.Feed) *frontendv1.FeedRow {
	h.t.Helper()
	rows := h.rows(feed)
	if len(rows) != 1 {
		h.t.Fatalf("rows = %d, want exactly 1", len(rows))
	}
	return rows[0]
}

// records returns every captured log record.
func (h *harness) records() []dlog.Record { return h.log.Records() }

// hasRecord reports whether a record with this level and operation was
// captured.
func (h *harness) hasRecord(level, operation string) bool {
	for _, record := range h.records() {
		if record.Level == level && record.Operation == operation {
			return true
		}
	}
	return false
}

// pageRows returns a served page's rows, failing when the page is an error.
func pageRows(t *testing.T, page *frontendv1.FeedPage) []*frontendv1.FeedRow {
	t.Helper()
	success, ok := page.GetResult().(*frontendv1.FeedPage_Success)
	if !ok {
		t.Fatalf("page = %T, want a success", page.GetResult())
	}
	return success.Success.GetRows()
}

// rowIDs renders rows as their identities, which is what an ordering assertion
// actually cares about.
func rowIDs(rows []*frontendv1.FeedRow) []string {
	out := make([]string, 0, len(rows))
	for _, row := range rows {
		out = append(out, row.GetId().GetValue())
	}
	return out
}

// ---- the resolver's own surface ----

func TestNewRefusesWithoutALogSurface(t *testing.T) {
	// Arrange, Act.
	_, err := New(Deps{WorkspaceDir: func(ids.WorkspaceID) (string, error) { return "", nil }})

	// Assert.
	if err == nil {
		t.Fatal("New succeeded with no log surface, want a refusal")
	}
}

func TestNewRefusesWithoutAWorkspaceDirResolver(t *testing.T) {
	// Arrange, Act.
	_, err := New(Deps{Log: &fakeSurfaces{log: dlog.NewTestLogger()}})

	// Assert.
	if err == nil {
		t.Fatal("New succeeded with no workspace-dir resolver, want a refusal")
	}
}

func TestUnresolvableWorkspaceIsRecordedAsAnInvariantViolation(t *testing.T) {
	// Arrange: a surface whose workspace sink refuses.
	log := dlog.NewTestLogger()
	r, err := newResolver(Deps{
		Log:          &fakeSurfaces{log: log, dirErr: fmt.Errorf("no sink")},
		WorkspaceDir: func(ids.WorkspaceID) (string, error) { return "/tmp/ws", nil },
		Encode:       testEncode,
		EncodeFeed:   testEncodeFeed,
	})
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}

	// Act.
	r.mu.Lock()
	r.logger(testWorkspace)
	r.mu.Unlock()

	// Assert: an ERROR naming the violation, not a silent global write.
	found := false
	for _, record := range log.Records() {
		if record.Level == "error" && record.Operation == "daemon.feed.workspace_sink_unavailable" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.workspace_sink_unavailable", log.Records())
	}
}

// TestAWorkspaceLookupFailureIsClassifiedByItsCause separates the two things a
// failed workspace lookup can mean. The lookup runs under the DAEMON'S OWN
// context, so a cancelled read is the daemon shutting down -- a fact, recorded
// at INFO, whose global fallback is not cached because the workspace is still
// perfectly resolvable. Any other cause is the invariant violation it always
// was: an ERROR, with the global fallback cached.
func TestAWorkspaceLookupFailureIsClassifiedByItsCause(t *testing.T) {
	tests := []struct {
		name      string
		cause     error
		wantLevel string
		wantOp    string
		wantCache bool
	}{
		{
			name:      "the daemon's context was cancelled",
			cause:     context.Canceled,
			wantLevel: "info",
			wantOp:    "daemon.feed.workspace_lookup_cancelled",
			wantCache: false,
		},
		{
			name:      "the daemon's context hit its deadline",
			cause:     context.DeadlineExceeded,
			wantLevel: "info",
			wantOp:    "daemon.feed.workspace_lookup_cancelled",
			wantCache: false,
		},
		{
			name:      "the workspace is genuinely unresolvable",
			cause:     errors.New("no such workspace"),
			wantLevel: "error",
			wantOp:    "daemon.feed.workspace_unresolved",
			wantCache: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			r, err := newResolver(Deps{
				Log:          &fakeSurfaces{log: log},
				WorkspaceDir: func(ids.WorkspaceID) (string, error) { return "", tc.cause },
				Encode:       testEncode,
				EncodeFeed:   testEncodeFeed,
			})
			if err != nil {
				t.Fatalf("newResolver: %v", err)
			}

			// Act.
			r.mu.Lock()
			r.logger(testWorkspace)
			_, cached := r.loggers[testWorkspace]
			r.mu.Unlock()

			// Assert.
			found := false
			for _, record := range log.Records() {
				if record.Level == tc.wantLevel && record.Operation == tc.wantOp {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want a %s %s", log.Records(), tc.wantLevel, tc.wantOp)
			}
			if cached != tc.wantCache {
				t.Fatalf("the global fallback cached = %v, want %v", cached, tc.wantCache)
			}
		})
	}
}

func TestUpsertReplacesARowWholeAndKeepsItsPlace(t *testing.T) {
	// Arrange: two rows, then a re-push of the first.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "first")
	h.deliverPrompt("turn-2", "second")

	// Act: the first prompt arrives again with different text.
	h.deliverPrompt("turn-1", "first, corrected")

	// Assert: still two rows, and the re-pushed one did not move.
	rows := h.rows(rootFeed())
	if len(rows) != 2 {
		t.Fatalf("rows = %d, want 2 (an upsert replaces, never appends)", len(rows))
	}
	first := rows[0].GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0].GetText().GetText()
	if first != "first, corrected" {
		t.Fatalf("first row text = %q, want the corrected text", first)
	}
}

func TestRowIdentityIsStableAcrossPushes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	firstID := h.only(rootFeed()).GetId().GetValue()

	// Act.
	h.deliverPrompt("turn-1", "hello again")

	// Assert.
	if got := h.only(rootFeed()).GetId().GetValue(); got != firstID {
		t.Fatalf("id = %q, want the stable %q", got, firstID)
	}
}

func TestRetireRowRemovesItFromTheFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	id := h.only(rootFeed()).GetId()

	// Act.
	h.resolver.RetireRow(testWorkspace, rootFeed(), id)

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0 after retirement", len(rows))
	}
}

func TestOutputAddressPlacesEveryRowOnTheAddressedFeed(t *testing.T) {
	// Arrange: a merge lease's output address.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	parent := feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Merge: &lease},
		Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "lease-7", Sub: "conflicts:1"}}
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{
		Feed: feedid.Feed{Merge: &lease}, Parent: &parent,
	})

	// Act.
	h.deliverPrompt("turn-1", "resolve the conflict")

	// Assert: on the merge feed, under the addressed row, and NOT on the root.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("root rows = %d, want 0 while an output address is in force", len(rows))
	}
	row := h.only(feedid.Feed{Merge: &lease})
	if row.GetParent().GetRow().GetValue() != testEncode(parent).GetValue() {
		t.Fatalf("parent = %q, want the addressed row", row.GetParent().GetRow().GetValue())
	}
}

func TestUpsertAtOutputAddressLandsOnTheAddressedFeedAndParent(t *testing.T) {
	// Arrange: a merge lease has addressed the session at one of its tabs.
	h := newHarness(t)
	lease := ids.LeaseID("lease-9")
	parent := feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Merge: &lease},
		Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "conflicts", Sub: "1"}}
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{
		Feed: feedid.Feed{Merge: &lease}, Parent: &parent,
	})

	// Act.
	h.resolver.UpsertAtOutputAddress(testWorkspace,
		feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-1"},
		&frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{}}})

	// Assert: the row's identity is the ADDRESSED feed's, and it is parented to
	// the addressed row, so the resolver's own later draw upserts it in place.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("root rows = %d, want 0 while an output address is in force", len(rows))
	}
	row := h.only(feedid.Feed{Merge: &lease})
	want := feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Merge: &lease},
		Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-1"}}
	if row.GetId().GetValue() != testEncode(want).GetValue() {
		t.Fatalf("row id = %q, want the addressed feed's %q", row.GetId().GetValue(), testEncode(want).GetValue())
	}
	if row.GetParent().GetRow().GetValue() != testEncode(parent).GetValue() {
		t.Fatalf("parent = %q, want the addressed row", row.GetParent().GetRow().GetValue())
	}
}

func TestUpsertAtOutputAddressLandsOnTheRootWhenNoAddressStands(t *testing.T) {
	// Arrange: no lease holder has addressed the session.
	h := newHarness(t)

	// Act.
	h.resolver.UpsertAtOutputAddress(testWorkspace,
		feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-1"},
		&frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{}}})

	// Assert.
	row := h.only(rootFeed())
	want := feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Root: true},
		Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-1"}}
	if row.GetId().GetValue() != testEncode(want).GetValue() {
		t.Fatalf("row id = %q, want the root feed's %q", row.GetId().GetValue(), testEncode(want).GetValue())
	}
}

func TestClearedOutputAddressRestoresTheRootFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})

	// Act.
	h.resolver.SetOutputAddress(testWorkspace, nil)
	h.deliverPrompt("turn-1", "back on the root")

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 1 {
		t.Fatalf("root rows = %d, want 1 once the address is cleared", len(rows))
	}
}

func TestUnplaceableAgentLandsOnTheRootWithAWarning(t *testing.T) {
	// Arrange: the main agent is established first, so a second unknown agent
	// really is unplaceable rather than merely first.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act: an activity for an agent whose creation was never seen.
	h.resolver.OnActivity(testWorkspace, &conversationv1.AgentId{Value: "agent-ghost"},
		responseSuccessActivity("unit-1", "orphaned prose"), noAddress())

	// Assert: drawn on the root, and loudly.
	if rows := h.rows(rootFeed()); len(rows) != 2 {
		t.Fatalf("root rows = %d, want 2 — an unplaceable row is never dropped", len(rows))
	}
	if !h.hasRecord("warn", "daemon.feed.unplaceable_agent") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.unplaceable_agent", h.records())
	}
}

func TestStandingTokenIsRetrievableAndNeverOnTheRow(t *testing.T) {
	// Arrange: an ask the vendor offered a standing form for.
	h := newHarness(t)
	standing := &conversationv1.AgentPermissionStanding{
		Changes: []*conversationv1.AgentPermissionChange{{
			Destination: conversationv1.AgentPermissionDestination_AGENT_PERMISSION_DESTINATION_SESSION,
		}},
	}

	// Act.
	h.resolver.OnPermission(testWorkspace, mainAgent(), &conversationv1.AgentPermission{
		Id:        &conversationv1.AgentPermissionId{Value: "ask-1"},
		GatedCall: &conversationv1.AgentActivityId{Value: "unit-1"},
		Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
			Prompt:          &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
			OfferedStanding: standing,
			StartedAt:       &conversationv1.AgentActivityStartedAt{AtMs: h.nowMs},
		}},
	}, noAddress())

	// Assert: presence on the row, the token held daemon-side.
	row := h.only(rootFeed())
	if row.GetPermission().GetStandingOffered() == nil {
		t.Fatal("standing_offered is unset, want the presence marker")
	}
	held, ok := h.resolver.StandingFor(testWorkspace, row.GetId())
	if !ok || len(held.GetChanges()) != 1 {
		t.Fatalf("StandingFor = (%v, %v), want the vendor's echoed standing", held, ok)
	}
}

func TestStandingForReportsAbsenceWhenNoneWasOffered(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, ok := h.resolver.StandingFor(testWorkspace, &frontendv1.FeedId{Value: "row|nothing"})

	// Assert.
	if ok {
		t.Fatal("StandingFor reported a standing for a row that never carried one")
	}
}

// ---- fixtures the family tests share ----

// deliverPrompt sends one user prompt through the sink.
func (h *harness) deliverPrompt(turn, text string) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  mainAgent(),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
			}},
		}},
	}, noAddress())
}

// responseSuccessActivity is a settled prose block.
func responseSuccessActivity(unit, markdown string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
			}},
		}},
	}
}

// openPage opens a reader's page, failing the test on a refusal.
func (h *harness) openPage(feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken) {
	h.t.Helper()
	page, token, err := h.resolver.OpenPage(context.Background(), testWorkspace, feed, reader)
	if err != nil {
		h.t.Fatalf("OpenPage: %v", err)
	}
	return page, token
}

// ---- THE MERGE BUBBLE: synthesized by the orchestrator, placed by us ----
//
// The feed resolver is MERGE-AGNOSTIC: it honors a generic output address and
// upserts whatever row the orchestrator hands it. Only the orchestrator and the
// footer know "merge" as a concept, so these cases pin the PLACEMENT and the
// sub-feed mechanics rather than any merge composition.

func TestAMergeHeadIsUpsertedWhereTheOrchestratorPlacesIt(t *testing.T) {
	// Arrange: the orchestrator's own head row.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	head := &frontendv1.FeedId{Value: "row|merge-head"}
	row := &frontendv1.FeedRow{
		Id: head,
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{
				Head: &frontendv1.FeedMergeHead{
					Glyph:   &frontendv1.FeedMergeGlyph{Icon: "merge"},
					Label:   &frontendv1.FeedMergeLabel{Text: "DWC/fix-flaky → master"},
					Runtime: &frontendv1.FeedMergeRuntime{StartedAtMs: 1_000},
					Fold:    &frontendv1.FeedMergeFold{Folded: false},
				},
				Result: &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}},
			}},
		}},
	}

	// Act.
	h.resolver.UpsertSynthesized(testWorkspace, rootFeed(), row)
	h.resolver.MintSubFeedHead(testWorkspace, head, feedid.Feed{Root: true}, feedid.Feed{Merge: &lease}, "DWC/fix-flaky → master")

	// Assert: on the root feed, and its sub-feed is addressable.
	rows := h.rows(rootFeed())
	if len(rows) != 1 || rows[0].GetActivity().GetMerge() == nil {
		t.Fatalf("rows = %+v, want the merge head", rows)
	}
	page, _ := h.openPage(feedid.Feed{Merge: &lease}, "reader-1")
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 1 || crumbs[0].GetTarget().GetValue() != head.GetValue() {
		t.Fatalf("crumbs = %+v, want the merge head", crumbs)
	}
	if !h.hasRecord("debug", "daemon.feed.synthesized") {
		t.Fatalf("records = %+v, want the synthesized branch recorded", h.records())
	}
}

func TestMergeTabsAreAppendOnlyTopLevelRowsOfTheBubblesOwnFeed(t *testing.T) {
	// Arrange: two rounds of the tests tab — a second round is a SECOND TAB,
	// never a reopened one.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mergeFeed := feedid.Feed{Merge: &lease}

	// Act.
	for round := uint32(1); round <= 2; round++ {
		h.resolver.UpsertSynthesized(testWorkspace, mergeFeed, &frontendv1.FeedRow{
			Id: h.resolver.rowID(testWorkspace, mergeFeed, feedid.RowKey{
				Kind: feedid.KindMergeTab, ID: "lease-7", Sub: "tests:" + string(rune('0'+round)),
			}),
			Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
				Label: &frontendv1.FeedMergeTabLabel{Text: "tests", Round: round},
				Kind: &frontendv1.FeedMergeTab_Tests{Tests: &frontendv1.FeedMergeTabTests{
					State: &frontendv1.FeedMergeTabTests_Live{Live: &frontendv1.FeedMergeTabLive{}},
				}},
			}},
		})
	}

	// Assert.
	rows := h.rows(mergeFeed)
	if len(rows) != 2 {
		t.Fatalf("tabs = %d, want 2 — a second round is a second tab", len(rows))
	}
	if rows[1].GetMergeTab().GetLabel().GetRound() != 2 {
		t.Fatalf("second round = %d, want 2", rows[1].GetMergeTab().GetLabel().GetRound())
	}
	for _, row := range rows {
		if row.GetParent() != nil {
			t.Fatal("a merge tab named a parent; tabs are top-level rows of the bubble's own feed")
		}
	}
}

func TestAnAgenticTabsRowsNestUnderItByTheOutputAddress(t *testing.T) {
	// Arrange: the orchestrator addresses the conflicts tab.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mergeFeed := feedid.Feed{Merge: &lease}
	tab := feedid.Ref{WS: testWorkspace, Feed: mergeFeed,
		Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "lease-7", Sub: "conflicts:1"}}
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{
		Feed: mergeFeed, Parent: &tab,
	})

	// Act: the lease session's own conversation.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-1", "fixing TestReconnect"), noAddress())

	// Assert: the tab's content IS the sub-feed rows parented to it.
	rows := h.rows(mergeFeed)
	if len(rows) != 1 {
		t.Fatalf("rows = %d, want the agent's row", len(rows))
	}
	if rows[0].GetParent().GetRow().GetValue() != testEncode(tab).GetValue() {
		t.Fatalf("parent = %q, want the conflicts tab", rows[0].GetParent().GetRow().GetValue())
	}
}

func TestARetiredMergeRowLeavesTheFeedButNotTheAlreadyStreamedHistory(t *testing.T) {
	// Arrange: a synthesized row a reader has already been streamed.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mergeFeed := feedid.Feed{Merge: &lease}
	id := &frontendv1.FeedId{Value: "row|merge-tab"}
	h.resolver.UpsertSynthesized(testWorkspace, mergeFeed, &frontendv1.FeedRow{
		Id:  id,
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}},
	})

	// Act.
	h.resolver.RetireRow(testWorkspace, mergeFeed, id)

	// Assert: gone from the pages.
	if rows := h.rows(mergeFeed); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0 after retirement", len(rows))
	}
	if !h.hasRecord("debug", "daemon.feed.retire_row") {
		t.Fatalf("records = %+v, want the retirement recorded", h.records())
	}
}

func TestRetiringARowThatIsNotThereSaysSo(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.RetireRow(testWorkspace, rootFeed(), &frontendv1.FeedId{Value: "row|nothing"})

	// Assert: recorded rather than silently tolerated.
	var found any
	for _, record := range h.records() {
		if record.Operation == "daemon.feed.retire_row" {
			found = record.Context["found"]
		}
	}
	if found != false {
		t.Fatalf("logged found = %v, want false", found)
	}
}

func TestARowWithNoIdentityIsRefusedLoudly(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.UpsertSynthesized(testWorkspace, rootFeed(), &frontendv1.FeedRow{})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if !h.hasRecord("error", "daemon.feed.row_without_identity") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.row_without_identity", h.records())
	}
}

func TestEachWorkspaceHoldsItsOwnFeedUniverse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	other := ids.WorkspaceID("ws-2")

	// Act.
	h.deliverPrompt("turn-1", "on ws-1")
	h.resolver.OnPrompt(other, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-9"},
		Agent:  mainAgent(),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{}},
	}, noAddress())

	// Assert: one row each, and neither leaked.
	if rows := h.rows(rootFeed()); len(rows) != 1 {
		t.Fatalf("ws-1 rows = %d, want 1", len(rows))
	}
	h.resolver.mu.Lock()
	otherRows := len(h.resolver.feed(h.resolver.state(other), rootFeed()).order)
	h.resolver.mu.Unlock()
	if otherRows != 1 {
		t.Fatalf("ws-2 rows = %d, want 1", otherRows)
	}
}

// TestRowPlacedIsTracedAtFirstDraw pins the ordering-trace log: every row's
// plane and seq are logged at INFO the moment it is first placed, so a "why did
// this row sort here" question is answerable from the logs alone.
func TestRowPlacedIsTracedAtFirstDraw(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act — draw one live prompt row.
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, textBlock("hello"))

	// Assert — an INFO daemon.feed.row_placed carries the plane and a seq.
	var placed *dlog.Record
	for i := range h.records() {
		rec := h.records()[i]
		if rec.Level == "info" && rec.Operation == "daemon.feed.row_placed" {
			placed = &rec
			break
		}
	}
	if placed == nil {
		t.Fatalf("no INFO daemon.feed.row_placed record; records = %+v", h.records())
	}
	if placed.Context["plane"] != "live" {
		t.Fatalf("plane = %v, want \"live\"", placed.Context["plane"])
	}
	if placed.Context["turn"] != "turn-1" {
		t.Fatalf("turn = %v, want \"turn-1\"", placed.Context["turn"])
	}
	if _, ok := placed.Context["seq"]; !ok {
		t.Fatalf("row_placed carried no seq; context = %+v", placed.Context)
	}
}

// TestRowPlaneNames pins the human-readable plane names the trace log uses.
func TestRowPlaneNames(t *testing.T) {
	cases := []struct {
		plane rowPlane
		want  string
	}{
		{planePorted, "ported"},
		{planeHistory, "history"},
		{planeLive, "live"},
	}
	for _, tc := range cases {
		if got := tc.plane.String(); got != tc.want {
			t.Fatalf("plane %d = %q, want %q", tc.plane, got, tc.want)
		}
	}
}
