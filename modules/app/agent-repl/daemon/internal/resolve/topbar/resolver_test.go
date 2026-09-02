package topbar

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

// instant is the fixed clock every test starts from.
var instant = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// testWS is the workspace every test resolves for.
const testWS = ids.WorkspaceID("ws-1")

// fixedClock is the injected clock: pinned, so nothing depends on wall-clock.
type fixedClock struct{ now time.Time }

// Now is the pinned instant.
func (c fixedClock) Now() time.Time { return c.now }

// testColors is the render-colors vocabulary as the file declares it, so the
// tests assert against the real table rather than an invented one.
func testColors() vocab.RenderColors {
	return vocab.RenderColors{
		TopbarConnectivity: map[string]string{
			linkConnected:  "green",
			linkConnecting: "blue",
			linkSevered:    "blue",
			linkDead:       "blue",
			linkNoSession:  "none",
		},
		TopbarTones: []string{"none", "blue", "purple", "red", "yellow", "green"},
	}
}

// testSurfaces is the log surfaces every test records into.
func testSurfaces(t *testing.T) *dlog.TestSurfaces {
	t.Helper()
	return dlog.NewTestSurfaces()
}

// harness is one resolver under test with its log records.
type harness struct {
	r   *resolver
	log *dlog.TestSurfaces
}

// newHarness builds a bound resolver on the fixed clock.
func newHarness(t *testing.T, opts ...Option) *harness {
	t.Helper()
	log := dlog.NewTestSurfaces()
	all := append([]Option{WithClock(fixedClock{now: instant})}, opts...)
	r, err := newResolver(testColors(), log, all...)
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}
	if err := r.SetWorkspaceDir(testWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	return &harness{r: r, log: log}
}

// ready installs the five facts readiness needs, so a test exercising anything
// else does not have to restate them.
func (h *harness) ready(t *testing.T) {
	t.Helper()
	h.r.SetNaming(testWS, Naming{
		Title: "fix-flaky-reconnect", Branch: "main", DefaultBranch: "main",
		ConfigDir: "/Users/dev/.claude",
	})
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))
	h.r.SetAccount(testWS, "dev@example.com")
	h.r.SetPermissionModePicker(testWS, picker("default", "default", "accept_edits", "plan"))
	h.r.OnSessionUpdate(testWS, contextUsage(142_300, 200_000, 71, "claude-opus-5"))
	// The two client hops of connectivity truth are up too: a serving shim
	// link alone is not a connected workspace (daemon.md invariant 11), so a
	// test that wants one hop down states that hop itself.
	h.r.SetParticipants(testWS, true, true)
}

// view is the workspace's last published view, failing when none exists.
func (h *harness) view(t *testing.T) *frontendv1.TopbarView {
	t.Helper()
	got, ok := h.r.Topic(testWS).Latest()
	if !ok {
		t.Fatalf("no topbar view was published")
	}
	return got
}

// sessionStarted is the session's opening facts.
func sessionStarted(vendorID, model string) *conversationv1.SessionStarted {
	return &conversationv1.SessionStarted{
		VendorSessionId: vendorID,
		EffectiveModel:  &conversationv1.AgentModel{Name: model},
		PermissionMode: &conversationv1.AgentPermissionMode{
			Mode: &conversationv1.AgentPermissionMode_Default{
				Default: &conversationv1.AgentPermissionModeDefault{},
			},
		},
	}
}

// picker builds a served permission-mode picker.
func picker(current string, modes ...string) *frontendv1.TopbarPermissionModePicker {
	out := &frontendv1.TopbarPermissionModePicker{
		Current: &frontendv1.TopbarPermissionModeOption{Mode: current, DisplayName: current},
	}
	for _, mode := range modes {
		out.Options = append(out.Options,
			&frontendv1.TopbarPermissionModeOption{Mode: mode, DisplayName: mode})
	}
	return out
}

// contextUsage is the vendor's own get_context_usage answer.
func contextUsage(total, max, percentage int64, model string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{
			ContextUsage: &conversationv1.SessionContextUsage{
				TotalTokens: total, MaxTokens: max, Percentage: percentage, Model: model,
			},
		},
	}
}

// modelChanged is a later writer of the effective model.
func modelChanged(model string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ModelChanged{
			ModelChanged: &conversationv1.SessionModelChanged{
				EffectiveModel: &conversationv1.AgentModel{Name: model},
			},
		},
	}
}

// modelOption is one catalog entry.
func modelOption(name, display string) *conversationv1.ModelOption {
	return &conversationv1.ModelOption{
		Model: &conversationv1.AgentModel{Name: name}, DisplayName: display,
	}
}

func TestNewRefusesWithoutLogSurfaces(t *testing.T) {
	// Arrange, Act
	_, err := New(testColors(), nil)

	// Assert
	if err == nil {
		t.Fatalf("New returned no error; a resolver that cannot log must not be built")
	}
}

func TestNewRefusesWithoutTheConnectivityTable(t *testing.T) {
	// Arrange, Act
	_, err := New(vocab.RenderColors{}, dlog.NewTestSurfaces())

	// Assert
	if err == nil {
		t.Fatalf("New returned no error; the daemon refuses to serve an unpainted state")
	}
}

func TestNewRefusesANonPositiveWarningCap(t *testing.T) {
	// Arrange, Act
	_, err := New(testColors(), dlog.NewTestSurfaces(), WithWarningCap(0))

	// Assert
	if err == nil {
		t.Fatalf("New accepted a zero warning cap")
	}
}

func TestNothingIsPublishedBeforeEveryRequiredFact(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: everything but the context usage.
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))
	h.r.SetAccount(testWS, "dev@example.com")
	h.r.SetPermissionModePicker(testWS, picker("default", "default"))

	// Assert
	if _, ok := h.r.Topic(testWS).Latest(); ok {
		t.Fatalf("a partial view was published; absence is the legal not-yet-resolved state")
	}
}

func TestTheLastRequiredFactPublishesTheWholeView(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	view := h.view(t)
	switch {
	case view.GetTitle() == nil, view.GetSessionLine() == nil, view.GetModelSelector() == nil,
		view.GetConnectivity() == nil, view.GetWarnings() == nil, view.GetContext() == nil,
		view.GetAccount() == nil, view.GetPermissionModePicker() == nil:
		t.Fatalf("view = %+v, want every element resolved", view)
	}
}

func TestAnIncompleteWorkspaceRecordsWhatItAwaits(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})

	// Assert
	records := h.log.Records()
	last := records[len(records)-1]
	if last.Context["awaiting"] == nil {
		t.Fatalf("record = %+v, want the awaited facts named", last)
	}
}

func TestAnIdenticalRepublishIsDeduplicated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	first := h.view(t)

	// Act
	h.r.SetAccount(testWS, "dev@example.com")

	// Assert
	if h.view(t) != first {
		t.Fatalf("an identical view was republished; the topic deduplicates by proto.Equal")
	}
}

func TestTheTitleIsTheNameAloneOnTheDefaultBranch(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetNaming(testWS, Naming{
		Title: "fix-flaky-reconnect", Branch: "main", DefaultBranch: "main", ConfigDir: "/root",
	})

	// Assert
	if got := h.view(t).GetTitle().GetText(); got != "fix-flaky-reconnect" {
		t.Fatalf("title = %q, want the name alone: the default branch says nothing", got)
	}
}

func TestTheTitleShowsABranchThatDiffersFromTheDefault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetNaming(testWS, Naming{
		Title: "DWC", Branch: "fix-flaky-reconnect", DefaultBranch: "main", ConfigDir: "/root",
	})

	// Assert
	if got := h.view(t).GetTitle().GetText(); got != "DWC · fix-flaky-reconnect" {
		t.Fatalf("title = %q, want the name and the branch", got)
	}
}

func TestTheTitleFallsBackToTheSlug(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetNaming(testWS, Naming{Slug: "ws-slug", DefaultBranch: "main", ConfigDir: "/root"})

	// Assert
	if got := h.view(t).GetTitle().GetText(); got != "ws-slug" {
		t.Fatalf("title = %q, want the slug when no display title was set", got)
	}
}

func TestTheSessionLineNamesTheSessionTheRootAndTheModel(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	want := "vend-1 · /Users/dev/.claude · claude-opus-5"
	if got := h.view(t).GetSessionLine().GetText(); got != want {
		t.Fatalf("session line = %q, want %q", got, want)
	}
}

func TestARotatedIdentityReachesTheSessionLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_IdentityRotated{
			IdentityRotated: &conversationv1.SessionIdentityRotated{
				PreviousVendorSessionId: "vend-1", VendorSessionId: "vend-2",
			},
		},
	})

	// Assert
	if got := h.view(t).GetSessionLine().GetText(); !contains(got, "vend-2") {
		t.Fatalf("session line = %q, want the rotated identity", got)
	}
}

func TestTheOpeningModelIsTheFirstWriter(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{
		modelOption("claude-opus-5", "Opus 5"),
		modelOption("claude-haiku-4-5", "Haiku 4.5"),
	})

	// Act
	h.ready(t)

	// Assert
	selected := h.view(t).GetModelSelector().GetSelected()
	if selected.GetDisplayName() != "Opus 5" {
		t.Fatalf("selected = %+v, want the opening effective model", selected)
	}
}

func TestTheLastModelChangeWins(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{
		modelOption("claude-opus-5", "Opus 5"),
		modelOption("claude-haiku-4-5", "Haiku 4.5"),
	})
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, modelChanged("claude-haiku-4-5"))
	h.r.OnSessionUpdate(testWS, modelChanged("claude-opus-5"))
	h.r.OnSessionUpdate(testWS, modelChanged("claude-haiku-4-5"))

	// Assert
	selected := h.view(t).GetModelSelector().GetSelected()
	if selected.GetDisplayName() != "Haiku 4.5" {
		t.Fatalf("selected = %+v, want the LAST model the shim stated", selected)
	}
}

func TestASelectionOutsideTheCatalogIsStillDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{modelOption("claude-opus-5", "Opus 5")})
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, modelChanged("claude-experimental-9"))

	// Assert
	selector := h.view(t).GetModelSelector()
	if selector.GetSelected().GetModel().GetName() != "claude-experimental-9" {
		t.Fatalf("selected = %+v, want the model the vendor says is running", selector.GetSelected())
	}
	if len(selector.GetOptions()) != 1 {
		t.Fatalf("options = %d, want the catalog unchanged", len(selector.GetOptions()))
	}
}

func TestTheSelectorRendersExactlyTheServedCatalog(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{
		modelOption("a", "A"), modelOption("b", "B"), modelOption("c", "C"),
	})
	h.ready(t)

	// Assert
	options := h.view(t).GetModelSelector().GetOptions()
	if len(options) != 3 || options[0].GetDisplayName() != "A" {
		t.Fatalf("options = %+v, want the catalog in display order", options)
	}
}

func TestThePickerServesExactlyTheGivenOptions(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	got := h.view(t).GetPermissionModePicker()
	if len(got.GetOptions()) != 3 {
		t.Fatalf("options = %+v, want exactly the served set", got.GetOptions())
	}
	if got.GetCurrent().GetMode() != "default" {
		t.Fatalf("current = %+v, want the mode the session facts stated", got.GetCurrent())
	}
}

func TestAPermissionModeChangeMovesTheCurrentOption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_PermissionModeChanged{
			PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{
				PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_Plan{
						Plan: &conversationv1.AgentPermissionModePlan{},
					},
				},
			},
		},
	})

	// Assert
	if got := h.view(t).GetPermissionModePicker().GetCurrent().GetMode(); got != "plan" {
		t.Fatalf("current = %q, want plan", got)
	}
}

func TestAModeInForceOutsideTheServedSetIsCurrentButNotOffered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_PermissionModeChanged{
			PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{
				PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_DontAsk{
						DontAsk: &conversationv1.AgentPermissionModeDontAsk{},
					},
				},
			},
		},
	})

	// Assert
	got := h.view(t).GetPermissionModePicker()
	if got.GetCurrent().GetMode() != "dont_ask" {
		t.Fatalf("current = %+v, want the mode in force", got.GetCurrent())
	}
	if got.GetCurrent().GetDisplayName() != "dont ask" {
		t.Fatalf("display = %q, want a label rather than the schema spelling", got.GetCurrent().GetDisplayName())
	}
	for _, option := range got.GetOptions() {
		if option.GetMode() == "dont_ask" {
			t.Fatalf("the picker offered a switch the daemon never served")
		}
	}
}

func TestALoggedInRootDrawsItsEmail(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	if got := h.view(t).GetAccount().GetLoggedIn().GetEmail(); got != "dev@example.com" {
		t.Fatalf("email = %q, want the logged-in address", got)
	}
}

func TestALoggedOutRootIsADrawnWarningRatherThanABlank(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetAccount(testWS, "")

	// Assert
	if h.view(t).GetAccount().GetLoggedOut() == nil {
		t.Fatalf("account = %+v, want the logged-out arm", h.view(t).GetAccount())
	}
}

func TestTheContextChipCarriesTheVendorsFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	if got := h.view(t).GetContext().GetText(); got != "142.3k" {
		t.Fatalf("chip = %q, want the formatted context size", got)
	}
}

func TestTheContextChipShrinksOnACompaction(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, contextUsage(12_000, 200_000, 6, "claude-opus-5"))

	// Assert
	if got := h.view(t).GetContext().GetText(); got != "12k" {
		t.Fatalf("chip = %q, want the shrunken figure", got)
	}
}

func TestTheChipsBreakdownIsAlwaysPopulated(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	if len(h.view(t).GetContext().GetBreakdown().GetSections()) == 0 {
		t.Fatalf("the hover content is empty; it needs no round-trip and is always populated")
	}
}

func TestAFrameForAnUnboundWorkspaceIsRecordedLoudly(t *testing.T) {
	// Arrange
	log := dlog.NewTestSurfaces()
	r, err := newResolver(testColors(), log, WithClock(fixedClock{now: instant}))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}

	// Act
	r.OnLink("ws-unbound", shimclient.LinkConnected)

	// Assert
	records := log.Records()
	if len(records) == 0 || records[len(records)-1].Context["invariant_violation"] == nil {
		t.Fatalf("records = %+v, want the invariant violation named", records)
	}
}

// contains reports whether needle appears in haystack.
func contains(haystack, needle string) bool {
	if len(needle) > len(haystack) {
		return false
	}
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return true
		}
	}
	return false
}

// TestStatusFactsReportsFalseBeforeTheSessionStarts covers the honest absence:
// a workspace whose session never opened has no facts for the /status panel.
func TestStatusFactsReportsFalseBeforeTheSessionStarts(t *testing.T) {
	// Arrange.
	r, ws := newModesResolver(t)

	// Act.
	facts, ok := r.StatusFacts(ws)

	// Assert.
	if ok {
		t.Fatalf("StatusFacts = %+v, true before the session started, want false", facts)
	}
}

// TestStatusFactsCarriesTheSessionsOwnFacts pins that the panel's account,
// model and mode are exactly what the session and the config root stated.
func TestStatusFactsCarriesTheSessionsOwnFacts(t *testing.T) {
	// Arrange.
	r, ws := newModesResolver(t)
	r.OnSessionStarted(ws, &conversationv1.SessionStarted{
		VendorSessionId: "vendor-1",
		EffectiveModel:  &conversationv1.AgentModel{Name: "opus"},
		PermissionMode: &conversationv1.AgentPermissionMode{
			Mode: &conversationv1.AgentPermissionMode_Plan{Plan: &conversationv1.AgentPermissionModePlan{}},
		},
	})
	r.SetAccount(ws, "someone@example.com")

	// Act.
	facts, ok := r.StatusFacts(ws)

	// Assert.
	if !ok {
		t.Fatal("StatusFacts reported false after the session started")
	}
	want := StatusFacts{Account: "someone@example.com", Model: "opus", PermissionMode: "plan"}
	if facts != want {
		t.Fatalf("facts = %+v, want %+v", facts, want)
	}
}
