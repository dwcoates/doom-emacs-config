package topbar

import (
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

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
	h.r.SetAccount(testWS, testAccount("dev@example.com"))
	h.r.OnSessionUpdate(testWS, contextUsage(142_300, 200_000, 71, "claude-opus-5"))
	// The two client hops of connectivity truth are up too: a serving shim
	// link alone is not a connected workspace (daemon.md invariant 11), so a
	// test that wants one hop down states that hop itself.
	h.r.SetParticipants(testWS, true, true)
}

// testAccount is the account cell almost every test wants: the one root the
// fixture spends from, offered as the only option and marked current.
func testAccount(email string) Account {
	return Account{
		Email:   email,
		Options: []AccountOption{{ConfigDir: "/Users/dev/.claude", Email: email, Current: true}},
	}
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

// setPicker installs a picker directly on the workspace's accumulated state,
// bypassing the ordinary sink path. Production never narrows the served set
// below the fixed switchable set (OnSessionStarted is the ONLY producer), so
// this white-box seam exists only to pin permissionModePicker's defensive
// fallback for a mode in force that a served set does not carry — a shape no
// real vendor input can produce once the served set is always the fixed one.
func (h *harness) setPicker(p *frontendv1.TopbarPermissionModePicker) {
	h.r.mutate(testWS, "test.set_picker", "test installed a picker", nil,
		func(s *wsState) { s.picker = p })
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

func TestNothingIsPublishedBeforeTheWorkspaceFacts(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: every SESSION fact, and neither workspace fact. Under the fixed
	// schema the gate is the workspace's own two and nothing else.
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))
	h.r.OnSessionUpdate(testWS, contextUsage(142_300, 200_000, 71, "claude-opus-5"))

	// Assert
	if _, ok := h.r.Topic(testWS).Latest(); ok {
		t.Fatalf("a view was published with no naming or account; absence is the legal not-yet-resolved state")
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
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

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

func TestTheTitleDropsABranchThatSaysNothing(t *testing.T) {
	tests := []struct {
		name         string
		naming       Naming
		sessionTitle string
		want         string
	}{
		{
			name:   "the default branch says nothing",
			naming: Naming{Title: "fix-flaky-reconnect", Branch: "main", DefaultBranch: "main", ConfigDir: "/root"},
			want:   "fix-flaky-reconnect",
		},
		{
			name:   "a branch equal to the name shown says nothing",
			naming: Naming{Title: "DWC/chess960-review-failures-enm", Branch: "DWC/chess960-review-failures-enm", DefaultBranch: "main", ConfigDir: "/root"},
			want:   "DWC/chess960-review-failures-enm",
		},
		{
			name:         "a branch equal to the slug still differs from the vendor title shown",
			naming:       Naming{Slug: "DWC/chess960-review-failures-enm", Branch: "DWC/chess960-review-failures-enm", DefaultBranch: "main", ConfigDir: "/root"},
			sessionTitle: "Add SPC j keybinding support",
			want:         "Add SPC j keybinding support · DWC/chess960-review-failures-enm",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.ready(t)

			// Act
			h.r.SetNaming(testWS, tt.naming)
			if tt.sessionTitle != "" {
				h.r.OnSessionUpdate(testWS, sessionTitle(tt.sessionTitle))
			}

			// Assert
			if got := h.view(t).GetTitle().GetText(); got != tt.want {
				t.Fatalf("title = %q, want %q", got, tt.want)
			}
		})
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

func TestThePickerServesExactlyTheFixedSwitchableSet(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	got := h.view(t).GetPermissionModePicker()
	if len(got.GetOptions()) != len(SwitchableModes) {
		t.Fatalf("options = %+v, want exactly the fixed switchable set", got.GetOptions())
	}
	if got.GetCurrent().GetMode() != "default" {
		t.Fatalf("current = %+v, want the mode the session facts stated", got.GetCurrent())
	}
}

func TestALiveDefaultIsTheDrawnCurrentButNotAnOffer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_PermissionModeChanged{
			PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{
				PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_Default{
						Default: &conversationv1.AgentPermissionModeDefault{},
					},
				},
			},
		},
	})

	// Assert
	got := h.view(t).GetPermissionModePicker()
	if got.GetCurrent().GetMode() != "default" {
		t.Fatalf("current = %+v, want the vendor's default drawn as the mode in force", got.GetCurrent())
	}
	for _, option := range got.GetOptions() {
		if option.GetMode() == "default" {
			t.Fatalf("options = %+v, want the live default absent from the offers", got.GetOptions())
		}
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
	// Arrange. The fixed switchable set always carries every vendor arm, so
	// no real session input can name a mode outside it; the served set is
	// narrowed directly to pin permissionModePicker's fallback.
	h := newHarness(t)
	h.ready(t)
	h.setPicker(picker("default", "default", "accept_edits", "plan"))

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
	h.r.SetAccount(testWS, testAccount(""))

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

// cutAgent is the agent a context cut arrives addressed to. The chip is
// session-scoped, so its value is immaterial, but the sink takes one.
var cutAgent = &conversationv1.AgentId{Value: "main"}

// clearedCut is the /clear arm of a context cut.
func clearedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	}
}

// compactedCut is the completed-compaction arm of a context cut.
func compactedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}},
	}
}

// compactionFailedCut is the failed-compaction arm: nothing was cut.
func compactionFailedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "the vendor refused"},
		},
	}
}

func TestAClearDropsTheStaleContextFigure(t *testing.T) {
	// Arrange: the chip is stating a real pre-clear total.
	h := newHarness(t)
	h.ready(t)

	// Act: a /clear discards the transcript that total was read off.
	h.r.OnContextCut(testWS, cutAgent, clearedCut())

	// Assert: the chip states the count is unknown rather than the stale total.
	if got := h.view(t).GetContext().GetText(); got != contextUnknownText {
		t.Fatalf("chip = %q, want the unknown dash after a clear, never the stale total", got)
	}
}

func TestACompletedCompactionDropsTheStaleContextFigure(t *testing.T) {
	// Arrange: the chip is stating the pre-compaction total.
	h := newHarness(t)
	h.ready(t)

	// Act: a completed compaction discards the transcript that total described.
	h.r.OnContextCut(testWS, cutAgent, compactedCut())

	// Assert: the chip states the count is unknown until the post-compaction
	// reading lands.
	if got := h.view(t).GetContext().GetText(); got != contextUnknownText {
		t.Fatalf("chip = %q, want the unknown dash during the compaction window", got)
	}
}

func TestAFailedCompactionKeepsTheContextFigure(t *testing.T) {
	// Arrange: the chip is stating a real total.
	h := newHarness(t)
	h.ready(t)

	// Act: a compaction that FAILED cut nothing, so the context is unchanged.
	h.r.OnContextCut(testWS, cutAgent, compactionFailedCut())

	// Assert: the chip keeps the figure the last reading stated.
	if got := h.view(t).GetContext().GetText(); got != "142.3k" {
		t.Fatalf("chip = %q, want the figure kept when nothing was cut", got)
	}
}

func TestANilContextCutLeavesTheChipUntouched(t *testing.T) {
	// Arrange: the chip is stating a real total.
	h := newHarness(t)
	h.ready(t)

	// Act: a nil cut carries no arm to react to.
	h.r.OnContextCut(testWS, cutAgent, nil)

	// Assert: the chip is unchanged.
	if got := h.view(t).GetContext().GetText(); got != "142.3k" {
		t.Fatalf("chip = %q, want the figure left standing on a nil cut", got)
	}
}

func TestTheFreshReadingReplacesTheUnknownDashAfterAClear(t *testing.T) {
	// Arrange: a clear has dropped the chip to the unknown dash.
	h := newHarness(t)
	h.ready(t)
	h.r.OnContextCut(testWS, cutAgent, clearedCut())

	// Act: the vendor's fresh reading for the cut context arrives.
	h.r.OnSessionUpdate(testWS, contextUsage(40_000, 200_000, 20, "claude-opus-5"))

	// Assert: the chip states the fresh near-baseline figure, not the dash.
	if got := h.view(t).GetContext().GetText(); got != "40k" {
		t.Fatalf("chip = %q, want the fresh post-clear reading", got)
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
	r.SetAccount(ws, testAccount("someone@example.com"))

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

// ---------------------------------------------------------------------------
// Fast mode is no longer drawn (TopbarView tag 10 retired, 2026-10-02).
// ---------------------------------------------------------------------------

func TestAFastModeStatementChangesNoTopbarView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	before := h.view(t)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_FastMode{FastMode: &conversationv1.SessionFastMode{
			State: &conversationv1.SessionFastMode_On{On: &conversationv1.SessionFastModeOn{}},
		}},
	})

	// Assert
	if after := h.view(t); !proto.Equal(before, after) {
		t.Fatalf("view after a fast-mode statement = %+v, want unchanged %+v", after, before)
	}
}

func TestTheTopbarPublishesOnTheWorkspaceFactsAloneWithNoSession(t *testing.T) {
	// Arrange: the two workspace facts and NOT ONE session fact — exactly what
	// a hibernated or cold-gated workspace has after a daemon boot.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "fix-flaky-reconnect", ConfigDir: "/root"})

	// Act
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert: the strip is published rather than withheld. Gating on a session
	// fact is what left it BLANK for as long as the state stood.
	if _, ok := h.r.Topic(testWS).Latest(); !ok {
		t.Fatalf("no topbar view was published, want the strip on naming and account alone")
	}
}

func TestTheSessionLessStripDrawsEveryCellItAlwaysDraws(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "fix-flaky-reconnect", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act
	h.r.SetParked(testWS, true)

	// Assert: the strip has ONE shape, so the always-drawn cells are all here.
	view := h.view(t)
	for _, tc := range []struct {
		element string
		present bool
	}{
		{"title", view.GetTitle() != nil},
		{"session_line", view.GetSessionLine() != nil},
		{"connectivity", view.GetConnectivity() != nil},
		{"account", view.GetAccount() != nil},
		{"context", view.GetContext() != nil},
		{"warnings", view.GetWarnings() != nil},
	} {
		if !tc.present {
			t.Errorf("%s is absent from the session-less strip, want drawn", tc.element)
		}
	}
}

func TestTheSessionLessStripLeavesItsTwoControlsAbsent(t *testing.T) {
	// Arrange: a fully resolved workspace, so every control HAS been resolved
	// and could have been carried through the park.
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetParked(testWS, true)

	// Assert: absence is how "no session has stated this" is said; the client
	// draws the dash in the slot.
	view := h.view(t)
	for _, tc := range []struct {
		element string
		present bool
	}{
		{"model_selector", view.GetModelSelector() != nil},
		{"permission_mode_picker", view.GetPermissionModePicker() != nil},
	} {
		if tc.present {
			t.Errorf("%s is present on the session-less strip, want absent", tc.element)
		}
	}
}

func TestTheParkedStripsChipStatesTheContextTheSessionHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetParked(testWS, true)

	// Assert: what a revival would carry, not a blank.
	if got, want := h.view(t).GetContext().GetText(), "142.3k"; got != want {
		t.Fatalf("context = %q, want %q", got, want)
	}
}

func TestAChipWithNoContextEverStatesZero(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})

	// Act
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert: 0 is the honest figure; a blank reads as "loading".
	if got, want := h.view(t).GetContext().GetText(), "0"; got != want {
		t.Fatalf("context = %q, want %q", got, want)
	}
}

func TestTheColdGatedChipStatesWhatAColdReadWouldReread(t *testing.T) {
	// Arrange: the gate's own count is what the shim read for THIS resume.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "explanation-engine", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Assert
	if got, want := h.view(t).GetContext().GetText(), "101.1k"; got != want {
		t.Fatalf("context = %q, want %q", got, want)
	}
}

func TestTheParkedChipsHoverLeadsWithWhyTheFigureIsNotLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetParked(testWS, true)

	// Assert: the strip has no width for the sentence, so the hover carries it.
	sections := h.view(t).GetContext().GetBreakdown().GetSections()
	if len(sections) == 0 {
		t.Fatalf("the breakdown has no sections at all")
	}
	if got, want := sections[0].GetHeading().GetText(), parkedLine(); got != want {
		t.Fatalf("heading = %q, want %q", got, want)
	}
}

func TestTheColdGatedChipsHoverLeadsWithTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Assert
	sections := h.view(t).GetContext().GetBreakdown().GetSections()
	if got, want := sections[0].GetHeading().GetText(), "cold context, awaiting your answer"; got != want {
		t.Fatalf("heading = %q, want %q", got, want)
	}
}

func TestALiveSessionsHoverLeadsWithItsSpend(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert: no reason section is invented for a session that is running.
	sections := h.view(t).GetContext().GetBreakdown().GetSections()
	if got, want := sections[0].GetHeading().GetText(), "session"; got != want {
		t.Fatalf("heading = %q, want %q", got, want)
	}
}

func TestTheParkedStripStatesTheParkAsAWarningLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetParked(testWS, true)

	// Assert: the retired whole-view state's fact is not lost.
	if got, want := lastWarningLine(t, h), parkedLine(); got != want {
		t.Fatalf("line = %q, want %q", got, want)
	}
}

func TestTheColdGatedStripStatesTheGateAsAWarningLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Assert
	if got, want := lastWarningLine(t, h), "cold context, awaiting your answer"; got != want {
		t.Fatalf("line = %q, want %q", got, want)
	}
}

func TestTheSessionLessWarningCarriesNoOverlay(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetParked(testWS, true)

	// Assert: the line IS the warning; there is nothing further to reveal, and
	// a gate is answered in the feed's card and nowhere else.
	warnings := h.view(t).GetWarnings().GetWarnings()
	last := warnings[len(warnings)-1]
	if last.GetDetail() != nil {
		t.Fatalf("detail = %T, want no overlay behind a state line", last.GetDetail())
	}
}

func TestTheColdGateOutranksHibernationInTheStatedReason(t *testing.T) {
	// Arrange: both signals stand at once. The gate is waiting on the reader
	// and the park is waiting on nothing, so the strip names the gate.
	h := newHarness(t)
	h.ready(t)
	h.r.SetParked(testWS, true)

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Assert
	if got, want := lastWarningLine(t, h), "cold context, awaiting your answer"; got != want {
		t.Fatalf("line = %q, want %q", got, want)
	}
}

func TestTheParkIsStillStatedOnceTheColdGateIsAnswered(t *testing.T) {
	// Arrange: the park outlives the gate, so retiring the gate must not
	// retire it — the workspace really is still stood down.
	h := newHarness(t)
	h.ready(t)
	h.r.SetParked(testWS, true)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: false})

	// Assert
	if got, want := lastWarningLine(t, h), parkedLine(); got != want {
		t.Fatalf("line = %q, want %q", got, want)
	}
}

func TestTheStripGetsItsControlsBackOnRevival(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.SetParked(testWS, true)

	// Act: the reviving spawn's own link state is what lifts the park.
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	view := h.view(t)
	if view.GetModelSelector() == nil || view.GetPermissionModePicker() == nil {
		t.Fatalf("view = %+v, want the session-scoped controls back", view)
	}
}

func TestTheStripGetsItsControlsBackWhenTheColdGateIsAnswered(t *testing.T) {
	// Arrange: the gate stood before any session fact, which is the real
	// order — the shim refuses the start, and only the answer opens a session.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "explanation-engine", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Act: the answer retires the gate and the re-opened session states its
	// own facts.
	h.r.SetColdGate(testWS, ColdGate{Standing: false})
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))
	h.r.OnSessionUpdate(testWS, contextUsage(142_300, 200_000, 71, "claude-opus-5"))

	// Assert
	view := h.view(t)
	if view.GetModelSelector() == nil || view.GetPermissionModePicker() == nil {
		t.Fatalf("view = %+v, want the session-scoped controls back", view)
	}
}

func TestTheSessionLessStripIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "explanation-engine", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Assert: a strip drawn with dashes where its controls belong has to be
	// diagnosable at the default level.
	records := h.log.Records()
	last := records[len(records)-1]
	if last.Level != "info" {
		t.Fatalf("level = %q, want info for the session-less strip", last.Level)
	}
	if last.Context["cold_gate"] != true {
		t.Fatalf("cold_gate = %v, want the standing gate named", last.Context["cold_gate"])
	}
}

func TestTheReturnOfTheSessionFactsIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 101_100})

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: false})

	// Assert
	records := h.log.Records()
	last := records[len(records)-1]
	if last.Level != "info" {
		t.Fatalf("level = %q, want info for the return of the session facts", last.Level)
	}
	if last.Message != "the topbar's session facts arrived and the strip is whole" {
		t.Fatalf("message = %q, want the return named", last.Message)
	}
}

func TestTheIncompleteTopbarIsRecordedAtInfoWithItsGatesNamed(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})

	// Assert: a blank topbar has to be diagnosable at the default level.
	records := h.log.Records()
	last := records[len(records)-1]
	if last.Level != "info" {
		t.Fatalf("level = %q, want info for an incomplete topbar", last.Level)
	}
	if last.Context["awaiting"] != "account" {
		t.Fatalf("awaiting = %v, want every outstanding gate named", last.Context["awaiting"])
	}
}

func TestTheTitleIsTheVendorsSummaryWhenItHasStatedOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, sessionTitle("Add SPC j keybinding support"))

	// Assert: the summary answers "which conversation is this"; the directory's
	// name does not.
	if got, want := h.view(t).GetTitle().GetText(), "Add SPC j keybinding support"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

func TestTheTitleIsTheWorkspaceNameUntilTheVendorStatesASummary(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	if got, want := h.view(t).GetTitle().GetText(), "fix-flaky-reconnect"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

func TestTheVendorsSummaryKeepsTheBranchBesideIt(t *testing.T) {
	// Arrange: a branch worth showing is a DIFFERENT fact from the name the
	// summary replaces, so the rule about it is unchanged.
	h := newHarness(t)
	h.ready(t)
	h.r.SetNaming(testWS, Naming{
		Title: "fix-flaky-reconnect", Branch: "DWC/fix", DefaultBranch: "main", ConfigDir: "/root",
	})

	// Act
	h.r.OnSessionUpdate(testWS, sessionTitle("Add SPC j keybinding support"))

	// Assert
	if got, want := h.view(t).GetTitle().GetText(), "Add SPC j keybinding support · DWC/fix"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

func TestASummaryTheVendorRestatesReplacesTheOneBeforeIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnSessionUpdate(testWS, sessionTitle("first guess"))

	// Act
	h.r.OnSessionUpdate(testWS, sessionTitle("what it turned out to be"))

	// Assert
	if got, want := h.view(t).GetTitle().GetText(), "what it turned out to be"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

func TestTheTitleIsTheDaemonsSynthesisWhenTheVendorHasStatedNone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetSynthesizedTitle(testWS, "Wire up the reconnect backoff")

	// Assert: the daemon's own summary beats the bare workspace name.
	if got, want := h.view(t).GetTitle().GetText(), "Wire up the reconnect backoff"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

func TestTheVendorsSummaryOutranksTheDaemonsSynthesis(t *testing.T) {
	// Arrange: both are present.
	h := newHarness(t)
	h.ready(t)
	h.r.SetSynthesizedTitle(testWS, "the daemon's guess")

	// Act
	h.r.OnSessionUpdate(testWS, sessionTitle("the vendor's own summary"))

	// Assert: a future CLI that emits ai-title must transparently supersede ours.
	if got, want := h.view(t).GetTitle().GetText(), "the vendor's own summary"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

func TestAnEmptySynthesizedTitleFallsBackToTheWorkspaceName(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.SetSynthesizedTitle(testWS, "a title to be retracted")

	// Act: retract it, as the synthesizer does after a /clear.
	h.r.SetSynthesizedTitle(testWS, "")

	// Assert
	if got, want := h.view(t).GetTitle().GetText(), "fix-flaky-reconnect"; got != want {
		t.Fatalf("title = %q, want %q", got, want)
	}
}

// parkedLine is the park's sentence as a reader on this machine reads it: a
// LOCAL wall clock, because the daemon and the reader share one.
func parkedLine() string {
	return "hibernated since " + instant.In(time.Local).Format("15:04")
}

// lastWarningLine is the sentence on the strip's LAST warning — where a
// standing state sorts, because the dropdown draws the newest first and a
// state was true before anything that is wrong right now went wrong.
func lastWarningLine(t *testing.T, h *harness) string {
	t.Helper()
	warnings := h.view(t).GetWarnings().GetWarnings()
	if len(warnings) == 0 {
		t.Fatalf("the warning strip is empty, want the state stated as a line")
	}
	return warnings[len(warnings)-1].GetLine().GetText()
}

// sessionTitle is the vendor's own summary of the conversation.
func sessionTitle(text string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Title{
			Title: &conversationv1.SessionTitle{Text: text},
		},
	}
}

func TestTheAccountCellOffersEveryRootWithTheCurrentOneMarked(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetAccount(testWS, Account{
		Email: "dev@example.com",
		Options: []AccountOption{
			{ConfigDir: "/Users/dev/.claude", Email: "dev@example.com", Current: true},
			{ConfigDir: "/Users/dev/.claude-work", Email: "work@example.com"},
		},
	})

	// Assert
	options := h.view(t).GetAccount().GetOptions()
	if len(options) != 2 {
		t.Fatalf("options = %d, want both roots the daemon knows", len(options))
	}
	if options[0].GetConfigDir() != "/Users/dev/.claude" || !options[0].GetCurrent() {
		t.Fatalf("option[0] = %+v, want the current root marked", options[0])
	}
	if options[1].GetCurrent() {
		t.Fatalf("option[1] = %+v, want the other root unmarked", options[1])
	}
}

func TestALoggedOutOptionCarriesTheLoggedOutArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetAccount(testWS, Account{
		Email: "dev@example.com",
		Options: []AccountOption{
			{ConfigDir: "/Users/dev/.claude", Email: "dev@example.com", Current: true},
			{ConfigDir: "/Users/dev/.claude-work"},
		},
	})

	// Assert
	options := h.view(t).GetAccount().GetOptions()
	if options[1].GetLoggedOut() == nil {
		t.Fatalf("option[1] = %+v, want the logged-out arm", options[1])
	}
}

func TestALoggedInOptionCarriesItsEmail(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetAccount(testWS, Account{
		Email:   "dev@example.com",
		Options: []AccountOption{{ConfigDir: "/Users/dev/.claude", Email: "dev@example.com", Current: true}},
	})

	// Assert
	options := h.view(t).GetAccount().GetOptions()
	if got := options[0].GetLoggedIn().GetEmail(); got != "dev@example.com" {
		t.Fatalf("option email = %q, want the root's own address", got)
	}
}

func TestAOneRootMachineStillOffersThatOneOption(t *testing.T) {
	// Arrange — the dropdown is a one-row list rather than nothing at all
	// (owner ruling, 2026-09-13).
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert
	if got := len(h.view(t).GetAccount().GetOptions()); got != 1 {
		t.Fatalf("options = %d on a one-root machine, want 1", got)
	}
}

// TestAFactForAnUnboundWorkspaceStatesTheViolationAtError pins that the
// violation is an ERROR, stated once, rather than context on quieter records.
func TestAFactForAnUnboundWorkspaceStatesTheViolationAtError(t *testing.T) {
	// Arrange
	log := dlog.NewTestSurfaces()
	r, err := New(testColors(), log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act
	r.SetParked("ws-unbound", true)
	r.SetParked("ws-unbound", false)

	// Assert
	n := 0
	for _, rec := range log.Records() {
		if rec.Level == "error" && rec.Operation == "daemon.topbar.unbound_workspace" {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("unbound-workspace ERROR records = %d, want 1", n)
	}
}

// TestConcurrentChangesLeaveTheNewestViewPublished pins the topbar's
// publication order: each change's view is published under the lock that
// rendered it, so once concurrent changes are all in, the topic holds the
// render of the state they left, never a stale view published late.
func TestConcurrentChangesLeaveTheNewestViewPublished(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	const writers = 64
	start := make(chan struct{})
	var wg sync.WaitGroup
	for i := 0; i < writers; i++ {
		wg.Add(1)
		go func(i int64) {
			defer wg.Done()
			<-start
			h.r.OnSessionUpdate(testWS, contextUsage(1_000*(i+1), 200_000, i, "claude-opus-5"))
		}(int64(i))
	}

	// Act
	close(start)
	wg.Wait()

	// Assert
	h.r.mu.Lock()
	want, err := h.r.render(h.r.stateLocked(testWS))
	h.r.mu.Unlock()
	if err != nil || want == nil {
		t.Fatalf("render = (%v, %v), want a complete view", want, err)
	}
	got, ok := h.r.Topic(testWS).Latest()
	if !ok || !proto.Equal(got, want) {
		t.Fatalf("published view = %v, want the render of the final state %v", got, want)
	}
}
