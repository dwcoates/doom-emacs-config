package topbar

import (
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/claudesettings"
)

const (
	effortUnspecified = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED
	effortLow         = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
	effortMedium      = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM
	effortHigh        = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH
	effortMax         = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX
)

// effortCatalogOption is a catalog row that accepts LEVELS.
func effortCatalogOption(name string, levels ...conversationv1.AgentEffortLevel) *conversationv1.ModelOption {
	option := modelOption(name, name)
	option.Capabilities = &conversationv1.ModelCapabilities{
		EffortSupport: &conversationv1.ModelCapabilities_EffortSupported{
			EffortSupported: &conversationv1.ModelEffortSupported{Levels: levels},
		},
	}
	return option
}

// noEffortCatalogOption is a catalog row whose model takes no level.
func noEffortCatalogOption(name string) *conversationv1.ModelOption {
	option := modelOption(name, name)
	option.Capabilities = &conversationv1.ModelCapabilities{
		EffortSupport: &conversationv1.ModelCapabilities_EffortUnsupported{
			EffortUnsupported: &conversationv1.ModelEffortUnsupported{},
		},
	}
	return option
}

// settingsDefault is a config root persisting LEVEL at the top level.
func settingsDefault(level conversationv1.AgentEffortLevel) claudesettings.Effort {
	return claudesettings.Effort{Path: "/Users/dev/.claude/settings.json", Default: level}
}

// supportedSelector is the selector's supported arm, failing when absent.
func supportedSelector(t *testing.T, view *frontendv1.TopbarView) *frontendv1.TopbarEffortSelectorSupported {
	t.Helper()
	supported := view.GetEffortSelector().GetSupported()
	if supported == nil {
		t.Fatalf("effort selector = %v, want the supported arm", view.GetEffortSelector())
	}
	return supported
}

// levelsOf lists the offered levels.
func levelsOf(options []*frontendv1.TopbarEffortOption) []conversationv1.AgentEffortLevel {
	out := make([]conversationv1.AgentEffortLevel, 0, len(options))
	for _, option := range options {
		out = append(out, option.GetLevel())
	}
	return out
}

func TestTheEffortSelectorIsAbsentWithNoSession(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act.
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))

	// Assert.
	if got := h.view(t).GetEffortSelector(); got != nil {
		t.Errorf("effort selector = %v, want absent with no session", got)
	}
}

func TestTheEffortSelectorIsAbsentWhenTheModelsCapabilitiesWereNeverStated(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{modelOption("claude-opus-5", "Opus")})

	// Assert.
	if got := h.view(t).GetEffortSelector(); got != nil {
		t.Errorf("effort selector = %v, want absent: no guess either way", got)
	}
}

func TestAModelThatTakesNoLevelDrawsTheUnsupportedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{noEffortCatalogOption("claude-opus-5")})

	// Assert.
	if h.view(t).GetEffortSelector().GetUnsupported() == nil {
		t.Errorf("effort selector = %v, want the unsupported arm", h.view(t).GetEffortSelector())
	}
}

func TestTheStartingLevelIsTheConfigRootsPersistedLevel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium, effortHigh)})

	// Act.
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))

	// Assert.
	current := supportedSelector(t, h.view(t)).GetCurrent()
	if current.GetLevel() != effortMedium || current.GetDisplayName() != "medium" {
		t.Errorf("current = %v, want medium", current)
	}
}

func TestAPerModelOverrideOutranksTheTopLevelLevel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium, effortHigh)})
	settings := settingsDefault(effortMedium)
	settings.PerModel = map[string]conversationv1.AgentEffortLevel{"claude-opus-5": effortHigh}

	// Act.
	h.r.SetEffortSettings(testWS, settings)

	// Assert.
	if got := supportedSelector(t, h.view(t)).GetCurrent().GetLevel(); got != effortHigh {
		t.Errorf("current = %v, want the per-model high", got)
	}
}

func TestAConfirmedPickOutranksTheSettings(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium, effortHigh)})
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))

	// Act.
	h.r.SetPickedEffort(testWS, effortLow)

	// Assert.
	if got := supportedSelector(t, h.view(t)).GetCurrent().GetLevel(); got != effortLow {
		t.Errorf("current = %v, want the picked low", got)
	}
}

func TestTheSelectorOffersExactlyTheModelsLevelsInTheVendorsOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortHigh, effortMedium, effortLow)})

	// Assert.
	got := levelsOf(supportedSelector(t, h.view(t)).GetOptions())
	if want := []conversationv1.AgentEffortLevel{effortHigh, effortMedium, effortLow}; !slices.Equal(got, want) {
		t.Errorf("options = %v, want %v", got, want)
	}
}

func TestALevelTheShimCouldNotSpellIsNotOffered(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortMedium, effortUnspecified)})

	// Assert.
	got := levelsOf(supportedSelector(t, h.view(t)).GetOptions())
	if want := []conversationv1.AgentEffortLevel{effortMedium}; !slices.Equal(got, want) {
		t.Errorf("options = %v, want %v", got, want)
	}
}

func TestTheSelectorIsAbsentWhenNoLevelIsKnownToBeInForce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, claudesettings.Effort{Path: "/Users/dev/.claude/settings.json"})

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium)})

	// Assert.
	if got := h.view(t).GetEffortSelector(); got != nil {
		t.Errorf("effort selector = %v, want absent rather than a guessed level", got)
	}
}

func TestTheSelectorIsAbsentWhenTheLevelInForceIsNotOneTheModelAccepts(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, settingsDefault(effortMax))

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium)})

	// Assert.
	if got := h.view(t).GetEffortSelector(); got != nil {
		t.Errorf("effort selector = %v, want absent: current is always one of the options", got)
	}
}

func TestTheSelectedRowIsFoundByTheVendorsResolutionOfAnAlias(t *testing.T) {
	// Arrange: the vendor reports the dated id while the catalog offers the alias.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))
	alias := effortCatalogOption("opus", effortMedium)
	alias.Capabilities.ResolvedModel = &conversationv1.AgentModel{Name: "claude-opus-5"}

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{alias})

	// Assert.
	if got := supportedSelector(t, h.view(t)).GetCurrent().GetLevel(); got != effortMedium {
		t.Errorf("current = %v, want medium off the alias row", got)
	}
}

func TestEffortLevelsAnswersTheSelectedModelsLevels(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortHigh)})

	// Act.
	got, ok := h.r.EffortLevels(testWS)

	// Assert.
	if want := []conversationv1.AgentEffortLevel{effortLow, effortHigh}; !ok || !slices.Equal(got, want) {
		t.Errorf("EffortLevels = (%v, %v), want (%v, true)", got, ok, want)
	}
}

func TestEffortLevelsReportsFalseForAModelThatTakesNoLevel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{noEffortCatalogOption("claude-opus-5")})

	// Act.
	_, ok := h.r.EffortLevels(testWS)

	// Assert.
	if ok {
		t.Errorf("EffortLevels ok = true, want false")
	}
}

func TestEffortLevelsReportsFalseWithNoSession(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow)})

	// Act.
	h.r.SetParked(testWS, true)
	_, ok := h.r.EffortLevels(testWS)

	// Assert.
	if ok {
		t.Errorf("EffortLevels ok = true, want false while parked")
	}
}

func TestPickedEffortReportsFalseBeforeAnyPick(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, ok := h.r.PickedEffort(testWS)

	// Assert.
	if ok {
		t.Errorf("PickedEffort ok = true, want false")
	}
}

func TestPickedEffortAnswersThePick(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.r.SetPickedEffort(testWS, effortHigh)
	got, ok := h.r.PickedEffort(testWS)

	// Assert.
	if !ok || got != effortHigh {
		t.Errorf("PickedEffort = (%v, %v), want (high, true)", got, ok)
	}
}

func TestTheEffortSelectorsSourceIsRecordedOnceWhenItChanges(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium)})

	// Act.
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))
	h.r.SetParticipants(testWS, true, true)

	// Assert.
	var standings []any
	for _, record := range h.log.Records() {
		if record.Operation == "daemon.topbar.effort_source" {
			standings = append(standings, record.Context["standing"])
		}
	}
	if len(standings) == 0 || standings[len(standings)-1] != "medium:effort_level" {
		t.Fatalf("standings = %v, want the last naming medium from the settings' effortLevel", standings)
	}
	if count := slices.Index(standings, "medium:effort_level"); count != len(standings)-1 {
		t.Errorf("standings = %v, want medium:effort_level recorded once", standings)
	}
}

func TestAnUnknownLevelIsRecordedWithItsReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, claudesettings.Effort{Path: "/Users/dev/.claude/settings.json"})

	// Act.
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow)})

	// Assert.
	records := h.log.Records()
	var last map[string]any
	for _, record := range records {
		if record.Operation == "daemon.topbar.effort_source" {
			last = record.Context
		}
	}
	if last["standing"] != "level_unknown:unset" || last["drawn"] != false {
		t.Errorf("last effort record = %v, want level_unknown:unset, not drawn", last)
	}
}

// effortPushed is the shim's push of the vendor's applied level.
func effortPushed(level conversationv1.AgentEffortLevel) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_EffortChanged{
			EffortChanged: &conversationv1.SessionEffortChanged{EffectiveEffort: level},
		},
	}
}

func TestThePushedLevelOutranksThePickAndTheSettings(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium, effortHigh)})
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))
	h.r.SetPickedEffort(testWS, effortLow)

	// Act.
	h.r.OnSessionUpdate(testWS, effortPushed(effortHigh))

	// Assert.
	if got := supportedSelector(t, h.view(t)).GetCurrent().GetLevel(); got != effortHigh {
		t.Errorf("current = %v, want the pushed high", got)
	}
}

func TestAPushNamesTheLevelWhenTheSettingsNameNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetEffortSettings(testWS, claudesettings.Effort{Path: "/Users/dev/.claude/settings.json"})
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium)})

	// Act.
	h.r.OnSessionUpdate(testWS, effortPushed(effortMedium))

	// Assert.
	if got := supportedSelector(t, h.view(t)).GetCurrent().GetLevel(); got != effortMedium {
		t.Errorf("current = %v, want the pushed medium", got)
	}
}

func TestANewSessionForgetsTheLastSessionsPush(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortMedium, effortHigh)})
	h.r.SetEffortSettings(testWS, settingsDefault(effortMedium))
	h.r.OnSessionUpdate(testWS, effortPushed(effortHigh))

	// Act.
	h.r.OnSessionStarted(testWS, sessionStarted("vend-2", "claude-opus-5"))

	// Assert.
	if got := supportedSelector(t, h.view(t)).GetCurrent().GetLevel(); got != effortMedium {
		t.Errorf("current = %v, want the settings' medium until the new session pushes", got)
	}
}

func TestThePushedSourceIsRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{effortCatalogOption("claude-opus-5", effortLow, effortHigh)})

	// Act.
	h.r.OnSessionUpdate(testWS, effortPushed(effortHigh))

	// Assert.
	var last map[string]any
	for _, record := range h.log.Records() {
		if record.Operation == "daemon.topbar.effort_source" {
			last = record.Context
		}
	}
	if last["standing"] != "high:vendor_push" {
		t.Errorf("last effort record = %v, want high:vendor_push", last)
	}
}
