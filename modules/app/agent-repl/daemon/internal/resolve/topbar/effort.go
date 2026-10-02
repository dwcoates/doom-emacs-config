package topbar

import (
	"slices"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/claudesettings"
	"claude-repld/internal/dlog"
	"claude-repld/internal/effortlevel"
	"claude-repld/internal/ids"
)

// THE EFFORT SELECTOR (topbar.proto TopbarEffortSelector; design record
// 2026-10-02 decision 2).
//
// THE CURRENT LEVEL IS ALWAYS A KNOWN LEVEL, never a placeholder. THE VENDOR'S
// PUSH IS THE AUTHORITY (SessionUpdate.effort_changed, owner ruling
// 2026-10-02): the level its next request sends, its own defaults, clamps and
// downgrades applied. Before the session's first push the level the shim
// confirmed for a pick stands, and before that the level the session's config
// root persists (claudesettings). When none states one the selector is ABSENT
// and the record says why: drawing a guess as the level in force is exactly
// what the ruling forbids.

// The sources a current level is named by in the record, beside the settings
// read's own (claudesettings.Source).
const (
	// effortSourcePushed is the vendor's own statement, pushed by the shim.
	effortSourcePushed = "vendor_push"
	// effortSourcePicked is a level the shim confirmed for a SetEffort pick.
	effortSourcePicked = "set_effort"
)

// SetEffortSettings installs what the session's config root persists for the
// effort level, read at workspace initialization and on every account switch.
func (r *resolver) SetEffortSettings(ws ids.WorkspaceID, settings claudesettings.Effort) {
	r.mutate(ws, "daemon.topbar.set_effort_settings", "the topbar took the config root's effort settings",
		dlog.Context{
			"path":                settings.Path,
			"effort_level":        settings.Default.String(),
			"model_settings_keys": len(settings.PerModel),
		},
		func(s *wsState) { s.effortSettings = settings })
}

// SetPickedEffort installs the level the shim confirmed for a pick. It holds
// for the rest of the workspace's session, across every later shim.
func (r *resolver) SetPickedEffort(ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) {
	r.mutate(ws, "daemon.topbar.set_picked_effort", "the topbar took the effort level the shim confirmed",
		dlog.Context{"effort": level.String()},
		func(s *wsState) { s.pickedEffort = level })
}

// PickedEffort answers the level a pick put in force, false before any pick.
func (r *resolver) PickedEffort(ws ids.WorkspaceID) (conversationv1.AgentEffortLevel, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	level := r.stateLocked(ws).pickedEffort
	return level, level != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED
}

// EffortLevels answers the levels the selected model accepts, in the vendor's
// order, reporting false when the model takes no level or its capabilities
// were never stated. SetEffort validates against it: the daemon forwards only
// a level the selected model was served as accepting.
func (r *resolver) EffortLevels(ws ids.WorkspaceID) ([]conversationv1.AgentEffortLevel, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	if s.sessionless() {
		return nil, false
	}
	levels, supported := acceptedLevels(selectedOption(s))
	return levels, supported
}

// selectedOption is the catalog row the session's model is: the row named
// exactly so, else the row whose vendor resolution it is (the vendor reports a
// dated id where the catalog offers an alias). Nil when no row is.
func selectedOption(s *wsState) *conversationv1.ModelOption {
	for _, option := range s.catalog {
		if option.GetModel().GetName() == s.model {
			return option
		}
	}
	for _, option := range s.catalog {
		if resolved := option.GetCapabilities().GetResolvedModel(); resolved != nil && resolved.GetName() == s.model {
			return option
		}
	}
	return nil
}

// acceptedLevels reads a catalog row's effort support. The second result is
// false for a row that takes no level AND for one that stated nothing; the
// caller that must tell the two apart reads the arm itself.
func acceptedLevels(option *conversationv1.ModelOption) ([]conversationv1.AgentEffortLevel, bool) {
	supported := option.GetCapabilities().GetEffortSupported()
	if supported == nil {
		return nil, false
	}
	// A level the shim could not spell arrives UNSPECIFIED, and an option is
	// never UNSPECIFIED (topbar.proto): it is not offered.
	levels := make([]conversationv1.AgentEffortLevel, 0, len(supported.GetLevels()))
	for _, level := range supported.GetLevels() {
		if level != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
			levels = append(levels, level)
		}
	}
	return levels, true
}

// effortCurrent is the level in force and where it was read from: the
// vendor's push, else the pick, else the config root's persisted level for the
// session's model.
func effortCurrent(s *wsState) (conversationv1.AgentEffortLevel, string) {
	if s.pushedEffort != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return s.pushedEffort, effortSourcePushed
	}
	if s.pickedEffort != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return s.pickedEffort, effortSourcePicked
	}
	level, source := s.effortSettings.For(s.model)
	return level, string(source)
}

// effortSelector renders the selector, or nil while it has nothing true to
// show: no session, a model whose capabilities were never stated, or a model
// that takes a level with no level known to be in force.
func effortSelector(s *wsState) *frontendv1.TopbarEffortSelector {
	if s.sessionless() {
		return nil
	}
	option := selectedOption(s)
	if option.GetCapabilities().GetEffortUnsupported() != nil {
		return &frontendv1.TopbarEffortSelector{
			Support: &frontendv1.TopbarEffortSelector_Unsupported{Unsupported: &frontendv1.TopbarEffortSelectorUnsupported{}},
		}
	}
	levels, supported := acceptedLevels(option)
	if !supported {
		return nil
	}
	current, _ := effortCurrent(s)
	if !slices.Contains(levels, current) {
		return nil
	}
	options := make([]*frontendv1.TopbarEffortOption, 0, len(levels))
	for _, level := range levels {
		options = append(options, effortOption(level))
	}
	return &frontendv1.TopbarEffortSelector{
		Support: &frontendv1.TopbarEffortSelector_Supported{Supported: &frontendv1.TopbarEffortSelectorSupported{
			Current: effortOption(current),
			Options: options,
		}},
	}
}

// effortOption is one level as the selector offers it.
func effortOption(level conversationv1.AgentEffortLevel) *frontendv1.TopbarEffortOption {
	return &frontendv1.TopbarEffortOption{Level: level, DisplayName: effortDisplayName(level)}
}

// effortDisplayName is the vendor's own spelling of a level ("medium"), the
// word its /effort command takes.
func effortDisplayName(level conversationv1.AgentEffortLevel) string {
	return effortlevel.Word(level)
}

// effortStanding is what the selector states and why, for the edge record: a
// change in it is what a reader asking "why does the selector show this" needs.
func effortStanding(s *wsState) string {
	if s.sessionless() {
		return "no_session"
	}
	option := selectedOption(s)
	switch {
	case option.GetCapabilities().GetEffortUnsupported() != nil:
		return "unsupported"
	case option.GetCapabilities().GetEffortSupported() == nil:
		return "capabilities_unstated"
	}
	level, source := effortCurrent(s)
	if level == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return "level_unknown:" + source
	}
	levels, _ := acceptedLevels(option)
	if !slices.Contains(levels, level) {
		return "level_not_accepted:" + effortDisplayName(level) + ":" + source
	}
	return effortDisplayName(level) + ":" + source
}
