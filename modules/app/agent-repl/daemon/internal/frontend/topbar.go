package frontend

// topbar.go resolves the TOPBAR VIEW: the title line, the session line, the
// model selector's contents, the connectivity indicator and the accounting
// line, each as the exact string or flag the client draws.
//
// NOTHING HERE IS DERIVED CLIENT-SIDE, which is the whole reason the view
// exists. The title used to be concatenated in each frontend from a workspace
// path and a branch it fetched separately; the connectivity glyph used to be a
// per-frontend switch over the SessionConnectivity enum, so the same enum drew
// a different glyph in Emacs than in the webapp. Both are one answer now.
//
// THE FENCE IS NOT MINTED HERE. It arrives already minted by the daemon's one
// fence composer (internal/ssm.Fence, reachable on every WorkspaceState) and
// is carried through untouched. A resolver that composed one would be a second
// author of a token whose only guarantee is that two of them are equal exactly
// when neither identity rotated.

import (
	"fmt"
	"path/filepath"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TopbarInputs is every fact the topbar renders, as the daemon holds them
// before composition. It is a struct rather than a parameter list because the
// view is published whole: a fact added here is a compile error at the call
// site until the daemon decides what it is, which is what keeps a
// partially-resolved topbar unrepresentable.
type TopbarInputs struct {
	// Workspace is the absolute workspace directory. It is both the routing
	// key and the base of the title.
	Workspace string
	// Fence is the workspace's staleness token, MINTED ELSEWHERE. See the file
	// comment: this package carries it and never composes it.
	Fence string
	// Branch is the git branch worth showing beside the workspace name, or
	// empty when the daemon has none to show.
	Branch string
	// SessionID is the agent-repl session that owns the workspace, and
	// ClaudeSessionID the vendor conversation it drives. Both are identities
	// the topbar's expanded state states verbatim; neither is routing.
	SessionID       string
	ClaudeSessionID string
	// ModelDisplay is the current selection as the selector button shows it.
	// Empty renders the selector's placeholder.
	ModelDisplay string
	// ModelOptions is the selectable menu, already in display order.
	ModelOptions []*frontendv1.ModelOption
	// Connectivity is the SSM's verdict, adopted and never re-inferred.
	Connectivity frontendv1.SessionConnectivity
	// AccountingLine is the settled turn's composed summary, or empty when no
	// turn has settled yet. It is the SAME sentence the footer's accounting
	// cell carries — the topbar shows the prose and never the verdict.
	AccountingLine string
}

// TopbarView resolves one workspace's topbar completely.
//
// It REFUSES rather than degrading. A topbar with no workspace has no routing
// key, one with no fence cannot be told from a stale push, and one whose
// connectivity this build does not know would draw an indicator nobody chose.
// Each of those returns an error so the publisher withholds the view entirely:
// a half-resolved topbar is a topbar the client would have to finish, which is
// exactly what this contract removed.
func TopbarView(in TopbarInputs) (*frontendv1.TopbarView, error) {
	if in.Workspace == "" {
		return nil, fmt.Errorf("frontend: topbar view requires a workspace")
	}
	if in.Fence == "" {
		return nil, fmt.Errorf("frontend: topbar view for workspace %q requires the workspace's fence; an unfenced push cannot be told from a stale one", in.Workspace)
	}
	connectivity, err := TopbarConnectivity(in.Connectivity)
	if err != nil {
		return nil, fmt.Errorf("frontend: topbar view for workspace %q: %w", in.Workspace, err)
	}
	return &frontendv1.TopbarView{
		Workspace:      in.Workspace,
		Title:          topbarTitle(in.Workspace, in.Branch),
		SessionLine:    topbarSessionLine(in.SessionID, in.ClaudeSessionID),
		ModelDisplay:   in.ModelDisplay,
		ModelOptions:   in.ModelOptions,
		Connectivity:   connectivity,
		AccountingLine: in.AccountingLine,
		Fence:          in.Fence,
	}, nil
}

// topbarTitle composes the one title line: the workspace's own name, and the
// branch beside it when there is one worth showing. Composed here so no client
// ever joins two identity fragments and picks its own separator.
func topbarTitle(workspace, branch string) string {
	name := filepath.Base(workspace)
	if branch == "" {
		return name
	}
	return name + " (" + branch + ")"
}

// topbarSessionLine composes the identity line the expanded topbar shows.
//
// A workspace with no session yet gets an EMPTY line rather than a line about
// nothing: the topbar renders the absence as an absence, which is what a
// workspace between sessions actually is.
func topbarSessionLine(sessionID, claudeSessionID string) string {
	if sessionID == "" {
		return ""
	}
	if claudeSessionID == "" {
		return "session " + sessionID
	}
	return "session " + sessionID + " · conversation " + claudeSessionID
}

// The connectivity indicator's resolved appearance, one row per
// SessionConnectivity value.
//
// THE TONES ARE THE SHARED VOCABULARY'S, not a palette of this file's own:
// each is one of the color-class names proto/vocab/render-colors.json defines,
// assigned to match the RenderState the same condition resolves a workspace to
// — hibernation is teal there and teal here, every broken-route condition is
// blue there and blue here, and an operating session is green. A tone this
// file invented would be the third answer to a question that file exists to
// have exactly one of.
//
// The GLYPHS are literal, so no client maps the enum, and the TITLES are the
// tooltip text verbatim.
const (
	toneNone     = "none"
	toneBlue     = "blue"
	toneTeal     = "teal"
	toneGreen    = "green"
	glyphUnknown = "○"
	glyphAsleep  = "◍"
	glyphPending = "◌"
	glyphLive    = "●"
	glyphFaulted = "◐"
	glyphSevered = "✕"
)

// TopbarConnectivity resolves the SSM's connectivity verdict into the glyph,
// tone and tooltip the indicator draws.
//
// The switch is CLOSED and its default is loud: a connectivity value this
// build does not know is a wire the daemon was not built for, and drawing it
// as one of the known states would report a condition nobody resolved. The
// error travels to the publisher, which withholds the whole view.
func TopbarConnectivity(c frontendv1.SessionConnectivity) (*frontendv1.TopbarConnectivity, error) {
	switch c {
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_UNSPECIFIED:
		return &frontendv1.TopbarConnectivity{
			Tone:  toneNone,
			Glyph: glyphUnknown,
			Title: "connectivity has not been resolved for this workspace yet",
		}, nil
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED:
		return &frontendv1.TopbarConnectivity{
			Tone:  toneTeal,
			Glyph: glyphAsleep,
			Title: "asleep on purpose — a revival decision brings it back",
		}, nil
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_CONNECTING:
		return &frontendv1.TopbarConnectivity{
			Tone:  toneBlue,
			Glyph: glyphPending,
			Title: "connecting — the session's route is still being established",
		}, nil
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL:
		return &frontendv1.TopbarConnectivity{
			Tone:  toneGreen,
			Glyph: glyphLive,
			Title: "connected",
		}, nil
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_DEGRADED:
		return &frontendv1.TopbarConnectivity{
			Tone:  toneBlue,
			Glyph: glyphFaulted,
			Title: "connected, with an open fault on the session's route",
		}, nil
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_UNAVAILABLE:
		return &frontendv1.TopbarConnectivity{
			Tone:  toneBlue,
			Glyph: glyphSevered,
			Title: "no route to the session",
		}, nil
	default:
		return nil, fmt.Errorf("session connectivity %d is not one this build resolves an indicator for", int32(c))
	}
}
