package frontend

import (
	"fmt"
	"path/filepath"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// Workspace key canonicalization at frame ingress
// ---------------------------------------------------------------------------
//
// The workspace field is an OPAQUE MAP KEY everywhere downstream: it keys the
// command lanes (lanes.go), the registry's records, the session lookups, and
// every published frame's addressing. Nothing downstream compares paths, they
// all compare strings, so two spellings of the same directory are two
// workspaces.
//
// That is not hypothetical. A restart_session carrying ".../async-gui-open-oyn/"
// — the same workspace as ".../async-gui-open-oyn" with one trailing separator —
// minted a second workspace record and a second session (s_ad1f9d9db8f2bff3)
// beside the real one, on its own lane, invisible to every lookup keyed by the
// clean spelling.
//
// The fix is ONE choke point rather than a rule every downstream map has to
// remember: the read loop canonicalizes the field the instant the frame is
// decoded, BEFORE the lane key is computed and before any handler sees it, so
// no later code can observe an uncanonical key at all.

// canonicalWorkspaceKey is the one spelling of a workspace key. filepath.Clean
// collapses redundant separators and "." elements and strips trailing
// separators, which is exactly the drift observed on the wire.
//
// An absent workspace stays absent: a workspace-less command is a legitimate
// daemon-global command (the roster publish, the shutdown controls), and
// cleaning "" would invent "." for it.
func canonicalWorkspaceKey(ws string) string {
	if ws == "" {
		return ""
	}
	return filepath.Clean(ws)
}

// normalizeCommandWorkspace rewrites a decoded command's workspace field to its
// canonical spelling, in place, before anything keys off it.
func normalizeCommandWorkspace(cmd *frontendv1.FrontendCommand) {
	if cmd == nil {
		return
	}
	if key := canonicalWorkspaceKey(cmd.GetWorkspace()); key != cmd.GetWorkspace() {
		cmd.Workspace = key
	}
}

// workspaceKeyError names a workspace field that carried something but
// canonicalizes to no addressable path — "./", ".", and friends. It is a
// refusal rather than a silent global-lane fallthrough: the client named a
// workspace, and the daemon cannot tell which one.
//
// It returns nil for the absent workspace, which is not an error.
func workspaceKeyError(ws string) error {
	if ws == "." {
		return fmt.Errorf("frontend: workspace %q canonicalizes to no path; a workspace key must name a directory", ws)
	}
	return nil
}
