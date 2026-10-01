package workspace

import (
	"context"
	"fmt"
	"net/url"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// OpenExternal opens a clicked link in the PINNED external browser profile. A
// clicked link never navigates the webview, and it never lands in whatever
// window happened to be frontmost.
func (v *verbs) OpenExternal(ctx context.Context, ws ids.WorkspaceID, link string) error {
	record, log, err := v.owned(ctx, "OpenExternal", ws)
	if err != nil {
		return err
	}
	parsed, err := url.Parse(link)
	if err != nil || parsed.Scheme == "" || parsed.Host == "" {
		return refuse(log, "OpenExternal", ArmInvalidUrl,
			fmt.Sprintf("%q is not an absolute url", link), false)
	}
	if v.deps.Browser == nil {
		return refuse(log, "OpenExternal", ArmNoBrowserConfigured,
			"this daemon has no external browser configured", false)
	}
	// ROUTE THE CHROME PROFILE BY THE SESSION'S ACCOUNT. The account in force
	// is the reader's choice first, then the routing the path decides
	// (accountRoot), so a link opens in the SAME Chrome window — personal or
	// work — the session spends from. A failure to determine the account is
	// not a reason to leave the click unanswered: it routes to the pinned
	// default profile and says so loudly, because the browser resolves the
	// profile-to-window mapping and a missing account only costs the routing.
	profile := v.openExternalProfile(ctx, log, record)
	if err := v.deps.Browser.Open(ctx, link, profile); err != nil {
		// A launcher that would not run is a LANDED arm, not an internal
		// error: OpenExternalError.launch_failed carries the launcher's own
		// account of the failure in `detail`, so the click is answered rather
		// than collapsed into CodeInternal.
		log.Warn(opOpenExternal, "the external browser did not open the link", dlog.Context{
			"url": link, "cause": err.Error(),
		})
		return refuseWith(log, "OpenExternal", ArmLaunchFailed,
			fmt.Sprintf("the external browser did not open %q: %v", link, err), false,
			map[string]any{"detail": err.Error()})
	}
	log.Info(opOpenExternal, "opened a link externally", dlog.Context{"url": link})
	return nil
}

// openExternalProfile resolves the Chrome profile a clicked link opens in for
// the workspace's session.
//
// The account in force is accountRoot's — the reader's SelectAccount choice
// first, then the path routing — so the profile follows the account the
// session actually spends from, not merely the path it lives on. Every step
// that cannot answer (a session or account root that will not read) falls back
// to an empty account, which the opener routes to its pinned default profile
// and logs. Nothing here fails the click: the profile only decides WHICH
// browser window a link lands in.
func (v *verbs) openExternalProfile(ctx context.Context, log dlog.Logger, record wsm.Workspace) string {
	configDir, err := v.accountRoot(ctx, record)
	if err != nil {
		log.Warn(opOpenExternal, "could not resolve the session's account root; routing the link to the default browser profile", dlog.Context{
			"workspace": string(record.ID), "cause": err.Error(),
		})
		return v.deps.Browser.ProfileForAccount("")
	}
	acct, err := v.deps.Accounts.Read(ctx, configDir)
	if err != nil {
		log.Warn(opOpenExternal, "could not read the session's account; routing the link to the default browser profile", dlog.Context{
			"workspace": string(record.ID), "config_dir": configDir, "cause": err.Error(),
		})
		return v.deps.Browser.ProfileForAccount("")
	}
	return v.deps.Browser.ProfileForAccount(acct.Email)
}

// OpenInEditor RELAYS a web link click onto the workspace's host stream
// verbatim. The daemon validates the workspace and the path and OPENS NOTHING
// ITSELF: there is no ack and no command loop, because the editor is the only
// party that knows what "open" means.
//
// The path must name something INSIDE the workspace: relaying a path that
// escapes it would have the editor open a file the click never addressed.
func (v *verbs) OpenInEditor(ctx context.Context, ws ids.WorkspaceID, path string, line *uint32) error {
	record, log, err := v.owned(ctx, "OpenInEditor", ws)
	if err != nil {
		return err
	}
	if strings.TrimSpace(path) == "" {
		return refuse(log, "OpenInEditor", ArmPathEscapesWorkspace, "no path was named", false)
	}

	// The resolved form is for the CONTAINMENT CHECK ONLY. The contract says
	// the daemon relays the path VERBATIM — exactly as the feed row carried it
	// — so what travels is the caller's spelling, not the daemon's.
	absolute := path
	if !filepath.IsAbs(absolute) {
		absolute = filepath.Join(record.Dir, absolute)
	}
	absolute = filepath.Clean(absolute)
	if !within(record.Dir, absolute) {
		return refuse(log, "OpenInEditor", ArmPathEscapesWorkspace,
			fmt.Sprintf("%q resolves to %q, which is outside workspace %q", path, absolute, record.Dir), false)
	}

	v.deps.Host.OpenInEditor(ws, path, line)
	log.Info(opOpenInEditor, "relayed an open-in-editor click", dlog.Context{
		"path": path, "resolved": absolute, "has_line": line != nil,
	})
	return nil
}

// OpenDaemonFileInEditor relays an open of a file the daemon holds for the
// workspace onto its host stream, exactly as a workspace file's click is
// relayed: Emacs opens it in its one shared popup. The workspace is checked as
// every verb checks it; the path is the daemon's own resolution of a token it
// served, so it is not held to the worktree.
func (v *verbs) OpenDaemonFileInEditor(ctx context.Context, ws ids.WorkspaceID, path string) error {
	_, log, err := v.owned(ctx, "OpenInEditor", ws)
	if err != nil {
		return err
	}
	if !filepath.IsAbs(path) {
		err := fmt.Errorf("workspace: a daemon file to open must be absolute, got %q", path)
		log.Error(opOpenInEditor, "refused to relay a daemon file that is not absolute", dlog.Context{"path": path})
		return err
	}
	v.deps.Host.OpenInEditor(ws, path, nil)
	log.Info(opOpenInEditor, "relayed an open-in-editor of a file the daemon holds", dlog.Context{"path": path})
	return nil
}

// within reports whether path is the directory itself or lives beneath it.
func within(dir, path string) bool {
	dir = filepath.Clean(dir)
	if path == dir {
		return true
	}
	return strings.HasPrefix(path, dir+string(filepath.Separator))
}
