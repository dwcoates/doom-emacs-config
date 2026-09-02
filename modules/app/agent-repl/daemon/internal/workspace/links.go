package workspace

import (
	"context"
	"fmt"
	"net/url"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// OpenExternal opens a clicked link in the PINNED external browser profile. A
// clicked link never navigates the webview, and it never lands in whatever
// window happened to be frontmost.
func (v *verbs) OpenExternal(ctx context.Context, ws ids.WorkspaceID, link string) error {
	_, log, err := v.owned(ctx, "OpenExternal", ws)
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
	if err := v.deps.Browser.Open(ctx, link); err != nil {
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

// within reports whether path is the directory itself or lives beneath it.
func within(dir, path string) bool {
	dir = filepath.Clean(dir)
	if path == dir {
		return true
	}
	return strings.HasPrefix(path, dir+string(filepath.Separator))
}
