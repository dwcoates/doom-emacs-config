package workspace

import (
	"context"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// OpenExternal opens a clicked link in the Chrome profile the session's
// account signs in as. A clicked link never navigates the webview, and it never
// lands in whatever window happened to be frontmost.
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
	// work — the session spends from. An account that cannot be determined
	// fails the click: there is no default profile to send it to instead.
	email, err := v.openExternalAccount(ctx, record)
	if err == nil {
		err = v.deps.Browser.Open(ctx, link, email)
	}
	if err != nil {
		// A launch that could not happen is a LANDED arm, not an internal
		// error: OpenExternalError.launch_failed carries the cause in
		// `detail`, so the click is answered rather than collapsed into
		// CodeInternal.
		log.Error(opOpenExternal, "the external browser did not open the link", dlog.Context{
			"workspace": string(record.ID), "url": link, "email": email, "cause": err.Error(),
		})
		return refuseWith(log, "OpenExternal", ArmLaunchFailed,
			fmt.Sprintf("the external browser did not open %q: %v", link, err), false,
			map[string]any{"detail": err.Error()})
	}
	log.Info(opOpenExternal, "opened a link externally", dlog.Context{"url": link})
	return nil
}

// openExternalAccount answers the email of the account the workspace's session
// spends from: the account root accountRoot resolves, read for its signed-in
// address. A logged-out root answers an empty email, which is a state and not
// a failure. A root or account that will not read is an error.
func (v *verbs) openExternalAccount(ctx context.Context, record wsm.Workspace) (string, error) {
	configDir, err := v.accountRoot(ctx, record)
	if err != nil {
		return "", fmt.Errorf("resolving the session's account root: %w", err)
	}
	acct, err := v.deps.Accounts.Read(ctx, configDir)
	if err != nil {
		return "", fmt.Errorf("reading the session's account in %s: %w", configDir, err)
	}
	return acct.Email, nil
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

// BriefLinkUnresolved is the question a feed link that resolved to no file
// sends its workspace (prompts/feed-link-unresolved.md).
const BriefLinkUnresolved = "feed-link-unresolved"

// FaultKindUnknownFile is the transient footer line an unresolved feed link
// raises: a non-escalating fault kind, so the status stands as it was.
const FaultKindUnknownFile = "unknown_file"

// feedLinkResolverSite names this file's resolver for the question an
// unresolved link sends, relative to the agent-repl module root, so the agent
// asked to propose a fallback knows where to look.
const feedLinkResolverSite = "daemon/internal/workspace/links.go (resolveFeedLink)"

// bareLinkSubdir is where a BARE file name is looked for first: the agent-repl
// module inside the worktree, where the agents writing these links work.
const bareLinkSubdir = "modules/app/agent-repl"

// UnresolvedLink is what OpenFeedLink answers for a link that named no file:
// the href as clicked, and the question to submit to the workspace.
type UnresolvedLink struct {
	// Href is the link exactly as clicked.
	Href string
	// Question is the composed follow-up prompt, unquoted: the caller quotes
	// the source bubble ahead of it.
	Question string
}

// feedLink is one feed link's resolution.
type feedLink struct {
	// path is the existing file the link resolved to, absolute; empty when
	// none of the candidates exists.
	path string
	// line is the `:<line>` suffix's line, nil when the link carried none.
	line *uint32
	// candidates are every place looked, in order.
	candidates []string
}

// lineSuffix matches a trailing `:<line>` on a link.
var lineSuffix = regexp.MustCompile(`^(.+):([1-9][0-9]*)$`)

// resolveFeedLink resolves a feed link's href against a worktree, in the
// contract's order (OpenInEditorFeedLink), first existing file winning:
//
//  1. a HOME-RELATIVE path (`~/rest`), beneath home (dirpath.Absolute);
//  2. an absolute path, as given;
//  3. a path with a directory part, relative to the worktree root;
//  4. a BARE name, under <worktree>/modules/app/agent-repl, then under the git
//     project root of the worktree.
//
// An optional `:<line>` suffix is split off first. Containment is NOT judged
// here: the caller refuses a resolved path outside the worktree.
//
// A `~`-prefixed path dirpath.Absolute refuses (`~user/...`, another user's
// home) is looked for nowhere: its one candidate is the href as written, and
// the link is unresolved.
func resolveFeedLink(worktree, home, href string) feedLink {
	var out feedLink
	name := href
	if m := lineSuffix.FindStringSubmatch(href); m != nil {
		if n, err := strconv.ParseUint(m[2], 10, 32); err == nil {
			line := uint32(n)
			out.line = &line
			name = m[1]
		}
	}
	switch {
	case strings.HasPrefix(name, "~"):
		expanded, err := dirpath.Absolute(name, home)
		if err != nil {
			out.candidates = []string{name}
			return out
		}
		out.candidates = []string{expanded}
	case filepath.IsAbs(name):
		out.candidates = []string{filepath.Clean(name)}
	case strings.ContainsRune(name, '/'):
		out.candidates = []string{filepath.Join(worktree, name)}
	default:
		out.candidates = []string{filepath.Join(worktree, bareLinkSubdir, name)}
		if root, ok := gitProjectRoot(worktree); ok {
			out.candidates = append(out.candidates, filepath.Join(root, name))
		}
	}
	for _, candidate := range out.candidates {
		if _, err := os.Stat(candidate); err == nil {
			out.path = candidate
			return out
		}
	}
	return out
}

// gitProjectRoot answers the git project root a directory lies in: the
// nearest directory, from dir upward, holding a `.git` entry (a directory for
// a main checkout, a file for a linked worktree). It reads the filesystem
// only; git is never run.
func gitProjectRoot(dir string) (string, bool) {
	for current := filepath.Clean(dir); ; {
		if _, err := os.Stat(filepath.Join(current, ".git")); err == nil {
			return current, true
		}
		parent := filepath.Dir(current)
		if parent == current {
			return "", false
		}
		current = parent
	}
}

// OpenFeedLink resolves a non-web link clicked in a prompt or response bubble
// and relays the file it names onto the workspace's host stream, exactly as a
// workspace file's click is relayed.
//
// A LINK THAT NAMES NO FILE IS ANSWERED, NEVER DROPPED. Under REPORT the
// footer draws the transient `unknown_file` line (the status is untouched),
// and the composed question asking the agent which file it meant is handed
// back for the caller to submit, beside the `link_unresolved` refusal. Without
// it (the `web_fallback` arm: an ambiguous bare name the client will open as a
// web URL) the refusal is the whole answer.
func (v *verbs) OpenFeedLink(ctx context.Context, ws ids.WorkspaceID, href string, report bool) (*UnresolvedLink, error) {
	record, log, err := v.owned(ctx, "OpenInEditor", ws)
	if err != nil {
		return nil, err
	}
	if strings.TrimSpace(href) == "" {
		return nil, refuse(log, "OpenInEditor", ArmPathEscapesWorkspace, "no link was named", false)
	}
	link := resolveFeedLink(record.Dir, v.deps.HomeDir, href)
	if link.path == "" {
		if !report {
			return nil, v.silentlyUnresolved(log, href, link)
		}
		return v.unresolvedLink(ctx, log, record, href, link)
	}
	if !within(record.Dir, link.path) {
		return nil, refuse(log, "OpenInEditor", ArmPathEscapesWorkspace,
			fmt.Sprintf("link %q resolves to %q, which is outside workspace %q", href, link.path, record.Dir), false)
	}
	v.deps.Host.OpenInEditor(ws, link.path, link.line)
	log.Info(opOpenInEditor, "relayed a feed link's open-in-editor click", dlog.Context{
		"href": href, "resolved": link.path, "has_line": link.line != nil,
	})
	return nil, nil
}

// silentlyUnresolved is OpenFeedLink's answer for a `web_fallback` link that
// named no file: the refusal alone, with no footer line and no question,
// because the client opens the name on the web instead.
func (v *verbs) silentlyUnresolved(log dlog.Logger, href string, link feedLink) error {
	log.Info(opOpenInEditor, "a web-fallback feed link resolved to no file; the client opens it on the web, so nothing is reported", dlog.Context{
		"href": href, "candidates": link.candidates,
	})
	return refuseWith(log, "OpenInEditor", ArmLinkUnresolved,
		fmt.Sprintf("link %q resolved to no file (looked at %s); the client falls back to the web", href, strings.Join(link.candidates, ", ")),
		false, map[string]any{"href": href})
}

// unresolvedLink is OpenFeedLink's answer for a link that named no file: the
// transient footer line, then the composed question and the refusal.
func (v *verbs) unresolvedLink(ctx context.Context, log dlog.Logger, record wsm.Workspace, href string, link feedLink) (*UnresolvedLink, error) {
	name := filepath.Base(link.candidates[0])
	log.Info(opOpenInEditor, "a feed link resolved to no file", dlog.Context{
		"href": href, "candidates": link.candidates,
	})
	v.deps.Footer.OpenFault(record.ID, footer.Fault{
		ID:     FaultKindUnknownFile + ":" + href,
		Kind:   FaultKindUnknownFile,
		Detail: name,
		At:     v.now(),
	})
	question, err := v.linkQuestion(ctx, log, href, link.candidates)
	if err != nil {
		return nil, err
	}
	return &UnresolvedLink{Href: href, Question: question},
		refuseWith(log, "OpenInEditor", ArmLinkUnresolved,
			fmt.Sprintf("link %q resolved to no file (looked at %s)", href, strings.Join(link.candidates, ", ")),
			false, map[string]any{"href": href})
}

// linkQuestion composes the unresolved link's question from its brief. A brief
// that will not load or splice fails the click loudly, exactly as every other
// composed brief does: an empty question is never sent. OpenInEditorError has
// no brief arm, so the failure is an error (the transport's internal answer)
// beside the daemon-scoped prompts fault, never a `link_unresolved` that would
// claim a question was sent.
func (v *verbs) linkQuestion(ctx context.Context, log dlog.Logger, href string, candidates []string) (string, error) {
	brief, err := v.load(v.deps.PromptsDir, BriefLinkUnresolved)
	if err != nil {
		log.Error(opOpenInEditor, "could not read the unresolved-link brief", dlog.Context{
			"brief": BriefLinkUnresolved, "cause": err.Error(),
		})
		v.raisePromptsFault(ctx, log, fmt.Sprintf("the %s brief is unreadable: %v", BriefLinkUnresolved, err))
		return "", fmt.Errorf("open feed link %q: the %s brief is unreadable: %w", href, BriefLinkUnresolved, err)
	}
	looked := make([]string, 0, len(candidates))
	for i, candidate := range candidates {
		looked = append(looked, fmt.Sprintf("%d. `%s`", i+1, candidate))
	}
	question, err := v.splice(brief, map[string]string{
		"href":           href,
		"candidates":     strings.Join(looked, "\n"),
		"resolver":       filepath.Join(v.deps.CheckoutRoot, feedLinkResolverSite),
		"agent_repl_dir": v.deps.CheckoutRoot,
	})
	if err != nil {
		log.Error(opOpenInEditor, "could not splice the unresolved-link brief", dlog.Context{
			"brief": BriefLinkUnresolved, "cause": err.Error(),
		})
		v.raisePromptsFault(ctx, log, fmt.Sprintf("the %s brief will not splice: %v", BriefLinkUnresolved, err))
		return "", fmt.Errorf("open feed link %q: the %s brief will not splice: %w", href, BriefLinkUnresolved, err)
	}
	v.clearPromptsFault(ctx, log)
	return question, nil
}
