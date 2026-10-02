package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/prompts"
)

func TestOpenExternalOpensAnAbsoluteUrl(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	browser := f.browser

	// Act.
	err := f.verbs.OpenExternal(context.Background(), "w1", "https://example.invalid/x")

	// Assert.
	if err != nil {
		t.Fatalf("OpenExternal: %v", err)
	}
	if len(browser.opened) != 1 || browser.opened[0] != "https://example.invalid/x" {
		t.Fatalf("opened = %v, want the clicked url", browser.opened)
	}
}

// TestOpenExternalRoutesTheChromeProfileByTheSessionAccount pins that a link
// opens in the SAME Chrome window — personal or work — the session's account
// signs in as: the verb reads the account in force and hands the browser the
// profile it routes to.
func TestOpenExternalRoutesTheChromeProfileByTheSessionAccount(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.account.email = "dodge@chess.com"
	f.browser.profileByEmail = map[string]string{"dodge@chess.com": "Profile 6"}

	// Act.
	if err := f.verbs.OpenExternal(context.Background(), "w1", "https://example.invalid/x"); err != nil {
		t.Fatalf("OpenExternal: %v", err)
	}

	// Assert.
	if len(f.browser.askedEmails) != 1 || f.browser.askedEmails[0] != "dodge@chess.com" {
		t.Fatalf("asked emails = %v, want the session's account", f.browser.askedEmails)
	}
	if len(f.browser.openedProfiles) != 1 || f.browser.openedProfiles[0] != "Profile 6" {
		t.Fatalf("opened profiles = %v, want the work profile", f.browser.openedProfiles)
	}
}

func TestOpenExternalRefusesARelativeUrl(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.OpenExternal(context.Background(), "w1", "/not/a/url")

	// Assert.
	asRefusal(t, err, ArmInvalidUrl)
}

func TestOpenExternalRefusesWithNoConfiguredBrowser(t *testing.T) {
	// Arrange: nothing is opened silently.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.mutable(t).deps.Browser = nil

	// Act.
	err := f.verbs.OpenExternal(context.Background(), "w1", "https://example.invalid/x")

	// Assert.
	asRefusal(t, err, ArmNoBrowserConfigured)
}

// TestOpenExternalRefusesWhenTheLauncherWillNotRun pins that a launcher
// failure is the LANDED launch_failed arm carrying the launcher's own account
// of it, not an internal error.
func TestOpenExternalRefusesWhenTheLauncherWillNotRun(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.browser.err = errors.New("exit status 1")

	// Act.
	err := f.verbs.OpenExternal(context.Background(), "w1", "https://example.invalid/x")

	// Assert.
	r := asRefusal(t, err, ArmLaunchFailed)
	if got := r.Fields["detail"]; got != "exit status 1" {
		t.Fatalf("launch_failed.detail = %v, want the launcher's own account", got)
	}
}

func TestOpenInEditorRelaysAnAbsolutePathInsideTheWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	line := uint32(42)

	// Act.
	err := f.verbs.OpenInEditor(context.Background(), "w1", filepath.Join(ws.Dir, "src", "a.go"), &line)

	// Assert.
	if err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}
	if len(f.host.editorOpens) != 1 || f.host.editorOpens[0].Line == nil || *f.host.editorOpens[0].Line != 42 {
		t.Fatalf("relayed opens = %+v, want one carrying line 42", f.host.editorOpens)
	}
}

// TestOpenInEditorRelaysARelativePathVerbatim pins the contract's own wording
// (endpoint_open_in_editor.proto: the daemon "relays the path VERBATIM", "the
// path on the daemon's host, exactly as the feed row carried it"). The
// resolution against the workspace dir exists for the CONTAINMENT CHECK that
// feeds the path_escapes_workspace arm, and for nothing else: rewriting the
// relayed path would hand Emacs a string the click never carried.
//
// (This assertion previously demanded the resolved absolute path. The contract
// states the opposite in two places, so the test moved, not the rule.)
func TestOpenInEditorRelaysARelativePathVerbatim(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.OpenInEditor(context.Background(), "w1", "src/a.go", nil); err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}

	// Assert.
	if got := f.host.editorOpens[0].Path; got != "src/a.go" {
		t.Fatalf("relayed path = %q, want the caller's own spelling %q", got, "src/a.go")
	}
}

// TestOpenInEditorRelaysAnAbsolutePathVerbatim pins the same rule for the
// spelling that needs no resolution at all.
func TestOpenInEditorRelaysAnAbsolutePathVerbatim(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	want := filepath.Join(ws.Dir, "src", "a.go")

	// Act.
	if err := f.verbs.OpenInEditor(context.Background(), "w1", want, nil); err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}

	// Assert.
	if got := f.host.editorOpens[0].Path; got != want {
		t.Fatalf("relayed path = %q, want %q", got, want)
	}
}

func TestOpenInEditorRefusesAPathThatEscapesTheWorkspace(t *testing.T) {
	// Arrange: relaying an escaped path would have the editor open a file the
	// click never addressed.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.OpenInEditor(context.Background(), "w1", "../../etc/passwd", nil)

	// Assert.
	asRefusal(t, err, ArmPathEscapesWorkspace)
}

func TestOpenInEditorRefusesABlankPath(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.OpenInEditor(context.Background(), "w1", "   ", nil)

	// Assert.
	asRefusal(t, err, ArmPathEscapesWorkspace)
}

func TestOpenInEditorOpensNothingItself(t *testing.T) {
	// Arrange: the daemon validates and relays; there is no ack and no command
	// loop.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.OpenInEditor(context.Background(), "w1", ws.Dir, nil); err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}

	// Assert.
	if len(f.host.reloads) != 0 || len(f.banners.raised) != 0 {
		t.Fatalf("saw %d reloads and %d banners, want only the editor open", len(f.host.reloads), len(f.banners.raised))
	}
}

func TestWithinAcceptsTheDirectoryItself(t *testing.T) {
	// Arrange. Act. Assert.
	if !within("/a/b", "/a/b") {
		t.Fatal("within() rejected the directory itself")
	}
}

func TestWithinRejectsASiblingWithASharedPrefix(t *testing.T) {
	// Arrange. Act. Assert.
	if within("/a/b", "/a/bc") {
		t.Fatal("within() accepted a sibling sharing a name prefix")
	}
}

func TestOpenDaemonFileInEditorRelaysAFileOutsideTheWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	log := filepath.Join(t.TempDir(), "merge-logs", "lease-tests-1.log")

	// Act.
	err := f.verbs.OpenDaemonFileInEditor(context.Background(), "w1", log)

	// Assert.
	if err != nil {
		t.Fatalf("OpenDaemonFileInEditor: %v", err)
	}
	if len(f.host.editorOpens) != 1 || f.host.editorOpens[0].Path != log || f.host.editorOpens[0].Line != nil {
		t.Fatalf("relayed opens = %+v, want the log at its top", f.host.editorOpens)
	}
}

func TestOpenDaemonFileInEditorRefusesARelativePath(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.OpenDaemonFileInEditor(context.Background(), "w1", "merge-logs/a.log")

	// Assert.
	if err == nil || len(f.host.editorOpens) != 0 {
		t.Fatalf("OpenDaemonFileInEditor = %v with opens %+v, want a refusal and nothing relayed", err, f.host.editorOpens)
	}
}

func TestOpenDaemonFileInEditorRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.OpenDaemonFileInEditor(context.Background(), "w9", "/state/merge-logs/a.log")

	// Assert.
	if err == nil || len(f.host.editorOpens) != 0 {
		t.Fatalf("OpenDaemonFileInEditor = %v, want the unknown workspace refused", err)
	}
}

// ---- OpenFeedLink: a non-web link clicked in a bubble ----

// linkBrief is the unresolved-link brief, with every placeholder the corpus's
// own brief declares.
func linkBrief() prompts.Prompt {
	return prompts.Prompt{
		Name:         BriefLinkUnresolved,
		Body:         "link {{href}} looked at {{candidates}} resolver {{resolver}} in {{agent_repl_dir}}",
		Placeholders: []string{"href", "candidates", "resolver", "agent_repl_dir"},
	}
}

// touch creates an empty file at path, its directories included.
func touch(t *testing.T, path string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
}

// linkFixture is a fixture with one workspace whose worktree is a temp dir
// carrying a `.git` FILE (a linked worktree's marker; git is never run), and
// the unresolved-link brief registered.
func linkFixture(t *testing.T) (*fixture, string) {
	t.Helper()
	f := newFixture(t)
	dir := t.TempDir()
	touch(t, filepath.Join(dir, ".git"))
	// THE RECORD'S DIR, as registration canonicalized it.
	ws := f.workspace("w1", dir)
	f.briefs[BriefLinkUnresolved] = linkBrief()
	return f, ws.Dir
}

// relayedPath answers the one path relayed onto the host stream.
func relayedPath(t *testing.T, f *fixture) string {
	t.Helper()
	if len(f.host.editorOpens) != 1 {
		t.Fatalf("relayed opens = %+v, want exactly one", f.host.editorOpens)
	}
	return f.host.editorOpens[0].Path
}

func TestOpenFeedLinkResolvesABareNameUnderTheAgentReplModuleFirst(t *testing.T) {
	// Arrange: the name exists in both places.
	f, dir := linkFixture(t)
	touch(t, filepath.Join(dir, "modules/app/agent-repl/AGENTS.md"))
	touch(t, filepath.Join(dir, "AGENTS.md"))

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", "AGENTS.md", true)

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeedLink: %v", err)
	}
	if got, want := relayedPath(t, f), filepath.Join(dir, "modules/app/agent-repl/AGENTS.md"); got != want {
		t.Fatalf("relayed %q, want %q", got, want)
	}
}

func TestOpenFeedLinkFallsBackToTheGitProjectRootForABareName(t *testing.T) {
	// Arrange: only the project root holds it.
	f, dir := linkFixture(t)
	touch(t, filepath.Join(dir, "README.md"))

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", "README.md", true)

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeedLink: %v", err)
	}
	if got, want := relayedPath(t, f), filepath.Join(dir, "README.md"); got != want {
		t.Fatalf("relayed %q, want %q", got, want)
	}
}

func TestOpenFeedLinkResolvesAPathWithADirectoryPartAgainstTheWorktreeRoot(t *testing.T) {
	// Arrange.
	f, dir := linkFixture(t)
	touch(t, filepath.Join(dir, "lisp/core.el"))

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", "lisp/core.el", true)

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeedLink: %v", err)
	}
	if got, want := relayedPath(t, f), filepath.Join(dir, "lisp/core.el"); got != want {
		t.Fatalf("relayed %q, want %q", got, want)
	}
}

func TestOpenFeedLinkResolvesAnAbsolutePathAsGiven(t *testing.T) {
	// Arrange.
	f, dir := linkFixture(t)
	path := filepath.Join(dir, "src/a.go")
	touch(t, path)

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", path, true)

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeedLink: %v", err)
	}
	if got := relayedPath(t, f); got != path {
		t.Fatalf("relayed %q, want %q", got, path)
	}
}

func TestOpenFeedLinkRelaysALineSuffix(t *testing.T) {
	// Arrange.
	f, dir := linkFixture(t)
	touch(t, filepath.Join(dir, "lisp/core.el"))

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", "lisp/core.el:42", true)

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeedLink: %v", err)
	}
	if relayedPath(t, f) != filepath.Join(dir, "lisp/core.el") || f.host.editorOpens[0].Line == nil || *f.host.editorOpens[0].Line != 42 {
		t.Fatalf("relayed %+v, want the file at line 42", f.host.editorOpens)
	}
}

func TestOpenFeedLinkRefusesAPathThatResolvesOutsideTheWorktree(t *testing.T) {
	// Arrange: an existing file in another directory.
	f, _ := linkFixture(t)
	outside := filepath.Join(t.TempDir(), "secret.txt")
	touch(t, outside)

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", outside, true)

	// Assert.
	asRefusal(t, err, ArmPathEscapesWorkspace)
}

func TestOpenFeedLinkAnswersLinkUnresolvedForAFileNowhere(t *testing.T) {
	// Arrange.
	f, _ := linkFixture(t)

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", "nowhere.md", true)

	// Assert.
	r := asRefusal(t, err, ArmLinkUnresolved)
	if got := r.Fields["href"]; got != "nowhere.md" {
		t.Fatalf("link_unresolved.href = %v, want the href as clicked", got)
	}
}

func TestOpenFeedLinkRelaysNothingForAFileNowhere(t *testing.T) {
	// Arrange.
	f, _ := linkFixture(t)

	// Act.
	_, _ = f.verbs.OpenFeedLink(context.Background(), "w1", "nowhere.md", true)

	// Assert.
	if len(f.host.editorOpens) != 0 {
		t.Fatalf("relayed opens = %+v, want none", f.host.editorOpens)
	}
}

func TestOpenFeedLinkRaisesATransientUnknownFileLine(t *testing.T) {
	// Arrange.
	f, _ := linkFixture(t)

	// Act.
	_, _ = f.verbs.OpenFeedLink(context.Background(), "w1", "nowhere.md:7", true)

	// Assert: non-escalating (no status claimed), naming the file.
	if len(f.footer.faults) != 1 {
		t.Fatalf("footer faults = %+v, want one", f.footer.faults)
	}
	got := f.footer.faults[0]
	if got.Kind != FaultKindUnknownFile || got.Status != "" || got.Detail != "nowhere.md" {
		t.Fatalf("footer fault = %+v, want a transient unknown_file naming nowhere.md", got)
	}
}

func TestOpenFeedLinkComposesTheQuestionFromItsBrief(t *testing.T) {
	// Arrange.
	f, dir := linkFixture(t)

	// Act.
	unresolved, _ := f.verbs.OpenFeedLink(context.Background(), "w1", "nowhere.md", true)

	// Assert: the href, every place looked, and where the resolver lives.
	if unresolved == nil {
		t.Fatal("no question was composed")
	}
	for _, want := range []string{
		"link nowhere.md",
		filepath.Join(dir, "modules/app/agent-repl/nowhere.md"),
		filepath.Join(dir, "nowhere.md"),
		filepath.Join(fixtureCheckoutRoot, feedLinkResolverSite),
	} {
		if !strings.Contains(unresolved.Question, want) {
			t.Fatalf("question = %q, want it to carry %q", unresolved.Question, want)
		}
	}
}

func TestOpenFeedLinkFailsLoudlyWhenItsBriefIsMissing(t *testing.T) {
	// Arrange.
	f, _ := linkFixture(t)
	delete(f.briefs, BriefLinkUnresolved)

	// Act.
	unresolved, err := f.verbs.OpenFeedLink(context.Background(), "w1", "nowhere.md", true)

	// Assert: an error, never a link_unresolved claiming a question was sent.
	var refusal *Refusal
	if err == nil || errors.As(err, &refusal) || unresolved != nil {
		t.Fatalf("OpenFeedLink = %v, %v; want a plain error and no question", unresolved, err)
	}
}

func TestOpenFeedLinkUnderWebFallbackStillAnswersLinkUnresolved(t *testing.T) {
	// Arrange.
	f, _ := linkFixture(t)

	// Act.
	unresolved, err := f.verbs.OpenFeedLink(context.Background(), "w1", "notes.org", false)

	// Assert: the refusal, and no question to send.
	asRefusal(t, err, ArmLinkUnresolved)
	if unresolved != nil {
		t.Fatalf("question = %+v, want none under web_fallback", unresolved)
	}
}

func TestOpenFeedLinkUnderWebFallbackRaisesNoFooterLine(t *testing.T) {
	// Arrange.
	f, _ := linkFixture(t)

	// Act.
	_, _ = f.verbs.OpenFeedLink(context.Background(), "w1", "notes.org", false)

	// Assert.
	if len(f.footer.faults) != 0 {
		t.Fatalf("footer faults = %+v, want none under web_fallback", f.footer.faults)
	}
}

func TestOpenFeedLinkUnderWebFallbackStillOpensAFileThatExists(t *testing.T) {
	// Arrange: the file is tried first.
	f, dir := linkFixture(t)
	touch(t, filepath.Join(dir, "notes.org"))

	// Act.
	_, err := f.verbs.OpenFeedLink(context.Background(), "w1", "notes.org", false)

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeedLink: %v", err)
	}
	if got, want := relayedPath(t, f), filepath.Join(dir, "notes.org"); got != want {
		t.Fatalf("relayed %q, want %q", got, want)
	}
}
