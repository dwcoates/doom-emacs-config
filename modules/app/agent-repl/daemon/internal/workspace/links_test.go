package workspace

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
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
