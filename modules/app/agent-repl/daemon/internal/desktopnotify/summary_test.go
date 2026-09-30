package desktopnotify

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/ids"
)

// fakeHeadless answers one scripted response and records the request.
type fakeHeadless struct {
	text string
	err  error
	req  *headless.Request
}

func (h *fakeHeadless) Run(_ context.Context, req headless.Request) (headless.Response, error) {
	h.req = &req
	return headless.Response{Text: h.text, Model: req.Model}, h.err
}

func (h *fakeHeadless) Bin() string { return "fake-claude" }

// fakeConfigDirs answers one account root.
type fakeConfigDirs struct {
	dir string
	ok  bool
}

func (c fakeConfigDirs) ConfigDirFor(ids.WorkspaceID) (string, bool) { return c.dir, c.ok }

// briefDir writes the summary brief into a temp prompts directory.
func briefDir(t *testing.T, body string) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, BriefSummary+".md"), []byte(body), 0o600); err != nil {
		t.Fatalf("write brief: %v", err)
	}
	return dir
}

const validBrief = "<!-- used by: x; placeholders: {{answer}} -->\nSummarize:\n{{answer}}\n"

func newSummarizer(t *testing.T, h *fakeHeadless, dirs fakeConfigDirs, prompts string) (Summarizer, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	return Summarizer{Headless: h, ConfigDirs: dirs, PromptsDir: prompts, Log: log}, log
}

func TestSummarizeAsksSonnetUnderTheWorkspacesAccount(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "Fixed the flaky test.\nAdded a regression test."}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "a long answer")

	// Assert
	if got != "Fixed the flaky test.\nAdded a regression test." {
		t.Fatalf("summary = %q", got)
	}
	if h.req.Model != headless.ModelSonnet || h.req.Site != SummarySite || h.req.ConfigDir != "/acct" {
		t.Fatalf("request = %+v, want sonnet under turn_summary on /acct", *h.req)
	}
	if !strings.Contains(h.req.Prompt, "a long answer") {
		t.Fatalf("prompt %q does not carry the answer", h.req.Prompt)
	}
}

func TestSummarizeKeepsAtMostThreeLines(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "one\n\ntwo\nthree\nfour\n"}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "one\ntwo\nthree" {
		t.Fatalf("summary = %q, want the first three non-blank lines", got)
	}
}

func TestSummarizeAnEmptyAnswerMakesNoCall(t *testing.T) {
	// Arrange
	h := &fakeHeadless{}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "  \n")

	// Assert
	if got != NoAnswerLine || h.req != nil {
		t.Fatalf("summary = %q (called=%v), want the no-answer line and no call", got, h.req != nil)
	}
}

func TestSummarizeSurfacesAFailedCall(t *testing.T) {
	// Arrange
	h := &fakeHeadless{err: &headless.Error{Cause: headless.CauseTimeout, Detail: "30s"}}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "Summary unavailable: timeout" {
		t.Fatalf("summary = %q, want the cause named", got)
	}
	r, ok := hasRecord(log, "error", "the turn-summary model call failed")
	if !ok || r.Context["cause"] != headless.CauseTimeout || r.Context["workspace"] != "ws1" {
		t.Fatalf("record = %+v (found %v), want the cause and workspace", r, ok)
	}
}

func TestSummarizeRecordsAStandDownAsInfoNotError(t *testing.T) {
	// Arrange
	h := &fakeHeadless{err: &headless.Error{Cause: headless.CauseExitStatus, Detail: "signal: killed"}}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	got := s.Summarize(ctx, "ws1", "answer")

	// Assert
	if got != "Summary unavailable: "+StoodDownCause {
		t.Fatalf("summary = %q, want the stand-down named", got)
	}
	r, ok := hasRecord(log, "info", "the daemon stood down during the turn-summary call")
	if !ok || r.Context["workspace"] != "ws1" || r.Context["detail"] != "exit_status: signal: killed" {
		t.Fatalf("record = %+v (found %v), want the workspace and the call's detail", r, ok)
	}
	if _, bad := hasRecord(log, "error", "the turn-summary model call failed"); bad {
		t.Fatal("a stand-down was recorded as a failed call")
	}
}

func TestSummarizeSurfacesAnEmptyAnswerFromTheModel(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "\n  \n"}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "Summary unavailable: the model answered empty" {
		t.Fatalf("summary = %q", got)
	}
	if _, ok := hasRecord(log, "error", "the turn-summary model call answered empty"); !ok {
		t.Fatal("an empty model answer left no ERROR record")
	}
}

func TestSummarizeSurfacesAMissingBrief(t *testing.T) {
	// Arrange
	h := &fakeHeadless{}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, t.TempDir())

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "Summary unavailable: the summary brief could not be read" || h.req != nil {
		t.Fatalf("summary = %q (called=%v)", got, h.req != nil)
	}
	if _, ok := hasRecord(log, "error", "the turn-summary brief could not be read"); !ok {
		t.Fatal("a missing brief left no ERROR record")
	}
}

func TestSummarizeSurfacesABriefWithTheWrongPlaceholder(t *testing.T) {
	// Arrange
	h := &fakeHeadless{}
	brief := "<!-- used by: x; placeholders: {{digest}} -->\n{{digest}}\n"
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, brief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "Summary unavailable: the summary brief could not be spliced" || h.req != nil {
		t.Fatalf("summary = %q (called=%v)", got, h.req != nil)
	}
	if _, ok := hasRecord(log, "error", "the turn-summary brief could not be spliced"); !ok {
		t.Fatal("an unspliceable brief left no ERROR record")
	}
}

func TestSummarizeSurfacesAWorkspaceWithNoAccount(t *testing.T) {
	// Arrange
	h := &fakeHeadless{}
	s, log := newSummarizer(t, h, fakeConfigDirs{ok: false}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "Summary unavailable: no account for this workspace" || h.req != nil {
		t.Fatalf("summary = %q (called=%v)", got, h.req != nil)
	}
	if _, ok := hasRecord(log, "error", "no account root for this workspace; the turn was not summarized"); !ok {
		t.Fatal("a workspace with no account left no ERROR record")
	}
}

func TestSummarizeCapsTheAnswerItSends(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "ok"}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))
	answer := strings.Repeat("a", MaxAnswerRunes) + "TAIL"

	// Act
	s.Summarize(context.Background(), "ws1", answer)

	// Assert
	if strings.Contains(h.req.Prompt, "TAIL") {
		t.Fatal("the answer past the cap reached the model")
	}
}
