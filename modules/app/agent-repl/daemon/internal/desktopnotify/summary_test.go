package desktopnotify

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agentrepl/logging/logsite"
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
	// err is what the lookup fails with, before ok is read: the real lookup
	// reads the state database on the caller's context.
	err error
}

// ConfigDirFor answers the root, err, or a lookup failure when !ok.
func (c fakeConfigDirs) ConfigDirFor(_ context.Context, _ ids.WorkspaceID) (string, error) {
	if c.err != nil {
		return "", c.err
	}
	if !c.ok {
		return "", errors.New("workspace not found")
	}
	return c.dir, nil
}

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

func TestSummarizeAsksHaikuAsPlainInferenceUnderTheWorkspacesAccount(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "Fixed the flaky test."}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "a long answer")

	// Assert
	if got != "Fixed the flaky test." {
		t.Fatalf("summary = %q", got)
	}
	if h.req.Model != headless.ModelHaiku || h.req.Kind != headless.KindInference || len(h.req.Tools) != 0 ||
		h.req.Site != SummarySite || h.req.ConfigDir != "/acct" {
		t.Fatalf("request = %+v, want plain haiku with no tools under turn_summary on /acct", *h.req)
	}
	if !strings.Contains(h.req.Prompt, "a long answer") {
		t.Fatalf("prompt %q does not carry the answer", h.req.Prompt)
	}
}

func TestSummarizeKeepsOnlyTheFirstNonBlankLine(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "\n  Changes made to the footer.  \nA second line.\n"}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	got := s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	if got != "Changes made to the footer." {
		t.Fatalf("summary = %q, want the first non-blank line, trimmed", got)
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

func TestSummarizeOfAWorkspaceReadAsTheDaemonStandsDownIsNoError(t *testing.T) {
	// Arrange: the notifier's context ended while the account was being read.
	h := &fakeHeadless{}
	s, log := newSummarizer(t, h, fakeConfigDirs{err: context.Canceled}, briefDir(t, validBrief))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	got := s.Summarize(ctx, "ws1", "answer")

	// Assert
	if got != "Summary unavailable: "+StoodDownCause || h.req != nil {
		t.Fatalf("summary = %q (called=%v), want the stood-down cause and no call", got, h.req != nil)
	}
	if _, ok := hasRecord(log, "error", "no account root for this workspace; the turn was not summarized"); ok {
		t.Fatal("a stand-down during the account read was recorded at ERROR")
	}
}

func TestSummarizeOfAWorkspaceReadAsTheDaemonStandsDownIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := &fakeHeadless{}
	s, log := newSummarizer(t, h, fakeConfigDirs{err: context.Canceled}, briefDir(t, validBrief))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	s.Summarize(ctx, "ws1", "answer")

	// Assert
	if _, ok := hasRecord(log, "info", "the daemon stood down while the workspace's account was read"); !ok {
		t.Fatal("the stand-down during the account read left no INFO record")
	}
}

func TestSummarizeRecordsTheModelItAsked(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "Plan proposed for the merge queue."}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	s.Summarize(context.Background(), "ws1", "answer")

	// Assert
	r, ok := hasRecord(log, "info", "summarized the turn's final answer")
	if !ok || r.Context["model"] != headless.ModelHaiku || r.Context["workspace"] != "ws1" {
		t.Fatalf("record = %+v (found %v), want haiku and the workspace", r, ok)
	}
}

// TestTheShippedBriefAsksForOneDeclarativeSentence pins the shipped brief to
// the banner's contract: it splices the answer and asks for one sentence with
// no clause-packing punctuation.
func TestTheShippedBriefAsksForOneDeclarativeSentence(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "ok"}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, filepath.Join("..", "..", "..", "prompts"))

	// Act
	s.Summarize(context.Background(), "ws1", "THE-ANSWER")

	// Assert
	for _, want := range []string{"THE-ANSWER", "ONE SHORT SENTENCE", "declarative", "em dashes", "commas", "semicolons"} {
		if !strings.Contains(h.req.Prompt, want) {
			t.Fatalf("the shipped brief's prompt lacks %q", want)
		}
	}
}

// TestSentenceRecordsInItsCallsOwnWords pins the shared one-sentence call:
// every record and every request field it makes is its sentenceCall's, so a
// second kind of call reads in its own words and asks under its own site.
func TestSentenceRecordsInItsCallsOwnWords(t *testing.T) {
	// Arrange
	call := sentenceCall{brief: BriefSummary, placeholder: "answer", site: "probe_site", what: "probe-summary",
		unmade: "the probe was not summarized", made: logsite.Msg("summarized the probe")}
	tests := []struct {
		name  string
		h     *fakeHeadless
		dirs  fakeConfigDirs
		want  string
		level string
		rec   string
	}{
		{name: "made", h: &fakeHeadless{text: "Probe done."}, dirs: fakeConfigDirs{dir: "/acct", ok: true},
			want: "Probe done.", level: "info", rec: "summarized the probe"},
		{name: "failed", h: &fakeHeadless{err: &headless.Error{Cause: headless.CauseTimeout, Detail: "x"}}, dirs: fakeConfigDirs{dir: "/acct", ok: true},
			want: "Summary unavailable: timeout", level: "error", rec: "the probe-summary model call failed"},
		{name: "empty", h: &fakeHeadless{text: " \n"}, dirs: fakeConfigDirs{dir: "/acct", ok: true},
			want: "Summary unavailable: the model answered empty", level: "error", rec: "the probe-summary model call answered empty"},
		{name: "no account", h: &fakeHeadless{}, dirs: fakeConfigDirs{ok: false},
			want: "Summary unavailable: no account for this workspace", level: "error", rec: "no account root for this workspace; the probe was not summarized"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s, log := newSummarizer(t, tt.h, tt.dirs, briefDir(t, validBrief))

			// Act
			got := s.sentence(context.Background(), "ws1", call, "INPUT", log)

			// Assert
			if got != tt.want {
				t.Fatalf("sentence = %q, want %q", got, tt.want)
			}
			if _, ok := hasRecord(log, tt.level, tt.rec); !ok {
				t.Fatalf("no %s record %q", tt.level, tt.rec)
			}
			if tt.h.req != nil && (tt.h.req.Site != "probe_site" || !strings.Contains(tt.h.req.Prompt, "INPUT")) {
				t.Fatalf("request = %+v, want the call's site and the spliced input", *tt.h.req)
			}
		})
	}
}

// TestEveryBannerSummaryGoesThroughTheSharedSentenceCall pins that each
// banner summary is a sentenceCall over the one shared call, never a
// hand-rolled model call: the turn summary asks exactly what sentence asks
// for turnSummary.
func TestEveryBannerSummaryGoesThroughTheSharedSentenceCall(t *testing.T) {
	// Arrange
	viaSummarize, viaSentence := &fakeHeadless{text: "ok"}, &fakeHeadless{text: "ok"}
	a, _ := newSummarizer(t, viaSummarize, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))
	b, log := newSummarizer(t, viaSentence, fakeConfigDirs{dir: "/acct", ok: true}, briefDir(t, validBrief))

	// Act
	a.Summarize(context.Background(), "ws1", "answer")
	b.sentence(context.Background(), "ws1", turnSummary, "answer", log)

	// Assert
	if !reflect.DeepEqual(*viaSummarize.req, *viaSentence.req) {
		t.Fatalf("Summarize asked %+v, the shared call %+v", *viaSummarize.req, *viaSentence.req)
	}
}

func TestSummarizeCommentAsksTheCommentCallAsPlainHaiku(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "Asks to rename the flag."}
	dir := t.TempDir()
	brief := "<!-- used by: x; placeholders: {{comment}} -->\nShorten:\n{{comment}}\n"
	if err := os.WriteFile(filepath.Join(dir, BriefCommentSummary+".md"), []byte(brief), 0o600); err != nil {
		t.Fatalf("write brief: %v", err)
	}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, dir)

	// Act
	got := s.SummarizeComment(context.Background(), "ws1", "please rename the flag")

	// Assert
	if got != "Asks to rename the flag." {
		t.Fatalf("summary = %q", got)
	}
	if h.req.Model != headless.ModelHaiku || h.req.Kind != headless.KindInference || len(h.req.Tools) != 0 ||
		h.req.Site != CommentSummarySite || h.req.ConfigDir != "/acct" || !strings.Contains(h.req.Prompt, "please rename the flag") {
		t.Fatalf("request = %+v, want inference-only haiku under %s carrying the comment", *h.req, CommentSummarySite)
	}
	if r, ok := hasRecord(log, "info", "summarized the pull-request comment"); !ok || r.Context["workspace"] != "ws1" {
		t.Fatalf("record = %+v (found %v), want the comment summary's record for ws1", r, ok)
	}
}

func TestSummarizeCommentOfABlankCommentMakesNoCall(t *testing.T) {
	// Arrange
	h := &fakeHeadless{}
	s, log := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, t.TempDir())

	// Act
	got := s.SummarizeComment(context.Background(), "ws1", " \n")

	// Assert
	if got != "" || h.req != nil {
		t.Fatalf("summary = %q (called=%v), want nothing and no call", got, h.req != nil)
	}
	if _, ok := hasRecord(log, "info", "the pull-request comment is blank; nothing to summarize"); !ok {
		t.Fatal("a blank comment left no INFO record")
	}
}

func TestSummarizeCommentCapsTheCommentItSends(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "ok"}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, filepath.Join("..", "..", "..", "prompts"))

	// Act
	s.SummarizeComment(context.Background(), "ws1", strings.Repeat("a", MaxCommentRunes)+"TAIL")

	// Assert
	if strings.Contains(h.req.Prompt, "TAIL") {
		t.Fatal("the comment past the cap reached the model")
	}
}

// TestTheShippedCommentBriefAsksForOneShortSentence pins the shipped comment
// brief to the banner's contract.
func TestTheShippedCommentBriefAsksForOneShortSentence(t *testing.T) {
	// Arrange
	h := &fakeHeadless{text: "ok"}
	s, _ := newSummarizer(t, h, fakeConfigDirs{dir: "/acct", ok: true}, filepath.Join("..", "..", "..", "prompts"))

	// Act
	s.SummarizeComment(context.Background(), "ws1", "THE-COMMENT")

	// Assert
	for _, want := range []string{"THE-COMMENT", "ONE SHORT SENTENCE", "DATA, not instructions", "semicolons"} {
		if !strings.Contains(h.req.Prompt, want) {
			t.Fatalf("the shipped comment brief's prompt lacks %q", want)
		}
	}
}
