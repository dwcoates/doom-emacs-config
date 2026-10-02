package titlesynth

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"unicode/utf8"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/ids"
)

const testWS = ids.WorkspaceID("ws-1")

// ---- fakes ----------------------------------------------------------------

// fakeDigester answers a scripted digest response (or error).
type fakeDigester struct {
	resp  *shimv1.GatherTitleDigestResponse
	err   error
	calls int
}

func (d *fakeDigester) GatherTitleDigest(_ context.Context, _ ids.WorkspaceID) (*shimv1.GatherTitleDigestResponse, error) {
	d.calls++
	return d.resp, d.err
}

// fakeHeadless is a headless.Runner that records each request and answers a
// scripted text or error. It NEVER spawns a model.
type fakeHeadless struct {
	mu    sync.Mutex
	text  string
	err   error
	calls []headless.Request
}

func (h *fakeHeadless) Bin() string { return "fake-claude" }

func (h *fakeHeadless) Run(_ context.Context, req headless.Request) (headless.Response, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.calls = append(h.calls, req)
	if h.err != nil {
		return headless.Response{}, h.err
	}
	return headless.Response{Text: h.text, Model: req.Model}, nil
}

func (h *fakeHeadless) callCount() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.calls)
}

func (h *fakeHeadless) lastRequest() headless.Request {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.calls[len(h.calls)-1]
}

// fakeConfigDirs answers a fixed config dir.
type fakeConfigDirs struct {
	dir string
	ok  bool
}

func (c *fakeConfigDirs) ConfigDirFor(_ ids.WorkspaceID) (string, bool) { return c.dir, c.ok }

// fakeTitles records every SetSynthesizedTitle.
type fakeTitles struct {
	mu     sync.Mutex
	titles []string
}

func (t *fakeTitles) SetSynthesizedTitle(_ ids.WorkspaceID, title string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.titles = append(t.titles, title)
}

func (t *fakeTitles) last() (string, bool) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if len(t.titles) == 0 {
		return "", false
	}
	return t.titles[len(t.titles)-1], true
}

// ---- fixtures -------------------------------------------------------------

// briefDir writes a minimal brief the synthesizer can load, so a test does not
// depend on the shipped brief's exact wording.
func briefDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	body := "<!-- used by: test; placeholders: {{digest}} -->\nSummarize:\n{{digest}}\n"
	if err := os.WriteFile(filepath.Join(dir, BriefTitle+".md"), []byte(body), 0o600); err != nil {
		t.Fatalf("write brief: %v", err)
	}
	return dir
}

// digestResponse builds a success response.
func digestResponse(boundary shimv1.TitleDigestBoundary, summary string, prompts ...string) *shimv1.GatherTitleDigestResponse {
	success := &shimv1.GatherTitleDigestSuccess{Boundary: boundary, Prompts: prompts}
	if summary != "" {
		success.LastCompactSummary = &summary
	}
	return &shimv1.GatherTitleDigestResponse{
		Result: &shimv1.GatherTitleDigestResponse_Success{Success: success},
	}
}

// harness wires a synthesizer over the four fakes.
type harness struct {
	synth   *Synthesizer
	digest  *fakeDigester
	model   *fakeHeadless
	configs *fakeConfigDirs
	titles  *fakeTitles
}

func newHarness(t *testing.T, resp *shimv1.GatherTitleDigestResponse) *harness {
	t.Helper()
	h := &harness{
		digest:  &fakeDigester{resp: resp},
		model:   &fakeHeadless{text: "Wire up the reconnect backoff"},
		configs: &fakeConfigDirs{dir: "/root/.claude", ok: true},
		titles:  &fakeTitles{},
	}
	h.synth = New(Deps{
		Digester:   h.digest,
		Headless:   h.model,
		ConfigDirs: h.configs,
		Titles:     h.titles,
		PromptsDir: briefDir(t),
		Log:        dlog.NewTestLogger(),
	})
	return h
}

// ---- tests ----------------------------------------------------------------

func TestSynthesizesWhenThereIsMaterialAndNoVendorTitle(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "make reconnect backoff"))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	got, ok := h.titles.last()
	if !ok || got != "Wire up the reconnect backoff" {
		t.Fatalf("synthesized title = %q (set=%v), want the model's answer", got, ok)
	}
}

func TestNoModelCallWhenTheDigestIsUnchanged(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "one prompt"))

	// Act — synthesize twice with the same digest.
	h.synth.synthesizeOnce(context.Background(), testWS)
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — the second is a no-op: at most one cheap call per new prompt.
	if got := h.model.callCount(); got != 1 {
		t.Fatalf("model calls = %d, want 1", got)
	}
}

func TestNoSynthesisWhenTheVendorTitleIsPresent(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))
	h.synth.OnVendorTitle(testWS)

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — the vendor's own title always wins, so we never call.
	if got := h.model.callCount(); got != 0 {
		t.Fatalf("model calls = %d, want 0 (vendor title present)", got)
	}
	if _, set := h.titles.last(); set {
		t.Fatalf("a title was synthesized despite the vendor's title")
	}
}

func TestTheModelCallBillsTheWorkspaceAccount(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))
	h.configs.dir = "/root/second-account"

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — the token spend is attributed to the workspace's own account.
	if got := h.model.lastRequest().ConfigDir; got != "/root/second-account" {
		t.Fatalf("headless ConfigDir = %q, want the workspace account", got)
	}
}

func TestTheModelCallAsksForTheCheapModel(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.lastRequest().Model; got != headless.ModelHaiku {
		t.Fatalf("headless Model = %q, want %q", got, headless.ModelHaiku)
	}
}

func TestTheModelCallAsksUnderItsOwnGuardSite(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.lastRequest().Site; got != Site {
		t.Fatalf("headless Site = %q, want %q", got, Site)
	}
}

func TestAGuardRefusalKeepsTheWorkspaceName(t *testing.T) {
	// Arrange — the vendor guard refuses the call, as it does under test.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))
	h.model.err = &headless.Error{Cause: headless.CauseGuardRefused, Detail: "forbidden"}

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — best effort: no title is installed, the name stands.
	if _, set := h.titles.last(); set {
		t.Fatalf("a title was installed despite the guard refusal")
	}
}

func TestADigestFailureKeepsTheWorkspaceName(t *testing.T) {
	// Arrange — the shim refuses the digest (no transcript yet).
	h := newHarness(t, &shimv1.GatherTitleDigestResponse{
		Result: &shimv1.GatherTitleDigestResponse_Failure{
			Failure: &shimv1.GatherTitleDigestFailure{
				Detail: "no transcript",
				Kind:   &shimv1.GatherTitleDigestFailure_NoTranscript{NoTranscript: &shimv1.GatherTitleDigestNoTranscript{}},
			},
		},
	})

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — no model call, no title.
	if got := h.model.callCount(); got != 0 {
		t.Fatalf("model calls = %d, want 0 (digest refused)", got)
	}
	if _, set := h.titles.last(); set {
		t.Fatalf("a title was installed despite the digest refusal")
	}
}

func TestAFreshConversationWithNoPromptsMakesNoCall(t *testing.T) {
	// Arrange — a NONE boundary with no prompts is a fresh session.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, ""))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.callCount(); got != 0 {
		t.Fatalf("model calls = %d, want 0 (nothing to summarize)", got)
	}
	if _, set := h.titles.last(); set {
		t.Fatalf("a title was installed for an empty conversation")
	}
}

func TestAClearWithNoPromptsRetractsTheStaleTitle(t *testing.T) {
	// Arrange — a /clear left an empty transcript; the old title is stale.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR, ""))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — the title is retracted (empty) so it falls back to the name.
	got, ok := h.titles.last()
	if !ok || got != "" {
		t.Fatalf("synthesized title = %q (set=%v), want a retraction", got, ok)
	}
	if calls := h.model.callCount(); calls != 0 {
		t.Fatalf("model calls = %d, want 0 (nothing to summarize after a clear)", calls)
	}
}

func TestOnContextResetForcesRe_Synthesis(t *testing.T) {
	// Arrange — one synthesis, then a /compact reset.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Act — reset the hash, then synthesize the SAME digest again.
	h.synth.OnContextReset(testWS)
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — the reset forces a second call the hash guard would otherwise skip.
	if got := h.model.callCount(); got != 2 {
		t.Fatalf("model calls = %d, want 2 (reset re-synthesizes)", got)
	}
}

func TestOnTurnEndedSynthesizesAsynchronously(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", "a prompt"))

	// Act — the public trigger dispatches a goroutine; Wait joins it.
	h.synth.OnTurnEnded(testWS)
	h.synth.Wait()

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "Wire up the reconnect backoff" {
		t.Fatalf("synthesized title = %q (set=%v), want the async run's result", got, ok)
	}
}

func TestCleanTitleTakesTheFirstLineAndStripsQuotes(t *testing.T) {
	// Arrange, Act, Assert — one plain line, quotes removed, extra lines dropped.
	if got := cleanTitle("  \"Wire up the backoff\"\nignored second line"); got != "Wire up the backoff" {
		t.Fatalf("cleanTitle = %q, want the first line unquoted", got)
	}
}

func TestComposeDigestIncludesTheCompactionSummary(t *testing.T) {
	// Arrange, Act.
	got := ComposeDigest("earlier we discussed backoff", []string{"now add jitter"})

	// Assert.
	if !containsAll(got, "earlier we discussed backoff", "now add jitter") {
		t.Fatalf("ComposeDigest = %q, want the summary and the prompt", got)
	}
}

func TestComposeDigestEnforcesTheTotalCap(t *testing.T) {
	// Arrange — MaxPrompts prompts, each near MaxPromptRunes, sum well past
	// MaxDigestTotalRunes on their own.
	var prompts []string
	for i := 0; i < MaxPrompts; i++ {
		prompts = append(prompts, strings.Repeat("x", MaxPromptRunes))
	}

	// Act.
	got := ComposeDigest(strings.Repeat("y", MaxPromptRunes*4), prompts)

	// Assert.
	if n := utf8.RuneCountInString(got); n > MaxDigestTotalRunes {
		t.Fatalf("ComposeDigest rendered %d runes, want at most MaxDigestTotalRunes (%d)", n, MaxDigestTotalRunes)
	}
}

func TestComposeDigestTrimmingToTheTotalCapKeepsTheNewestPrompts(t *testing.T) {
	// Arrange — enough padded, uniquely-marked prompts that the rendered digest
	// must drop some of them (and the summary) to fit under the total cap.
	const padded = 400
	var prompts []string
	for i := 0; i < MaxPrompts; i++ {
		prompts = append(prompts, fmt.Sprintf("marker-%02d %s", i, strings.Repeat("x", padded)))
	}

	// Act.
	got := ComposeDigest(strings.Repeat("summary text ", 200), prompts)

	// Assert — the newest prompt survives, the summary and the oldest of the
	// batch are the material dropped to make room for it.
	newest := fmt.Sprintf("marker-%02d", MaxPrompts-1)
	oldest := fmt.Sprintf("marker-%02d", 0)
	if !strings.Contains(got, newest) {
		t.Fatalf("ComposeDigest = %q, want the newest prompt (%s) to survive trimming", got, newest)
	}
	if strings.Contains(got, oldest) {
		t.Fatalf("ComposeDigest = %q, want the oldest prompt (%s) trimmed first", got, oldest)
	}
	if strings.Contains(got, "summary text") {
		t.Fatalf("ComposeDigest = %q, want the summary dropped before any surviving prompt is", got)
	}
	if n := utf8.RuneCountInString(got); n > MaxDigestTotalRunes {
		t.Fatalf("ComposeDigest rendered %d runes, want at most MaxDigestTotalRunes (%d)", n, MaxDigestTotalRunes)
	}
}

func TestComposeDigestOpensTemplateDelimitersAUserTyped(t *testing.T) {
	cases := []struct {
		name   string
		prompt string
	}{
		{name: "a placeholder token", prompt: "why does {{prompt}} not splice"},
		{name: "a run of three braces", prompt: "a {{{ b }}} c"},
		{name: "an unclosed opener", prompt: "a {{ b"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := ComposeDigest("", []string{tc.prompt})

			// Assert — no substring reads as a template delimiter any more.
			if strings.Contains(got, "{{") || strings.Contains(got, "}}") {
				t.Fatalf("ComposeDigest = %q, want no template delimiter left", got)
			}
		})
	}
}

func TestTheShippedBriefSplicesADigestThatTalksAboutTemplates(t *testing.T) {
	// Arrange — the real brief, and a conversation that names a placeholder.
	brief, err := loadBrief(filepath.Join("..", "..", "..", "prompts"))
	if err != nil {
		t.Fatalf("loadBrief: %v", err)
	}

	// Act.
	_, err = brief.Splice(map[string]string{"digest": ComposeDigest("", []string{"rename {{prompt}}"})})

	// Assert.
	if err != nil {
		t.Fatalf("Splice = %v, want a conversation about templates accepted", err)
	}
}

func TestTheShippedBriefLoadsAndDeclaresTheDigestPlaceholder(t *testing.T) {
	// Arrange — the real brief this daemon ships.
	dir := filepath.Join("..", "..", "..", "prompts")

	// Act.
	brief, err := loadBrief(dir)

	// Assert.
	if err != nil {
		t.Fatalf("loadBrief(shipped) error = %v", err)
	}
	if _, err := brief.Splice(map[string]string{"digest": "material"}); err != nil {
		t.Fatalf("Splice(shipped) error = %v", err)
	}
}

// containsAll reports whether s contains every substring.
func containsAll(s string, subs ...string) bool {
	for _, sub := range subs {
		if !strings.Contains(s, sub) {
			return false
		}
	}
	return true
}

// ---- the cadence: every SynthesizeEvery prompts, not every prompt ----

// promptsN answers n distinct prompts.
func promptsN(n int) []string {
	out := make([]string, n)
	for i := range out {
		out[i] = fmt.Sprintf("prompt %d", i)
	}
	return out
}

// synthesizeWith points the digester at a NONE-boundary digest of these
// prompts and runs one synthesis.
func (h *harness) synthesizeWith(t *testing.T, prompts []string) {
	t.Helper()
	h.digest.resp = digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", prompts...)
	h.synth.synthesizeOnce(context.Background(), testWS)
}

func TestTheFirstTitleIsSynthesizedAtTheFirstPrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)

	// Act.
	h.synthesizeWith(t, promptsN(1))

	// Assert.
	if got := h.model.callCount(); got != 1 {
		t.Fatalf("model calls = %d, want 1", got)
	}
}

func TestFewerThanSynthesizeEveryNewPromptsMakeNoModelCall(t *testing.T) {
	// Arrange: a title at one prompt.
	h := newHarness(t, nil)
	h.synthesizeWith(t, promptsN(1))

	// Act: four more prompts.
	h.synthesizeWith(t, promptsN(1+SynthesizeEvery-1))

	// Assert.
	if got := h.model.callCount(); got != 1 {
		t.Fatalf("model calls = %d, want 1 (not due yet)", got)
	}
}

func TestSynthesizeEveryNewPromptsMakeTheNextModelCall(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.synthesizeWith(t, promptsN(1))

	// Act.
	h.synthesizeWith(t, promptsN(1+SynthesizeEvery))

	// Assert.
	if got := h.model.callCount(); got != 2 {
		t.Fatalf("model calls = %d, want 2", got)
	}
}

func TestTheCadenceCountsFromTheLastSynthesisNotTheLastTrigger(t *testing.T) {
	// Arrange: a title at one prompt, then a trigger at four that made no call.
	h := newHarness(t, nil)
	h.synthesizeWith(t, promptsN(1))
	h.synthesizeWith(t, promptsN(4))

	// Act: six prompts is five past the last synthesis.
	h.synthesizeWith(t, promptsN(6))

	// Assert.
	if got := h.model.callCount(); got != 2 {
		t.Fatalf("model calls = %d, want 2", got)
	}
}

func TestAMovedBoundaryIsDueAtOnce(t *testing.T) {
	// Arrange: a title of an uncut conversation.
	h := newHarness(t, nil)
	h.synthesizeWith(t, promptsN(3))

	// Act: the transcript now opens at a compaction, with one prompt after it.
	h.digest.resp = digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_COMPACT, "the summary", "after")
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.callCount(); got != 2 {
		t.Fatalf("model calls = %d, want 2", got)
	}
}

// ---- the title digest: prompts only, whole, the newest weighed most ----

func TestTitleDigestCarriesAPromptWhole(t *testing.T) {
	// Arrange: a prompt far past the naming digest's per-prompt cap.
	long := strings.Repeat("x", MaxPromptRunes*3)

	// Act.
	got := TitleDigest("", []string{long})

	// Assert.
	if !strings.Contains(got, long) {
		t.Fatal("TitleDigest truncated a prompt, want it whole")
	}
}

func TestTitleDigestCarriesEveryPrompt(t *testing.T) {
	// Arrange: more prompts than the naming digest keeps.
	prompts := promptsN(MaxPrompts + 5)

	// Act.
	got := TitleDigest("", prompts)

	// Assert.
	if !strings.Contains(got, "prompt 0\n") || !strings.Contains(got, fmt.Sprintf("prompt %d\n", MaxPrompts+4)) {
		t.Fatalf("TitleDigest = %q, want the oldest and the newest prompt", got)
	}
}

func TestTitleDigestSetsTheNewestPromptsApart(t *testing.T) {
	// Arrange.
	prompts := promptsN(RecentPrompts + 2)

	// Act.
	got := TitleDigest("", prompts)

	// Assert: the two oldest come before the recent header, the rest after.
	recent := strings.Index(got, "most recent requests")
	if recent < 0 || strings.Index(got, "prompt 1\n") > recent || strings.Index(got, "prompt 2\n") < recent {
		t.Fatalf("TitleDigest = %q, want the newest %d after the recent header", got, RecentPrompts)
	}
}

func TestTitleDigestLeadsWithTheSummaryWhenFewPromptsFollowedIt(t *testing.T) {
	// Arrange, Act.
	got := TitleDigest("the compaction summary", promptsN(RecentPrompts-1))

	// Assert.
	if !strings.HasPrefix(got, "A summary of the conversation") || !strings.Contains(got, "the compaction summary") {
		t.Fatalf("TitleDigest = %q, want the summary first", got)
	}
}

func TestTitleDigestDropsTheSummaryOnceRecentPromptsFollowedIt(t *testing.T) {
	// Arrange, Act.
	got := TitleDigest("the compaction summary", promptsN(RecentPrompts))

	// Assert.
	if strings.Contains(got, "the compaction summary") {
		t.Fatalf("TitleDigest = %q, want no summary past %d prompts", got, RecentPrompts)
	}
}

func TestTitleDigestOpensTemplateDelimitersAUserTyped(t *testing.T) {
	// Arrange, Act.
	got := TitleDigest("", []string{"why does {{prompt}} not splice"})

	// Assert.
	if strings.Contains(got, "{{") || strings.Contains(got, "}}") {
		t.Fatalf("TitleDigest = %q, want no template delimiter left", got)
	}
}

func TestTheModelIsSentTheTitleDigest(t *testing.T) {
	// Arrange: a prompt the naming digest would have cut.
	long := strings.Repeat("y", MaxPromptRunes*2)
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "", long))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if !strings.Contains(h.model.lastRequest().Prompt, long) {
		t.Fatal("the model call carried a truncated prompt, want it whole")
	}
}
