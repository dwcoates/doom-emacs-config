package titlesynth

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"unicode/utf8"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/wsm"
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

func (c *fakeConfigDirs) ConfigDirFor(_ context.Context, _ ids.WorkspaceID) (string, error) {
	if !c.ok {
		return "", errors.New("workspace not found")
	}
	return c.dir, nil
}

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

// fakeStore is a map-backed SummaryStore whose every method can be made to fail.
type fakeStore struct {
	mu        sync.Mutex
	summaries map[ids.WorkspaceID]wsm.WorkspaceSummary
	reads     int
	readErr   error
	putErr    error
	resetErr  error
	dropErr   error
}

func newFakeStore() *fakeStore {
	return &fakeStore{summaries: map[ids.WorkspaceID]wsm.WorkspaceSummary{}}
}

func (f *fakeStore) WorkspaceSummaryOf(_ context.Context, ws ids.WorkspaceID) (wsm.WorkspaceSummary, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.reads++
	if f.readErr != nil {
		return wsm.WorkspaceSummary{}, false, f.readErr
	}
	sum, ok := f.summaries[ws]
	return sum, ok, nil
}

func (f *fakeStore) PutWorkspaceSummary(_ context.Context, ws ids.WorkspaceID, sum wsm.WorkspaceSummary) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.putErr != nil {
		return f.putErr
	}
	f.summaries[ws] = sum
	return nil
}

func (f *fakeStore) ResetWorkspaceSummaryDigest(_ context.Context, ws ids.WorkspaceID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.resetErr != nil {
		return f.resetErr
	}
	if sum, ok := f.summaries[ws]; ok {
		sum.Digest = nil
		f.summaries[ws] = sum
	}
	return nil
}

func (f *fakeStore) DropWorkspaceSummary(_ context.Context, ws ids.WorkspaceID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.dropErr != nil {
		return f.dropErr
	}
	delete(f.summaries, ws)
	return nil
}

func (f *fakeStore) stored(ws ids.WorkspaceID) (wsm.WorkspaceSummary, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	sum, ok := f.summaries[ws]
	return sum, ok
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
func digestResponse(boundary shimv1.TitleDigestBoundary, said ...string) *shimv1.GatherTitleDigestResponse {
	return &shimv1.GatherTitleDigestResponse{
		Result: &shimv1.GatherTitleDigestResponse_Success{
			Success: &shimv1.GatherTitleDigestSuccess{Boundary: boundary, Prompts: said},
		},
	}
}

// harness wires a synthesizer over the five fakes.
type harness struct {
	synth   *Synthesizer
	digest  *fakeDigester
	model   *fakeHeadless
	configs *fakeConfigDirs
	titles  *fakeTitles
	store   *fakeStore
	log     *dlog.TestLogger
}

func newHarness(t *testing.T, resp *shimv1.GatherTitleDigestResponse) *harness {
	t.Helper()
	h := &harness{
		digest:  &fakeDigester{resp: resp},
		model:   &fakeHeadless{text: "Wire up the reconnect backoff"},
		configs: &fakeConfigDirs{dir: "/root/.claude", ok: true},
		titles:  &fakeTitles{},
		store:   newFakeStore(),
		log:     dlog.NewTestLogger(),
	}
	h.synth = New(Deps{
		Digester:   h.digest,
		Headless:   h.model,
		ConfigDirs: h.configs,
		Titles:     h.titles,
		Store:      h.store,
		PromptsDir: briefDir(t),
		Log:        h.log,
	})
	return h
}

// ---- tests ----------------------------------------------------------------

func TestSynthesizesWhenThereIsMaterial(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "make reconnect backoff"))

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
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "one prompt"))

	// Act — synthesize twice with the same digest.
	h.synth.synthesizeOnce(context.Background(), testWS)
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert — the second is a no-op: at most one cheap call per new prompt.
	if got := h.model.callCount(); got != 1 {
		t.Fatalf("model calls = %d, want 1", got)
	}
}

func TestTheModelCallBillsTheWorkspaceAccount(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
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
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.lastRequest().Model; got != headless.ModelHaiku {
		t.Fatalf("headless Model = %q, want %q", got, headless.ModelHaiku)
	}
}

func TestTheModelCallAsksUnderItsOwnGuardSite(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.lastRequest().Site; got != Site {
		t.Fatalf("headless Site = %q, want %q", got, Site)
	}
}

func TestAGuardRefusalKeepsTheWorkspaceName(t *testing.T) {
	// Arrange — the vendor guard refuses the call, as it does under test.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
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
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE))

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
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR))

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
	// Arrange — one synthesis, then a /clear reset.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
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
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))

	// Act — the public trigger dispatches a goroutine; Wait joins it.
	h.synth.OnTurnEnded(testWS)
	h.synth.Wait()

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "Wire up the reconnect backoff" {
		t.Fatalf("synthesized title = %q (set=%v), want the async run's result", got, ok)
	}
}

func TestOnPromptSynthesizesAsynchronously(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))

	// Act.
	h.synth.OnPrompt(testWS)
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
	h.digest.resp = digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, prompts...)
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

	// Act: the transcript now opens at a clear, with one prompt after it.
	h.digest.resp = digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR, "after")
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.callCount(); got != 2 {
		t.Fatalf("model calls = %d, want 2", got)
	}
}

// ---- the title digest: every prompt, whole, none weighed above another ----

func TestTitleDigestCarriesAPromptWhole(t *testing.T) {
	// Arrange: a prompt far past the naming digest's per-prompt cap.
	long := strings.Repeat("x", MaxPromptRunes*3)

	// Act.
	got := TitleDigest([]string{long})

	// Assert.
	if !strings.Contains(got, long) {
		t.Fatal("TitleDigest truncated a prompt, want it whole")
	}
}

func TestTitleDigestCarriesEveryPrompt(t *testing.T) {
	// Arrange: more prompts than the naming digest keeps.
	said := promptsN(MaxPrompts + 5)

	// Act.
	got := TitleDigest(said)

	// Assert.
	if !strings.Contains(got, "prompt 0\n") || !strings.Contains(got, fmt.Sprintf("prompt %d\n", MaxPrompts+4)) {
		t.Fatalf("TitleDigest = %q, want the oldest and the newest prompt", got)
	}
}

func TestTitleDigestListsThePromptsOldestFirstUnderOneHeader(t *testing.T) {
	// Arrange, Act.
	got := TitleDigest([]string{"first", "second"})

	// Assert: one list, no prompt set apart from another.
	want := "Every request the user sent in this session, oldest first:\n- first\n- second\n"
	if got != want {
		t.Fatalf("TitleDigest = %q, want %q", got, want)
	}
}

func TestTitleDigestOpensTemplateDelimitersAUserTyped(t *testing.T) {
	// Arrange, Act.
	got := TitleDigest([]string{"why does {{prompt}} not splice"})

	// Assert.
	if strings.Contains(got, "{{") || strings.Contains(got, "}}") {
		t.Fatalf("TitleDigest = %q, want no template delimiter left", got)
	}
}

// ---- the user's words: injected meta spans are not the request ----

func TestUsersWords(t *testing.T) {
	tests := []struct {
		name string
		in   []string
		want []string
	}{
		{name: "a plain prompt is kept whole", in: []string{"fix the backoff"}, want: []string{"fix the backoff"}},
		{name: "an injected span is stripped", in: []string{prompts.Wrap("Do not wait. ") + "fix the backoff" + prompts.Wrap(" then merge")}, want: []string{"fix the backoff"}},
		{name: "a prompt of nothing but injected spans is dropped", in: []string{prompts.Wrap("merge brief"), "fix the backoff"}, want: []string{"fix the backoff"}},
		{name: "no prompts are no words", in: nil, want: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := UsersWords(tt.in)

			// Assert.
			if err != nil {
				t.Fatalf("UsersWords error = %v", err)
			}
			if len(got) != len(tt.want) || strings.Join(got, "|") != strings.Join(tt.want, "|") {
				t.Fatalf("UsersWords = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestUsersWordsRefusesAnUnbalancedSpan(t *testing.T) {
	// Arrange, Act.
	_, err := UsersWords([]string{"fine", prompts.MetaOpen + "never closed"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "prompt 1") {
		t.Fatalf("UsersWords error = %v, want the unbalanced prompt named", err)
	}
}

func TestAnUnbalancedSpanMakesNoModelCallAndLogsTheCause(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, prompts.MetaOpen+"never closed"))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.callCount(); got != 0 {
		t.Fatalf("model calls = %d, want 0", got)
	}
	if _, set := h.titles.last(); set {
		t.Fatal("a title was installed from a prompt that could not be read")
	}
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opSynth})
	if !ok {
		t.Fatalf("no error record for the unbalanced span (records = %v)", h.log.Records())
	}
	if rec.Context["workspace"] != string(testWS) || rec.Context["prompts"] != 1 || !strings.Contains(fmt.Sprint(rec.Context["cause"]), "never closes") {
		t.Fatalf("error record context = %v, want the workspace, the prompt count and the cause", rec.Context)
	}
}

func TestAClearWithOnlyInjectedSpansRetractsTheStaleTitle(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR, prompts.Wrap("merge brief")))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "" {
		t.Fatalf("synthesized title = %q (set=%v), want a retraction", got, ok)
	}
	if calls := h.model.callCount(); calls != 0 {
		t.Fatalf("model calls = %d, want 0", calls)
	}
}

func TestTheModelIsSentTheUsersWordsAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "fix the backoff"+prompts.Wrap("then invoke the merge queue")))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	sent := h.model.lastRequest().Prompt
	if !strings.Contains(sent, "fix the backoff") || strings.Contains(sent, "merge queue") {
		t.Fatalf("model prompt = %q, want the user's words without the injected span", sent)
	}
}

func TestTheModelIsSentTheTitleDigest(t *testing.T) {
	// Arrange: a prompt the naming digest would have cut.
	long := strings.Repeat("y", MaxPromptRunes*2)
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, long))

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if !strings.Contains(h.model.lastRequest().Prompt, long) {
		t.Fatal("the model call carried a truncated prompt, want it whole")
	}
}

// ---- persistence ----------------------------------------------------------

// errorRecord answers the synthesizer's error record, failing the test when
// there is none.
func (h *harness) errorRecord(t *testing.T) dlog.Record {
	t.Helper()
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opSynth})
	if !ok {
		t.Fatalf("no %s error record: %+v", opSynth, h.log.Records())
	}
	return rec
}

// storedDigestOf is the digest the synthesizer would persist for RESP.
func storedDigestOf(resp *shimv1.GatherTitleDigestResponse) *wsm.SummaryDigest {
	success := resp.GetSuccess()
	return &wsm.SummaryDigest{Hash: digestHash(success), Prompts: len(success.GetPrompts()), Boundary: success.GetBoundary()}
}

func TestRestoreInstallsThePersistedSummary(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary"}

	// Act.
	err := h.synth.Restore(context.Background(), testWS)

	// Assert.
	if err != nil {
		t.Fatalf("Restore = %v, want nil", err)
	}
	if got, ok := h.titles.last(); !ok || got != "Persist the topbar summary" {
		t.Fatalf("installed title = %q (set=%v), want the persisted summary", got, ok)
	}
}

func TestRestoreWithNothingPersistedInstallsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)

	// Act.
	err := h.synth.Restore(context.Background(), testWS)

	// Assert.
	if err != nil {
		t.Fatalf("Restore = %v, want nil", err)
	}
	if got, ok := h.titles.last(); ok {
		t.Fatalf("installed title %q, want none", got)
	}
}

func TestRestoreLoadsAWorkspaceOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary"}
	if err := h.synth.Restore(context.Background(), testWS); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Act.
	if err := h.synth.Restore(context.Background(), testWS); err != nil {
		t.Fatalf("second Restore: %v", err)
	}

	// Assert.
	if h.store.reads != 1 || len(h.titles.titles) != 1 {
		t.Fatalf("reads = %d, installs = %d; want one of each", h.store.reads, len(h.titles.titles))
	}
}

func TestAFailedRestoreIsSurfacedLoggedAndRetried(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary"}
	h.store.readErr = errors.New("database is locked")

	// Act.
	err := h.synth.Restore(context.Background(), testWS)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Restore = %v, want the read's failure", err)
	}
	if got, ok := h.titles.last(); ok {
		t.Fatalf("installed title %q after a failed read, want none", got)
	}
	rec := h.errorRecord(t)
	if rec.Context["workspace"] != string(testWS) || rec.Context["cause"] != "database is locked" {
		t.Fatalf("error record context = %+v, want the workspace and the cause", rec.Context)
	}
	h.store.readErr = nil
	if err := h.synth.Restore(context.Background(), testWS); err != nil {
		t.Fatalf("retried Restore: %v", err)
	}
	if got, ok := h.titles.last(); !ok || got != "Persist the topbar summary" {
		t.Fatalf("installed title after the retry = %q (set=%v), want the persisted summary", got, ok)
	}
}

func TestARestoredDigestKeepsTheCadence(t *testing.T) {
	// Arrange: the persisted summary was distilled from exactly this digest.
	resp := digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt")
	h := newHarness(t, resp)
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary", Digest: storedDigestOf(resp)}
	if err := h.synth.Restore(context.Background(), testWS); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert: the restart spends no model call on a summary already standing.
	if got := h.model.callCount(); got != 0 {
		t.Fatalf("model calls = %d, want 0", got)
	}
}

func TestARestoredResetDigestIsDueAtOnce(t *testing.T) {
	// Arrange: a /clear reset the digest before the restart.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR, "a prompt"))
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary"}
	if err := h.synth.Restore(context.Background(), testWS); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got := h.model.callCount(); got != 1 {
		t.Fatalf("model calls = %d, want 1", got)
	}
}

func TestATriggerBeforeRestoreLoadsThePersistedSummary(t *testing.T) {
	// Arrange.
	resp := digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt")
	h := newHarness(t, resp)
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary", Digest: storedDigestOf(resp)}

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "Persist the topbar summary" {
		t.Fatalf("installed title = %q (set=%v), want the persisted summary", got, ok)
	}
	if got := h.model.callCount(); got != 0 {
		t.Fatalf("model calls = %d, want 0", got)
	}
}

func TestATriggerWhoseLoadFailsMakesNoCall(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
	h.store.readErr = errors.New("database is locked")

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if h.digest.calls != 0 || h.model.callCount() != 0 {
		t.Fatalf("digest calls = %d, model calls = %d; want none", h.digest.calls, h.model.callCount())
	}
	if rec := h.errorRecord(t); rec.Context["cause"] != "database is locked" {
		t.Fatalf("error record context = %+v, want the cause", rec.Context)
	}
}

func TestRestoreAfterANewerSynthesisLeavesTheNewerTitle(t *testing.T) {
	// Arrange: a trigger loads the store and distills a new summary.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Act.
	if err := h.synth.Restore(context.Background(), testWS); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "Wire up the reconnect backoff" {
		t.Fatalf("installed title = %q (set=%v), want the newer synthesis", got, ok)
	}
}

func TestASynthesisPersistsTheTitleAndItsDigest(t *testing.T) {
	// Arrange.
	resp := digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt", "another")
	h := newHarness(t, resp)

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	want := wsm.WorkspaceSummary{Title: "Wire up the reconnect backoff", Digest: storedDigestOf(resp)}
	if got, ok := h.store.stored(testWS); !ok || !reflect.DeepEqual(got, want) {
		t.Fatalf("stored summary = %+v (found=%v), want %+v", got, ok, want)
	}
}

func TestAFailedPersistStillInstallsTheTitleAndIsLogged(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
	h.store.putErr = errors.New("disk full")

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "Wire up the reconnect backoff" {
		t.Fatalf("installed title = %q (set=%v), want the synthesis", got, ok)
	}
	if rec := h.errorRecord(t); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] != "disk full" {
		t.Fatalf("error record context = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestOnContextResetPersistsTheResetAndKeepsTheTitle(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Act.
	h.synth.OnContextReset(testWS)

	// Assert.
	want := wsm.WorkspaceSummary{Title: "Wire up the reconnect backoff"}
	if got, ok := h.store.stored(testWS); !ok || !reflect.DeepEqual(got, want) {
		t.Fatalf("stored summary = %+v (found=%v), want %+v", got, ok, want)
	}
}

func TestAFailedResetPersistIsLogged(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE, "a prompt"))
	h.synth.synthesizeOnce(context.Background(), testWS)
	h.store.resetErr = errors.New("disk full")

	// Act.
	h.synth.OnContextReset(testWS)

	// Assert.
	if rec := h.errorRecord(t); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] != "disk full" {
		t.Fatalf("error record context = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestARetractionDropsThePersistedSummary(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR))
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary"}

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if _, ok := h.store.stored(testWS); ok {
		t.Fatalf("a retracted summary is still persisted")
	}
	if got, ok := h.titles.last(); !ok || got != "" {
		t.Fatalf("installed title = %q (set=%v), want a retraction", got, ok)
	}
}

func TestAFailedRetractionDropIsLoggedAndStillRetracts(t *testing.T) {
	// Arrange.
	h := newHarness(t, digestResponse(shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR))
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Title: "Persist the topbar summary"}
	h.store.dropErr = errors.New("disk full")

	// Act.
	h.synth.synthesizeOnce(context.Background(), testWS)

	// Assert.
	if got, ok := h.titles.last(); !ok || got != "" {
		t.Fatalf("installed title = %q (set=%v), want a retraction", got, ok)
	}
	if rec := h.errorRecord(t); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] != "disk full" {
		t.Fatalf("error record context = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestNewRefusesAMissingStore(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	deps := h.synth.deps
	deps.Store = nil
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatalf("New accepted deps with no store, want a panic")
		}
	}()

	// Act.
	New(deps)
}
