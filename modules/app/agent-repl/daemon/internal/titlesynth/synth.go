package titlesynth

import (
	"context"
	"fmt"
	"hash/fnv"
	"strconv"
	"strings"
	"sync"
	"unicode/utf8"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/runecap"
)

// opSynth is the operation the synthesizer's records carry.
const opSynth = "daemon.titlesynth"

// Synthesizer produces synthesized workspace titles, one workspace at a time.
//
// It is a sessionwatcher sink implemented structurally: OnSessionStarted,
// OnPrompt and OnTurnEnded are the synthesis triggers, and OnContextReset
// resets the digest hash. The heavy work — a shim call and a
// model call — runs on a goroutine so a trigger never blocks the watcher's
// stream, and a per-workspace inflight/dirty guard collapses a burst of
// triggers into at most one extra run.
type Synthesizer struct {
	deps Deps

	mu    sync.Mutex
	state map[ids.WorkspaceID]*wsState

	// wg joins every dispatched goroutine so shutdown can wait for them.
	wg sync.WaitGroup
}

// wsState is one workspace's synthesis bookkeeping.
type wsState struct {
	// lastHash is the digest hash of the last synthesis. A trigger whose
	// digest hashes to this makes no model call.
	lastHash string
	// haveHash reports whether lastHash has been set (an empty digest hashes to
	// a real value, so a zero string cannot double as "never synthesized").
	haveHash bool
	// lastPrompts and lastBoundary are the prompt count and the boundary the
	// last synthesis summarized: the cadence counts new prompts from them.
	lastPrompts  int
	lastBoundary shimv1.TitleDigestBoundary
	// inFlight reports that a run goroutine is executing for this workspace.
	inFlight bool
	// dirty reports that a trigger arrived while a run was in flight, so one
	// more run is owed when the current one finishes.
	dirty bool
}

// New builds a synthesizer. It panics on a missing collaborator, because a
// half-wired synthesizer is a boot defect, not a runtime condition.
func New(deps Deps) *Synthesizer {
	if deps.Digester == nil || deps.Headless == nil || deps.ConfigDirs == nil || deps.Titles == nil || deps.Log == nil {
		panic("titlesynth: New requires Digester, Headless, ConfigDirs, Titles and Log")
	}
	if deps.GatherTimeout == 0 {
		deps.GatherTimeout = DefaultGatherTimeout
	}
	if deps.SynthesizeTimeout == 0 {
		deps.SynthesizeTimeout = DefaultSynthesizeTimeout
	}
	return &Synthesizer{deps: deps, state: map[ids.WorkspaceID]*wsState{}}
}

// ---- sessionwatcher.TitleSink (implemented structurally) ------------------

// OnSessionStarted triggers synthesis when a session names itself: a resumed or
// adopted conversation may already carry prompts with no vendor title.
func (s *Synthesizer) OnSessionStarted(ws ids.WorkspaceID) { s.dispatch(ws) }

// OnPrompt triggers synthesis at a main-agent prompt row: the prompt has been
// made, so the first summary need not wait for the first turn to end.
func (s *Synthesizer) OnPrompt(ws ids.WorkspaceID) { s.dispatch(ws) }

// OnTurnEnded triggers synthesis after a turn: by then the vendor has recorded
// the turn's prompt in the transcript the digest reads.
func (s *Synthesizer) OnTurnEnded(ws ids.WorkspaceID) { s.dispatch(ws) }

// OnContextReset resets the digest hash so the next trigger re-synthesizes. A
// /clear moves the boundary, so the last synthesis no longer describes the
// conversation.
func (s *Synthesizer) OnContextReset(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	st := s.stateFor(ws)
	st.lastHash = ""
	st.haveHash = false
	st.lastPrompts = 0
}

// Wait blocks until every dispatched run has finished. It exists for a clean
// shutdown and for tests that assert an asynchronous run's effect.
func (s *Synthesizer) Wait() { s.wg.Wait() }

// stateFor returns the workspace's state, creating it on first use. The caller
// holds s.mu.
func (s *Synthesizer) stateFor(ws ids.WorkspaceID) *wsState {
	st := s.state[ws]
	if st == nil {
		st = &wsState{}
		s.state[ws] = st
	}
	return st
}

// dispatch runs a synthesis on a goroutine unless one is already in flight, in
// which case it marks the workspace dirty so one more run follows.
func (s *Synthesizer) dispatch(ws ids.WorkspaceID) {
	s.mu.Lock()
	st := s.stateFor(ws)
	if st.inFlight {
		st.dirty = true
		s.mu.Unlock()
		return
	}
	st.inFlight = true
	s.wg.Add(1)
	s.mu.Unlock()

	go s.run(ws)
}

// run drains the workspace's synthesis, re-running once for each dirty flag a
// trigger set while it worked, then clears inFlight.
func (s *Synthesizer) run(ws ids.WorkspaceID) {
	defer s.wg.Done()
	for {
		s.synthesizeOnce(context.Background(), ws)

		s.mu.Lock()
		st := s.stateFor(ws)
		if st.dirty {
			st.dirty = false
			s.mu.Unlock()
			continue
		}
		st.inFlight = false
		st.dirty = false
		s.mu.Unlock()
		return
	}
}

// synthesizeOnce is the whole synthesis flow for one workspace, synchronous so
// it is exercised directly: gather the digest, guard on the digest hash and the
// cadence, make the cheap model call under the workspace's account, and install
// the result. Every failure is best-effort and leaves the previous title
// standing.
func (s *Synthesizer) synthesizeOnce(ctx context.Context, ws ids.WorkspaceID) {
	digest, ok := s.gather(ctx, ws)
	if !ok {
		return
	}

	prompts := digest.GetPrompts()
	said, err := UsersWords(prompts)
	if err != nil {
		s.deps.Log.Error(opSynth, "a prompt's injected meta spans could not be stripped; not distilling a summary", dlog.Context{
			"workspace": string(ws), "prompts": len(prompts), "cause": err.Error(),
		})
		return
	}

	// A CLEARED CONVERSATION WITH NOTHING SAID retracts a stale title: the old
	// summary described a conversation that is gone.
	if len(said) == 0 {
		if digest.GetBoundary() == shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_CLEAR {
			s.retract(ws)
		}
		return
	}

	hash := digestHash(digest)
	s.mu.Lock()
	st := s.stateFor(ws)
	if st.haveHash && st.lastHash == hash {
		s.mu.Unlock()
		s.deps.Log.Debug(opSynth, "the title digest is unchanged; no model call", dlog.Context{
			"workspace": string(ws),
		})
		return
	}
	if due, why := cadenceDue(st, digest); !due {
		s.mu.Unlock()
		s.deps.Log.Debug(opSynth, "the title is not due for re-synthesis yet; no model call", dlog.Context{
			"workspace": string(ws), "prompts": len(prompts), "synthesized_at_prompts": st.lastPrompts, "why": why,
		})
		return
	}
	s.mu.Unlock()

	title, ok := s.callModel(ctx, ws, said)
	if !ok {
		return
	}

	s.deps.Titles.SetSynthesizedTitle(ws, title)
	s.mu.Lock()
	st = s.stateFor(ws)
	st.lastHash = hash
	st.haveHash = true
	st.lastPrompts = len(prompts)
	st.lastBoundary = digest.GetBoundary()
	s.mu.Unlock()
	s.deps.Log.Info(opSynth, "installed a synthesized workspace title", dlog.Context{
		"workspace": string(ws), "boundary": digest.GetBoundary().String(), "prompts": len(prompts),
	})
}

// cadenceDue reports whether a changed digest is due a model call, and why.
// The caller holds s.mu.
//
// THE FIRST PROMPT, THEN EVERY SynthesizeEvery PROMPTS (owner rulings,
// 2026-10-02 and 2026-10-14). A context's FIRST title is due at once, so a
// workspace is never left on its bare name waiting for its fifth prompt; after
// it, a new title is due only once SynthesizeEvery more prompts have been made. A digest that is not the
// continuation of the one last summarized -- another boundary, or fewer prompts
// than before -- is a new context, and is due at once as well.
func cadenceDue(st *wsState, digest *shimv1.GatherTitleDigestSuccess) (bool, string) {
	prompts := len(digest.GetPrompts())
	switch {
	case !st.haveHash:
		return true, "first title of this context"
	case digest.GetBoundary() != st.lastBoundary:
		return true, "the boundary moved"
	case prompts < st.lastPrompts:
		return true, "the prompt count went back"
	case prompts-st.lastPrompts >= SynthesizeEvery:
		return true, "enough new prompts"
	default:
		return false, "too few new prompts"
	}
}

// gather asks the shim for the digest, returning false on any failure (all of
// which leave the previous title standing and none of which is a fault).
func (s *Synthesizer) gather(ctx context.Context, ws ids.WorkspaceID) (*shimv1.GatherTitleDigestSuccess, bool) {
	gctx, cancel := context.WithTimeout(ctx, s.deps.GatherTimeout)
	defer cancel()
	resp, err := s.deps.Digester.GatherTitleDigest(gctx, ws)
	if err != nil {
		s.deps.Log.Info(opSynth, "the title digest could not be gathered; keeping the workspace name", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return nil, false
	}
	success, ok := resp.GetResult().(*shimv1.GatherTitleDigestResponse_Success)
	if !ok {
		s.deps.Log.Info(opSynth, "the shim refused the title digest; keeping the workspace name", dlog.Context{
			"workspace": string(ws), "detail": resp.GetFailure().GetDetail(),
		})
		return nil, false
	}
	return success.Success, true
}

// callModel composes the brief and makes the one cheap headless call under the
// workspace's account, returning the cleaned title and false on any failure.
func (s *Synthesizer) callModel(ctx context.Context, ws ids.WorkspaceID, said []string) (string, bool) {
	brief, err := loadBrief(s.deps.PromptsDir)
	if err != nil {
		s.deps.Log.Error(opSynth, "the synthesized-title brief could not be read", dlog.Context{
			"workspace": string(ws), "brief": BriefTitle, "cause": err.Error(),
		})
		return "", false
	}
	question, err := brief.Splice(map[string]string{"digest": TitleDigest(said)})
	if err != nil {
		s.deps.Log.Error(opSynth, "the synthesized-title brief could not be spliced", dlog.Context{
			"workspace": string(ws), "brief": BriefTitle, "cause": err.Error(),
		})
		return "", false
	}

	configDir, err := s.deps.ConfigDirs.ConfigDirFor(ctx, ws)
	if err != nil {
		s.deps.Log.Info(opSynth, "no account root for this workspace; not synthesizing a title", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return "", false
	}

	sctx, cancel := context.WithTimeout(ctx, s.deps.SynthesizeTimeout)
	defer cancel()
	resp, err := s.deps.Headless.Run(sctx, headless.Request{
		Site:      Site,
		Model:     headless.ModelHaiku,
		Format:    headless.FormatText,
		ConfigDir: configDir,
		Prompt:    question,
		Timeout:   s.deps.SynthesizeTimeout,
	})
	if err != nil {
		// A GUARD REFUSAL OR A MODEL FAILURE IS NOT A FAULT. Synthesis is an
		// enhancement over the name, so it is recorded at INFO — nobody has to
		// act on the name still standing.
		s.deps.Log.Info(opSynth, "the synthesized-title model call did not answer; keeping the workspace name", dlog.Context{
			"workspace": string(ws), "model": headless.ModelHaiku, "cause": headless.CauseOf(err),
		})
		return "", false
	}

	title := cleanTitle(resp.Text)
	if title == "" {
		s.deps.Log.Info(opSynth, "the synthesized-title model call answered empty; keeping the workspace name", dlog.Context{
			"workspace": string(ws),
		})
		return "", false
	}
	return title, true
}

// retract drops a stale synthesized title and forgets its hash.
func (s *Synthesizer) retract(ws ids.WorkspaceID) {
	s.deps.Titles.SetSynthesizedTitle(ws, "")
	s.mu.Lock()
	st := s.stateFor(ws)
	st.lastHash = ""
	st.haveHash = false
	st.lastPrompts = 0
	s.mu.Unlock()
	s.deps.Log.Debug(opSynth, "retracted the synthesized title after a clear", dlog.Context{
		"workspace": string(ws),
	})
}

// loadBrief reads the brief at use time, so an edit takes effect without a
// daemon bounce (mirrors the naming call's read-at-use-time discipline).
func loadBrief(dir string) (prompts.Prompt, error) {
	return prompts.Load(dir, BriefTitle)
}

// UsersWords reduces the digest's prompts to what the user said: each prompt
// with agent-repl's injected meta spans stripped (prompts.StripSentinels), and
// a prompt that was nothing but injected spans dropped. The spans are the
// editor's decoration of a send (a one-shot's postprocessing directive, a
// merge brief), not the user's request, so a summary distilled from them would
// state the decoration as the task.
//
// An unbalanced span is the producer bug StripSentinels surfaces, and is
// returned rather than guessed at.
func UsersWords(all []string) ([]string, error) {
	said := make([]string, 0, len(all))
	for i, p := range all {
		stripped, err := prompts.StripSentinels(p)
		if err != nil {
			return nil, fmt.Errorf("prompt %d: %w", i, err)
		}
		if stripped = strings.TrimSpace(stripped); stripped != "" {
			said = append(said, stripped)
		}
	}
	return said, nil
}

// TitleDigest renders what the TOPBAR SUMMARY is distilled from (owner
// ruling, 2026-10-14): every prompt the user sent since the last /clear,
// including those before a /compact, whole and untruncated, oldest first, and
// no response. No prompt is weighed above another: the summary encapsulates
// the whole session's task.
//
// It is the title's own composition, deliberately not ComposeDigest's: a fork's
// naming call still reads the capped composition, which bounds a call that
// names a workspace in three words.
func TitleDigest(said []string) string {
	var b strings.Builder
	b.WriteString("Every request the user sent in this session, oldest first:\n")
	writePrompts(&b, said)
	return quoteTemplateBraces(b.String())
}

// writePrompts renders prompts as a list, each whole.
func writePrompts(b *strings.Builder, prompts []string) {
	for _, p := range prompts {
		b.WriteString("- ")
		b.WriteString(strings.TrimSpace(p))
		b.WriteString("\n")
	}
}

// ComposeDigest renders the digest material the brief summarizes: the
// compaction summary when there is one, then the most recent prompts, each
// bounded so one conversation cannot grow the model prompt without limit.
//
// It is the composition the workspace naming call a fork makes
// (internal/workspace) reads. The topbar summary reads TitleDigest, which the
// owner ruled whole and untruncated (2026-10-02).
//
// THE DIGEST IS QUOTED EVIDENCE, NEVER A TEMPLATE. prompts.Prompt.Splice
// refuses any `{{...}}` token surviving in its output — its guard against a
// malformed brief — and a conversation that merely TALKS about templates (this
// repository's own briefs spell `{{prompt}}`) was refused wholesale. So a
// "{{" or "}}" the user typed is opened to "{ {" / "} }" here, where the text
// becomes a splice value, and the brief guard stays whole.
func ComposeDigest(summary string, all []string) string {
	return quoteTemplateBraces(composeDigest(summary, all))
}

// templateBraces opens every template-token delimiter a user typed.
var templateBraces = strings.NewReplacer("{{", "{ {", "}}", "} }")

// quoteTemplateBraces opens the template delimiters in s until no substring of
// it reads as a `{{...}}` token. A run of three braces needs a second pass,
// because one replacement leaves a delimiter where the run's tail meets it.
func quoteTemplateBraces(s string) string {
	for {
		opened := templateBraces.Replace(s)
		if opened == s {
			return s
		}
		s = opened
	}
}

// composeDigest is ComposeDigest's text before its braces are opened.
//
// It applies the per-item caps first (the summary at MaxPromptRunes*4, each
// prompt at MaxPromptRunes, and the tail of at most MaxPrompts of them), then
// renders and checks the WHOLE result against MaxDigestTotalRunes. Trimming
// to that total cap drops the oldest material first — the summary, then the
// oldest surviving prompt, one at a time — so the newest prompts are what
// survive, which is what a title should reflect.
func composeDigest(summary string, all []string) string {
	trimmedSummary := ""
	if strings.TrimSpace(summary) != "" {
		trimmedSummary = runecap.Ellipsis(strings.TrimSpace(summary), MaxPromptRunes*4)
	}
	recent := all
	if len(recent) > MaxPrompts {
		recent = recent[len(recent)-MaxPrompts:]
	}
	trimmedPrompts := make([]string, len(recent))
	for i, p := range recent {
		trimmedPrompts[i] = runecap.Ellipsis(strings.TrimSpace(p), MaxPromptRunes)
	}

	for utf8.RuneCountInString(renderDigest(trimmedSummary, trimmedPrompts)) > MaxDigestTotalRunes {
		if trimmedSummary != "" {
			trimmedSummary = ""
			continue
		}
		if len(trimmedPrompts) == 0 {
			// Nothing left to drop; the bare "The user's requests:" header is as
			// small as this can get.
			break
		}
		trimmedPrompts = trimmedPrompts[1:]
	}
	return renderDigest(trimmedSummary, trimmedPrompts)
}

// renderDigest is composeDigest's rendering step, factored out so the total
// cap can measure it repeatedly without duplicating the layout.
func renderDigest(summary string, prompts []string) string {
	var b strings.Builder
	if summary != "" {
		b.WriteString("A summary of the earlier conversation:\n")
		b.WriteString(summary)
		b.WriteString("\n\n")
	}
	b.WriteString("The user's requests:\n")
	for _, p := range prompts {
		b.WriteString("- ")
		b.WriteString(p)
		b.WriteString("\n")
	}
	return b.String()
}

// cleanTitle reduces the model's answer to one plain line: the first non-empty
// line, trimmed, with any wrapping quotes or backticks stripped. The brief
// forbids the model the commas, semicolons and em-dashes a single sentence does
// not need, so this does no punctuation surgery — stripping them would corrupt a
// title the model wrote correctly.
func cleanTitle(text string) string {
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		return strings.Trim(line, "\"'`")
	}
	return ""
}

// digestHash is the digest's fingerprint: the boundary and the prompts, so any change to what a title would summarize changes the hash and
// any repeat leaves it unchanged. It writes to the hash directly rather than
// through fmt, which production code does not use (the durable-logging bypass
// guard). fnv's Write never errors, so its returns are discarded.
func digestHash(d *shimv1.GatherTitleDigestSuccess) string {
	h := fnv.New64a()
	_, _ = h.Write([]byte("b:" + strconv.Itoa(int(d.GetBoundary())) + "\n"))
	for _, p := range d.GetPrompts() {
		// The NUL separator cannot appear in a prompt, so no two prompt sets
		// hash alike by concatenation.
		_, _ = h.Write([]byte("p:" + p))
		_, _ = h.Write([]byte{0})
	}
	return strconv.FormatUint(h.Sum64(), 16)
}
