package titlesynth

import (
	"context"
	"hash/fnv"
	"strconv"
	"strings"
	"sync"
	"unicode/utf8"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
)

// opSynth is the operation the synthesizer's records carry.
const opSynth = "daemon.titlesynth"

// Synthesizer produces synthesized workspace titles, one workspace at a time.
//
// It is a sessionwatcher sink implemented structurally: OnSessionStarted and
// OnTurnEnded are the synthesis triggers, OnVendorTitle stops synthesis, and
// OnContextReset resets the digest hash. The heavy work — a shim call and a
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
	// vendorTitle reports that the vendor has stated an ai-title, after which
	// this workspace is never synthesized again.
	vendorTitle bool
	// lastHash is the digest hash of the last synthesis. A trigger whose
	// digest hashes to this makes no model call.
	lastHash string
	// haveHash reports whether lastHash has been set (an empty digest hashes to
	// a real value, so a zero string cannot double as "never synthesized").
	haveHash bool
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

// OnTurnEnded triggers synthesis after a turn: a new prompt changes the digest.
func (s *Synthesizer) OnTurnEnded(ws ids.WorkspaceID) { s.dispatch(ws) }

// OnVendorTitle stops synthesizing this workspace: the vendor now states its
// own ai-title, which always wins.
func (s *Synthesizer) OnVendorTitle(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stateFor(ws).vendorTitle = true
}

// OnContextReset resets the digest hash so the next trigger re-synthesizes. A
// /clear or a completed /compact moves the boundary, so the last synthesis no
// longer describes the conversation.
func (s *Synthesizer) OnContextReset(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	st := s.stateFor(ws)
	st.lastHash = ""
	st.haveHash = false
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

// dispatch runs a synthesis on a goroutine unless one is already in flight (in
// which case it marks the workspace dirty so one more run follows) or the
// vendor has stated a title (in which case there is nothing to synthesize).
func (s *Synthesizer) dispatch(ws ids.WorkspaceID) {
	s.mu.Lock()
	st := s.stateFor(ws)
	if st.vendorTitle {
		s.mu.Unlock()
		return
	}
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
		if st.dirty && !st.vendorTitle {
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
// it is exercised directly: gather the digest, guard on the vendor title and
// the digest hash, make the cheap model call under the workspace's account, and
// install the result. Every failure is best-effort and leaves the workspace
// name standing.
func (s *Synthesizer) synthesizeOnce(ctx context.Context, ws ids.WorkspaceID) {
	s.mu.Lock()
	if s.stateFor(ws).vendorTitle {
		s.mu.Unlock()
		return
	}
	s.mu.Unlock()

	digest, ok := s.gather(ctx, ws)
	if !ok {
		return
	}

	prompts := digest.GetPrompts()
	summary := digest.GetLastCompactSummary()

	// A CLEARED CONVERSATION WITH NOTHING SAID retracts a stale title: the old
	// summary described a conversation that is gone.
	if len(prompts) == 0 && summary == "" {
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
	s.mu.Unlock()

	title, ok := s.callModel(ctx, ws, summary, prompts)
	if !ok {
		return
	}

	s.deps.Titles.SetSynthesizedTitle(ws, title)
	s.mu.Lock()
	st = s.stateFor(ws)
	st.lastHash = hash
	st.haveHash = true
	s.mu.Unlock()
	s.deps.Log.Info(opSynth, "installed a synthesized workspace title", dlog.Context{
		"workspace": string(ws), "boundary": digest.GetBoundary().String(), "prompts": len(prompts),
	})
}

// gather asks the shim for the digest, returning false on any failure (all of
// which leave the workspace name standing and none of which is a fault).
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
func (s *Synthesizer) callModel(ctx context.Context, ws ids.WorkspaceID, summary string, prompts []string) (string, bool) {
	brief, err := loadBrief(s.deps.PromptsDir)
	if err != nil {
		s.deps.Log.Error(opSynth, "the synthesized-title brief could not be read", dlog.Context{
			"workspace": string(ws), "brief": BriefTitle, "cause": err.Error(),
		})
		return "", false
	}
	question, err := brief.Splice(map[string]string{"digest": ComposeDigest(summary, prompts)})
	if err != nil {
		s.deps.Log.Error(opSynth, "the synthesized-title brief could not be spliced", dlog.Context{
			"workspace": string(ws), "brief": BriefTitle, "cause": err.Error(),
		})
		return "", false
	}

	configDir, ok := s.deps.ConfigDirs.ConfigDirFor(ws)
	if !ok {
		s.deps.Log.Info(opSynth, "no account root for this workspace; not synthesizing a title", dlog.Context{
			"workspace": string(ws),
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

// ComposeDigest renders the digest material the brief summarizes: the
// compaction summary when there is one, then the most recent prompts, each
// bounded so one conversation cannot grow the model prompt without limit.
//
// It is the ONE composition of "what this conversation has been about", read
// by the title synthesizer and by the workspace naming call a fork makes
// (internal/workspace), so the two cannot drift into two notions of it.
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
func composeDigest(summary string, all []string) string {
	var b strings.Builder
	if strings.TrimSpace(summary) != "" {
		b.WriteString("A summary of the earlier conversation:\n")
		b.WriteString(truncateRunes(strings.TrimSpace(summary), MaxPromptRunes*4))
		b.WriteString("\n\n")
	}
	recent := all
	if len(recent) > MaxPrompts {
		recent = recent[len(recent)-MaxPrompts:]
	}
	b.WriteString("The user's requests:\n")
	for _, p := range recent {
		b.WriteString("- ")
		b.WriteString(truncateRunes(strings.TrimSpace(p), MaxPromptRunes))
		b.WriteString("\n")
	}
	return b.String()
}

// truncateRunes bounds a string to at most n runes, appending an ellipsis when
// it cut. It counts runes, not bytes, so a multibyte prompt is never split
// mid-character.
func truncateRunes(s string, n int) string {
	if utf8.RuneCountInString(s) <= n {
		return s
	}
	runes := []rune(s)
	return string(runes[:n]) + "…"
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

// digestHash is the digest's fingerprint: the boundary, the summary and the
// prompts, so any change to what a title would summarize changes the hash and
// any repeat leaves it unchanged. It writes to the hash directly rather than
// through fmt, which production code does not use (the durable-logging bypass
// guard). fnv's Write never errors, so its returns are discarded.
func digestHash(d *shimv1.GatherTitleDigestSuccess) string {
	h := fnv.New64a()
	_, _ = h.Write([]byte("b:" + strconv.Itoa(int(d.GetBoundary())) + "\n"))
	_, _ = h.Write([]byte("s:" + d.GetLastCompactSummary() + "\n"))
	for _, p := range d.GetPrompts() {
		// The NUL separator cannot appear in a prompt, so no two prompt sets
		// hash alike by concatenation.
		_, _ = h.Write([]byte("p:" + p))
		_, _ = h.Write([]byte{0})
	}
	return strconv.FormatUint(h.Sum64(), 16)
}
