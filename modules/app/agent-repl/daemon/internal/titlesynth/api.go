// Package titlesynth synthesizes the workspace summary the topbar draws.
//
// THE SUMMARY DISTILLS EVERY PROMPT THE USER SENT (owner ruling, 2026-10-14).
// It asks the shim for the title DIGEST (every prompt the user sent since the
// last /clear, including the prompts before any /compact), makes ONE cheap
// headless Haiku call to distill them into a single imperative sentence that
// states the workspace's task, and installs that as the topbar's SYNTHESIZED
// title. It is the first precedence: it wins over the vendor's own ai-title,
// which is only drawn until the first distillation lands, and over the
// workspace name.
//
// THE CADENCE IS THE FIRST PROMPT, THEN EVERY SynthesizeEvery PROMPTS (owner
// ruling, 2026-10-14: "when the first prompt is made, whether it was made
// automatically during workspace generation as the preliminary prompt, or by
// the user explicitly, and then subsequently updated every 5 new prompts").
// Synthesis is considered at session start, at every main-agent prompt row,
// and at the end of every turn; a model call is made only for the first
// summary of a context (since the last /clear), and after that once every
// SynthesizeEvery prompts. A digest unchanged since the last synthesis costs
// nothing either. A /clear resets both, so the next trigger re-synthesizes.
//
// WHY BOTH THE PROMPT ROW AND THE TURN END. The prompt row is the earliest
// edge at which a prompt has been made, so the first summary appears while the
// first turn is still running. But the row the daemon is served is the shim's
// own stream-plane prompt, and the vendor writes the transcript record the
// digest reads separately, so a digest gathered at the row may not hold that
// prompt yet. By a turn's end the vendor has answered the prompt, which it
// does only after recording it, so the turn-end trigger always reads the
// prompt. The digest hash and the cadence make the second trigger free when
// the first one already saw the prompt.
//
// THE MODEL SEES ONLY THE USER'S OWN WORDS: the prompts, whole and
// untruncated, oldest first, with agent-repl's injected meta spans stripped
// (they are the editor's decoration, not the user's request) and no response.
//
// THE SUMMARY OUTLIVES THE DAEMON. Each distillation is recorded in wsm with
// the digest it was distilled from, and Restore installs it again when the
// next daemon registers the workspace, so a restart neither drops a workspace
// back to its bare name nor spends a model call re-distilling a summary that
// was already standing.
//
// BEST EFFORT, NEVER A FAULT. The summary is an enhancement over the workspace
// name; a shim that cannot answer, a guard that refuses the vendor call, or a
// model that returns nothing all leave the previous title standing and are
// recorded at INFO/DEBUG, never WARN — there is nothing for anyone to
// remediate.
package titlesynth

import (
	"context"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Digester gathers one workspace's title digest from its shim.
type Digester interface {
	// GatherTitleDigest reads the workspace's transcript for the material a
	// title is synthesized from. A workspace with no live shim answers the
	// not-found error, which is not a fault.
	GatherTitleDigest(ctx context.Context, ws ids.WorkspaceID) (*shimv1.GatherTitleDigestResponse, error)
}

// SummaryStore keeps each workspace's summary and the digest it was distilled
// from, so a restarted daemon draws the summary at once and keeps the cadence
// rather than spending a model call to re-distill what was already standing.
type SummaryStore interface {
	// WorkspaceSummaryOf loads the workspace's summary; the bool is false when
	// none stands.
	WorkspaceSummaryOf(ctx context.Context, ws ids.WorkspaceID) (wsm.WorkspaceSummary, bool, error)
	// PutWorkspaceSummary records the summary, replacing any it had.
	PutWorkspaceSummary(ctx context.Context, ws ids.WorkspaceID, sum wsm.WorkspaceSummary) error
	// ResetWorkspaceSummaryDigest forgets the digest, keeping the title.
	ResetWorkspaceSummaryDigest(ctx context.Context, ws ids.WorkspaceID) error
	// DropWorkspaceSummary deletes the summary.
	DropWorkspaceSummary(ctx context.Context, ws ids.WorkspaceID) error
}

// TitleSink installs the synthesized title into the topbar.
type TitleSink interface {
	// SetSynthesizedTitle installs the title (empty retracts it).
	SetSynthesizedTitle(ws ids.WorkspaceID, title string)
}

// Deps are the synthesizer's collaborators. Every one is an interface or a
// value so the whole flow is exercised with a mocked model and a mocked shim.
type Deps struct {
	// Digester gathers the digest from the workspace's shim.
	Digester Digester
	// Headless is the cheap one-shot model runner — the SAME client the
	// classifier and the naming call use, guarded by the vendor guard.
	Headless headless.Runner
	// ConfigDirs resolves the per-workspace account root for token
	// attribution: the synthesized title's tokens are billed to the account
	// the workspace's directory routes to.
	ConfigDirs account.WorkspaceConfigDirs
	// Titles installs the synthesized title into the topbar.
	Titles TitleSink
	// Store keeps the summary and its cadence across daemon restarts.
	Store SummaryStore
	// PromptsDir is where the headless brief is read from AT USE TIME, so an
	// edit takes effect without a daemon bounce.
	PromptsDir string
	// Log is the synthesizer's canonical logger.
	Log dlog.Logger
	// GatherTimeout bounds the shim digest call. Zero uses DefaultGatherTimeout.
	GatherTimeout time.Duration
	// SynthesizeTimeout bounds the headless model call. Zero uses
	// DefaultSynthesizeTimeout.
	SynthesizeTimeout time.Duration
}

// Defaults for the injectable bounds.
const (
	// BriefTitle is the headless brief this package reads.
	BriefTitle = "synthesized-title-from-digest"
	// Site is the vendor-guard site the synthesis call asks under, beside
	// "classifier", "workspace_naming" and "login".
	Site = "title_synthesis"
	// DefaultGatherTimeout bounds one GatherTitleDigest call. It reads a local
	// file the shim already owns, so it is quick; a call that has not answered
	// in this long has hung.
	DefaultGatherTimeout = 10 * time.Second
	// DefaultSynthesizeTimeout bounds one headless model call. It is a handful
	// of tokens from a small model, the same shape as the naming call's bound.
	DefaultSynthesizeTimeout = 15 * time.Second
	// SynthesizeEvery is the cadence: after a context's first title, the
	// model is asked again only once this many more prompts have been made.
	SynthesizeEvery = 5
	// MaxPrompts caps how many of the most recent prompts ride the model
	// prompt, so a long conversation's digest cannot grow the call without
	// bound. The most recent prompts are the ones a title should reflect.
	MaxPrompts = 40
	// MaxPromptRunes caps one prompt's length in the model prompt, so a single
	// pasted wall of text cannot dominate the call.
	MaxPromptRunes = 1000
	// MaxDigestTotalRunes caps ComposeDigest's WHOLE rendered output, on top of
	// the per-item caps above. The per-item caps alone still let the pieces sum
	// past 44,000 runes (a 4,000-rune summary plus 40 prompts at 1,000 runes
	// each), which is more than a title call — a handful of tokens from a small
	// model — needs to spend. When the rendered digest is over this cap, the
	// OLDEST material is dropped first (the summary, then the oldest surviving
	// prompt, one at a time) so the newest prompts are always what survives:
	// recency is what a synthesized or fork-naming title should reflect.
	MaxDigestTotalRunes = 8000
)
