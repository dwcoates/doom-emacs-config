// Package titlesynth synthesizes a workspace's title when the vendor has
// written no ai-title.
//
// THE VENDOR'S TITLE ALWAYS WINS. The topbar draws the vendor's own ai-title
// summary in place of the workspace name whenever the vendor has stated one,
// but many CLI versions never write one, so those workspaces fell back to the
// bare directory name. This package fills that gap: it asks the shim for the
// title DIGEST (the prompts since the last context boundary, plus the
// compaction summary when the last boundary was a /compact), makes ONE cheap
// headless model call to summarize it into a single short sentence, and installs
// that as the topbar's SYNTHESIZED title — the middle precedence, below the
// vendor's ai-title and above the workspace name.
//
// COST IS CONTROLLED BY A DIGEST HASH. Synthesis fires at session start and at
// the end of every turn, but a call is made only when the digest actually
// CHANGED since the last synthesis (a new prompt, or a compaction). Two triggers
// with the same digest cost nothing, so the steady state is at most one cheap
// call per new prompt. A /clear or /compact resets the hash so the next trigger
// re-synthesizes, and the vendor stating a title stops synthesis outright.
//
// BEST EFFORT, NEVER A FAULT. The synthesized title is an enhancement over the
// workspace name; a shim that cannot answer, a guard that refuses the vendor
// call, or a model that returns nothing all leave the name standing and are
// recorded at INFO/DEBUG, never WARN — there is nothing for anyone to remediate.
package titlesynth

import (
	"context"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/ids"
)

// Digester gathers one workspace's title digest from its shim.
type Digester interface {
	// GatherTitleDigest reads the workspace's transcript for the material a
	// title is synthesized from. A workspace with no live shim answers the
	// not-found error, which is not a fault.
	GatherTitleDigest(ctx context.Context, ws ids.WorkspaceID) (*shimv1.GatherTitleDigestResponse, error)
}

// ConfigDirSource resolves the account config dir a workspace's headless call
// must bill. The synthesized title's tokens are attributed to the SAME account
// the session spends as, so a workspace on a second account never spends the
// default account's allowance on its title.
type ConfigDirSource interface {
	// ConfigDirFor answers the workspace's account config dir, and false when
	// the workspace is not one this daemon can resolve a root for.
	ConfigDirFor(ws ids.WorkspaceID) (string, bool)
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
	// ConfigDirs resolves the per-workspace account root for token attribution.
	ConfigDirs ConfigDirSource
	// Titles installs the synthesized title into the topbar.
	Titles TitleSink
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
