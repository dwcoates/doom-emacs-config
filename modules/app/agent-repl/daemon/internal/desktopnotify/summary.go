package desktopnotify

import (
	"context"
	"strings"
	"time"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/runecap"
	"agent-repld/internal/standdown"
	"agent-repld/internal/textline"
)

// opSummary is the operation the turn summary's records carry.
const opSummary = "daemon.desktopnotify.summary"

// The turn summary's headless call.
const (
	// BriefSummary is the brief the summary call is composed from.
	BriefSummary = "turn-summary-from-answer"
	// SummarySite is the vendor-guard site the summary call asks under.
	SummarySite = "turn_summary"
	// SummaryModel is the model the summary is asked of: one short sentence
	// is a job for the fast model, and the banner should not wait on more.
	SummaryModel = headless.ModelHaiku
	// DefaultSummaryTimeout bounds one summary call: one sentence from Haiku
	// over one answer. MEASURED 2026-10-14: 1.9-4.0s end to end, a
	// MaxAnswerRunes answer included, so the bound is just over twice that.
	DefaultSummaryTimeout = 10 * time.Second
	// MaxAnswerRunes caps the answer the call is handed, so one enormous
	// answer cannot grow the call without bound. The head of an answer is
	// where its conclusion is stated.
	MaxAnswerRunes = 12000
)

// StoodDownCause names a summary the daemon's stand-down cut short.
const StoodDownCause = "the daemon stood down"

// NoAnswerLine is the body of a completed turn that named no final answer.
const NoAnswerLine = "The turn ended with no final answer."

// Summarizer writes the completed-turn banner's body: a one-sentence Haiku
// summary of the turn's final answer, asked as bare inference (no tools, no
// system prompt), so the brief and the answer are all the model sees.
type Summarizer struct {
	// Headless runs the model call.
	Headless headless.Runner
	// ConfigDirs bills the call to the workspace's account.
	ConfigDirs account.WorkspaceConfigDirs
	// PromptsDir holds the brief, read at use time.
	PromptsDir string
	// Timeout bounds the call. Zero is DefaultSummaryTimeout.
	Timeout time.Duration
	// Log is the summarizer's canonical logger.
	Log dlog.Logger
}

// Summarize answers the banner body for answer. A summary that could not be
// made is recorded at ERROR and the body says so, naming the cause: the
// banner still tells the user the turn completed.
func (s Summarizer) Summarize(ctx context.Context, ws ids.WorkspaceID, answer string) string {
	log := s.Log.With(dlog.Context{"workspace": string(ws)})
	if strings.TrimSpace(answer) == "" {
		log.Info(opSummary, "the turn named no final answer; nothing to summarize", nil)
		return NoAnswerLine
	}
	summary, cause := s.summarize(ctx, ws, answer, log)
	if cause != "" {
		return "Summary unavailable: " + cause
	}
	return summary
}

// summarize makes the model call, answering the summary or the cause of its
// failure (already recorded).
func (s Summarizer) summarize(ctx context.Context, ws ids.WorkspaceID, answer string, log dlog.Logger) (string, string) {
	brief, err := prompts.Load(s.PromptsDir, BriefSummary)
	if err != nil {
		log.Error(opSummary, "the turn-summary brief could not be read", dlog.Context{
			"brief": BriefSummary, "cause": err.Error(),
		})
		return "", "the summary brief could not be read"
	}
	question, err := brief.Splice(map[string]string{"answer": runecap.Head(answer, MaxAnswerRunes)})
	if err != nil {
		log.Error(opSummary, "the turn-summary brief could not be spliced", dlog.Context{
			"brief": BriefSummary, "cause": err.Error(),
		})
		return "", "the summary brief could not be spliced"
	}
	configDir, err := s.ConfigDirs.ConfigDirFor(ctx, ws)
	if standdown.CutShort(ctx, err) {
		// ctx is the notifier's lifetime: the daemon stood down while the
		// workspace's account was being read, which is the stand-down
		// working, not a workspace with no account.
		log.Info(opSummary, "the daemon stood down while the workspace's account was read", dlog.Context{
			"cause": ctx.Err().Error(), "detail": err.Error(),
		})
		return "", StoodDownCause
	}
	if err != nil {
		log.Error(opSummary, "no account root for this workspace; the turn was not summarized", dlog.Context{"cause": err.Error()})
		return "", "no account for this workspace"
	}
	timeout := s.Timeout
	if timeout == 0 {
		timeout = DefaultSummaryTimeout
	}
	resp, err := s.Headless.Run(ctx, headless.Request{
		Site:      SummarySite,
		Model:     SummaryModel,
		Format:    headless.FormatText,
		Plain:     true,
		ConfigDir: configDir,
		Prompt:    question,
		Timeout:   timeout,
	})
	if standdown.CutShort(ctx, err) {
		// ctx is the notifier's lifetime: the daemon stood down mid-call and
		// killed the child, which is the stand-down working, not a failure.
		log.Info(opSummary, "the daemon stood down during the turn-summary call", dlog.Context{
			"model": SummaryModel, "cause": ctx.Err().Error(), "detail": err.Error(),
		})
		return "", StoodDownCause
	}
	if err != nil {
		cause := headless.CauseOf(err)
		log.Error(opSummary, "the turn-summary model call failed", dlog.Context{
			"model": SummaryModel, "cause": cause, "detail": err.Error(),
		})
		return "", cause
	}
	summary := textline.FirstNonBlank(resp.Text)
	if summary == "" {
		log.Error(opSummary, "the turn-summary model call answered empty", dlog.Context{"model": SummaryModel})
		return "", "the model answered empty"
	}
	log.Info(opSummary, "summarized the turn's final answer", dlog.Context{
		"model": SummaryModel, "duration_ms": resp.Duration.Milliseconds(),
	})
	return summary, ""
}
