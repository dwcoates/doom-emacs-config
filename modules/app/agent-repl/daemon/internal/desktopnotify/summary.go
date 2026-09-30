package desktopnotify

import (
	"context"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
)

// opSummary is the operation the turn summary's records carry.
const opSummary = "daemon.desktopnotify.summary"

// The turn summary's headless call.
const (
	// BriefSummary is the brief the summary call is composed from.
	BriefSummary = "turn-summary-from-answer"
	// SummarySite is the vendor-guard site the summary call asks under.
	SummarySite = "turn_summary"
	// DefaultSummaryTimeout bounds one summary call: a few lines from Sonnet
	// over one answer.
	DefaultSummaryTimeout = 30 * time.Second
	// MaxAnswerRunes caps the answer the call is handed, so one enormous
	// answer cannot grow the call without bound. The head of an answer is
	// where its conclusion is stated.
	MaxAnswerRunes = 12000
	// MaxSummaryLines is the banner body's line budget.
	MaxSummaryLines = 3
)

// StoodDownCause names a summary the daemon's stand-down cut short.
const StoodDownCause = "the daemon stood down"

// NoAnswerLine is the body of a completed turn that named no final answer.
const NoAnswerLine = "The turn ended with no final answer."

// ConfigDirs resolves the account a workspace's headless call bills, so the
// summary is paid for by the account the workspace's session spends as.
type ConfigDirs interface {
	ConfigDirFor(ws ids.WorkspaceID) (string, bool)
}

// Summarizer writes the completed-turn banner's body: a Sonnet summary of the
// turn's final answer.
type Summarizer struct {
	// Headless runs the model call.
	Headless headless.Runner
	// ConfigDirs bills the call to the workspace's account.
	ConfigDirs ConfigDirs
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
	question, err := brief.Splice(map[string]string{"answer": truncateRunes(answer, MaxAnswerRunes)})
	if err != nil {
		log.Error(opSummary, "the turn-summary brief could not be spliced", dlog.Context{
			"brief": BriefSummary, "cause": err.Error(),
		})
		return "", "the summary brief could not be spliced"
	}
	configDir, ok := s.ConfigDirs.ConfigDirFor(ws)
	if !ok {
		log.Error(opSummary, "no account root for this workspace; the turn was not summarized", nil)
		return "", "no account for this workspace"
	}
	timeout := s.Timeout
	if timeout == 0 {
		timeout = DefaultSummaryTimeout
	}
	resp, err := s.Headless.Run(ctx, headless.Request{
		Site:      SummarySite,
		Model:     headless.ModelSonnet,
		Format:    headless.FormatText,
		ConfigDir: configDir,
		Prompt:    question,
		Timeout:   timeout,
	})
	if err != nil && ctx.Err() != nil {
		// ctx is the notifier's lifetime: the daemon stood down mid-call and
		// killed the child, which is the stand-down working, not a failure.
		log.Info(opSummary, "the daemon stood down during the turn-summary call", dlog.Context{
			"model": headless.ModelSonnet, "cause": ctx.Err().Error(), "detail": err.Error(),
		})
		return "", StoodDownCause
	}
	if err != nil {
		cause := headless.CauseOf(err)
		log.Error(opSummary, "the turn-summary model call failed", dlog.Context{
			"model": headless.ModelSonnet, "cause": cause, "detail": err.Error(),
		})
		return "", cause
	}
	summary := firstLines(resp.Text, MaxSummaryLines)
	if summary == "" {
		log.Error(opSummary, "the turn-summary model call answered empty", dlog.Context{"model": headless.ModelSonnet})
		return "", "the model answered empty"
	}
	log.Info(opSummary, "summarized the turn's final answer", dlog.Context{
		"model": headless.ModelSonnet, "duration_ms": resp.Duration.Milliseconds(),
	})
	return summary, ""
}

// firstLines keeps the first n non-blank lines of text, trimmed.
func firstLines(text string, n int) string {
	var kept []string
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		kept = append(kept, line)
		if len(kept) == n {
			break
		}
	}
	return strings.Join(kept, "\n")
}

// truncateRunes keeps the first n runes of text.
func truncateRunes(text string, n int) string {
	runes := []rune(text)
	if len(runes) <= n {
		return text
	}
	return string(runes[:n])
}
