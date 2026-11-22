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
	// over one answer, most of it the `claude -p` command's own start.
	// MEASURED 2026-11-18 from the owner's run log: 29 calls, the slowest
	// 4.97s, 5.68s and 6.11s, and one killed at the earlier 10s bound (it was
	// sized from 1.9-4.0s on 2026-10-14). Doubled to 20s at the owner's
	// ruling, about three times the measured worst.
	DefaultSummaryTimeout = 20 * time.Second
	// MaxAnswerRunes caps the answer the call is handed, so one enormous
	// answer cannot grow the call without bound. The head of an answer is
	// where its conclusion is stated.
	MaxAnswerRunes = 12000
)

// The pull-request comment banner's call.
const (
	// BriefCommentSummary is the brief the comment summary is composed from.
	BriefCommentSummary = "pull-request-comment-summary"
	// CommentSummarySite is the vendor-guard site the comment summary asks
	// under.
	CommentSummarySite = "pull_request_comment_summary"
	// MaxCommentRunes caps the comment the call is handed, as MaxAnswerRunes
	// caps an answer: a comment's head is where its ask is stated.
	MaxCommentRunes = 4000
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
	log := s.Log.With(bannerContext(ws, nil))
	if strings.TrimSpace(answer) == "" {
		log.Info(opSummary, "the turn named no final answer; nothing to summarize", nil)
		return NoAnswerLine
	}
	return s.sentence(ctx, ws, turnSummary, runecap.Head(answer, MaxAnswerRunes), log)
}

// SummarizeComment answers the comment banner's summary of COMMENT, the
// comment's markdown, through the same one-sentence call as Summarize. A
// blank comment is summarized as nothing, with no call: the banner names its
// author alone.
func (s Summarizer) SummarizeComment(ctx context.Context, ws ids.WorkspaceID, comment string) string {
	log := s.Log.With(bannerContext(ws, nil))
	if strings.TrimSpace(comment) == "" {
		log.Info(opSummary, "the pull-request comment is blank; nothing to summarize", nil)
		return ""
	}
	return s.sentence(ctx, ws, commentSummary, runecap.Head(comment, MaxCommentRunes), log)
}

// sentenceCall is one kind of one-sentence Haiku call a banner's body is
// asked of: every kind shares Summarizer.sentence's steps and records, and
// differs only in the brief it is composed from and the words it is
// recorded in.
type sentenceCall struct {
	// brief is the brief's name, and placeholder the one the input is
	// spliced into.
	brief, placeholder string
	// site is the vendor-guard site the call asks under.
	site string
	// what names the call in its records ("turn-summary").
	what string
	// unmade is what a workspace with no account leaves unsummarized ("the
	// turn was not summarized").
	unmade string
	// made is the record of a summary made.
	made string
}

// turnSummary is the completed-turn banner's call.
var turnSummary = sentenceCall{
	brief: BriefSummary, placeholder: "answer", site: SummarySite, what: "turn-summary",
	unmade: "the turn was not summarized", made: "summarized the turn's final answer",
}

// commentSummary is the pull-request comment banner's call.
var commentSummary = sentenceCall{
	brief: BriefCommentSummary, placeholder: "comment", site: CommentSummarySite, what: "comment-summary",
	unmade: "the pull-request comment was not summarized", made: "summarized the pull-request comment",
}

// sentence makes CALL over INPUT, answering the banner body: the summary, or
// "Summary unavailable: " and the cause of its failure (already recorded).
func (s Summarizer) sentence(ctx context.Context, ws ids.WorkspaceID, call sentenceCall, input string, log dlog.Logger) string {
	summary, cause := s.summarize(ctx, ws, call, input, log)
	if cause != "" {
		return "Summary unavailable: " + cause
	}
	return summary
}

// summarize makes the model call, answering the summary or the cause of its
// failure (already recorded).
func (s Summarizer) summarize(ctx context.Context, ws ids.WorkspaceID, call sentenceCall, input string, log dlog.Logger) (string, string) {
	brief, err := prompts.Load(s.PromptsDir, call.brief)
	if err != nil {
		log.Error(opSummary, "the "+call.what+" brief could not be read", dlog.Context{
			"brief": call.brief, "cause": err.Error(),
		})
		return "", "the summary brief could not be read"
	}
	question, err := brief.Splice(map[string]string{call.placeholder: input})
	if err != nil {
		log.Error(opSummary, "the "+call.what+" brief could not be spliced", dlog.Context{
			"brief": call.brief, "cause": err.Error(),
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
		log.Error(opSummary, "no account root for this workspace; "+call.unmade, dlog.Context{"cause": err.Error()})
		return "", "no account for this workspace"
	}
	timeout := s.Timeout
	if timeout == 0 {
		timeout = DefaultSummaryTimeout
	}
	resp, err := s.Headless.Run(ctx, headless.Request{
		Site:      call.site,
		Model:     SummaryModel,
		Kind:      headless.KindInference,
		ConfigDir: configDir,
		Prompt:    question,
		Timeout:   timeout,
	})
	if standdown.CutShort(ctx, err) {
		// ctx is the notifier's lifetime: the daemon stood down mid-call and
		// killed the child, which is the stand-down working, not a failure.
		log.Info(opSummary, "the daemon stood down during the "+call.what+" call", dlog.Context{
			"model": SummaryModel, "cause": ctx.Err().Error(), "detail": err.Error(),
		})
		return "", StoodDownCause
	}
	if err != nil {
		cause := headless.CauseOf(err)
		log.Error(opSummary, "the "+call.what+" model call failed", dlog.Context{
			"model": SummaryModel, "cause": cause, "detail": err.Error(),
		})
		return "", cause
	}
	summary := textline.FirstNonBlank(resp.Text)
	if summary == "" {
		log.Error(opSummary, "the "+call.what+" model call answered empty", dlog.Context{"model": SummaryModel})
		return "", "the model answered empty"
	}
	log.Info(opSummary, call.made, dlog.Context{
		"model": SummaryModel, "duration_ms": resp.Duration.Milliseconds(),
	})
	return summary, ""
}
