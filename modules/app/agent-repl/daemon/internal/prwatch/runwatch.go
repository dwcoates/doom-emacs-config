package prwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"regexp"
	"strconv"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// The workflow-run follow: a workspace told ONCE that a GitHub Actions run
// finished (agentrepl/v1/github_workflow_run.proto). Each tick reads every
// waited-on run once; a run still going is told nothing, and a finished one
// owes each subscriber its one notice while its subscription is removed, in
// one transaction (wsm.OweGithubWorkflowRunCompletion).

// opRunPoll is the operation the run polls record under.
const opRunPoll = "daemon.prwatch.run_poll"

// BriefRunNotice is the brief a run's notice is composed from.
const BriefRunNotice = "github-workflow-run-notice"

// RunOrigin is every run notice's prompt origin.
const RunOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE

// runPath is a github.com workflow run's path: /owner/repo/actions/runs/id,
// optionally followed by an attempt, a job, or a fragment.
var runPath = regexp.MustCompile(`^/([A-Za-z0-9][A-Za-z0-9-]*)/([A-Za-z0-9._-]+)/actions/runs/([1-9][0-9]*)(?:/.*)?$`)

// ParseRunURL answers the run a github.com url names, or an
// *UnparsableError for any other text.
func ParseRunURL(raw string) (wsm.GithubWorkflowRunKey, error) {
	u, err := url.Parse(strings.TrimSpace(raw))
	if err != nil || (u.Scheme != "https" && u.Scheme != "http") || !strings.EqualFold(u.Host, "github.com") {
		return wsm.GithubWorkflowRunKey{}, &UnparsableError{Text: raw, Reason: "not a github.com url"}
	}
	m := runPath.FindStringSubmatch(u.Path)
	if m == nil {
		return wsm.GithubWorkflowRunKey{}, &UnparsableError{Text: raw, Reason: "not a workflow run's address (/OWNER/REPO/actions/runs/ID)"}
	}
	id, err := strconv.ParseUint(m[3], 10, 64)
	if err != nil {
		return wsm.GithubWorkflowRunKey{}, &UnparsableError{Text: raw, Reason: "the run id is out of range"}
	}
	return wsm.GithubWorkflowRunKey{Owner: m[1], Repo: m[2], RunID: id}, nil
}

// runAPIPath is the REST path of KEY's run.
func runAPIPath(key wsm.GithubWorkflowRunKey) string {
	return fmt.Sprintf("repos/%s/%s/actions/runs/%d", key.Owner, key.Repo, key.RunID)
}

// Run is one read of a workflow run, as GitHub answered it.
type Run struct {
	Name       string
	Status     string
	Conclusion string
	HeadSHA    string
	URL        string
	// ReadAt is when the daemon made the read, by its clock.
	ReadAt time.Time
}

// Completed reports whether GitHub reported the run finished.
func (r Run) Completed() bool { return r.Status == "completed" }

// decodeRun reads GitHub's answer for one run.
func decodeRun(body []byte) (Run, error) {
	var shaped struct {
		Name       string  `json:"name"`
		Status     string  `json:"status"`
		Conclusion *string `json:"conclusion"`
		HeadSHA    string  `json:"head_sha"`
		HTMLURL    string  `json:"html_url"`
	}
	if err := json.Unmarshal(body, &shaped); err != nil {
		return Run{}, fmt.Errorf("prwatch: decode a workflow run: %w", err)
	}
	if shaped.Status == "" {
		return Run{}, fmt.Errorf("prwatch: GitHub's workflow run names no status")
	}
	r := Run{Name: shaped.Name, Status: shaped.Status, HeadSHA: shaped.HeadSHA, URL: shaped.HTMLURL}
	if shaped.Conclusion != nil {
		r.Conclusion = *shaped.Conclusion
	}
	return r, nil
}

// readRun reads KEY's run conditionally. Any failure is the leaf's
// *ghclient.Failure (recorded there) or a decode error.
func (w *Watcher) readRun(ctx context.Context, key wsm.GithubWorkflowRunKey) (Run, error) {
	body, err := w.reader.get(ctx, runAPIPath(key))
	if err != nil {
		return Run{}, err
	}
	r, err := decodeRun(body)
	r.ReadAt = w.deps.Clock.Now()
	return r, err
}

// Proto is the run as the contract spells it.
func (r Run) Proto() *agentreplv1.GithubWorkflowRun { return runProto(r) }

// ReadRun reads KEY's run as it stands, for a subscribe to answer with. Any
// failure is the leaf's *ghclient.Failure or a decode error, which
// FailureProto spells.
func (w *Watcher) ReadRun(ctx context.Context, key wsm.GithubWorkflowRunKey) (Run, error) {
	return w.readRun(ctx, key)
}

// RunCompletionOf is the notice SUBSCRIBER is owed for KEY's finished RUN.
// HELD is the store's meaning (wsm.GithubWorkflowRunCompletion.Held). A run
// that has not finished owes nothing, so asking for its completion is a
// defect.
func RunCompletionOf(subscriber ids.WorkspaceID, key wsm.GithubWorkflowRunKey, run Run, held bool) wsm.GithubWorkflowRunCompletion {
	if !run.Completed() {
		panic(fmt.Sprintf("prwatch: the completion of %s owed for a run that has not finished", key))
	}
	return wsm.GithubWorkflowRunCompletion{Subscriber: subscriber, Run: key, Held: held,
		Sentences: []string{runCompletedSentence(run)}, Status: runStatusWords(run)}
}

// Wake has the watcher deliver every owed notice now rather than at its next
// tick: a subscribe to a run already finished owes its notice and wakes it.
// The delivery runs on the watcher's own loop, so it never races a tick's.
func (w *Watcher) Wake() {
	select {
	case w.wake <- struct{}{}:
	default:
		// A wake is already pending; that delivery reads this notice too.
	}
}

// pollRuns reads each waited-on run once and owes its completion to every
// subscription when it finished.
func (w *Watcher) pollRuns(ctx context.Context) {
	subs, err := w.deps.Store.LiveGithubWorkflowRunSubscriptions(ctx)
	if err != nil && ctx.Err() != nil {
		w.deps.Log.Debug(opRunPoll, "the run poll stopped with its context", nil)
		return
	}
	if err != nil {
		w.deps.Log.Error(opRunPoll, "could not read the workflow-run subscriptions; no run is polled this tick", dlog.WithCause(nil, err))
		return
	}
	byRun, order := grouped(subs, func(s wsm.GithubWorkflowRunSubscription) wsm.GithubWorkflowRunKey { return s.Run }, itself[wsm.GithubWorkflowRunSubscription])
	for _, key := range order {
		if ctx.Err() != nil {
			return
		}
		w.pollRun(ctx, key, byRun[key])
	}
}

// pollRun reads KEY's run and advances SUBS.
func (w *Watcher) pollRun(ctx context.Context, key wsm.GithubWorkflowRunKey, subs []wsm.GithubWorkflowRunSubscription) {
	fields := dlog.Context{"workflow_run": key.String(), "subscribers": len(subs)}
	failing := failingOf(subs, func(s wsm.GithubWorkflowRunSubscription) *wsm.PullRequestFailure { return s.Failing })
	recovered := false
	for _, f := range failing {
		recovered = recovered || f != nil
	}
	if resets, waiting := w.rateLimitWaitOf(failing); waiting {
		w.deps.Log.Debug(opRunPoll, "the rate limit is spent; the run is read once it resets", dlog.Merge(fields, dlog.Context{"resets_at": resets}))
		return
	}
	r, err := w.readRun(ctx, key)
	if err != nil {
		if ctx.Err() == nil {
			w.recordRunFailure(ctx, key, err, fields)
		}
		return
	}
	if !r.Completed() {
		// A RUN STILL GOING IS TOLD NOTHING (owner ruling, 2026-11-15): no
		// queued, started, job or step notice. Only a recovery is recorded,
		// so the stored health clears.
		if !recovered {
			return
		}
		if err := w.deps.Store.RecordGithubWorkflowRunRead(ctx, key, r.ReadAt); err != nil {
			w.deps.Log.Error(opRunPoll, "could not record that the run's reads work again; the next poll records it", dlog.WithCause(fields, err))
			return
		}
		w.deps.Log.Info(opRunPoll, "the run's reads work again and it has not finished; its subscribers are told nothing", dlog.Merge(fields, dlog.Context{"status": r.Status}))
		return
	}

	fields["conclusion"] = r.Conclusion
	for _, sub := range subs {
		subFields := dlog.Merge(fields, dlog.Context{"subscriber": string(sub.Subscriber)})
		owed, err := w.deps.Store.OweGithubWorkflowRunCompletion(ctx, RunCompletionOf(sub.Subscriber, key, r, true))
		switch {
		case err != nil:
			w.deps.Log.Error(opRunPoll, "could not owe the finished run's notice; the next poll reads it again", dlog.WithCause(subFields, err))
		case !owed:
			w.deps.Log.Info(opRunPoll, "the subscription ended since it was read; its finished run is owed nothing", subFields)
		default:
			w.deps.Log.Info(opRunPoll, "the run finished; its one notice is owed and its subscription ended", subFields)
		}
	}
}

// recordRunFailure records a failed read for every subscription to KEY.
func (w *Watcher) recordRunFailure(ctx context.Context, key wsm.GithubWorkflowRunKey, err error, fields dlog.Context) {
	w.recordReadFailure(ctx, opRunPoll, "workflow run", err, fields,
		func(f wsm.PullRequestFailure) ([]ids.WorkspaceID, error) {
			return w.deps.Store.RecordGithubWorkflowRunReadFailure(ctx, key, f)
		},
		func(subscriber ids.WorkspaceID, sentences []string) error {
			return w.deps.Store.OweGithubWorkflowRunNotice(ctx, subscriber, key, sentences)
		})
}

// githubWorkflowRunNotice is O as the notice delivery submits it.
func githubWorkflowRunNotice(store Store, o wsm.GithubWorkflowRunOwed) owedNotice {
	return owedNotice{subscriber: o.Subscriber, turn: o.Turn, sentences: o.Sentences, pending: o.Pending, status: o.Status,
		label: "GitHub workflow run `" + o.Run.String() + "`", noun: "workflow-run", subject: "GitHub workflow run", brief: BriefRunNotice, origin: RunOrigin,
		fields: dlog.Context{"subscriber": string(o.Subscriber), "workflow_run": o.Run.String()},
		record: func(ctx context.Context, turn ids.TurnID, sentences []string, took int) error {
			return store.RecordGithubWorkflowRunNoticeSubmitted(ctx, o.Subscriber, o.Run, turn, sentences, took)
		}}
}

// ---- The contract's spellings ----

// RunRefProto is a key as the contract spells it.
func RunRefProto(key wsm.GithubWorkflowRunKey) *agentreplv1.GithubWorkflowRunRef {
	return &agentreplv1.GithubWorkflowRunRef{Repository: &agentreplv1.GithubRepositoryRef{Owner: key.Owner, Repo: key.Repo}, RunId: key.RunID}
}

// RunKeyOf is a contract ref as the store keys it.
func RunKeyOf(ref *agentreplv1.GithubWorkflowRunRef) wsm.GithubWorkflowRunKey {
	return wsm.GithubWorkflowRunKey{Owner: ref.GetRepository().GetOwner(), Repo: ref.GetRepository().GetRepo(), RunID: ref.GetRunId()}
}

// RunSubscriptionProto renders a stored subscription as the contract spells
// it. Its subscriber names the workspace by id; the caller that holds the
// registry fills the directory.
func RunSubscriptionProto(sub wsm.GithubWorkflowRunSubscription) *agentreplv1.GithubWorkflowRunSubscription {
	return &agentreplv1.GithubWorkflowRunSubscription{Subscriber: &workspacev1.WorkspaceRef{Id: string(sub.Subscriber)}, Run: RunRefProto(sub.Run),
		Health: healthProto(sub.LastRead, sub.Failing)}
}

// runProto is R as the contract spells it.
func runProto(r Run) *agentreplv1.GithubWorkflowRun {
	out := &agentreplv1.GithubWorkflowRun{WorkflowName: r.Name, HeadSha: r.HeadSHA, Url: r.URL}
	if !r.Completed() {
		out.Progress = &agentreplv1.GithubWorkflowRun_Unfinished{Unfinished: &agentreplv1.GithubWorkflowRunUnfinished{Status: r.Status}}
		return out
	}
	out.Progress = &agentreplv1.GithubWorkflowRun_Completed{Completed: &agentreplv1.GithubWorkflowRunCompleted{Conclusion: runConclusionProto(r.Conclusion)}}
	return out
}

// runConclusionProto is GitHub's conclusion word as the contract spells it. A
// word this schema revision does not name is the set wrapper with no arm.
func runConclusionProto(word string) *agentreplv1.GithubWorkflowRunConclusion {
	c := &agentreplv1.GithubWorkflowRunConclusion{}
	switch word {
	case "success":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_Success{Success: &agentreplv1.GithubWorkflowRunConclusionSuccess{}}
	case "failure":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_Failure{Failure: &agentreplv1.GithubWorkflowRunConclusionFailure{}}
	case "cancelled":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_Cancelled{Cancelled: &agentreplv1.GithubWorkflowRunConclusionCancelled{}}
	case "timed_out":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_TimedOut{TimedOut: &agentreplv1.GithubWorkflowRunConclusionTimedOut{}}
	case "skipped":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_Skipped{Skipped: &agentreplv1.GithubWorkflowRunConclusionSkipped{}}
	case "neutral":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_Neutral{Neutral: &agentreplv1.GithubWorkflowRunConclusionNeutral{}}
	case "action_required":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_ActionRequired{ActionRequired: &agentreplv1.GithubWorkflowRunConclusionActionRequired{}}
	case "stale":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_Stale{Stale: &agentreplv1.GithubWorkflowRunConclusionStale{}}
	case "startup_failure":
		c.Conclusion = &agentreplv1.GithubWorkflowRunConclusion_StartupFailure{StartupFailure: &agentreplv1.GithubWorkflowRunConclusionStartupFailure{}}
	}
	return c
}

// ---- The words a run's notice is written in ----

// runConclusionWords names a conclusion as the notice says it: GitHub's word,
// its underscores spaced, or a plain statement when GitHub named none.
func runConclusionWords(word string) string {
	if word == "" {
		return "with no conclusion GitHub named"
	}
	return strings.ReplaceAll(word, "_", " ")
}

// runCompletedSentence is the one sentence a finished run is told in: the
// workflow's name, quoted since a repository author wrote it, and how the run
// ended.
func runCompletedSentence(r Run) string {
	return fmt.Sprintf("The run of workflow %q finished: %s.", r.Name, runConclusionWords(r.Conclusion))
}

// runStatusWords is the line a run's notice ends with: its head commit and
// its web address.
func runStatusWords(r Run) string {
	return fmt.Sprintf("Head commit %s: %s", r.HeadSHA, r.URL)
}
