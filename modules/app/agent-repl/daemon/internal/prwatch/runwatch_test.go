package prwatch

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/wsm"
)

// runK is the run every test waits on.
var runK = wsm.GithubWorkflowRunKey{Owner: "o", Repo: "r", RunID: 43771255095}

// pathRun is the one read a poll of runK makes.
func pathRun() string { return runAPIPath(runK) }

// runJSON is runK as GitHub answers it; CONCLUSION nil is JSON null.
func runJSON(status string, conclusion *string) string {
	return mustJSON(map[string]any{"name": "CEAC sandbox release", "status": status, "conclusion": conclusion,
		"head_sha": "0123456789abcdef0123456789abcdef01234567", "html_url": "https://github.com/o/r/actions/runs/43771255095"})
}

func word(s string) *string { return &s }

// subscribeRun records a subscription of the rig's workspace to runK.
func (r *rig) subscribeRun(t *testing.T) {
	t.Helper()
	if _, err := r.db.SubscribeGithubWorkflowRun(context.Background(), wsm.GithubWorkflowRunSubscription{Subscriber: r.ws, Run: runK, SubscribedAt: t0, LastRead: t0}); err != nil {
		t.Fatalf("SubscribeGithubWorkflowRun: %v", err)
	}
}

// runSubs is the rig's workspace's run subscriptions.
func (r *rig) runSubs(t *testing.T) []wsm.GithubWorkflowRunSubscription {
	t.Helper()
	subs, err := r.db.WorkspaceGithubWorkflowRunSubscriptions(context.Background(), r.ws)
	if err != nil {
		t.Fatal(err)
	}
	return subs
}

// runReads counts the reads of runK the rig's GitHub took.
func (r *rig) runReads() int {
	r.gh.mu.Lock()
	defer r.gh.mu.Unlock()
	n := 0
	for _, c := range r.gh.calls {
		if c == "GET "+pathRun() {
			n++
		}
	}
	return n
}

func TestParseRunURL(t *testing.T) {
	tests := []struct {
		name string
		text string
		want wsm.GithubWorkflowRunKey
		ok   bool
	}{
		{name: "a run", text: "https://github.com/ChessCom/explanation-engine/actions/runs/43771255095", want: wsm.GithubWorkflowRunKey{Owner: "ChessCom", Repo: "explanation-engine", RunID: 43771255095}, ok: true},
		{name: "a run's job", text: "https://github.com/o/r/actions/runs/5/job/9", want: wsm.GithubWorkflowRunKey{Owner: "o", Repo: "r", RunID: 5}, ok: true},
		{name: "an attempt with a fragment", text: " https://github.com/o/r.js/actions/runs/5/attempts/2#summary ", want: wsm.GithubWorkflowRunKey{Owner: "o", Repo: "r.js", RunID: 5}, ok: true},
		{name: "a pull request", text: "https://github.com/o/r/pull/5", ok: false},
		{name: "a workflow, not a run", text: "https://github.com/o/r/actions/workflows/release.yaml", ok: false},
		{name: "another host", text: "https://gitlab.com/o/r/actions/runs/5", ok: false},
		{name: "a zero id", text: "https://github.com/o/r/actions/runs/0", ok: false},
		{name: "a bare id", text: "43771255095", ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseRunURL(tt.text)

			// Assert
			var unparsable *UnparsableError
			if tt.ok && (err != nil || got != tt.want) {
				t.Fatalf("ParseRunURL(%q) = %v, %v; want %v", tt.text, got, err, tt.want)
			}
			if !tt.ok && !errors.As(err, &unparsable) {
				t.Fatalf("ParseRunURL(%q) = %v, %v; want an *UnparsableError", tt.text, got, err)
			}
		})
	}
}

func TestAFinishedRunIsToldOnceAndEndsItsSubscription(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribeRun(t)
	r.gh.set(pathRun(), runJSON("completed", word("failure")))

	// Act
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	want := BriefRunNotice + "|GitHub workflow run `o/r/runs/43771255095`|" +
		`The run of workflow "CEAC sandbox release" finished: failure.` +
		"|Head commit 0123456789abcdef0123456789abcdef01234567: https://github.com/o/r/actions/runs/43771255095"
	if len(told) != 1 || told[0] != want {
		t.Fatalf("told = %q, want once %q", told, want)
	}
	if r.prompts.got[0].origin != RunOrigin || r.prompts.got[0].ws != r.ws {
		t.Fatalf("submission = %+v, want the workflow-run notice origin to the subscriber", r.prompts.got[0])
	}
	if subs := r.runSubs(t); len(subs) != 0 {
		t.Fatalf("subscriptions = %+v, want the subscription ended", subs)
	}
	if got := r.runReads(); got != 1 {
		t.Fatalf("reads = %d, want the run read once and never again", got)
	}
}

func TestAnUnfinishedRunIsToldNothing(t *testing.T) {
	for _, status := range []string{"queued", "in_progress", "waiting"} {
		t.Run(status, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribeRun(t)
			r.gh.set(pathRun(), runJSON(status, nil))

			// Act
			r.tick()
			r.tick()

			// Assert
			if told := r.told(); len(told) != 0 {
				t.Fatalf("told = %q, want no progress notice", told)
			}
			if subs := r.runSubs(t); len(subs) != 1 || subs[0].Failing != nil {
				t.Fatalf("subscriptions = %+v, want the one, healthy", subs)
			}
		})
	}
}

func TestEveryConclusionIsSaidInGithubsWord(t *testing.T) {
	tests := []struct {
		conclusion *string
		want       string
	}{
		{conclusion: word("success"), want: "finished: success."},
		{conclusion: word("cancelled"), want: "finished: cancelled."},
		{conclusion: word("timed_out"), want: "finished: timed out."},
		{conclusion: word("startup_failure"), want: "finished: startup failure."},
		{conclusion: word("a_new_one"), want: "finished: a new one."},
		{conclusion: nil, want: "finished: with no conclusion GitHub named."},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribeRun(t)
			r.gh.set(pathRun(), runJSON("completed", tt.conclusion))

			// Act
			r.tick()

			// Assert
			if told := r.told(); len(told) != 1 || !contains(told[0], tt.want) {
				t.Fatalf("told = %q, want %q", told, tt.want)
			}
		})
	}
}

func TestRunConclusionProtoNamesEveryDocumentedConclusion(t *testing.T) {
	tests := []struct {
		word string
		want any
	}{
		{word: "success", want: &agentreplv1.GithubWorkflowRunConclusion_Success{}},
		{word: "failure", want: &agentreplv1.GithubWorkflowRunConclusion_Failure{}},
		{word: "cancelled", want: &agentreplv1.GithubWorkflowRunConclusion_Cancelled{}},
		{word: "timed_out", want: &agentreplv1.GithubWorkflowRunConclusion_TimedOut{}},
		{word: "skipped", want: &agentreplv1.GithubWorkflowRunConclusion_Skipped{}},
		{word: "neutral", want: &agentreplv1.GithubWorkflowRunConclusion_Neutral{}},
		{word: "action_required", want: &agentreplv1.GithubWorkflowRunConclusion_ActionRequired{}},
		{word: "stale", want: &agentreplv1.GithubWorkflowRunConclusion_Stale{}},
		{word: "startup_failure", want: &agentreplv1.GithubWorkflowRunConclusion_StartupFailure{}},
		{word: "a_new_one", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.word, func(t *testing.T) {
			// Act
			got := runConclusionProto(tt.word).GetConclusion()

			// Assert
			if tt.want == nil {
				if got != nil {
					t.Fatalf("conclusion %q = %T, want the wrapper with no arm", tt.word, got)
				}
				return
			}
			if got == nil || typeName(got) != typeName(tt.want) {
				t.Fatalf("conclusion %q = %T, want %T", tt.word, got, tt.want)
			}
		})
	}
}

// typeName is V's dynamic type, for comparing oneof arms.
func typeName(v any) string { return fmt.Sprintf("%T", v) }

func TestRejectedCredentialsAreToldOnceAndTheRunIsStillToldWhenItFinishes(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribeRun(t)
	r.gh.set(pathRun(), &ghclient.Failure{Kind: ghclient.KindUnauthorized, Message: "Bad credentials", Status: 401})

	// Act: two failing polls of one standing failure, then the credentials
	// renewed and the run finished.
	r.tick()
	r.tick()
	failing := r.runSubs(t)
	r.gh.set(pathRun(), runJSON("completed", word("success")))
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 2 || !contains(told[0], "GitHub rejected agent-repl's credentials") || !contains(told[1], "finished: success.") {
		t.Fatalf("told = %q, want one notice of the rejected credentials, then the completion", told)
	}
	if len(failing) != 1 || failing[0].Failing == nil || failing[0].Failing.Kind != wsm.PullRequestFailureUnauthorized {
		t.Fatalf("subscription while failing = %+v, want it failing unauthorized", failing)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opRunPoll,
		Message: "GitHub rejected the credentials; each subscription whose failure began is owed a notice to surface it"}); !ok {
		t.Fatalf("no record of the rejected credentials: %+v", r.log.Records())
	}
}

func TestAnUnreachableRunIsToldToNobody(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribeRun(t)
	r.gh.set(pathRun(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "no route to host"})

	// Act
	r.tick()

	// Assert
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %q, want nothing", told)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opRunPoll,
		Message: "the workflow run could not be read; its subscriptions are failing, and no subscriber is told"}); !ok {
		t.Fatalf("no record of the failed read: %+v", r.log.Records())
	}
}

func TestAnUnfinishedRunsRecoveryClearsTheHealthAndTellsNothing(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribeRun(t)
	r.gh.set(pathRun(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "no route to host"})
	r.tick()
	r.gh.set(pathRun(), runJSON("in_progress", nil))
	r.clock.Set(t0.Add(time.Minute))

	// Act
	r.tick()

	// Assert
	subs := r.runSubs(t)
	if len(subs) != 1 || subs[0].Failing != nil || !subs[0].LastRead.Equal(t0.Add(time.Minute)) {
		t.Fatalf("subscriptions = %+v, want healthy, read at the recovery", subs)
	}
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %q, want nothing", told)
	}
}

func TestASpentRateLimitIsWaitedOutForARun(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribeRun(t)
	r.gh.set(pathRun(), &ghclient.Failure{Kind: ghclient.KindRateLimited, Status: 403, ResetsAt: t0.Add(time.Hour)})
	r.tick()

	// Act
	r.tick()

	// Assert
	if got := r.runReads(); got != 1 {
		t.Fatalf("reads = %d, want none past the spent limit", got)
	}
}

func TestAWakeDeliversAnOwedRunNoticeWithoutPolling(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.prompts.submitted = make(chan struct{}, 1)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		defer close(done)
		r.w.Run(ctx, r.prompts)
	}()
	<-r.clock.Asked // the first tick ran; the loop waits on its cadence
	r.gh.mu.Lock()
	callsBefore := len(r.gh.calls)
	r.gh.mu.Unlock()
	run := Run{remote: remoteRun{Name: "release", Status: "completed", Conclusion: "success", HeadSHA: "abc", URL: "https://github.com/o/r/actions/runs/1"}}
	if _, err := r.db.OweGithubWorkflowRunCompletion(ctx, RunCompletionOf(r.ws, runK, run, false)); err != nil {
		t.Fatal(err)
	}

	// Act
	r.w.Wake()
	<-r.prompts.submitted
	cancel()
	<-done

	// Assert
	if told := r.told(); len(told) != 1 || !contains(told[0], `"release" finished: success.`) {
		t.Fatalf("told = %q, want the owed completion", told)
	}
	r.gh.mu.Lock()
	defer r.gh.mu.Unlock()
	if len(r.gh.calls) != callsBefore {
		t.Fatalf("calls = %v, want no read made by the wake", r.gh.calls[callsBefore:])
	}
}

func TestAWakeOfADaemonThatDoesNotServeDeliversNothing(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.serves = false
	run := Run{remote: remoteRun{Name: "release", Status: "completed", Conclusion: "success"}}
	if _, err := r.db.OweGithubWorkflowRunCompletion(context.Background(), RunCompletionOf(r.ws, runK, run, false)); err != nil {
		t.Fatal(err)
	}

	// Act
	r.w.DeliverNow(context.Background(), r.prompts)

	// Assert
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %q, want nothing from a daemon that does not serve", told)
	}
}

func TestRunCompletionOfAnUnfinishedRunIsADefect(t *testing.T) {
	// Arrange
	run := Run{remote: remoteRun{Name: "release", Status: "in_progress"}}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("RunCompletionOf did not panic for an unfinished run")
		}
	}()

	// Act
	RunCompletionOf("w", runK, run, true)
}

func TestReadRunAnswersTheRunAndItsFailure(t *testing.T) {
	tests := []struct {
		name        string
		answer      any
		wantFinish  bool
		wantErrKind ghclient.Kind
		wantDecode  bool
	}{
		{name: "an unfinished run", answer: runJSON("in_progress", nil)},
		{name: "a finished run", answer: runJSON("completed", word("success")), wantFinish: true},
		{name: "rejected credentials", answer: &ghclient.Failure{Kind: ghclient.KindUnauthorized, Status: 401}, wantErrKind: ghclient.KindUnauthorized},
		{name: "a run GitHub cannot find", answer: &ghclient.Failure{Kind: ghclient.KindNotFound, Status: 404}, wantErrKind: ghclient.KindNotFound},
		{name: "an answer with no status", answer: `{"name":"x"}`, wantDecode: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.gh.set(pathRun(), tt.answer)

			// Act
			run, err := r.w.ReadRun(context.Background(), runK)

			// Assert
			var gh *ghclient.Failure
			switch {
			case tt.wantErrKind != "":
				if !errors.As(err, &gh) || gh.Kind != tt.wantErrKind {
					t.Fatalf("ReadRun error = %v, want %s", err, tt.wantErrKind)
				}
			case tt.wantDecode:
				if err == nil {
					t.Fatal("ReadRun = nil error, want the undecodable answer refused")
				}
			default:
				if err != nil || run.Completed() != tt.wantFinish || run.Proto().GetWorkflowName() != "CEAC sandbox release" {
					t.Fatalf("ReadRun = %+v, %v; want finished %v", run.Proto(), err, tt.wantFinish)
				}
			}
		})
	}
}
