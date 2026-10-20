package server

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"sync"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const opEvaluate = "daemon.server.evaluate_in_editor"

// evaluationDeadline is how long EvaluateInEditor waits for the editor's
// answer. Every answer records its measured round trip (latency_ms), the
// figure the deadline is held to.
const evaluationDeadline = 10 * time.Second

// editorEvals correlates the evaluations pushed to editors with their
// answers, by token.
//
// A TOKEN IS THIS DAEMON'S AND COUNTED: `<prefix>-<n>`, n minted in order. A
// token of this daemon's that no evaluation still waits on is a LATE answer,
// told apart from one this daemon never minted without remembering every
// token it has seen.
//
// AN EVALUATION NEVER OUTWAITS ITS EDITOR: it is registered BEFORE the editor
// is looked for, so the editor's host stream closing at any moment after the
// look wakes it (editorGone), and none can wait out the deadline on a stream
// that is gone.
type editorEvals struct {
	prefix  string
	mu      sync.Mutex
	minted  uint64
	pending map[uint64]*pendingEval
}

// pendingEval is one evaluation waiting on its answer.
type pendingEval struct {
	ws     ids.WorkspaceID
	answer chan *agentreplv1.EditorEvaluationOutcome
	gone   chan struct{}
}

func newEditorEvals(prefix string) *editorEvals {
	return &editorEvals{prefix: prefix, pending: map[uint64]*pendingEval{}}
}

// open registers an evaluation for WS and answers its token.
func (e *editorEvals) open(ws ids.WorkspaceID) (string, *pendingEval) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.minted++
	p := &pendingEval{ws: ws, answer: make(chan *agentreplv1.EditorEvaluationOutcome, 1), gone: make(chan struct{})}
	e.pending[e.minted] = p
	return e.prefix + "-" + strconv.FormatUint(e.minted, 10), p
}

// end stops waiting on TOKEN's evaluation.
func (e *editorEvals) end(token string) {
	if n, ok := e.seq(token); ok {
		e.mu.Lock()
		delete(e.pending, n)
		e.mu.Unlock()
	}
}

// answerResult is what became of an answer.
type answerResult int

const (
	// answerTaken: the evaluation waited on it.
	answerTaken answerResult = iota
	// answerLate: the token is this daemon's, but nothing waits on it now.
	answerLate
	// answerUnknown: this daemon never minted the token, or minted it for
	// another workspace.
	answerUnknown
)

// take hands OUTCOME to TOKEN's evaluation.
func (e *editorEvals) take(token string, ws ids.WorkspaceID, outcome *agentreplv1.EditorEvaluationOutcome) answerResult {
	n, ok := e.seq(token)
	if !ok {
		return answerUnknown
	}
	e.mu.Lock()
	defer e.mu.Unlock()
	if n == 0 || n > e.minted {
		return answerUnknown
	}
	p, waiting := e.pending[n]
	if !waiting {
		return answerLate
	}
	if p.ws != ws {
		return answerUnknown
	}
	delete(e.pending, n)
	p.answer <- outcome
	return answerTaken
}

// editorGone wakes every evaluation waiting on WS's editor.
func (e *editorEvals) editorGone(ws ids.WorkspaceID) {
	e.mu.Lock()
	defer e.mu.Unlock()
	for n, p := range e.pending {
		if p.ws == ws {
			close(p.gone)
			delete(e.pending, n)
		}
	}
}

// seq reads a token's count, false for a token not of this daemon's form.
func (e *editorEvals) seq(token string) (uint64, bool) {
	rest, ok := strings.CutPrefix(token, e.prefix+"-")
	if !ok {
		return 0, false
	}
	n, err := strconv.ParseUint(rest, 10, 64)
	return n, err == nil
}

// EvaluateInEditor pushes elisp to the editor serving the workspace and
// answers with the editor's outcome.
func (s *server) EvaluateInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.EvaluateInEditorRequest],
) (*connect.Response[agentreplv1.EvaluateInEditorResponse], error) {
	const rpc = "EvaluateInEditor"
	if err := validateEvaluateInEditorRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.EvaluateInEditorResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	token, pending := s.evals.open(ws)
	defer s.evals.end(token)
	fields := dlog.Context{"token": token}
	if !s.hostStreamHeld(ws) {
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{Arm: "no_editor", Reason: "no editor holds the workspace's host stream", Info: true})))
	}
	sent := time.Now()
	s.hostEventStream(ws).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_Evaluate{Evaluate: &agentreplv1.HostEvaluate{
			Token: &agentreplv1.EditorEvaluationToken{Value: token}, Text: req.Msg.GetText()}},
	})
	subject.Log.Debug(opEvaluate, "pushed an evaluation to the workspace's editor", fields)
	deadline := time.NewTimer(s.evalDeadline)
	defer deadline.Stop()
	select {
	case outcome := <-pending.answer:
		subject.Log.Info(opEvaluate, "the editor answered an evaluation", dlog.Merge(fields, dlog.Context{
			"latency_ms": time.Since(sent).Milliseconds(), "outcome": evaluationOutcomeName(outcome)}))
		resp.Result = &agentreplv1.EvaluateInEditorResponse_Success{Success: &agentreplv1.EvaluateInEditorSuccess{Outcome: outcome}}
		return connect.NewResponse(resp), nil
	case <-pending.gone:
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{Arm: "no_editor",
			Reason: "the workspace's editor closed its host stream before it answered", Info: true})))
	case <-deadline.C:
		subject.Log.Warn(opEvaluate, "the editor did not answer an evaluation within the deadline", dlog.Merge(fields, dlog.Context{
			"deadline_ms": s.evalDeadline.Milliseconds()}))
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{Arm: "timed_out",
			Reason: fmt.Sprintf("the editor did not answer within %s", s.evalDeadline),
			Fields: map[string]any{"deadline_ms": uint32(s.evalDeadline.Milliseconds())}})))
	case <-ctx.Done():
		subject.Log.Info(opEvaluate, "the caller left before the editor answered", dlog.WithCause(fields, ctx.Err()))
		return nil, connect.NewError(connect.CodeCanceled, ctx.Err())
	}
}

// AnswerEditorEvaluation hands the editor's outcome to the evaluation waiting
// on its token. A late answer is discarded, recorded, and still taken.
func (s *server) AnswerEditorEvaluation(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerEditorEvaluationRequest],
) (*connect.Response[agentreplv1.AnswerEditorEvaluationResponse], error) {
	const rpc = "AnswerEditorEvaluation"
	if err := validateAnswerEditorEvaluationRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AnswerEditorEvaluationResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	token := req.Msg.GetToken().GetValue()
	fields := dlog.Context{"token": token}
	switch s.evals.take(token, subject.Record.ID, req.Msg.GetOutcome()) {
	case answerUnknown:
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{Arm: "unknown_token",
			Reason: fmt.Sprintf("the token %q names no evaluation this daemon pushed to the workspace", token), NotFound: true})))
	case answerLate:
		subject.Log.Info(opEvaluate, "the editor answered an evaluation nobody waits on any more; the answer is discarded", fields)
	}
	resp.Result = &agentreplv1.AnswerEditorEvaluationResponse_Success{Success: &agentreplv1.AnswerEditorEvaluationSuccess{}}
	return connect.NewResponse(resp), nil
}

// evaluationOutcomeName names an outcome's arm for the record.
func evaluationOutcomeName(outcome *agentreplv1.EditorEvaluationOutcome) string {
	switch outcome.GetOutcome().(type) {
	case *agentreplv1.EditorEvaluationOutcome_Value:
		return "value"
	case *agentreplv1.EditorEvaluationOutcome_ReadError:
		return "read_error"
	case *agentreplv1.EditorEvaluationOutcome_EvalError:
		return "eval_error"
	}
	return "unset"
}
