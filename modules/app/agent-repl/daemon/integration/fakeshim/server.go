package main

import (
	"context"
	"encoding/base64"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// hub is a fan-out of pushed frames to every open stream of one family.
type hub[T any] struct {
	mu   sync.Mutex
	next int
	subs map[int]chan T
}

func newHub[T any]() *hub[T] { return &hub[T]{subs: map[int]chan T{}} }

func (h *hub[T]) subscribe() (int, chan T) {
	h.mu.Lock()
	defer h.mu.Unlock()
	id := h.next
	h.next++
	ch := make(chan T, 256)
	h.subs[id] = ch
	return id, ch
}

func (h *hub[T]) unsubscribe(id int) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if ch, ok := h.subs[id]; ok {
		delete(h.subs, id)
		close(ch)
	}
}

func (h *hub[T]) publish(v T) {
	h.mu.Lock()
	defer h.mu.Unlock()
	for _, ch := range h.subs {
		ch <- v
	}
}

// dropAll severs every open stream of the family, which the daemon must read
// as a link failure and redial.
func (h *hub[T]) dropAll() {
	h.mu.Lock()
	defer h.mu.Unlock()
	for id, ch := range h.subs {
		delete(h.subs, id)
		close(ch)
	}
}

func (h *hub[T]) count() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.subs)
}

// agentFrame is one pushed frame addressed to an agent stream.
type agentFrame struct {
	agent string
	frame *conversationv1.AgentFrame
}

// bashFrame is one pushed frame addressed to a detached shell's stream.
type bashFrame struct {
	work string
	bash *conversationv1.AgentBash
}

// server implements shim.v1 against scripted answers and pushed frames.
type server struct {
	rec     *Recorder
	profile Profile
	log     *logSink

	sessions            *hub[*conversationv1.SessionUpdate]
	sessionStreamOpened bool

	agents *hub[agentFrame]
	bashes *hub[bashFrame]

	mu      sync.Mutex
	answers map[string][]scriptedAnswer
	// bashStarts is the ORIGINAL start of each shell the fake has been told
	// about, keyed by the detached-work handle and by the in-turn activity id
	// the work detached from. WatchBash opens with it, exactly as the contract
	// says the stream opens ("`start` — the command, the ORIGINAL instant").
	bashStarts map[string]*conversationv1.AgentBash
	hung       bool
	unhang     chan struct{}
	// sessionKilled records an accepted KillSession: the process exits once
	// its answer has been written.
	sessionKilled bool
	vendorID      string
	// onSessionStarted is called once a vendor session id is assigned, so the
	// process can take the session kernel lock inside StartSession.
	onSessionStarted func(vendorSessionID string)
	// claimWorkspace takes the WORKSPACE kernel lock inside StartSession,
	// before anything else. False means another shim holds this conversation,
	// which is the `conversation_owned` arm.
	claimWorkspace func() bool
	// releaseLocks drops both kernel locks; a kill or a stand-down calls it.
	releaseLocks func()
	exit         func(code int, stderr string)
}

type scriptedAnswer struct {
	msg  proto.Message
	fail string
}

func newServer(rec *Recorder, p Profile, log *logSink) *server {
	return &server{
		rec:        rec,
		profile:    p,
		log:        log,
		sessions:   newHub[*conversationv1.SessionUpdate](),
		agents:     newHub[agentFrame](),
		bashes:     newHub[bashFrame](),
		answers:    map[string][]scriptedAnswer{},
		bashStarts: map[string]*conversationv1.AgentBash{},
		unhang:     make(chan struct{}),
	}
}

// queueAnswer files the next scripted answer for a verb.
func (s *server) queueAnswer(rpc string, msg proto.Message, fail string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.answers[rpc] = append(s.answers[rpc], scriptedAnswer{msg: msg, fail: fail})
}

// popAnswer takes the next scripted answer for a verb, if one is queued.
func (s *server) popAnswer(rpc string) (scriptedAnswer, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	q := s.answers[rpc]
	if len(q) == 0 {
		return scriptedAnswer{}, false
	}
	s.answers[rpc] = q[1:]
	return q[0], true
}

// hang stops the fake answering anything until unhung or killed.
func (s *server) hang() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.hung = true
}

func (s *server) release() {
	s.mu.Lock()
	defer s.mu.Unlock()
	if !s.hung {
		return
	}
	s.hung = false
	close(s.unhang)
	s.unhang = make(chan struct{})
}

// gate blocks while the fake is hung, bounded by the caller's context.
func (s *server) gate(ctx context.Context) error {
	s.mu.Lock()
	hung, ch := s.hung, s.unhang
	s.mu.Unlock()
	if !hung {
		return nil
	}
	select {
	case <-ch:
		return nil
	case <-ctx.Done():
		return connect.NewError(connect.CodeDeadlineExceeded, ctx.Err())
	}
}

// enter records the request and applies the hang gate. Every verb goes
// through it, so `expect` and `hang` work uniformly.
func (s *server) enter(ctx context.Context, rpc string, req proto.Message) error {
	s.rec.Record(rpc, req)
	// THE REQUEST RIDES THE LOG, not only the in-memory recorder. The recorder
	// dies with the process, and the verbs that END the process -- a forced
	// KillSession above all -- can only be asserted after the fact from
	// something that outlives it.
	fields := map[string]any{"verb": rpc}
	if raw, err := proto.Marshal(req); err == nil {
		fields["request"] = base64.StdEncoding.EncodeToString(raw)
	}
	s.log.write(rpc, fields)
	return s.gate(ctx)
}

// scripted answers a verb from the queue when one is scripted; the second
// result reports whether it did.
func scripted[Resp any, PResp interface {
	*Resp
	proto.Message
}](s *server, rpc string) (*connect.Response[Resp], bool, error) {
	a, ok := s.popAnswer(rpc)
	if !ok {
		return nil, false, nil
	}
	if a.fail != "" {
		return nil, true, connect.NewError(connect.CodeInternal, errors.New(a.fail))
	}
	typed, ok := a.msg.(PResp)
	if !ok {
		return nil, true, connect.NewError(connect.CodeInternal, errors.New("fakeshim: scripted answer has the wrong type for "+rpc))
	}
	return connect.NewResponse((*Resp)(typed)), true, nil
}

func (s *server) StartSession(ctx context.Context, req *connect.Request[shimv1.StartSessionRequest]) (*connect.Response[shimv1.StartSessionResponse], error) {
	if err := s.enter(ctx, RPCStartSession, req.Msg); err != nil {
		return nil, err
	}
	if s.profile.ExitOn == "start_session" {
		s.exit(s.profile.ExitCode, s.profile.Stderr)
	}
	// THE WORKSPACE LOCK IS TAKEN HERE, before the SDK would be touched. A
	// conversation another shim owns is refused, never waited on.
	if s.claimWorkspace != nil && !s.claimWorkspace() {
		return connect.NewResponse(&shimv1.StartSessionResponse{
			Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
				Detail: "another shim holds this workspace's conversation",
				Cause: &shimv1.StartSessionFailure_ConversationOwned{
					ConversationOwned: &shimv1.StartSessionConversationOwned{},
				},
			}},
		}), nil
	}
	if resp, done, err := scripted[shimv1.StartSessionResponse, *shimv1.StartSessionResponse](s, RPCStartSession); done {
		if err == nil {
			s.noteVendorSession(resp.Msg)
		}
		return resp, err
	}
	if _, isResume := req.Msg.GetSource().(*shimv1.StartSessionRequest_Resume); isResume && s.profile.ColdOnResume != nil && req.Msg.GetResume().GetColdRemediation() == nil {
		return connect.NewResponse(&shimv1.StartSessionResponse{
			Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
				Detail: "context has gone cold",
				Cause: &shimv1.StartSessionFailure_Cold{Cold: &conversationv1.SessionCold{
					ContextTokens:   s.profile.ColdOnResume.ContextTokens,
					LastRequestAtMs: s.profile.ColdOnResume.LastRequestAtMS,
					RequestedModel:  &conversationv1.AgentModel{Name: s.profile.ColdOnResume.RequestedModel},
					Reason: &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{
						CacheTtlMs: s.profile.ColdOnResume.CacheTTLMS,
					}},
				}},
			}},
		}), nil
	}

	vendorID := s.profile.VendorSessionID
	if r := req.Msg.GetResume(); r != nil {
		vendorID = r.GetVendorSessionId()
	}
	if vendorID == "" {
		vendorID = mintID()
	}
	resp := &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{
			Session: &conversationv1.SessionStarted{
				VendorSessionId: vendorID,
				Runtime:         &conversationv1.SessionRuntime{ShimBuildSha: s.buildSHA()},
				EffectiveModel:  &conversationv1.AgentModel{Name: DefaultModel},
				PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_Default{Default: &conversationv1.AgentPermissionModeDefault{}},
				},
				ModelCatalog: DefaultCatalog(),
				LiveWork:     s.liveWork(),
			},
		}},
	}
	s.noteVendorSession(resp)
	return connect.NewResponse(resp), nil
}

// claimFirstSessionStream reports whether this open is the FIRST session
// stream, which is the one DelayDiagnostics withholds its opening frames from.
func (s *server) claimFirstSessionStream() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.sessionStreamOpened {
		return false
	}
	s.sessionStreamOpened = true
	return true
}

// noteVendorSession takes the session kernel lock the moment a vendor session
// id is assigned, exactly as the real shim does inside StartSession.
func (s *server) noteVendorSession(resp *shimv1.StartSessionResponse) {
	id := resp.GetSuccess().GetSession().GetVendorSessionId()
	if id == "" {
		return
	}
	s.mu.Lock()
	first := s.vendorID == ""
	s.vendorID = id
	hook := s.onSessionStarted
	s.mu.Unlock()
	s.writeTranscript(id)
	if first && hook != nil {
		hook(id)
	}
}

// nonAlphanumeric spells the vendor CLI's projects/<name> encoding rule.
var nonAlphanumeric = regexp.MustCompile(`[^A-Za-z0-9]`)

// writeTranscript creates the conversation's transcript file exactly where the
// vendor CLI files it — $CLAUDE_CONFIG_DIR/projects/<encoded cwd>/<vendor
// session id>.jsonl. THE FILE IS THE RESUME'S DEATH EVIDENCE: the daemon's
// resume guard refuses a resume whose transcript is gone, so a fake that starts
// sessions without laying one down makes every re-open unresumable.
func (s *server) writeTranscript(vendorSessionID string) {
	configDir := os.Getenv("CLAUDE_CONFIG_DIR")
	if configDir == "" {
		return
	}
	cwd, err := os.Getwd()
	if err != nil {
		return
	}
	dir := filepath.Join(configDir, "projects", nonAlphanumeric.ReplaceAllString(cwd, "-"))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return
	}
	path := filepath.Join(dir, vendorSessionID+".jsonl")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return
	}
	defer f.Close()
	_, _ = f.WriteString(`{"type":"session_started","sessionId":"` + vendorSessionID + `"}` + "\n")
}

// liveWork decodes the profile's already-running items. A profile that cannot
// be decoded is a scripting error and dies loudly rather than opening a session
// whose stated membership silently differs from what the test wrote.
func (s *server) liveWork() []*conversationv1.AgentDetachedWork {
	if len(s.profile.LiveWork) == 0 {
		return nil
	}
	out := make([]*conversationv1.AgentDetachedWork, 0, len(s.profile.LiveWork))
	for i, raw := range s.profile.LiveWork {
		item := &conversationv1.AgentDetachedWork{}
		if err := proto.Unmarshal(raw, item); err != nil {
			panic(sprintf("fakeshim: profile live_work[%d] does not decode: %v", i, err))
		}
		out = append(out, item)
	}
	return out
}

func (s *server) buildSHA() string {
	if s.profile.BuildSHA != "" {
		return s.profile.BuildSHA
	}
	return DefaultBuildSHA
}

func (s *server) WatchSession(ctx context.Context, req *connect.Request[shimv1.WatchSessionRequest], stream *connect.ServerStream[shimv1.WatchSessionResponse]) error {
	if err := s.enter(ctx, RPCWatchSession, req.Msg); err != nil {
		return err
	}
	if s.profile.ExitOn == "watch_session" {
		s.exit(s.profile.ExitCode, s.profile.Stderr)
	}
	id, ch := s.sessions.subscribe()
	defer s.sessions.unsubscribe(id)

	if !s.profile.DelayDiagnostics || !s.claimFirstSessionStream() {
		if err := stream.Send(&shimv1.WatchSessionResponse{Update: HealthyDiagnostics()}); err != nil {
			return err
		}
		// The opening context usage rides the same open: the topbar publishes
		// nothing until it holds one, exactly as against the real shim.
		if err := stream.Send(&shimv1.WatchSessionResponse{Update: DefaultContextUsage()}); err != nil {
			return err
		}
	}
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case u, ok := <-ch:
			if !ok {
				// drop_stream severed the link: end without a terminal, which
				// the daemon must read as a connectivity failure.
				return connect.NewError(connect.CodeUnavailable, errors.New("fakeshim: session stream dropped"))
			}
			if err := stream.Send(&shimv1.WatchSessionResponse{Update: u}); err != nil {
				return err
			}
		}
	}
}

func (s *server) WatchAgent(ctx context.Context, req *connect.Request[shimv1.WatchAgentRequest], stream *connect.ServerStream[shimv1.WatchAgentResponse]) error {
	if err := s.enter(ctx, RPCWatchAgent, req.Msg); err != nil {
		return err
	}
	target := req.Msg.GetTarget().GetValue()
	id, ch := s.agents.subscribe()
	defer s.agents.unsubscribe(id)

	if err := stream.Send(&shimv1.WatchAgentResponse{
		Frame: &shimv1.WatchAgentResponse_Page{Page: EmptyFloorPage()},
	}); err != nil {
		return err
	}
	seq := 0
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case f, ok := <-ch:
			if !ok {
				return connect.NewError(connect.CodeUnavailable, errors.New("fakeshim: agent stream dropped"))
			}
			if f.agent != "" && target != "" && f.agent != target {
				continue
			}
			seq++
			if err := stream.Send(&shimv1.WatchAgentResponse{
				Frame: &shimv1.WatchAgentResponse_Entry{Entry: &conversationv1.HistoryEntryAt{
					At:    &conversationv1.HistoryPointer{Value: pointerAt(target, seq)},
					Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: f.frame}},
				}},
			}); err != nil {
				return err
			}
		}
	}
}

func (s *server) WatchBash(ctx context.Context, req *connect.Request[shimv1.WatchBashRequest], stream *connect.ServerStream[shimv1.WatchBashResponse]) error {
	if err := s.enter(ctx, RPCWatchBash, req.Msg); err != nil {
		return err
	}
	work := req.Msg.GetWork().GetValue()
	id, ch := s.bashes.subscribe()
	defer s.bashes.unsubscribe(id)

	// THE OPENING FRAME IS THE CONTRACT'S: a WatchBash stream opens with the
	// shell's `start`. It is also what makes the open observable — the daemon's
	// client takes the first frame as the open's answer — so a stream that
	// sent nothing until the next delta would stall every caller.
	if err := stream.Send(&shimv1.WatchBashResponse{Bash: s.bashStart(work)}); err != nil {
		return err
	}
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case f, ok := <-ch:
			if !ok {
				return connect.NewError(connect.CodeUnavailable, errors.New("fakeshim: bash stream dropped"))
			}
			if f.work != work {
				continue
			}
			if err := stream.Send(&shimv1.WatchBashResponse{Bash: f.bash}); err != nil {
				return err
			}
		}
	}
}

func (s *server) StartTurn(ctx context.Context, req *connect.Request[shimv1.StartTurnRequest]) (*connect.Response[shimv1.StartTurnResponse], error) {
	if err := s.enter(ctx, RPCStartTurn, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.StartTurnResponse, *shimv1.StartTurnResponse](s, RPCStartTurn); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.StartTurnResponse{
		Result: &shimv1.StartTurnResponse_Success{Success: &shimv1.StartTurnSuccess{
			Prompt: &conversationv1.AgentPrompt{
				Id:     req.Msg.GetTurn(),
				Agent:  &conversationv1.AgentId{Value: MainAgentID},
				Said:   req.Msg.GetSaid(),
				Origin: req.Msg.GetOrigin(),
			},
		}},
	}), nil
}

func (s *server) UpdateAgent(ctx context.Context, req *connect.Request[shimv1.UpdateAgentRequest]) (*connect.Response[shimv1.UpdateAgentResponse], error) {
	if err := s.enter(ctx, RPCUpdateAgent, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.UpdateAgentResponse, *shimv1.UpdateAgentResponse](s, RPCUpdateAgent); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.UpdateAgentResponse{
		Result: &shimv1.UpdateAgentResponse_Success{Success: &shimv1.UpdateAgentSuccess{}},
	}), nil
}

func (s *server) KillTurn(ctx context.Context, req *connect.Request[shimv1.KillTurnRequest]) (*connect.Response[shimv1.KillTurnResponse], error) {
	if err := s.enter(ctx, RPCKillTurn, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.KillTurnResponse, *shimv1.KillTurnResponse](s, RPCKillTurn); done {
		return resp, err
	}
	// A KILLED TURN ENDS ON THE STREAM, as the real shim's does: the daemon
	// learns a turn is over from the agent's terminal frame and from nothing
	// else, so a fake that only ANSWERED would leave every waiter on freeness
	// blocked forever.
	s.agents.publish(agentFrame{agent: MainAgentID, frame: &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: MainAgentID},
		Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: &conversationv1.AgentInterruptedByUser{}},
			}},
		}},
	}})
	return connect.NewResponse(&shimv1.KillTurnResponse{
		Result: &shimv1.KillTurnResponse_Success{Success: &shimv1.KillTurnSuccess{
			Killed: &conversationv1.TurnKilled{How: &conversationv1.TurnKilled_AgentOnly{AgentOnly: &conversationv1.TurnKilledAgentOnly{}}},
		}},
	}), nil
}

// killedSession reports that a KillSession was ACCEPTED, which is the process's
// cue to exit once the answer is written.
func (s *server) killedSession() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.sessionKilled
}

func (s *server) KillSession(ctx context.Context, req *connect.Request[shimv1.KillSessionRequest]) (*connect.Response[shimv1.KillSessionResponse], error) {
	if err := s.enter(ctx, RPCKillSession, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.KillSessionResponse, *shimv1.KillSessionResponse](s, RPCKillSession); done {
		if err == nil && resp.Msg.GetSuccess() != nil {
			s.mu.Lock()
			s.sessionKilled = true
			s.mu.Unlock()
		}
		return resp, err
	}
	s.mu.Lock()
	s.sessionKilled = true
	s.mu.Unlock()
	// BOTH LOCKS GO WITH THE SESSION. The process exits right after this
	// answer is written, but releasing them here is what the real shim does
	// and is what a stand-down's successor waits on.
	if s.releaseLocks != nil {
		s.releaseLocks()
	}
	return connect.NewResponse(&shimv1.KillSessionResponse{
		Result: &shimv1.KillSessionResponse_Success{Success: &shimv1.KillSessionSuccess{
			Closed: &conversationv1.SessionKilled{How: &conversationv1.SessionKilled_Idle{Idle: &conversationv1.SessionKilledIdle{}}},
		}},
	}), nil
}

func (s *server) Hibernate(ctx context.Context, req *connect.Request[shimv1.HibernateRequest]) (*connect.Response[shimv1.HibernateResponse], error) {
	if err := s.enter(ctx, RPCHibernate, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.HibernateResponse, *shimv1.HibernateResponse](s, RPCHibernate); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.HibernateResponse{
		Result: &shimv1.HibernateResponse_Success{Success: &shimv1.HibernateSuccess{}},
	}), nil
}

func (s *server) SetSessionModel(ctx context.Context, req *connect.Request[shimv1.SetSessionModelRequest]) (*connect.Response[shimv1.SetSessionModelResponse], error) {
	if err := s.enter(ctx, RPCSetSessionModel, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.SetSessionModelResponse, *shimv1.SetSessionModelResponse](s, RPCSetSessionModel); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.SetSessionModelResponse{
		Result: &shimv1.SetSessionModelResponse_Success{Success: &shimv1.SetSessionModelSuccess{
			ModelChanged: &conversationv1.SessionModelChanged{EffectiveModel: req.Msg.GetModel()},
		}},
	}), nil
}

func (s *server) SetSessionPermissionMode(ctx context.Context, req *connect.Request[shimv1.SetSessionPermissionModeRequest]) (*connect.Response[shimv1.SetSessionPermissionModeResponse], error) {
	if err := s.enter(ctx, RPCSetSessionPermissionMode, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.SetSessionPermissionModeResponse, *shimv1.SetSessionPermissionModeResponse](s, RPCSetSessionPermissionMode); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.SetSessionPermissionModeResponse{
		Result: &shimv1.SetSessionPermissionModeResponse_Success{Success: &shimv1.SetSessionPermissionModeSuccess{}},
	}), nil
}

func (s *server) StopBash(ctx context.Context, req *connect.Request[shimv1.StopBashRequest]) (*connect.Response[shimv1.StopBashResponse], error) {
	if err := s.enter(ctx, RPCStopBash, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.StopBashResponse, *shimv1.StopBashResponse](s, RPCStopBash); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.StopBashResponse{
		Result: &shimv1.StopBashResponse_Success{Success: &shimv1.StopBashSuccess{}},
	}), nil
}

func (s *server) DetachForeground(ctx context.Context, req *connect.Request[shimv1.DetachForegroundRequest]) (*connect.Response[shimv1.DetachForegroundResponse], error) {
	if err := s.enter(ctx, RPCDetachForeground, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.DetachForegroundResponse, *shimv1.DetachForegroundResponse](s, RPCDetachForeground); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.DetachForegroundResponse{
		Result: &shimv1.DetachForegroundResponse_Success{Success: &shimv1.DetachForegroundSuccess{}},
	}), nil
}

func (s *server) ReadHistory(ctx context.Context, req *connect.Request[shimv1.ReadHistoryRequest]) (*connect.Response[shimv1.ReadHistoryResponse], error) {
	if err := s.enter(ctx, RPCReadHistory, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.ReadHistoryResponse, *shimv1.ReadHistoryResponse](s, RPCReadHistory); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.ReadHistoryResponse{
		Result: &shimv1.ReadHistoryResponse_Success{Success: &shimv1.ReadHistorySuccess{Page: EmptyFloorPage()}},
	}), nil
}

// The workflow verbs are kicked this wave: they answer the typed
// not-implemented refusal and open nothing.
func (s *server) GetWorkflow(ctx context.Context, req *connect.Request[shimv1.GetWorkflowRequest]) (*connect.Response[shimv1.GetWorkflowResponse], error) {
	return nil, notImplemented("GetWorkflow")
}

func (s *server) WatchWorkflow(ctx context.Context, req *connect.Request[shimv1.WatchWorkflowRequest], _ *connect.ServerStream[shimv1.WatchWorkflowResponse]) error {
	return notImplemented("WatchWorkflow")
}

func (s *server) StopWorkflow(ctx context.Context, req *connect.Request[shimv1.StopWorkflowRequest]) (*connect.Response[shimv1.StopWorkflowResponse], error) {
	return nil, notImplemented("StopWorkflow")
}

// NotImplementedMessage is the exact refusal text the workflow verbs answer.
const NotImplementedMessage = "intended arm: %sError.not_implemented: workflow is not implemented this wave"

func notImplemented(rpc string) error {
	return connect.NewError(connect.CodeUnimplemented, errors.New(sprintf(NotImplementedMessage, rpc)))
}

// rememberBashStart files a shell's start under a key (its detached-work
// handle, or the in-turn activity id it detached from).
func (s *server) rememberBashStart(key string, bash *conversationv1.AgentBash) {
	if key == "" || bash.GetStart() == nil {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	s.bashStarts[key] = bash
}

// aliasBashStart files an existing start under a second key, which is how a
// `detached`-origin announcement carries an in-turn shell's start onto the
// work handle its stream is addressed by.
func (s *server) aliasBashStart(work, unit string) {
	if work == "" || unit == "" {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if bash, ok := s.bashStarts[unit]; ok {
		s.bashStarts[work] = bash
	}
}

// bashStart answers the opening frame for a work handle: the start the fake
// was told about, or a bare one when the script never stated it.
func (s *server) bashStart(work string) *conversationv1.AgentBash {
	s.mu.Lock()
	defer s.mu.Unlock()
	if bash, ok := s.bashStarts[work]; ok {
		return bash
	}
	s.log.write("watch_bash_bare_start", map[string]any{"work": work})
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{},
			StartedAt: &conversationv1.AgentActivityStartedAt{},
		}},
	}
}

// rememberPushedBash files whatever a pushed agent frame teaches the fake
// about a shell's start: an in-turn bash unit's own start (keyed by its
// activity id), a `created`-origin announcement's start (keyed by the work
// handle), and a `detached`-origin announcement's alias from the unit it
// detached from onto the work handle.
func (s *server) rememberPushedBash(frame *conversationv1.AgentFrame) {
	if act := frame.GetUpdate().GetActivity(); act != nil {
		if bash := act.GetBash(); bash != nil {
			s.rememberBashStart(act.GetActivityId().GetValue(), bash)
		}
	}
	work := frame.GetDetachedWork()
	if work == nil {
		return
	}
	handle := work.GetWork().GetValue()
	if created := work.GetCreated(); created != nil {
		if bash := created.GetWorkCreated().GetBash(); bash != nil {
			s.rememberBashStart(handle, bash)
		}
		return
	}
	if detached := work.GetDetached(); detached != nil {
		s.aliasBashStart(handle, detached.GetDetachedFromId().GetValue())
	}
}
