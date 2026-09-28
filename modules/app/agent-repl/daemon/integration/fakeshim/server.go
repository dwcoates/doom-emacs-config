package main

import (
	"context"
	"encoding/base64"
	"errors"
	"fmt"
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

// agentFrame is one pushed HISTORY ENTRY addressed to an agent stream. The
// entry has two arms and the fake pushes both: an agent frame, and the user
// prompt the vendor lays down when a turn's prompt is delivered. Modelled as
// the two arms rather than as an opaque entry so a caller cannot push an
// entry with no arm set at all.
type agentFrame struct {
	agent  string
	frame  *conversationv1.AgentFrame
	prompt *conversationv1.AgentPrompt
	// pointer, when set, is the HistoryPointer this frame is delivered at
	// instead of the stream's next minted one — see Command.Pointer.
	pointer string
	// turn, when set, stamps the delivered entry — see Command.Turn.
	turn string
	// placeMs, when positive, is the delivered entry's recorded place — see
	// Command.PlaceMs.
	placeMs int64
	// retired, when set, makes this a WatchAgentResponse.retired frame: the
	// entry as last served, at its own pointer. It mints no pointer.
	retired *conversationv1.HistoryEntryAt
}

// stamp is the entry's turn stamp, or nil for an unstamped entry.
func (f agentFrame) stamp() *conversationv1.TurnId {
	if f.turn == "" {
		return nil
	}
	return &conversationv1.TurnId{Value: f.turn}
}

// place applies the pushed entry's recorded place, when one was stated.
func (f agentFrame) place(at *conversationv1.HistoryEntryAt) *conversationv1.HistoryEntryAt {
	if f.placeMs > 0 {
		at.Place = &conversationv1.HistoryEntryAt_RecordedPlace{
			RecordedPlace: &conversationv1.ConversationPlace{AtMs: f.placeMs},
		}
	}
	return at
}

// entry renders the pushed arm as the history entry WatchAgent delivers.
func (f agentFrame) entry() *conversationv1.HistoryEntry {
	if f.prompt != nil {
		return &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: f.prompt},
		}
	}
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: f.frame},
	}
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

	// bashMu serializes a bash push against a WatchBash subscription, which is
	// what makes a pushed frame IMPOSSIBLE to lose. A test pushes as soon as
	// the daemon has drawn the shell's head row, but the daemon opens the
	// WatchBash stream on its own goroutine: without this seam the push can
	// land while the hub has no subscriber for the work handle at all, and the
	// frame is dropped rather than delayed. Under GOMAXPROCS=1 that is the
	// ordinary outcome, not a rare one.
	bashMu sync.Mutex
	// bashLog is every frame pushed for a work handle, in publication order.
	// A WatchBash stream replays it from the snapshot taken atomically with
	// its own subscription, so each frame is delivered exactly once whether it
	// was pushed before the stream opened or after.
	bashLog map[string][]*conversationv1.AgentBash
	// bashEndings is HOW EACH RUN ENDED, once it has: the terminal frame that
	// was published for the handle.
	//
	// IT SURVIVES A STREAM DROP, unlike the log, because a redial does not
	// un-end a run. The real shim answers WatchBash out of its store, which
	// holds every row the run ever wrote, so a watch opened after the run
	// finished is handed the ending; a fake that answered a bare `start`
	// instead would report a finished command as one still going, and no test
	// here could see the difference.
	bashEndings map[string]*conversationv1.AgentBash

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
	// openPermissions is the agent and the gated call of each permission ask
	// the fake has been told to open, keyed by the ask's id. UpdateAgent
	// settles the ask off it: the daemon learns a permission was decided from
	// the SETTLE FRAME on the agent's stream and from nothing else, exactly as
	// it learns a killed turn ended from the terminal frame, so a fake that
	// only answered the rpc would leave the card open forever.
	openPermissions map[string]openPermission
	hung            bool
	unhang          chan struct{}
	// sessionKilled records an accepted KillSession: the process exits once
	// its answer has been written.
	sessionKilled bool
	// resumed records that this session was started as a RESUME, which is what
	// makes the profile's recorded conversation the opening page.
	resumed  bool
	vendorID string
	// started is the SessionStarted this fake last answered. Every new session
	// watch RE-ANNOUNCES it right after the opening diagnostics (landing 7),
	// which is what lets an adopting daemon attach purely.
	started *conversationv1.SessionStarted
	// turnInFlight is the turn a StartTurn opened and no main-agent terminal
	// has ended yet. The re-announcement states it as turn_in_flight, as the
	// real shim's reannounceStart does, so an adopting daemon learns from the
	// shim which of its open turn rows are still running.
	turnInFlight *conversationv1.TurnId
	// liveNow, once set_live_work has stated it, is the live membership every
	// later re-announcement states, as the real shim's reannounceStart
	// recomputes it; until then the re-announcement states what StartSession
	// answered with.
	liveNow    []*conversationv1.AgentDetachedWork
	liveNowSet bool
	// silencedBash are the detached-work handles whose WatchBash opens the
	// fake never answers: no opening frame, ever, until the caller gives up.
	// It is the real shim's WatchBash on a run the store holds no row for,
	// which waits for a first row that never comes.
	silencedBash map[string]bool
	// silentReannouncements counts the next WatchSession opens that send no
	// SessionStarted re-announcement.
	silentReannouncements int
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
		rec:             rec,
		profile:         p,
		log:             log,
		sessions:        newHub[*conversationv1.SessionUpdate](),
		agents:          newHub[agentFrame](),
		bashes:          newHub[bashFrame](),
		answers:         map[string][]scriptedAnswer{},
		bashLog:         map[string][]*conversationv1.AgentBash{},
		bashEndings:     map[string]*conversationv1.AgentBash{},
		bashStarts:      map[string]*conversationv1.AgentBash{},
		openPermissions: map[string]openPermission{},
		silencedBash:    map[string]bool{},
		unhang:          make(chan struct{}),
	}
}

// setLiveWork states the live membership every later re-announcement carries.
func (s *server) setLiveWork(live []*conversationv1.AgentDetachedWork) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.liveNow = live
	s.liveNowSet = true
}

// silenceBash makes every later WatchBash open for work go unanswered.
func (s *server) silenceBash(work string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.silencedBash[work] = true
}

// silenceReannouncement makes the next WatchSession open re-announce nothing.
func (s *server) silenceReannouncement() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.silentReannouncements++
}

// reannouncementSilenced takes one silenced re-announcement, reporting whether
// there was one to take.
func (s *server) reannouncementSilenced() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.silentReannouncements == 0 {
		return false
	}
	s.silentReannouncements--
	return true
}

// bashSilenced reports whether WatchBash opens for work go unanswered.
func (s *server) bashSilenced(work string) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.silencedBash[work]
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
	if s.profile.HangStartSession {
		<-ctx.Done()
		return nil, connect.NewError(connect.CodeDeadlineExceeded, ctx.Err())
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
	if detail := s.profile.VendorStartFailed; detail != "" {
		return connect.NewResponse(&shimv1.StartSessionResponse{
			Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
				Detail: detail,
				Cause: &shimv1.StartSessionFailure_VendorStartFailed{
					VendorStartFailed: &shimv1.StartSessionVendorStartFailed{},
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

	if r := req.Msg.GetResume(); r != nil && !s.hasTranscript(r.GetVendorSessionId()) {
		// THE REAL SHIM'S OWN REFUSAL. A resume names a conversation the
		// vendor can only continue from its transcript, so a missing file is
		// `unknown_session` and never a started session.
		return connect.NewResponse(&shimv1.StartSessionResponse{
			Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
				Detail: "no transcript exists for the named conversation",
				Cause: &shimv1.StartSessionFailure_UnknownSession{
					UnknownSession: &shimv1.StartSessionUnknownSession{},
				},
			}},
		}), nil
	}

	vendorID := s.profile.VendorSessionID
	if r := req.Msg.GetResume(); r != nil {
		vendorID = r.GetVendorSessionId()
		// A RESUMED SESSION HAS A BOOK. The real shim answers every watch off
		// the store, so a resume's opening page is the conversation and a
		// fresh start's is an empty floor; the fake keeps that difference so a
		// test asserting rehydrated rows is asserting the RESUME.
		s.mu.Lock()
		s.resumed = true
		s.mu.Unlock()
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
				// THE REAL SHIM'S UNSTATED MODE, which is `auto` (owner
				// ruling 2026-09-14) and no longer the vendor's `default`.
				PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_Auto{Auto: &conversationv1.AgentPermissionModeAuto{}},
				},
				ModelCatalog: DefaultCatalog(),
				LiveWork:     s.liveWork(),
			},
		}},
	}
	s.noteVendorSession(resp)
	return connect.NewResponse(resp), nil
}

// startedSession answers the SessionStarted this fake last announced, nil
// before any session has started, stating the turn in flight RIGHT NOW rather
// than the one StartSession answered with.
func (s *server) startedSession() *conversationv1.SessionStarted {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.started == nil {
		return nil
	}
	started := proto.Clone(s.started).(*conversationv1.SessionStarted)
	started.TurnInFlight = s.turnInFlight
	if s.liveNowSet {
		started.LiveWork = make([]*conversationv1.AgentDetachedWork, 0, len(s.liveNow))
		for _, item := range s.liveNow {
			started.LiveWork = append(started.LiveWork, proto.Clone(item).(*conversationv1.AgentDetachedWork))
		}
	}
	return started
}

// openTurn records the turn a successful StartTurn opened.
func (s *server) openTurn(turn *conversationv1.TurnId) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.turnInFlight = turn
}

// settleTurn ends the turn in flight when a pushed frame is the MAIN agent's
// terminal. A subagent's terminal, or any other frame, leaves it standing.
func (s *server) settleTurn(agent string, frame *conversationv1.AgentFrame) {
	if agent != MainAgentID {
		return
	}
	switch frame.GetResult().(type) {
	case *conversationv1.AgentFrame_Success, *conversationv1.AgentFrame_Failure:
		s.mu.Lock()
		defer s.mu.Unlock()
		s.turnInFlight = nil
	}
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
	s.started = resp.GetSuccess().GetSession()
	hook := s.onSessionStarted
	s.mu.Unlock()
	if !s.profile.NoTranscriptUntilTurn {
		s.writeTranscript(id)
	}
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
	path := transcriptPath(vendorSessionID)
	if path == "" {
		return
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return
	}
	defer f.Close()
	_, _ = f.WriteString(`{"type":"session_started","sessionId":"` + vendorSessionID + `"}` + "\n")
}

// transcriptPath renders where the vendor CLI files one conversation, empty
// when no account root is routed at all.
func transcriptPath(vendorSessionID string) string {
	configDir := os.Getenv("CLAUDE_CONFIG_DIR")
	if configDir == "" {
		return ""
	}
	cwd, err := os.Getwd()
	if err != nil {
		return ""
	}
	return filepath.Join(configDir, "projects", nonAlphanumeric.ReplaceAllString(cwd, "-"), vendorSessionID+".jsonl")
}

// hasTranscript reports whether the named conversation has a transcript on
// disk. A shim with NO routed account root can state nothing about the file, so
// it does not refuse on its absence.
func (s *server) hasTranscript(vendorSessionID string) bool {
	path := transcriptPath(vendorSessionID)
	if path == "" {
		return true
	}
	_, err := os.Stat(path)
	return err == nil
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

// openingPage is what a WatchAgent stream opens with: the resumed
// conversation's own book, or an empty floor when there is nothing to serve.
func (s *server) openingPage() *conversationv1.HistoryPage {
	s.mu.Lock()
	resumed := s.resumed
	s.mu.Unlock()
	if !resumed || len(s.profile.ResumeHistory) == 0 {
		return EmptyFloorPage()
	}
	page := EmptyFloorPage()
	for i, raw := range s.profile.ResumeHistory {
		entry := &conversationv1.HistoryEntry{}
		if err := proto.Unmarshal(raw, entry); err != nil {
			panic(sprintf("fakeshim: profile resume_history[%d] does not decode: %v", i, err))
		}
		page.Entries = append(page.Entries, &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: sprintf("resume-%d", i)},
			Entry: entry,
		})
	}
	return page
}

// buildSHA is the build this process reports: a profile's override, else the
// build its spawner stated in SHIM_BUILD_SHA — exactly what the real shim
// reports — else the default.
func (s *server) buildSHA() string {
	if s.profile.BuildSHA != "" {
		return s.profile.BuildSHA
	}
	if spawned := os.Getenv("SHIM_BUILD_SHA"); spawned != "" {
		return spawned
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
		if err := stream.Send(&shimv1.WatchSessionResponse{
			Frame: &shimv1.WatchSessionResponse_Update{Update: s.openingDiagnostics()},
		}); err != nil {
			return err
		}
		// The opening context usage rides the same open: the topbar publishes
		// nothing until it holds one, exactly as against the real shim.
		if err := stream.Send(&shimv1.WatchSessionResponse{
			Frame: &shimv1.WatchSessionResponse_Update{Update: DefaultContextUsage()},
		}); err != nil {
			return err
		}
		// THE RE-ANNOUNCEMENT (landing 7): the session's original
		// SessionStarted, once per watch, right after the opening
		// diagnostics — on EVERY new watch, so a daemon that adopts an
		// already-started shim learns the facts from the shim.
		if started := s.startedSession(); started != nil && !s.reannouncementSilenced() {
			if err := stream.Send(&shimv1.WatchSessionResponse{
				Frame: &shimv1.WatchSessionResponse_SessionStarted{SessionStarted: started},
			}); err != nil {
				return err
			}
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
			if err := stream.Send(&shimv1.WatchSessionResponse{
				Frame: &shimv1.WatchSessionResponse_Update{Update: u},
			}); err != nil {
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
		Frame: &shimv1.WatchAgentResponse_Page{Page: s.openingPage()},
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
			if f.retired != nil {
				if err := stream.Send(&shimv1.WatchAgentResponse{
					Frame: &shimv1.WatchAgentResponse_Retired{Retired: f.retired},
				}); err != nil {
					return err
				}
				continue
			}
			seq++
			at := f.pointer
			if at == "" {
				at = pointerAt(target, seq)
			}
			if err := stream.Send(&shimv1.WatchAgentResponse{
				Frame: &shimv1.WatchAgentResponse_Entry{Entry: f.place(&conversationv1.HistoryEntryAt{
					At:    &conversationv1.HistoryPointer{Value: at},
					Entry: f.entry(),
					Turn:  f.stamp(),
				})},
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
	if s.bashSilenced(work) {
		// NO OPENING FRAME, EVER: the open is held until the caller gives up.
		<-ctx.Done()
		return ctx.Err()
	}
	id, ch, backlog := s.subscribeBash(work)
	defer s.bashes.unsubscribe(id)

	// THE OPENING FRAME IS THE CONTRACT'S: a WatchBash stream opens with the
	// shell's `start`. It is also what makes the open observable — the daemon's
	// client takes the first frame as the open's answer — so a stream that
	// sent nothing until the next delta would stall every caller.
	if err := stream.Send(&shimv1.WatchBashResponse{Bash: s.bashStart(work)}); err != nil {
		return err
	}
	// THE BACKLOG COMES FIRST, in publication order: it is the frames that
	// were pushed before this subscription existed. Nothing in it can also
	// arrive on the channel -- the snapshot and the subscription were taken
	// under one lock -- so no frame is sent twice.
	for _, bash := range backlog {
		if err := stream.Send(&shimv1.WatchBashResponse{Bash: bash}); err != nil {
			return err
		}
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
	// THE FIRST TURN IS WHAT LAYS THE TRANSCRIPT DOWN under
	// NoTranscriptUntilTurn, exactly as the vendor does.
	s.mu.Lock()
	id := s.vendorID
	s.mu.Unlock()
	if s.profile.NoTranscriptUntilTurn && id != "" {
		s.writeTranscript(id)
	}
	if resp, done, err := scripted[shimv1.StartTurnResponse, *shimv1.StartTurnResponse](s, RPCStartTurn); done {
		if err == nil && resp.Msg.GetSuccess() != nil {
			s.openTurn(req.Msg.GetTurn())
		}
		return resp, err
	}
	s.openTurn(req.Msg.GetTurn())
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
	// A PERMISSION DECISION SETTLES ON THE STREAM, as the real shim's does:
	// the daemon draws the answered card from the ask's own settle frame and
	// from nothing else, so a fake that only ANSWERED would leave every card
	// the user decided drawn as open forever.
	if decision := req.Msg.GetInput().GetAnswer().GetPermissionDecision(); decision != nil {
		s.settlePermission(decision)
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
	// blocked forever. And the turn is no longer in flight to any later
	// re-announcement.
	terminal := &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: MainAgentID},
		Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: &conversationv1.AgentInterruptedByUser{}},
			}},
		}},
	}
	s.settleTurn(MainAgentID, terminal)
	s.agents.publish(agentFrame{agent: MainAgentID, frame: terminal})
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
	// The profile's standing refusals, in force from the fake's birth. A
	// scripted answer still wins: it is the narrower instruction.
	if s.profile.HibernateFailure != "" {
		return nil, connect.NewError(connect.CodeInternal, errors.New(s.profile.HibernateFailure))
	}
	if s.profile.HibernateTurnInFlight {
		return connect.NewResponse(&shimv1.HibernateResponse{
			Result: &shimv1.HibernateResponse_Error{Error: &shimv1.HibernateError{
				Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}},
			}},
		}), nil
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
	// THE FAKE JUDGES THE CALLER'S THRESHOLD THE WAY THE REAL SHIM DOES: a
	// model switch is a cold cache, and the shim refuses `cold` when the
	// context is STRICTLY ABOVE the threshold the request stated. A fake that
	// always succeeded would pass a daemon that stated no threshold at all —
	// which is exactly the defect that broke the model cell against the real
	// shim, where an unset field reads as zero and refuses every switch.
	if req.Msg.GetModel().GetName() != DefaultModel &&
		DefaultColdContextTokens > req.Msg.GetColdThresholdTokens() &&
		req.Msg.ColdRemediation == nil {
		return connect.NewResponse(&shimv1.SetSessionModelResponse{
			Result: &shimv1.SetSessionModelResponse_Failure{Failure: &shimv1.SetSessionModelFailure{
				Detail: fmt.Sprintf("switching to %q discards a %d-token warm cache",
					req.Msg.GetModel().GetName(), DefaultColdContextTokens),
				Cause: &shimv1.SetSessionModelFailure_Cold{Cold: &conversationv1.SessionCold{
					ContextTokens:  DefaultColdContextTokens,
					RequestedModel: req.Msg.GetModel(),
					Reason: &conversationv1.SessionCold_ModelSwitch{
						ModelSwitch: &conversationv1.SessionColdModelSwitch{},
					},
				}},
			}},
		}), nil
	}
	// The success body carries the change; the AUTHORITATIVE statement is the
	// `model_changed` push on the session stream, which a test states through
	// ShimControl.PushSessionUpdate. Publishing it from here would hide the
	// daemon invariant that the topbar moves on the PUSH and never on this
	// response body.
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

// GatherTitleDigest answers an EMPTY digest by default: boundary NONE with no
// prompts. The daemon's title synthesizer skips synthesis when the digest has
// nothing to summarize, so a fake with no scripted answer never provokes a
// headless call (which the vendor guard would refuse in a test anyway).
func (s *server) GatherTitleDigest(ctx context.Context, req *connect.Request[shimv1.GatherTitleDigestRequest]) (*connect.Response[shimv1.GatherTitleDigestResponse], error) {
	if err := s.enter(ctx, RPCGatherTitleDigest, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.GatherTitleDigestResponse, *shimv1.GatherTitleDigestResponse](s, RPCGatherTitleDigest); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.GatherTitleDigestResponse{
		Result: &shimv1.GatherTitleDigestResponse_Success{
			Success: &shimv1.GatherTitleDigestSuccess{Boundary: shimv1.TitleDigestBoundary_TITLE_DIGEST_BOUNDARY_NONE},
		},
	}), nil
}

// ReadTranscripts answers an EMPTY list by default: a directory holding no
// conversations is a success, and a fake with no scripted answer states the
// one thing that is certainly true of a workspace nothing has run in.
func (s *server) ReadTranscripts(ctx context.Context, req *connect.Request[shimv1.ReadTranscriptsRequest]) (*connect.Response[shimv1.ReadTranscriptsResponse], error) {
	if err := s.enter(ctx, RPCReadTranscripts, req.Msg); err != nil {
		return nil, err
	}
	if resp, done, err := scripted[shimv1.ReadTranscriptsResponse, *shimv1.ReadTranscriptsResponse](s, RPCReadTranscripts); done {
		return resp, err
	}
	return connect.NewResponse(&shimv1.ReadTranscriptsResponse{
		Result: &shimv1.ReadTranscriptsResponse_Success{Success: &shimv1.ReadTranscriptsSuccess{}},
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

// publishBash records a pushed frame in the work handle's log and hands it to
// every live stream, under the one lock a subscription also takes. It answers
// how many bash streams are open, which is what the control reply reports.
func (s *server) publishBash(work string, bash *conversationv1.AgentBash) int {
	s.bashMu.Lock()
	defer s.bashMu.Unlock()
	s.bashLog[work] = append(s.bashLog[work], bash)
	if bashEnds(bash) {
		s.bashEndings[work] = bash
	}
	s.bashes.publish(bashFrame{work: work, bash: bash})
	return s.bashes.count()
}

// subscribeBash opens a bash subscription together with the snapshot of what
// was published to the work handle before it existed. Taking both under
// bashMu is the whole guarantee: a frame is in the snapshot or on the channel,
// never in neither and never in both.
func (s *server) subscribeBash(work string) (int, chan bashFrame, []*conversationv1.AgentBash) {
	s.bashMu.Lock()
	defer s.bashMu.Unlock()
	id, ch := s.bashes.subscribe()
	backlog := append([]*conversationv1.AgentBash(nil), s.bashLog[work]...)
	// A RUN THAT HAS ENDED IS HANDED ITS ENDING, whatever the log still holds.
	// The log is severed on a redial and the ending is not, so this is the one
	// thing that keeps a reopened watch from reporting a finished command as
	// one still going -- which is what the real shim's store-backed replay
	// does. Appended only when the backlog does not already carry it, so no
	// frame is ever sent twice.
	if ending, ok := s.bashEndings[work]; ok && !endsWithTerminal(backlog) {
		backlog = append(backlog, ending)
	}
	return id, ch, backlog
}

// bashEnds reports whether a frame is a run's TERMINAL: the two arms that
// conclude it, and no other.
func bashEnds(bash *conversationv1.AgentBash) bool {
	return bash.GetSuccess() != nil || bash.GetFailure() != nil
}

// endsWithTerminal reports whether a backlog already concludes the run.
func endsWithTerminal(backlog []*conversationv1.AgentBash) bool {
	for _, bash := range backlog {
		if bashEnds(bash) {
			return true
		}
	}
	return false
}

// dropBashStreams severs every open bash stream AND forgets what was pushed
// to them. A redial is a fresh observer of a shell that has kept running, not
// a replay of the frames the severed stream already carried: handing it the
// log again would feed the daemon the same deltas twice and read as a spool
// gap.
//
// THE ENDINGS ARE NOT FORGOTTEN. A run that finished stays finished across a
// redial, and the real shim's store says so to every watch opened afterwards;
// dropping that here would make the fake report an ended command as live.
func (s *server) dropBashStreams() {
	s.bashMu.Lock()
	defer s.bashMu.Unlock()
	s.bashLog = map[string][]*conversationv1.AgentBash{}
	s.bashes.dropAll()
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

// openPermission is what the fake remembers about one open ask, so it can
// compose the settle frame the decision produces.
type openPermission struct {
	agent string
	gated *conversationv1.AgentActivityId
}

// rememberPushedPermission files an ask the fake was told to open, and forgets
// one whose settle frame was pushed by the test itself.
func (s *server) rememberPushedPermission(agent string, frame *conversationv1.AgentFrame) {
	p := frame.GetUpdate().GetPermission()
	if p == nil || p.GetId().GetValue() == "" {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if p.GetStart() == nil {
		delete(s.openPermissions, p.GetId().GetValue())
		return
	}
	s.openPermissions[p.GetId().GetValue()] = openPermission{agent: agent, gated: p.GetGatedCall()}
}

// settlePermission composes and publishes the settle frame for one decision,
// and reports whether there was an open ask to settle.
func (s *server) settlePermission(decision *conversationv1.AgentPermissionDecision) bool {
	ask := decision.GetAsk().GetValue()
	if ask == "" {
		return false
	}
	s.mu.Lock()
	open, known := s.openPermissions[ask]
	if known {
		delete(s.openPermissions, ask)
	}
	s.mu.Unlock()
	if !known {
		return false
	}

	success := &conversationv1.AgentPermissionSuccess{}
	switch d := decision.GetDecision().(type) {
	case *conversationv1.AgentPermissionDecision_Allowed:
		success.Decision = &conversationv1.AgentPermissionSuccess_Allowed{Allowed: d.Allowed}
	case *conversationv1.AgentPermissionDecision_Denied:
		success.Decision = &conversationv1.AgentPermissionSuccess_Denied{
			Denied: &conversationv1.AgentPermissionDenied{
				By: &conversationv1.AgentPermissionDenied_User{User: d.Denied},
			}}
	default:
		return false
	}
	agent := open.agent
	if agent == "" {
		agent = MainAgentID
	}
	s.agents.publish(agentFrame{agent: agent, frame: &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
				Id:        decision.GetAsk(),
				GatedCall: open.gated,
				Result:    &conversationv1.AgentPermission_Success{Success: success},
			}},
		}},
	}})
	return true
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

// openingDiagnostics is the health verdict every session stream opens with.
// A profile that states an OpeningFault makes it unhealthy on every stream,
// which is what the shim standing on a fault it never clears does.
func (s *server) openingDiagnostics() *conversationv1.SessionUpdate {
	opening := HealthyDiagnostics()
	if s.profile.OpeningFault != "" {
		opening = UnhealthyDiagnostics(s.profile.OpeningFault)
	}
	// The opening frame states the build this process runs, as every real
	// shim's diagnostics frame does.
	opening.GetDiagnostics().ShimBuild = s.buildSHA()
	return opening
}
