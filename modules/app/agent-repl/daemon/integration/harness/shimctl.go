package harness

import (
	"bufio"
	"context"
	"crypto/md5"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"net"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/grantedclock"

	"google.golang.org/protobuf/proto"
)

// ShimProfile is the fake shim's startup script for one workspace. It mirrors
// the fake's own Profile; the harness keeps its own copy so a test never
// imports a `main` package.
type ShimProfile struct {
	BuildSHA         string `json:"build_sha,omitempty"`
	DelayDiagnostics bool   `json:"delay_diagnostics,omitempty"`
	// OpeningFault makes the opening diagnostics push of every session stream
	// unhealthy, carrying one store_unreachable fault with this detail.
	OpeningFault string `json:"opening_fault,omitempty"`
	// OpeningNetworkUnreachable makes the opening diagnostics push of every
	// session stream unhealthy with one network_unreachable fault carrying
	// this detail: the shim saw the network unreachable.
	OpeningNetworkUnreachable string         `json:"opening_network_unreachable,omitempty"`
	ExitOn                    string         `json:"exit_on,omitempty"`
	ExitCode                  int            `json:"exit_code,omitempty"`
	Stderr                    string         `json:"stderr,omitempty"`
	ColdOnResume              *ShimColdFacts `json:"cold_on_resume,omitempty"`
	// VendorStartFailed answers every StartSession with the shim's
	// `vendor_start_failed` refusal carrying this detail: the shim process is
	// healthy and only the vendor failed to start inside it.
	VendorStartFailed string `json:"vendor_start_failed,omitempty"`
	// VendorStartRetryable labels the VendorStartFailed refusal RETRYABLE
	// (shim.v1 StartSessionVendorStartFailed.retry); unset labels it REJECTED.
	VendorStartRetryable bool `json:"vendor_start_retryable,omitempty"`
	// VendorStartFailTimes answers the FIRST n StartSessions with a RETRYABLE
	// `vendor_start_failed` refusal carrying VendorStartFailDetail, and every
	// one after them normally, as a vendor that was slow to start does. It
	// drives the daemon's vendor-start retry run end to end.
	VendorStartFailTimes  int    `json:"vendor_start_fail_times,omitempty"`
	VendorStartFailDetail string `json:"vendor_start_fail_detail,omitempty"`
	// VendorStartRejectTimes answers the FIRST n StartSessions with a REJECTED
	// (non-retryable) `vendor_start_failed` refusal carrying
	// VendorStartFailDetail, and every one after them normally: a start that
	// fails outright and a later start on the same shim that succeeds.
	VendorStartRejectTimes int `json:"vendor_start_reject_times,omitempty"`
	// HibernateFailure makes every Hibernate answer a transport-level error
	// with this detail; HibernateTurnInFlight makes every one answer the
	// typed turn_in_flight refusal. Both are in force from the fake's birth,
	// which a scripted answer filed after the daemon's first idle sweep is
	// not. See the fake's own Profile for why that matters.
	HibernateFailure      string `json:"hibernate_failure,omitempty"`
	HibernateTurnInFlight bool   `json:"hibernate_turn_in_flight,omitempty"`
	// HangStartSession makes the fake never answer StartSession, waiting out
	// the caller's context instead. It is what pins a boot's own bring-up
	// open on a workspace whose shim never answers.
	HangStartSession bool `json:"hang_start_session,omitempty"`
	// NoTranscriptUntilTurn withholds the transcript until the first turn, the
	// way the vendor does: a session bounced before its first turn names a
	// conversation with no transcript at all.
	NoTranscriptUntilTurn bool   `json:"no_transcript_until_turn,omitempty"`
	VendorSessionID       string `json:"vendor_session_id,omitempty"`
	// LiveWork is what the fake's SessionStarted states is already running,
	// each element one binary-encoded conversation.v1 AgentDetachedWork.
	LiveWork [][]byte `json:"live_work,omitempty"`
	// ResumeHistory is the conversation a RESUMED session already holds, each
	// element one binary-encoded conversation.v1 HistoryEntry stated NEWEST
	// FIRST. They seed the main agent's book, which the daemon reads through
	// ReadHistory when a reader opens the feed.
	ResumeHistory [][]byte `json:"resume_history,omitempty"`
	// ResumeTurns stamps ResumeHistory's entries with the turn each belongs
	// to, at the same index (newest first); absent or "" leaves an entry
	// unstamped, as pre-contract data is.
	ResumeTurns []string `json:"resume_turns,omitempty"`
	// HistoryPageSize is the fake store's page size in entries; zero is
	// FeedPageSize. ReadHistory pages each agent's book in it.
	HistoryPageSize int `json:"history_page_size,omitempty"`
}

// EncodeHistory renders history entries for a ShimProfile's ResumeHistory
// field. They are stated NEWEST FIRST, exactly as a producer serves a page.
func EncodeHistory(t testing.TB, newestFirst ...*conversationv1.HistoryEntry) [][]byte {
	t.Helper()
	out := make([][]byte, 0, len(newestFirst))
	for _, entry := range newestFirst {
		raw, err := proto.Marshal(entry)
		if err != nil {
			t.Fatalf("harness: encode a history entry: %v", err)
		}
		out = append(out, raw)
	}
	return out
}

// EncodeLiveWork renders detached-work announcements for a ShimProfile's
// LiveWork field.
func EncodeLiveWork(t testing.TB, items ...*conversationv1.AgentDetachedWork) [][]byte {
	t.Helper()
	out := make([][]byte, 0, len(items))
	for _, item := range items {
		raw, err := proto.Marshal(item)
		if err != nil {
			t.Fatalf("harness: encode live work: %v", err)
		}
		out = append(out, raw)
	}
	return out
}

// ShimColdFacts are the facts a scripted cold refusal states.
type ShimColdFacts struct {
	ContextTokens   uint64 `json:"context_tokens"`
	LastRequestAtMS int64  `json:"last_request_at_ms"`
	RequestedModel  string `json:"requested_model"`
	CacheTTLMS      int64  `json:"cache_ttl_ms"`
}

// The moments ExitOn recognizes.
const (
	ExitOnStartup      = "startup"
	ExitOnStartSession = "start_session"
	ExitOnWatchSession = "watch_session"
)

// The shim.v1 verb names the control protocol addresses.
const (
	RPCStartSession             = "StartSession"
	RPCWatchSession             = "WatchSession"
	RPCSetSessionModel          = "SetSessionModel"
	RPCSetSessionPermissionMode = "SetSessionPermissionMode"
	RPCSetSessionEffort         = "SetSessionEffort"
	RPCHibernate                = "Hibernate"
	RPCSendKeepalive            = "SendKeepalive"
	RPCKillSession              = "KillSession"
	RPCStartTurn                = "StartTurn"
	RPCWatchAgent               = "WatchAgent"
	RPCUpdateAgent              = "UpdateAgent"
	RPCKillTurn                 = "KillTurn"
	RPCWatchBash                = "WatchBash"
	RPCStopBash                 = "StopBash"
	RPCDetachForeground         = "DetachForeground"
	RPCReadTranscripts          = "ReadTranscripts"
	RPCGetForkPoint             = "GetForkPoint"
	RPCForkSession              = "ForkSession"
)

// Stream families drop_stream severs.
const (
	StreamSession = "session"
	StreamAgent   = "agent"
	StreamBash    = "bash"
)

// ShimInfo reports the fake's spawn facts.
type ShimInfo struct {
	Argv            []string          `json:"argv"`
	Env             map[string]string `json:"env"`
	Cwd             string            `json:"cwd"`
	PID             int               `json:"pid"`
	Listen          string            `json:"listen"`
	StoreSocket     string            `json:"store_socket"`
	LogFD           int               `json:"log_fd"`
	Fake            bool              `json:"fake"`
	WorkspaceLock   string            `json:"workspace_lock"`
	SessionLock     string            `json:"session_lock"`
	VendorSessionID string            `json:"vendor_session_id"`
}

// Flag answers the value of a flag in the recorded argv, and whether it was
// present at all — which is how a test asserts the spawn contract.
func (i ShimInfo) Flag(name string) (string, bool) {
	for idx, a := range i.Argv {
		if a == name {
			if idx+1 < len(i.Argv) {
				return i.Argv[idx+1], true
			}
			return "", true
		}
	}
	return "", false
}

// HasFlag reports whether a bare flag was passed.
func (i ShimInfo) HasFlag(name string) bool {
	for _, a := range i.Argv {
		if a == name {
			return true
		}
	}
	return false
}

// controlCommand is one line of the fake's control protocol.
type controlCommand struct {
	Op        string `json:"op"`
	RPC       string `json:"rpc,omitempty"`
	Agent     string `json:"agent,omitempty"`
	Pointer   string `json:"pointer,omitempty"`
	Turn      string `json:"turn,omitempty"`
	PlaceMs   int64  `json:"place_ms,omitempty"`
	Work      string `json:"work,omitempty"`
	Stream    string `json:"stream,omitempty"`
	Code      int    `json:"code,omitempty"`
	Payload   string `json:"payload,omitempty"`
	Fail      string `json:"fail,omitempty"`
	Stderr    string `json:"stderr,omitempty"`
	TimeoutMS int    `json:"timeout_ms,omitempty"`
}

type controlReply struct {
	OK      bool      `json:"ok"`
	Error   string    `json:"error,omitempty"`
	Payload string    `json:"payload,omitempty"`
	Count   int       `json:"count,omitempty"`
	Info    *ShimInfo `json:"info,omitempty"`
}

// ShimControl scripts one workspace's fake shim.
type ShimControl struct {
	// Socket is the control listener's path.
	Socket string

	t   testing.TB
	d   *Daemon
	mu  sync.Mutex
	c   net.Conn
	dec *bufio.Scanner
	enc *json.Encoder
	pid int
	// departed is a shim that exited before the harness connected
	// (OpenedShim): the pid is known, and no command can reach it.
	departed bool
}

// Shim answers the control client for a workspace's fake shim, waiting for the
// fake to bind its control listener.
func (d *Daemon) Shim(ws *workspacev1.WorkspaceRef) *ShimControl {
	d.t.Helper()
	socket := d.SocketPath(ws) + ".ctl"

	d.mu.Lock()
	existing, ok := d.shims[socket]
	d.mu.Unlock()
	if ok {
		return existing
	}

	s := &ShimControl{Socket: socket, t: d.t, d: d}
	s.connect()
	d.t.Cleanup(s.close)

	d.mu.Lock()
	d.shims[socket] = s
	d.mu.Unlock()
	return s
}

// OpenedShim answers the control client of the shim an OpenWorkspace that has
// just succeeded brought up for ws: connected while it lives, or -- when it has
// ALREADY EXITED -- a client holding only its pid, whose every command fails
// loudly as a departed shim.
//
// WHY. A workspace opened with a short idle cutoff (IdleCutoffMS: 50) can be
// hibernated, and its shim stopped, before the harness dials the control
// socket; Shim then waited the whole test context for a socket that would
// never come back (seen once the RAM disk's I/O stopped stalling the
// daemon's own hibernate: "waiting for the fake shim's control socket ...
// context deadline exceeded"). The bring-up records the shim's pid before it
// answers the open, so the shim this open spawned is known exactly: the
// latest spawn record for this socket. Every other caller still waits for a
// shim to appear, as a revival's next shim on the same socket does.
func (d *Daemon) OpenedShim(ws *workspacev1.WorkspaceRef) *ShimControl {
	d.t.Helper()
	socket := d.SocketPath(ws) + ".ctl"
	pid := d.latestShimSpawn(ws.GetDir(), strings.TrimSuffix(socket, ".ctl"))
	if pid == 0 {
		return d.Shim(ws)
	}
	s := &ShimControl{Socket: socket, t: d.t, d: d}
	s.connectOrDeparted(pid)
	d.t.Cleanup(s.close)
	d.mu.Lock()
	d.shims[socket] = s
	d.mu.Unlock()
	return s
}

// ShimSpawned reports whether this daemon has spawned a shim for ws, read off
// the workspace's own daemon sink.
func (d *Daemon) ShimSpawned(ws *workspacev1.WorkspaceRef) bool {
	d.t.Helper()
	return d.latestShimSpawn(ws.GetDir(), d.SocketPath(ws)) != 0
}

// latestShimSpawn answers the pid of the latest shim this daemon spawned on
// the uds, read off the workspace's own daemon sink, or 0 when it has spawned
// none there.
func (d *Daemon) latestShimSpawn(workspaceDir, uds string) int {
	d.t.Helper()
	pid := 0
	for _, r := range readLog(d.t, WorkspaceLogPath(workspaceDir, "daemon")) {
		if r.PID != d.PID() {
			continue
		}
		if r.Operation != "daemon.shimclient.spawn" || r.Message != "shim spawned" || r.Context["uds"] != uds {
			continue
		}
		if p, ok := r.Context["pid"].(float64); ok {
			pid = int(p)
		}
	}
	return pid
}

// connectOrDeparted is connect for the shim with pid: it stops polling, with
// the pid and no connection, once that shim has exited.
func (s *ShimControl) connectOrDeparted(pid int) {
	s.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if s.dialOpened() {
			return
		}
		if err := syscall.Kill(pid, 0); errors.Is(err, syscall.ESRCH) || isZombie(pid) {
			s.pid, s.departed = pid, true
			return
		}
		select {
		case <-ticker.C:
		case <-s.d.Ctx().Done():
			s.t.Fatalf("waiting for the fake shim's control socket %s (pid %d, alive): %v\ndaemon stderr:\n%s", s.Socket, pid, grantedclock.Explain(s.d.Ctx()), s.d.Stderr())
		}
	}
}

// ShimAt answers a control client for an explicit control socket path, for the
// tests that address a prelaunched or adopted shim by path.
func (d *Daemon) ShimAt(socket string) *ShimControl {
	d.t.Helper()
	s := &ShimControl{Socket: socket, t: d.t, d: d}
	s.connect()
	d.t.Cleanup(s.close)
	return s
}

// connect polls for the control socket and dials it, bounded by the context.
func (s *ShimControl) connect() {
	s.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if s.dial() {
			return
		}
		select {
		case <-ticker.C:
		case <-s.d.Ctx().Done():
			s.t.Fatalf("waiting for the fake shim's control socket %s: %v\ndaemon stderr:\n%s", s.Socket, grantedclock.Explain(s.d.Ctx()), s.d.Stderr())
		}
	}
}

// dial connects to the control socket once and reads the shim's identity,
// answering false when the socket does not answer yet.
func (s *ShimControl) dial() bool {
	s.t.Helper()
	conn, err := net.Dial("unix", s.Socket)
	if err != nil {
		return false
	}
	s.c = conn
	s.dec = bufio.NewScanner(conn)
	s.dec.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
	s.enc = json.NewEncoder(conn)
	reply := s.send(controlCommand{Op: "info"})
	if reply.Info == nil || reply.Info.PID == 0 {
		s.t.Fatalf("fake shim control %s: opening info carried no process identity", s.Socket)
	}
	s.pid = reply.Info.PID
	return true
}

// dialOpened is dial for the shim an open brought up, which the idle sweep may
// stop at any moment: a shim that accepted the connection and then exited
// before it answered the opening info is not a failure here but a shim that
// is leaving, and connectOrDeparted's next pass finds it gone. Every other
// caller keeps dial, where a silent shim is a failure.
func (s *ShimControl) dialOpened() bool {
	s.t.Helper()
	conn, err := net.Dial("unix", s.Socket)
	if err != nil {
		return false
	}
	s.mu.Lock()
	s.c = conn
	s.dec = bufio.NewScanner(conn)
	s.dec.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
	s.enc = json.NewEncoder(conn)
	s.mu.Unlock()
	reply, ok := s.sendAllowingRefusal(controlCommand{Op: "info"})
	if !ok {
		s.close()
		return false
	}
	if reply.Info == nil || reply.Info.PID == 0 {
		s.t.Fatalf("fake shim control %s: opening info carried no process identity", s.Socket)
	}
	s.pid = reply.Info.PID
	return true
}

func (s *ShimControl) close() {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.c != nil {
		s.c.Close()
		s.c = nil
	}
}

// send runs one command and answers its reply, failing the test on a refusal.
//
// THE REPLY IS AWAITED WITHIN THE RUN BUDGET, ON THE HARNESS'S SIDE. The
// budget is kept on the granted clock and has no wall deadline to hand the
// fake as a timeout, so a reply still unread when the budget ends has its read
// cut off here; a command with its own TimeoutMS still hands the fake that.
func (s *ShimControl) send(cmd controlCommand) controlReply {
	s.t.Helper()
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.c == nil && s.departed {
		s.t.Fatalf("fake shim control %s: the shim (pid %d) exited before the harness connected, so it cannot take %s", s.Socket, s.pid, cmd.Op)
	}
	if s.c == nil {
		s.t.Fatalf("fake shim control %s is closed", s.Socket)
	}
	if err := s.enc.Encode(cmd); err != nil {
		s.t.Fatalf("fake shim control %s: send %s: %v", s.Socket, cmd.Op, err)
	}
	conn := s.c
	stop := context.AfterFunc(s.d.Ctx(), func() { _ = conn.SetReadDeadline(time.Now()) })
	defer stop()
	if !s.dec.Scan() {
		if budget := s.d.Ctx(); budget.Err() != nil {
			s.t.Fatalf("fake shim control %s: no reply to %s within the run budget: %v", s.Socket, cmd.Op, grantedclock.Explain(budget))
		}
		s.t.Fatalf("fake shim control %s: no reply to %s: %v", s.Socket, cmd.Op, s.dec.Err())
	}
	var reply controlReply
	if err := json.Unmarshal(s.dec.Bytes(), &reply); err != nil {
		s.t.Fatalf("fake shim control %s: malformed reply %q: %v", s.Socket, s.dec.Text(), err)
	}
	if !reply.OK {
		s.t.Fatalf("fake shim control %s: %s refused: %s", s.Socket, cmd.Op, reply.Error)
	}
	return reply
}

// sendAllowingRefusal is send without the failure, for the callers that treat
// a refusal as the answer (a process that has already exited).
func (s *ShimControl) sendAllowingRefusal(cmd controlCommand) (controlReply, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.c == nil {
		return controlReply{}, false
	}
	if err := s.enc.Encode(cmd); err != nil {
		return controlReply{}, false
	}
	if !s.dec.Scan() {
		return controlReply{}, false
	}
	var reply controlReply
	if err := json.Unmarshal(s.dec.Bytes(), &reply); err != nil {
		return controlReply{}, false
	}
	return reply, reply.OK
}

// Info reports what the fake was launched with.
func (s *ShimControl) Info() ShimInfo {
	s.t.Helper()
	reply := s.send(controlCommand{Op: "info"})
	if reply.Info == nil {
		s.t.Fatalf("fake shim control %s: info carried nothing", s.Socket)
	}
	return *reply.Info
}

// PushSessionUpdate delivers a session-level fact on every open WatchSession.
func (s *ShimControl) PushSessionUpdate(u *conversationv1.SessionUpdate) {
	s.t.Helper()
	s.pushSessionUpdate(u)
}

// pushSessionUpdate delivers one update and answers how many session streams
// received it.
func (s *ShimControl) pushSessionUpdate(u *conversationv1.SessionUpdate) int {
	s.t.Helper()
	return s.send(controlCommand{Op: "push_session_update", Payload: encode(s.t, u)}).Count
}

// PushHealthyWhenSubscribed delivers the readiness diagnostics, retrying until
// at least one session stream is subscribed to receive it.
func (s *ShimControl) PushHealthyWhenSubscribed() {
	s.t.Helper()
	s.pushSessionUpdateWhenSubscribed("the readiness push", healthyUpdate())
}

// pushSessionUpdateWhenSubscribed delivers one session update, retrying until
// at least one session stream received it.
//
// A push to a hub nobody has subscribed to is DROPPED, and the daemon
// subscribes when its bring-up opens WatchSession, which a test cannot observe
// from the wire. Retrying on the subscriber count is what makes the push an
// event the daemon is guaranteed to see.
func (s *ShimControl) pushSessionUpdateWhenSubscribed(what string, u *conversationv1.SessionUpdate) {
	s.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if s.pushSessionUpdate(u) > 0 {
			return
		}
		select {
		case <-ticker.C:
		case <-s.d.Ctx().Done():
			s.t.Fatalf("fake shim control %s: no session stream ever subscribed to receive %s", s.Socket, what)
		}
	}
}

// healthyUpdate is the healthy diagnostics push that gates readiness.
func healthyUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
		}},
	}
}

// PushHealthy delivers the healthy diagnostics push that gates readiness and
// answers how many session streams received it.
func (s *ShimControl) PushHealthy() int {
	s.t.Helper()
	return s.pushSessionUpdate(healthyUpdate())
}

// PushUnhealthy delivers an unhealthy diagnostics push carrying faults,
// retrying until a session stream received it: a push dropped before the
// daemon's WatchSession subscribed would leave the fault unreported forever.
func (s *ShimControl) PushUnhealthy(faults ...*conversationv1.SessionFault) {
	s.t.Helper()
	s.pushSessionUpdateWhenSubscribed("the unhealthy push", &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Unhealthy{Unhealthy: &conversationv1.SessionUnhealthy{Faults: faults}},
		}},
	})
}

// EntryStamp is what a pushed agent frame's served entry carries besides the
// frame: its HistoryPointer (empty: the fake mints the next), the turn it is
// stamped with (empty: unstamped) and its recorded place in milliseconds
// (zero: unplaced). The real shim states all three on every entry it serves.
type EntryStamp struct {
	Pointer string
	Turn    string
	PlaceMs int64
}

// Ops of the two entry pushes, spelled here alone (stampedPush).
const (
	opPushAgentFrame = "push_agent_frame"
	opPushUserPrompt = "push_user_prompt"
)

// stampedPush is the one command every entry push sends, a frame (OP
// opPushAgentFrame) or a delivered prompt (opPushUserPrompt): AGENT's stream
// (empty: the payload's own agent), stamped AT.
func stampedPush(op, agent string, at EntryStamp, payload string) controlCommand {
	return controlCommand{Op: op, Agent: agent, Pointer: at.Pointer, Turn: at.Turn, PlaceMs: at.PlaceMs, Payload: payload}
}

// PushAgentFrameStamped delivers one agent frame on AGENT's WatchAgent stream
// as an entry stamped AT, every stamp at once.
func (s *ShimControl) PushAgentFrameStamped(agent string, at EntryStamp, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.send(stampedPush(opPushAgentFrame, agent, at, encode(s.t, f)))
}

// PushAgentFrame delivers one agent frame on the matching WatchAgent stream.
// An empty agent addresses the frame's own agent_id.
func (s *ShimControl) PushAgentFrame(agent string, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.PushAgentFrameStamped(agent, EntryStamp{}, f)
}

// PushAgentFrameAt delivers one agent frame AT A NAMED HistoryPointer, which is
// how a store row that TWO PLANES wrote reaches the daemon: one entry, one
// position, delivered once per write. An empty agent addresses the frame's own
// agent_id.
func (s *ShimControl) PushAgentFrameAt(agent, pointer string, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.PushAgentFrameStamped(agent, EntryStamp{Pointer: pointer}, f)
}

// PushAgentFrameIn delivers one agent frame STAMPED with the turn it was
// produced within, as the real shim stamps every row of an open turn.
func (s *ShimControl) PushAgentFrameIn(agent, turn string, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.PushAgentFrameStamped(agent, EntryStamp{Turn: turn}, f)
}

// PushAgentFramePlaced delivers one agent frame whose entry sits at AT_MS in
// its conversation (HistoryEntryAt.recorded_place), as the real shim states a
// place for every entry it serves. An empty agent addresses the frame's own
// agent_id.
func (s *ShimControl) PushAgentFramePlaced(agent string, atMs int64, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.PushAgentFrameStamped(agent, EntryStamp{PlaceMs: atMs}, f)
}

// FileAgentFrame files one agent frame of TURN into AGENT's book on the fake
// store WITHOUT delivering it on any stream: a row of a book this session
// never watches, such as a fork's parent's, which the fork reads by page.
func (s *ShimControl) FileAgentFrame(agent, turn string, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.send(controlCommand{Op: "file_agent_frame", Agent: agent, Turn: turn, Payload: encode(s.t, f)})
}

// FileUserPrompt files one delivered prompt into AGENT's book on the fake
// store WITHOUT delivering it on any stream (see FileAgentFrame).
func (s *ShimControl) FileUserPrompt(agent string, p *conversationv1.AgentPrompt) {
	s.t.Helper()
	s.send(controlCommand{Op: "file_user_prompt", Agent: agent, Payload: encode(s.t, p)})
}

// PushUserPrompt delivers one DELIVERED PROMPT on the matching WatchAgent
// stream — the history entry the vendor lays down when a turn's prompt is
// delivered, and the only path by which a prompt's own content blocks reach
// the feed resolver. An empty agent addresses the prompt's own recipient.
func (s *ShimControl) PushUserPrompt(agent string, p *conversationv1.AgentPrompt) {
	s.t.Helper()
	s.PushUserPromptStamped(agent, EntryStamp{}, p)
}

// PushUserPromptStamped delivers one DELIVERED PROMPT as an entry stamped AT:
// its pointer, its turn and its recorded place, as the real shim serves the
// prompt of the turn it opens.
func (s *ShimControl) PushUserPromptStamped(agent string, at EntryStamp, p *conversationv1.AgentPrompt) {
	s.t.Helper()
	s.send(stampedPush(opPushUserPrompt, agent, at, encode(s.t, p)))
}

// PushRetired delivers one RETIRED entry (WatchAgentResponse.retired) on the
// matching WatchAgent stream: the entry as last served, at its own pointer,
// which the store no longer serves. An empty agent reaches every stream.
func (s *ShimControl) PushRetired(agent string, at *conversationv1.HistoryEntryAt) {
	s.t.Helper()
	s.send(controlCommand{Op: "push_retired", Agent: agent, Payload: encode(s.t, at)})
}

// PushBash delivers one bash frame on the detached shell's WatchBash stream.
func (s *ShimControl) PushBash(work string, b *conversationv1.AgentBash) {
	s.t.Helper()
	s.send(controlCommand{Op: "push_bash", Work: work, Payload: encode(s.t, b)})
}

// Answer queues the next response for a verb.
func (s *ShimControl) Answer(rpc string, resp proto.Message) {
	s.t.Helper()
	s.send(controlCommand{Op: "answer", RPC: rpc, Payload: encode(s.t, resp)})
}

// AnswerFailure queues a transport-level failure for the next call of a verb.
func (s *ShimControl) AnswerFailure(rpc, detail string) {
	s.t.Helper()
	s.send(controlCommand{Op: "answer", RPC: rpc, Fail: detail})
}

// Count reports how many requests a verb has received.
func (s *ShimControl) Count(rpc string) int {
	s.t.Helper()
	return s.send(controlCommand{Op: "count", RPC: rpc}).Count
}

// expect pops the oldest unread request for a verb into a message.
// It answers the verb's TOTAL at pop time, observed by the fake under the same
// lock as the pop, so a caller that needs both never pays a second round trip
// to a shim a production step may already be ending.
func (s *ShimControl) expect(rpc string, into proto.Message) int {
	s.t.Helper()
	reply := s.send(controlCommand{Op: "expect", RPC: rpc})
	raw, err := base64.StdEncoding.DecodeString(reply.Payload)
	if err != nil {
		s.t.Fatalf("fake shim control: %s payload is not base64: %v", rpc, err)
	}
	if err := proto.Unmarshal(raw, into); err != nil {
		s.t.Fatalf("fake shim control: decode the %s request: %v", rpc, err)
	}
	return reply.Count
}

// ExpectStartSession pops the next StartSession request.
func (s *ShimControl) ExpectStartSession() *shimv1.StartSessionRequest {
	s.t.Helper()
	msg := &shimv1.StartSessionRequest{}
	s.expect(RPCStartSession, msg)
	return msg
}

// ExpectGetForkPoint pops the next GetForkPoint call: a PARENT's shim asked
// where a fork of its conversation ends.
func (s *ShimControl) ExpectGetForkPoint() *shimv1.GetForkPointRequest {
	s.t.Helper()
	msg := &shimv1.GetForkPointRequest{}
	s.expect(RPCGetForkPoint, msg)
	return msg
}

// ExpectForkSession pops the next ForkSession call: a CHILD's shim asked to
// make the vendor's fork of its parent.
func (s *ShimControl) ExpectForkSession() *shimv1.ForkSessionRequest {
	s.t.Helper()
	msg := &shimv1.ForkSessionRequest{}
	s.expect(RPCForkSession, msg)
	return msg
}

// ExpectWatchSession pops the next WatchSession open.
func (s *ShimControl) ExpectWatchSession() *shimv1.WatchSessionRequest {
	s.t.Helper()
	msg := &shimv1.WatchSessionRequest{}
	s.expect(RPCWatchSession, msg)
	return msg
}

// EndTurnUnwatched ends the turn on the shim with AGENT's terminal F, filed
// into AGENT's book and delivered on no stream: the turn ends while no daemon
// watches it, whatever state any daemon is in.
func (s *ShimControl) EndTurnUnwatched(agent string, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.send(controlCommand{Op: "end_turn_unwatched", Agent: agent, Payload: encode(s.t, f)})
}

// ExpectWatchBash pops the next WatchBash open.
func (s *ShimControl) ExpectWatchBash() *shimv1.WatchBashRequest {
	s.t.Helper()
	msg := &shimv1.WatchBashRequest{}
	s.expect(RPCWatchBash, msg)
	return msg
}

// ExpectStartTurn pops the next StartTurn request.
func (s *ShimControl) ExpectStartTurn() *shimv1.StartTurnRequest {
	s.t.Helper()
	msg, _ := s.ExpectStartTurnWithCount()
	return msg
}

// ExpectStartTurnWithCount pops the next StartTurn request AND the number of
// StartTurns the fake had received when it popped it.
//
// It exists for the assertion that a step sent EXACTLY ONE further turn when
// the same step then ends the shim: the merge teardown resubmits the displaced
// turn and immediately force-stops the session before removing the worktree,
// so a separate Count call afterwards races a process that is deliberately on
// its way out. The count observed at the pop cannot.
func (s *ShimControl) ExpectStartTurnWithCount() (*shimv1.StartTurnRequest, int) {
	s.t.Helper()
	msg := &shimv1.StartTurnRequest{}
	return msg, s.expect(RPCStartTurn, msg)
}

// ExpectUpdateAgent pops the next UpdateAgent request.
func (s *ShimControl) ExpectUpdateAgent() *shimv1.UpdateAgentRequest {
	s.t.Helper()
	msg := &shimv1.UpdateAgentRequest{}
	s.expect(RPCUpdateAgent, msg)
	return msg
}

// ExpectKillTurn pops the next KillTurn request.
func (s *ShimControl) ExpectKillTurn() *shimv1.KillTurnRequest {
	s.t.Helper()
	msg := &shimv1.KillTurnRequest{}
	s.expect(RPCKillTurn, msg)
	return msg
}

// ExpectSetSessionModel pops the next SetSessionModel request.
func (s *ShimControl) ExpectSetSessionModel() *shimv1.SetSessionModelRequest {
	s.t.Helper()
	msg := &shimv1.SetSessionModelRequest{}
	s.expect(RPCSetSessionModel, msg)
	return msg
}

// ExpectSetSessionPermissionMode pops the next SetSessionPermissionMode.
func (s *ShimControl) ExpectSetSessionPermissionMode() *shimv1.SetSessionPermissionModeRequest {
	s.t.Helper()
	msg := &shimv1.SetSessionPermissionModeRequest{}
	s.expect(RPCSetSessionPermissionMode, msg)
	return msg
}

// ExpectSetSessionEffort pops the next SetSessionEffort.
func (s *ShimControl) ExpectSetSessionEffort() *shimv1.SetSessionEffortRequest {
	s.t.Helper()
	msg := &shimv1.SetSessionEffortRequest{}
	s.expect(RPCSetSessionEffort, msg)
	return msg
}

// ExpectWatchAgentFor pops WatchAgent opens until one names the target. The
// session's MAIN watch is opened at bring-up with an unset target, so a test
// about a particular agent's watch has to look past it rather than assert on
// whichever open happens to be oldest.
func (s *ShimControl) ExpectWatchAgentFor(target string) *shimv1.WatchAgentRequest {
	s.t.Helper()
	for {
		req := s.ExpectWatchAgent()
		if req.GetTarget().GetValue() == target {
			return req
		}
	}
}

// ExpectWatchAgent pops the next WatchAgent open.
func (s *ShimControl) ExpectWatchAgent() *shimv1.WatchAgentRequest {
	s.t.Helper()
	msg := &shimv1.WatchAgentRequest{}
	s.expect(RPCWatchAgent, msg)
	return msg
}

// ExpectStopBash pops the next StopBash request.
func (s *ShimControl) ExpectStopBash() *shimv1.StopBashRequest {
	s.t.Helper()
	msg := &shimv1.StopBashRequest{}
	s.expect(RPCStopBash, msg)
	return msg
}

// DropStream severs every open stream of a family, which the daemon must read
// as a link failure.
func (s *ShimControl) DropStream(name string) {
	s.t.Helper()
	s.send(controlCommand{Op: "drop_stream", Stream: name})
}

// SetLiveWork states the live membership every later WatchSession
// re-announcement carries, as the real shim recomputes what is live now.
func (s *ShimControl) SetLiveWork(live ...*conversationv1.AgentDetachedWork) {
	s.t.Helper()
	s.send(controlCommand{Op: "set_live_work", Payload: encode(s.t, &conversationv1.SessionStarted{LiveWork: live})})
}

// AwaitAgentWatch returns once the daemon holds a WatchAgent stream on the
// fake. The daemon watches its agents TAIL-ONLY, so a frame pushed before its
// watch subscribes reaches no one: a test that pushes the moment a workspace
// is open waits on this first.
func (s *ShimControl) AwaitAgentWatch() {
	s.t.Helper()
	s.send(controlCommand{Op: "await_agent_watch"})
}

// SilenceBash makes every later WatchBash open for work go unanswered: no
// opening frame, ever, as the real shim's WatchBash on a run with no row.
func (s *ShimControl) SilenceBash(work string) {
	s.t.Helper()
	s.send(controlCommand{Op: "silence_bash", Work: work})
}

// AwaitBash makes every later WatchBash open for work send no `start` and wait
// for the first frame pushed, as the real shim's WatchBash does for a live run
// whose rows are all the sidecar's.
func (s *ShimControl) AwaitBash(work string) {
	s.t.Helper()
	s.send(controlCommand{Op: "await_bash", Work: work})
}

// SilenceNextReannouncement makes the next WatchSession open send no
// SessionStarted re-announcement, as a shim from before the re-announcement
// would: the successor adopting it mid-work never learns its facts.
func (s *ShimControl) SilenceNextReannouncement() {
	s.t.Helper()
	s.send(controlCommand{Op: "silence_reannouncement"})
}

// Hang stops the fake answering anything.
func (s *ShimControl) Hang() {
	s.t.Helper()
	s.send(controlCommand{Op: "hang"})
}

// Unhang releases a hung fake.
func (s *ShimControl) Unhang() {
	s.t.Helper()
	s.send(controlCommand{Op: "unhang"})
}

// Exit ends the fake process with a status and failure evidence on stderr.
func (s *ShimControl) Exit(code int, stderr string) {
	s.t.Helper()
	s.sendAllowingRefusal(controlCommand{Op: "exit", Code: code, Stderr: stderr})
}

// AwaitGone waits for the fake to release its workspace kernel lock, which is
// how the harness knows the process is really reaped.
func (s *ShimControl) AwaitGone() {
	s.t.Helper()
	// The stand-down can close the control connection before this call begins.
	// connect captured the process identity while the socket was known-live, so
	// waiting never silently succeeds merely because the dying fake cannot
	// answer one last info request.
	AwaitProcessGone(s.t, s.d.Ctx(), s.pid)
}

func encode(t testing.TB, m proto.Message) string {
	t.Helper()
	raw, err := proto.Marshal(m)
	if err != nil {
		t.Fatalf("harness: encode %T: %v", m, err)
	}
	return base64.StdEncoding.EncodeToString(raw)
}

func writeJSON(t testing.TB, path string, v any) {
	t.Helper()
	body, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		t.Fatalf("harness: encode %s: %v", path, err)
	}
	writeFile(t, path, string(body))
}

// profileFileName is the fake's per-workspace profile name for a directory.
func profileFileName(dir string) string {
	sum := md5.Sum([]byte(filepath.Clean(dir)))
	return hex.EncodeToString(sum[:]) + ".json"
}

// WorkspaceLockPath derives the workspace kernel lock's path exactly as the
// shim does, so a test can hold or probe it.
func WorkspaceLockPath(lockDir, dir string) string {
	sum := md5.Sum([]byte(filepath.Clean(dir)))
	return filepath.Join(lockDir, "workspace-"+hex.EncodeToString(sum[:])[:8]+".lock")
}
