package harness

import (
	"bufio"
	"crypto/md5"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"
)

// ShimProfile is the fake shim's startup script for one workspace. It mirrors
// the fake's own Profile; the harness keeps its own copy so a test never
// imports a `main` package.
type ShimProfile struct {
	BuildSHA         string         `json:"build_sha,omitempty"`
	DelayDiagnostics bool           `json:"delay_diagnostics,omitempty"`
	ExitOn           string         `json:"exit_on,omitempty"`
	ExitCode         int            `json:"exit_code,omitempty"`
	Stderr           string         `json:"stderr,omitempty"`
	ColdOnResume     *ShimColdFacts `json:"cold_on_resume,omitempty"`
	VendorSessionID  string         `json:"vendor_session_id,omitempty"`
	// LiveWork is what the fake's SessionStarted states is already running,
	// each element one binary-encoded conversation.v1 AgentDetachedWork.
	LiveWork [][]byte `json:"live_work,omitempty"`
}

// EncodeLiveWork renders detached-work announcements for a ShimProfile's
// LiveWork field.
func EncodeLiveWork(t *testing.T, items ...*conversationv1.AgentDetachedWork) [][]byte {
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
	RPCHibernate                = "Hibernate"
	RPCKillSession              = "KillSession"
	RPCStartTurn                = "StartTurn"
	RPCWatchAgent               = "WatchAgent"
	RPCUpdateAgent              = "UpdateAgent"
	RPCKillTurn                 = "KillTurn"
	RPCWatchBash                = "WatchBash"
	RPCStopBash                 = "StopBash"
	RPCDetachForeground         = "DetachForeground"
	RPCReadHistory              = "ReadHistory"
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

	t   *testing.T
	d   *Daemon
	mu  sync.Mutex
	c   net.Conn
	dec *bufio.Scanner
	enc *json.Encoder
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
		conn, err := net.Dial("unix", s.Socket)
		if err == nil {
			s.c = conn
			s.dec = bufio.NewScanner(conn)
			s.dec.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
			s.enc = json.NewEncoder(conn)
			return
		}
		select {
		case <-ticker.C:
		case <-s.d.Ctx().Done():
			s.t.Fatalf("waiting for the fake shim's control socket %s: %v\ndaemon stderr:\n%s", s.Socket, s.d.Ctx().Err(), s.d.Stderr())
		}
	}
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
func (s *ShimControl) send(cmd controlCommand) controlReply {
	s.t.Helper()
	if cmd.TimeoutMS == 0 {
		if deadline, ok := s.d.Ctx().Deadline(); ok {
			cmd.TimeoutMS = int(time.Until(deadline).Milliseconds())
		}
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.c == nil {
		s.t.Fatalf("fake shim control %s is closed", s.Socket)
	}
	if err := s.enc.Encode(cmd); err != nil {
		s.t.Fatalf("fake shim control %s: send %s: %v", s.Socket, cmd.Op, err)
	}
	if !s.dec.Scan() {
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
//
// A push to a hub nobody has subscribed to is DROPPED, and the daemon
// subscribes when its bring-up opens WatchSession — which a test cannot observe
// from the wire. Retrying on the subscriber count is what makes "the shim came
// up healthy" an event the daemon is guaranteed to see.
func (s *ShimControl) PushHealthyWhenSubscribed() {
	s.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if s.PushHealthy() > 0 {
			return
		}
		select {
		case <-ticker.C:
		case <-s.d.Ctx().Done():
			s.t.Fatalf("fake shim control %s: no session stream ever subscribed to receive the readiness push", s.Socket)
		}
	}
}

// PushHealthy delivers the healthy diagnostics push that gates readiness and
// answers how many session streams received it.
func (s *ShimControl) PushHealthy() int {
	s.t.Helper()
	return s.pushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
		}},
	})
}

// PushUnhealthy delivers an unhealthy diagnostics push carrying faults.
func (s *ShimControl) PushUnhealthy(faults ...*conversationv1.SessionFault) {
	s.t.Helper()
	s.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Unhealthy{Unhealthy: &conversationv1.SessionUnhealthy{Faults: faults}},
		}},
	})
}

// PushAgentFrame delivers one agent frame on the matching WatchAgent stream.
// An empty agent addresses the frame's own agent_id.
func (s *ShimControl) PushAgentFrame(agent string, f *conversationv1.AgentFrame) {
	s.t.Helper()
	s.send(controlCommand{Op: "push_agent_frame", Agent: agent, Payload: encode(s.t, f)})
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
func (s *ShimControl) expect(rpc string, into proto.Message) {
	s.t.Helper()
	reply := s.send(controlCommand{Op: "expect", RPC: rpc})
	raw, err := base64.StdEncoding.DecodeString(reply.Payload)
	if err != nil {
		s.t.Fatalf("fake shim control: %s payload is not base64: %v", rpc, err)
	}
	if err := proto.Unmarshal(raw, into); err != nil {
		s.t.Fatalf("fake shim control: decode the %s request: %v", rpc, err)
	}
}

// ExpectStartSession pops the next StartSession request.
func (s *ShimControl) ExpectStartSession() *shimv1.StartSessionRequest {
	s.t.Helper()
	msg := &shimv1.StartSessionRequest{}
	s.expect(RPCStartSession, msg)
	return msg
}

// ExpectWatchSession pops the next WatchSession open.
func (s *ShimControl) ExpectWatchSession() *shimv1.WatchSessionRequest {
	s.t.Helper()
	msg := &shimv1.WatchSessionRequest{}
	s.expect(RPCWatchSession, msg)
	return msg
}

// ExpectStartTurn pops the next StartTurn request.
func (s *ShimControl) ExpectStartTurn() *shimv1.StartTurnRequest {
	s.t.Helper()
	msg := &shimv1.StartTurnRequest{}
	s.expect(RPCStartTurn, msg)
	return msg
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

// ExpectKillSession pops the next KillSession request.
func (s *ShimControl) ExpectKillSession() *shimv1.KillSessionRequest {
	s.t.Helper()
	msg := &shimv1.KillSessionRequest{}
	s.expect(RPCKillSession, msg)
	return msg
}

// ExpectHibernate pops the next Hibernate request.
func (s *ShimControl) ExpectHibernate() *shimv1.HibernateRequest {
	s.t.Helper()
	msg := &shimv1.HibernateRequest{}
	s.expect(RPCHibernate, msg)
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

// ExpectWatchBash pops the next WatchBash open.
func (s *ShimControl) ExpectWatchBash() *shimv1.WatchBashRequest {
	s.t.Helper()
	msg := &shimv1.WatchBashRequest{}
	s.expect(RPCWatchBash, msg)
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
	info := ShimInfo{}
	if reply, ok := s.sendAllowingRefusal(controlCommand{Op: "info"}); ok && reply.Info != nil {
		info = *reply.Info
	}
	if info.PID == 0 {
		return
	}
	AwaitProcessGone(s.t, s.d.Ctx(), info.PID)
}

func encode(t *testing.T, m proto.Message) string {
	t.Helper()
	raw, err := proto.Marshal(m)
	if err != nil {
		t.Fatalf("harness: encode %T: %v", m, err)
	}
	return base64.StdEncoding.EncodeToString(raw)
}

func writeJSON(t *testing.T, path string, v any) {
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

// LockFiles lists the kernel locks that currently exist.
func (d *Daemon) LockFiles() []string {
	d.t.Helper()
	entries, err := os.ReadDir(d.LockDir)
	if err != nil {
		d.t.Fatalf("harness: read %s: %v", d.LockDir, err)
	}
	var out []string
	for _, e := range entries {
		if strings.HasSuffix(e.Name(), ".lock") {
			out = append(out, e.Name())
		}
	}
	return out
}
