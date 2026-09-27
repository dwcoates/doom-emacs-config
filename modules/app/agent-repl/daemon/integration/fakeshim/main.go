// Command fakeshim stands in for the real agent shim in the daemon's
// integration suite. It obeys the shim's spawn contract (argv, kernel locks,
// the fd 3 JSONL log, the shim.v1 service on a unix socket) and adds a
// control listener at `<uds>.ctl` through which a test scripts it.
//
// It never calls the vendor, so AGENT_REPL_FORBID_VENDOR_CALLS costs it
// nothing to honor.
package main

import (
	"bufio"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/proto/shim/v1/shimv1connect"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/proto"

	daemonserver "claude-repld/internal/server"
)

// killAnswerWriteBound is how long the KillSession answer has to reach the
// socket before this fake ends the process anyway.
//
// A LAST RESORT, not the mechanism: the barrier ends on the connection going
// quiet, and the work it covers is one flush of a few dozen bytes onto a unix
// socket. It is deliberately far under the daemon's own stand-down window, so
// an overrun still looks to the daemon like a fake that took a moment rather
// than one that hung.
const killAnswerWriteBound = 250 * time.Millisecond

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
}

func run(args []string) error {
	argv, err := ParseArgv(args)
	if err != nil {
		return err
	}
	cwd, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("fakeshim: cwd: %w", err)
	}
	absCwd, err := filepath.Abs(cwd)
	if err != nil {
		return fmt.Errorf("fakeshim: abs cwd: %w", err)
	}

	profile, err := LoadProfile(os.Getenv(EnvProfileDir), absCwd)
	if err != nil {
		return err
	}
	if sha := os.Getenv(EnvBuildSHA); sha != "" && profile.BuildSHA == "" {
		profile.BuildSHA = sha
	}

	log := newLogSink(argv.LogFD)
	defer log.close()
	// THE WORKSPACE ID COMES OFF THE LISTEN SOCKET, as it does in the real
	// shim: the spawn contract carries no workspace id, and the daemon names
	// the socket `<state>/sock/<workspace id>[.n<gen>].sock`. A basename that
	// does not spell one is a refusal, never a record filed under an id the
	// fleet never minted.
	workspaceID, err := workspaceIDFromListenSocket(argv.Listen)
	if err != nil {
		return err
	}
	log.bind(workspaceID, absCwd)

	// NO LOCK IS TAKEN AT STARTUP. Both kernel locks are taken inside
	// StartSession, before the SDK is touched, and released together on a kill
	// or a stand-down (project-lead ruling, shim be119abbf). An INERT shim --
	// one the relaunch engine prelaunched beside a live one -- therefore holds
	// neither, which is what lets it come up beside its predecessor at all.
	lockDir := LockDir(os.Getenv, os.Getenv("HOME"))
	wsLockPath := WorkspaceLockPath(lockDir, absCwd)

	if profile.ExitOn == "startup" {
		if profile.Stderr != "" {
			fmt.Fprintln(os.Stderr, profile.Stderr)
		}
		os.Exit(profile.ExitCode)
	}

	proc := &process{
		argv:    argv,
		cwd:     absCwd,
		lockDir: lockDir,
		wsLock:  wsLockPath,
		rec:     NewRecorder(),
		log:     log,
	}
	proc.srv = newServer(proc.rec, profile, log)
	proc.srv.exit = proc.die
	proc.srv.onSessionStarted = proc.takeSessionLock
	proc.srv.claimWorkspace = proc.takeWorkspaceLock
	proc.srv.releaseLocks = proc.releaseLocks

	rpcListener, err := listenUnix(argv.Listen)
	if err != nil {
		return err
	}
	defer rpcListener.Close()

	ctlListener, err := listenUnix(argv.Listen + ".ctl")
	if err != nil {
		return err
	}
	defer ctlListener.Close()

	path, handler := shimv1connect.NewShimHandler(proc.srv)
	mux := http.NewServeMux()
	// A STAND-DOWN ENDS THE PROCESS, exactly as the real shim's does: the
	// daemon's relaunch gate is the old process being REAPED, and a fake that
	// answered KillSession and kept running would leave every stand-down
	// waiting out its window and then force-killing.
	//
	// THE EXIT WAITS FOR THE ANSWER'S BYTES, NOT FOR THE HANDLER AND NOT FOR
	// THE REQUEST CONTEXT. A handler returning does not mean its answer has
	// left: over h2c the response's final frames are written by the
	// connection's own goroutine after `handlerDone`, and an `os.Exit` on the
	// handler's goroutine raced them -- the daemon read
	// `unavailable: unexpected EOF` from a `KillSession` this fake had
	// accepted, recorded "the session kill did not answer" against an orderly
	// stand-down, and failed the warning sweep with it.
	//
	// Waiting on the request's context was the first repair and it was one
	// step short: `serverConn.runHandler` cancels that context in a defer that
	// runs BEFORE the same defer's `rw.handlerDone()` produces those frames at
	// all, so the exit still landed ahead of the answer in about one
	// integration run in ten. The wait is now on the SOCKET having written
	// them -- see the daemon's server.WriteBarrier, which carries the whole account.
	answers := &daemonserver.WriteBarrier{}
	mux.Handle(path, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		standingDown := strings.HasSuffix(r.URL.Path, "/KillSession")
		mark := answers.Mark()
		handler.ServeHTTP(w, r)
		if standingDown && proc.srv.killedSession() {
			answered := r.Context()
			go func() {
				<-answered.Done()
				if !answers.AwaitWrittenSince(mark, killAnswerWriteBound) {
					proc.log.write("kill-answer-unwritten", map[string]any{
						"detail":   "the KillSession answer never reached the socket within the bound; the daemon will read a cut connection instead of the stand-down it asked for",
						"bound_ms": killAnswerWriteBound.Milliseconds(),
					})
				}
				proc.die(0, "")
			}()
		}
	}))
	httpSrv := &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}

	go proc.serveControl(ctlListener)

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGTERM, syscall.SIGINT)
	go func() {
		<-signals
		proc.die(0, "")
	}()

	log.write("listening", map[string]any{"uds": argv.Listen, "control": argv.Listen + ".ctl"})
	if err := httpSrv.Serve(answers.Listener(rpcListener)); err != nil && !errors.Is(err, http.ErrServerClosed) {
		return fmt.Errorf("fakeshim: serve: %w", err)
	}
	return nil
}

// listenUnix binds a unix socket, clearing a stale path first.
func listenUnix(path string) (net.Listener, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("fakeshim: socket dir %s: %w", filepath.Dir(path), err)
	}
	if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("fakeshim: clear socket %s: %w", path, err)
	}
	l, err := net.Listen("unix", path)
	if err != nil {
		return nil, fmt.Errorf("fakeshim: listen %s: %w", path, err)
	}
	return l, nil
}

// process is the fake's whole-process state: what it was launched with, the
// locks it holds, and the scripted server.
type process struct {
	argv    Argv
	cwd     string
	lockDir string
	wsLock  string
	rec     *Recorder
	log     *logSink
	srv     *server

	mu            sync.Mutex
	workspaceLock *heldLock
	sessionLock   *heldLock
	sessionPath   string
}

// takeWorkspaceLock takes workspace-<hash>.lock, exactly as the real shim does
// inside StartSession. It NEVER BLOCKS: a lock another shim holds is this
// conversation being owned elsewhere, which StartSession answers with
// `conversation_owned` rather than waiting on.
func (p *process) takeWorkspaceLock() bool {
	p.mu.Lock()
	if p.workspaceLock != nil {
		p.mu.Unlock()
		return true
	}
	p.mu.Unlock()
	held, err := tryLock(p.wsLock)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(3)
	}
	if held == nil {
		p.log.write("workspace_lock_refused", map[string]any{"path": p.wsLock, "cwd": p.cwd})
		return false
	}
	p.mu.Lock()
	p.workspaceLock = held
	p.mu.Unlock()
	p.log.write("workspace_lock_taken", map[string]any{"path": p.wsLock, "cwd": p.cwd})
	return true
}

// releaseLocks drops both kernel locks together, which is what a kill or a
// stand-down does: the session is over, and neither lock outlives it.
func (p *process) releaseLocks() {
	p.mu.Lock()
	ws, session := p.workspaceLock, p.sessionLock
	p.workspaceLock, p.sessionLock, p.sessionPath = nil, nil, ""
	p.mu.Unlock()
	ws.release()
	session.release()
	p.log.write("locks_released", map[string]any{"cwd": p.cwd})
}

// takeSessionLock takes session-<vendor session id>.lock, exactly as the real
// shim does inside StartSession.
func (p *process) takeSessionLock(vendorSessionID string) {
	path := SessionLockPath(p.lockDir, vendorSessionID)
	l, err := takeLock(path)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(3)
	}
	p.mu.Lock()
	p.sessionLock, p.sessionPath = l, path
	p.mu.Unlock()
	p.log.write("session_lock_taken", map[string]any{"path": path, "vendor_session_id": vendorSessionID})
}

// die ends the process, optionally leaving failure evidence on stderr. The
// kernel locks go with it, which is what makes the daemon's probe meaningful.
func (p *process) die(code int, stderr string) {
	if stderr != "" {
		fmt.Fprintln(os.Stderr, stderr)
	}
	p.log.write("exiting", map[string]any{"code": code})
	p.log.close()
	os.Exit(code)
}

func (p *process) info() *Info {
	p.mu.Lock()
	sessionPath := p.sessionPath
	p.mu.Unlock()
	p.srv.mu.Lock()
	vendorID := p.srv.vendorID
	p.srv.mu.Unlock()
	env := map[string]string{}
	for _, kv := range os.Environ() {
		for i := 0; i < len(kv); i++ {
			if kv[i] == '=' {
				env[kv[:i]] = kv[i+1:]
				break
			}
		}
	}
	return &Info{
		Argv:            os.Args,
		Env:             env,
		Cwd:             p.cwd,
		PID:             os.Getpid(),
		Listen:          p.argv.Listen,
		StoreSocket:     p.argv.StoreSocket,
		LogFD:           p.argv.LogFD,
		Fake:            p.argv.Fake,
		WorkspaceLock:   p.wsLock,
		SessionLock:     sessionPath,
		VendorSessionID: vendorID,
	}
}

// serveControl accepts control connections and runs each one's script.
func (p *process) serveControl(l net.Listener) {
	for {
		conn, err := l.Accept()
		if err != nil {
			return
		}
		go p.runControl(conn)
	}
}

func (p *process) runControl(conn net.Conn) {
	defer conn.Close()
	scanner := bufio.NewScanner(conn)
	scanner.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
	enc := json.NewEncoder(conn)
	for scanner.Scan() {
		reply := p.apply(scanner.Bytes())
		if err := enc.Encode(reply); err != nil {
			return
		}
	}
}

// apply executes one control command and produces its reply.
func (p *process) apply(line []byte) Reply {
	cmd, err := ParseCommand(line)
	if err != nil {
		return Reply{Error: err.Error()}
	}
	payload, err := cmd.Bytes()
	if err != nil {
		return Reply{Error: fmt.Sprintf("fakeshim: %s: payload is not base64: %v", cmd.Op, err)}
	}

	switch cmd.Op {
	case OpInfo:
		return Reply{OK: true, Info: p.info()}

	case OpCount:
		return Reply{OK: true, Count: p.rec.Count(cmd.RPC)}

	case OpExpect:
		ctx, cancel := commandContext(cmd)
		defer cancel()
		msg, total, err := p.rec.Expect(ctx, cmd.RPC)
		if err != nil {
			return Reply{Error: err.Error()}
		}
		raw, err := proto.Marshal(msg)
		if err != nil {
			return Reply{Error: fmt.Sprintf("fakeshim: encode %s request: %v", cmd.RPC, err)}
		}
		// The COUNT RIDES THE POP: see Recorder.Expect.
		return Reply{OK: true, Payload: base64.StdEncoding.EncodeToString(raw), Count: total}

	case OpAnswer:
		var msg proto.Message
		if len(payload) > 0 {
			msg = newResponse(cmd.RPC)
			if err := proto.Unmarshal(payload, msg); err != nil {
				return Reply{Error: fmt.Sprintf("fakeshim: decode %s answer: %v", cmd.RPC, err)}
			}
		}
		p.srv.queueAnswer(cmd.RPC, msg, cmd.Fail)
		return Reply{OK: true}

	case OpPushSessionUpdate:
		u := &conversationv1.SessionUpdate{}
		if err := proto.Unmarshal(payload, u); err != nil {
			return Reply{Error: fmt.Sprintf("fakeshim: decode session update: %v", err)}
		}
		// EVERY DIAGNOSTICS FRAME STATES THE SHIM'S BUILD, as the real shim's
		// do: a test pushing a verdict need not restate it.
		if diagnostics := u.GetDiagnostics(); diagnostics != nil && diagnostics.GetShimBuild() == "" {
			diagnostics.ShimBuild = p.srv.buildSHA()
		}
		p.srv.sessions.publish(u)
		return Reply{OK: true, Count: p.srv.sessions.count()}

	case OpPushAgentFrame:
		f := &conversationv1.AgentFrame{}
		if err := proto.Unmarshal(payload, f); err != nil {
			return Reply{Error: fmt.Sprintf("fakeshim: decode agent frame: %v", err)}
		}
		agent := cmd.Agent
		if agent == "" {
			agent = f.GetAgentId().GetValue()
		}
		p.srv.rememberPushedBash(f)
		p.srv.rememberPushedPermission(agent, f)
		p.srv.settleTurn(agent, f)
		p.srv.agents.publish(agentFrame{agent: agent, frame: f, pointer: cmd.Pointer, turn: cmd.Turn})
		return Reply{OK: true, Count: p.srv.agents.count()}

	case OpPushUserPrompt:
		prompt := &conversationv1.AgentPrompt{}
		if err := proto.Unmarshal(payload, prompt); err != nil {
			return Reply{Error: fmt.Sprintf("fakeshim: decode user prompt: %v", err)}
		}
		agent := cmd.Agent
		if agent == "" {
			agent = prompt.GetAgent().GetValue()
		}
		p.srv.agents.publish(agentFrame{agent: agent, prompt: prompt})
		return Reply{OK: true, Count: p.srv.agents.count()}

	case OpPushBash:
		b := &conversationv1.AgentBash{}
		if err := proto.Unmarshal(payload, b); err != nil {
			return Reply{Error: fmt.Sprintf("fakeshim: decode bash frame: %v", err)}
		}
		p.srv.rememberBashStart(cmd.Work, b)
		return Reply{OK: true, Count: p.srv.publishBash(cmd.Work, b)}

	case OpDropStream:
		switch cmd.Stream {
		case StreamSession:
			p.srv.sessions.dropAll()
		case StreamAgent:
			p.srv.agents.dropAll()
		case StreamBash:
			p.srv.dropBashStreams()
		}
		return Reply{OK: true}

	case OpSetLiveWork:
		started := &conversationv1.SessionStarted{}
		if err := proto.Unmarshal(payload, started); err != nil {
			return Reply{Error: fmt.Sprintf("fakeshim: decode live membership: %v", err)}
		}
		p.srv.setLiveWork(started.GetLiveWork())
		return Reply{OK: true}

	case OpSilenceBash:
		p.srv.silenceBash(cmd.Work)
		return Reply{OK: true}

	case OpHang:
		p.srv.hang()
		return Reply{OK: true}

	case OpUnhang:
		p.srv.release()
		return Reply{OK: true}

	case OpExit:
		// Answer before dying so the harness never races the socket close.
		go func() { p.die(cmd.Code, cmd.Stderr) }()
		return Reply{OK: true}
	}
	return Reply{Error: fmt.Sprintf("%v: %s", ErrUnknownOp, cmd.Op)}
}

// commandContext bounds one command's wait by the deadline the caller stated,
// so a script that expects a request the daemon never sends fails with a named
// verb instead of hanging the suite.
func commandContext(cmd Command) (context.Context, context.CancelFunc) {
	if cmd.TimeoutMS <= 0 {
		return context.WithCancel(context.Background())
	}
	return context.WithTimeout(context.Background(), time.Duration(cmd.TimeoutMS)*time.Millisecond)
}
