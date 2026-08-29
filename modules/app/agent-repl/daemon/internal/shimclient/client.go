package shimclient

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	"agentrepl/proto/shim/v1/shimv1connect"

	"connectrpc.com/connect"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// defaultKillGrace is how long a SIGTERMed shim has to exit before the
// SIGKILL. Bounded, because a wedged shim must not wedge the daemon's
// shutdown.
const defaultKillGrace = 5 * time.Second

// client is one shim connection AND, when it spawned the process, its
// supervisor. Adopted clients have no cmd: they supervise the LINK only, and
// their death evidence is the socket plus the workspace lock.
type client struct {
	log     dlog.Logger
	ws      ids.WorkspaceID
	udsPath string
	rpc     shimv1connect.ShimClient
	back    backoff
	grace   time.Duration

	// lockProbe answers whether the workspace's kernel lock reads FREE. It is
	// injected because sessionlock is shimclient's PEER, not its dependency;
	// nil means the caller supplied no death witness for an adopted shim, and
	// such a client then redials forever — never guessing death from a count.
	lockProbe func(ids.WorkspaceID) (bool, error)

	cmd    *exec.Cmd
	pgid   int
	stderr *ring

	link *linkFeed
	exit chan ExitInfo
	dead chan struct{}

	monitorCtx    context.Context
	cancelMonitor context.CancelFunc

	mu          sync.Mutex
	pid         int
	occupant    string
	detached    bool
	exited      bool
	attribution *KillAttribution
	exitInfo    *ExitInfo
}

// newClient builds an unstarted client for one shim socket.
func newClient(log dlog.Logger, ws ids.WorkspaceID, udsPath string, back backoff, probe func(ids.WorkspaceID) (bool, error)) *client {
	ctx, cancel := context.WithCancel(context.Background())
	return &client{
		log:           log,
		ws:            ws,
		udsPath:       udsPath,
		rpc:           shimv1connect.NewShimClient(newUDSClient(udsPath), udsBaseURL),
		back:          back,
		grace:         defaultKillGrace,
		lockProbe:     probe,
		link:          newLinkFeed(),
		exit:          make(chan ExitInfo, 1),
		dead:          make(chan struct{}),
		monitorCtx:    ctx,
		cancelMonitor: cancel,
	}
}

// ---- supervision ----

// PID is the supervised process's pid, or 0 when it is not known — an adopted
// shim whose lock holder yielded none.
func (c *client) PID() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.pid
}

// Exited yields exactly one ExitInfo when the process is gone, then closes.
func (c *client) Exited() <-chan ExitInfo { return c.exit }

// Connectivity yields every link state change: dialing, connected, redialing,
// dead.
func (c *client) Connectivity() <-chan LinkState { return c.link.states() }

// Occupy takes the in-memory occupancy guard, returning the release function.
// A second holder is REFUSED, named against the current one.
func (c *client) Occupy(holder string) (func(), error) {
	if holder == "" {
		return nil, invalid("Occupy", "holder", "is empty")
	}
	c.mu.Lock()
	if c.occupant != "" {
		current := c.occupant
		c.mu.Unlock()
		c.log.Warn("daemon.shimclient.occupy", "occupancy refused", dlog.Context{
			"workspace_id": string(c.ws), "holder": holder, "occupant": current,
		})
		return nil, &OccupiedError{Holder: current, Requested: holder}
	}
	c.occupant = holder
	c.mu.Unlock()

	c.log.Debug("daemon.shimclient.occupy", "occupancy taken", dlog.Context{
		"workspace_id": string(c.ws), "holder": holder,
	})
	var once sync.Once
	return func() {
		once.Do(func() {
			c.mu.Lock()
			c.occupant = ""
			c.mu.Unlock()
			c.log.Debug("daemon.shimclient.occupy", "occupancy released", dlog.Context{
				"workspace_id": string(c.ws), "holder": holder,
			})
		})
	}, nil
}

// Kill stops the process group, recording who asked and why: SIGTERM, a
// bounded wait, then SIGKILL, and the reaper decodes the exit either way.
func (c *client) Kill(attr KillAttribution) error {
	c.mu.Lock()
	switch {
	case c.detached:
		c.mu.Unlock()
		return ErrDetached
	case c.exited:
		c.mu.Unlock()
		c.log.Debug("daemon.shimclient.kill", "process already gone", dlog.Context{
			"workspace_id": string(c.ws), "actor": attr.Actor,
		})
		return nil
	case c.cmd == nil || c.pgid == 0:
		c.mu.Unlock()
		return ErrNoProcess
	}
	c.attribution = &attr
	pgid := c.pgid
	grace := c.grace
	c.mu.Unlock()

	c.log.Info("daemon.shimclient.kill", "stopping shim", dlog.Context{
		"workspace_id": string(c.ws), "pgid": pgid,
		"actor": attr.Actor, "reason": attr.Reason, "force": attr.Force,
	})

	if !attr.Force {
		if err := syscall.Kill(-pgid, syscall.SIGTERM); err != nil && !errors.Is(err, syscall.ESRCH) {
			c.log.Error("daemon.shimclient.kill", "SIGTERM failed", dlog.Context{
				"workspace_id": string(c.ws), "pgid": pgid, "error": err.Error(),
			})
			return fmt.Errorf("shimclient: SIGTERM %d: %w", pgid, err)
		}
		timer := time.NewTimer(grace)
		defer timer.Stop()
		select {
		case <-c.dead:
			return nil
		case <-timer.C:
			c.log.Warn("daemon.shimclient.kill", "graceful stop timed out; escalating", dlog.Context{
				"workspace_id": string(c.ws), "pgid": pgid, "grace_ms": grace.Milliseconds(),
			})
		}
	}

	if err := syscall.Kill(-pgid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
		c.log.Error("daemon.shimclient.kill", "SIGKILL failed", dlog.Context{
			"workspace_id": string(c.ws), "pgid": pgid, "error": err.Error(),
		})
		return fmt.Errorf("shimclient: SIGKILL %d: %w", pgid, err)
	}
	<-c.dead
	return nil
}

// Detach stops supervising while LEAVING THE PROCESS RUNNING — the handover's
// per-workspace transfer. Nothing is signaled and no exit is ever published.
func (c *client) Detach() {
	c.mu.Lock()
	if c.detached {
		c.mu.Unlock()
		return
	}
	c.detached = true
	pid := c.pid
	c.mu.Unlock()

	c.cancelMonitor()
	c.link.close()
	c.log.Info("daemon.shimclient.detach", "supervision handed over; process left running", dlog.Context{
		"workspace_id": string(c.ws), "pid": pid,
	})
}

// reap waits for the spawned process, decodes its exit, and publishes the
// evidence. It is the ONLY place a spawned shim's death is decided.
func (c *client) reap() {
	err := c.cmd.Wait()

	c.mu.Lock()
	if c.detached {
		c.mu.Unlock()
		return
	}
	c.mu.Unlock()

	info := ExitInfo{PID: c.PID(), Stderr: c.stderr.String()}
	switch {
	case err == nil:
		info.Code = 0
	default:
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			if status, ok := exitErr.Sys().(syscall.WaitStatus); ok {
				if status.Signaled() {
					info.Signal = status.Signal().String()
					info.Code = -1
				} else {
					info.Code = status.ExitStatus()
				}
			} else {
				info.Code = exitErr.ExitCode()
			}
		} else {
			info.Code = -1
			info.Stderr = info.Stderr + "\nwait failed: " + err.Error()
		}
	}
	c.publishExit(info)
}

// publishExit records the decoded exit once: the link goes dead, the exit
// channel yields it and closes, and every redial stops because the EVIDENCE
// says so.
func (c *client) publishExit(info ExitInfo) {
	c.mu.Lock()
	if c.exited || c.detached {
		c.mu.Unlock()
		return
	}
	c.exited = true
	info.Attribution = c.attribution
	c.exitInfo = &info
	c.mu.Unlock()

	ctx := dlog.Context{
		"workspace_id": string(c.ws), "pid": info.PID, "code": info.Code,
		"signal": info.Signal, "stderr": info.Stderr,
	}
	if info.Attribution != nil {
		ctx["actor"] = info.Attribution.Actor
		ctx["reason"] = info.Attribution.Reason
		c.log.Info("daemon.shimclient.exit", "supervised shim stopped as asked", ctx)
	} else {
		c.log.Error("daemon.shimclient.exit", "shim died", ctx)
	}

	close(c.dead)
	c.link.publish(LinkDead)
	c.link.close()
	c.exit <- info
	close(c.exit)
	c.cancelMonitor()
}

// exitedAlready reports whether death has already been decided.
func (c *client) exitedAlready() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.exited
}

// ---- bring-up and the redial loop ----

// openSupervisedSession opens the client's own session stream — the link's
// liveness evidence — with the open SELECTED against process death, because
// Connect's server-stream open blocks until the shim's first frame and a dead
// process must end the wait at once.
func (c *client) openSupervisedSession(parent context.Context) (Stream[*conversationv1.SessionUpdate], error) {
	ctx, cancel := context.WithCancel(parent)

	type opened struct {
		stream Stream[*conversationv1.SessionUpdate]
		err    error
	}
	result := make(chan opened, 1)
	go func() {
		stream, err := c.watchSession(ctx)
		result <- opened{stream: stream, err: err}
	}()

	select {
	case <-parent.Done():
		cancel()
		return nil, parent.Err()
	case <-c.dead:
		cancel()
		return nil, c.deathError()
	case r := <-result:
		if r.err != nil {
			cancel()
			return nil, r.err
		}
		return &cancelingStream[*conversationv1.SessionUpdate]{inner: r.stream, cancel: cancel}, nil
	}
}

// cancelingStream ties a stream's lifetime to the context that opened it, so
// closing the stream also releases the request.
type cancelingStream[T any] struct {
	inner  Stream[T]
	cancel context.CancelFunc
}

// Recv blocks for the next frame.
func (s *cancelingStream[T]) Recv() (T, error) { return s.inner.Recv() }

// Close ends the stream and cancels its request.
func (s *cancelingStream[T]) Close() {
	s.inner.Close()
	s.cancel()
}

// redial re-establishes the link to a still-running shim, FOREVER with capped
// backoff. It stops only when the evidence says the process is gone or the
// supervision context ends.
func (c *client) redial(ctx context.Context) (Stream[*conversationv1.SessionUpdate], error) {
	c.link.publish(LinkRedialing)
	for attempt := 0; ; attempt++ {
		if err := c.deathOrContext(ctx); err != nil {
			return nil, err
		}
		stream, err := c.openSupervisedSession(ctx)
		if err == nil {
			c.log.Info("daemon.shimclient.redial", "shim link re-established", dlog.Context{
				"uds": c.udsPath, "attempt": attempt,
			})
			c.link.publish(LinkConnected)
			return stream, nil
		}
		if stop := c.afterFailedDial(ctx, err, attempt); stop != nil {
			return nil, stop
		}
	}
}

// awaitHealthy consumes session frames until the first diagnostics arm says
// healthy. Unhealthy is an ANSWER, not readiness: the client keeps waiting.
func (c *client) awaitHealthy(ctx context.Context, stream Stream[*conversationv1.SessionUpdate]) error {
	frames, errs := recvLoop(stream)
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-c.dead:
			return c.deathError()
		case err := <-errs:
			return fmt.Errorf("shimclient: session stream ended during bring-up: %w", err)
		case update := <-frames:
			diagnostics := update.GetDiagnostics()
			if diagnostics == nil {
				continue
			}
			if diagnostics.GetHealthy() != nil {
				c.log.Info("daemon.shimclient.ready", "shim reported healthy", dlog.Context{
					"workspace_id": string(c.ws), "uds": c.udsPath,
				})
				return nil
			}
			c.log.Warn("daemon.shimclient.ready", "shim reported unhealthy; still waiting", dlog.Context{
				"workspace_id": string(c.ws), "faults": len(diagnostics.GetUnhealthy().GetFaults()),
			})
		}
	}
}

// monitor holds the session stream as the link's liveness evidence. A break
// while the process still lives is a REDIAL, forever, with capped backoff; a
// break with the process gone stops, because the evidence decided.
func (c *client) monitor(stream Stream[*conversationv1.SessionUpdate]) {
	ctx := c.monitorCtx
	for {
		frames, errs := recvLoop(stream)
		var broke error
	consume:
		for {
			select {
			case <-ctx.Done():
				stream.Close()
				return
			case <-c.dead:
				stream.Close()
				return
			case err := <-errs:
				broke = err
				break consume
			case <-frames:
				// The link is alive. Session facts reach their consumers on
				// their OWN WatchSession; this stream is the liveness evidence.
			}
		}
		stream.Close()
		if c.exitedAlready() || ctx.Err() != nil {
			return
		}
		c.log.Warn("daemon.shimclient.redial", "shim link broke; redialing", dlog.Context{
			"uds": c.udsPath, "error": errText(broke),
		})
		next, err := c.redial(ctx)
		if err != nil {
			c.log.Warn("daemon.shimclient.redial", "redial stopped", dlog.Context{
				"uds": c.udsPath, "error": err.Error(),
			})
			return
		}
		stream = next
	}
}

// witnessAdoptedDeath reports whether a dial failure is EVIDENCE that an
// adopted shim is gone: the socket refuses or is absent AND the workspace
// lock reads FREE. Without the injected witness nothing is concluded.
func (c *client) witnessAdoptedDeath(dialErr error) bool {
	c.mu.Lock()
	spawned := c.cmd != nil
	c.mu.Unlock()
	if spawned || c.lockProbe == nil || !isSocketGone(dialErr) {
		return false
	}
	free, err := c.lockProbe(c.ws)
	if err != nil {
		c.log.Warn("daemon.shimclient.redial", "lock probe could not tell; still redialing", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return false
	}
	if !free {
		return false
	}
	c.log.Error("daemon.shimclient.exit", "adopted shim is gone: socket refused and workspace lock free", dlog.Context{
		"workspace_id": string(c.ws), "uds": c.udsPath, "error": dialErr.Error(),
	})
	c.publishExit(ExitInfo{
		PID:    c.PID(),
		Code:   -1,
		Stderr: "adopted shim: no exit observed; socket refused and the workspace lock read free",
	})
	return true
}

// deathOrContext answers with the reason to stop before dialing again.
func (c *client) deathOrContext(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-c.dead:
		return c.deathError()
	default:
		return nil
	}
}

// deathError is the bring-up-ending error a dead process produces, carrying
// its exit decoding and stderr ring.
func (c *client) deathError() error {
	c.mu.Lock()
	info := c.exitInfo
	c.mu.Unlock()
	if info == nil {
		return errProcessDead
	}
	return &BringUpDeathError{Exit: *info}
}

// recvLoop pumps one stream into a frame channel and a one-shot error channel,
// so a blocking Recv can be SELECTED against process death.
func recvLoop[T any](stream Stream[T]) (<-chan T, <-chan error) {
	frames := make(chan T)
	errs := make(chan error, 1)
	go func() {
		for {
			frame, err := stream.Recv()
			if err != nil {
				errs <- err
				return
			}
			frames <- frame
		}
	}()
	return frames, errs
}

// isSocketGone reports whether a dial error means the listener is not there:
// ECONNREFUSED or ENOENT on the unix socket.
func isSocketGone(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, syscall.ECONNREFUSED) || errors.Is(err, syscall.ENOENT) || errors.Is(err, os.ErrNotExist) {
		return true
	}
	var opErr *net.OpError
	if errors.As(err, &opErr) {
		return errors.Is(opErr.Err, syscall.ECONNREFUSED) || errors.Is(opErr.Err, syscall.ENOENT)
	}
	return false
}

// errText renders a stream break for a log record; a producer-side end is
// io.EOF and says so.
func errText(err error) string {
	if err == nil {
		return ""
	}
	if errors.Is(err, io.EOF) {
		return "producer ended the stream"
	}
	return err.Error()
}

// ---- the verbs, 1:1 over the generated client ----

// StartSession starts or resumes the session. Session facts travel only here.
func (c *client) StartSession(ctx context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	return unary(ctx, c, "start_session", req, validateStartSessionRequest, c.rpc.StartSession)
}

// WatchSession opens the session update stream.
func (c *client) WatchSession(ctx context.Context) (Stream[*conversationv1.SessionUpdate], error) {
	return c.watchSession(ctx)
}

// watchSession is the one place a session stream is opened — the verb and the
// client's own liveness stream share it.
func (c *client) watchSession(ctx context.Context) (Stream[*conversationv1.SessionUpdate], error) {
	return openStream(ctx, c, "watch_session", &shimv1.WatchSessionRequest{}, nil, c.rpc.WatchSession,
		func(resp *shimv1.WatchSessionResponse) (*conversationv1.SessionUpdate, error) {
			update := resp.GetUpdate()
			if update == nil {
				return nil, invalid("WatchSessionResponse", "WatchSessionResponse.update", "is unset on a pushed frame")
			}
			return update, nil
		})
}

// SetSessionModel switches the session's model; the cold arm is an answer.
func (c *client) SetSessionModel(ctx context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error) {
	return unary(ctx, c, "set_session_model", req, validateSetSessionModelRequest, c.rpc.SetSessionModel)
}

// SetSessionPermissionMode switches the session's permission mode.
func (c *client) SetSessionPermissionMode(ctx context.Context, req *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error) {
	return unary(ctx, c, "set_session_permission_mode", req, validateSetSessionPermissionModeRequest, c.rpc.SetSessionPermissionMode)
}

// Hibernate stands the session down for the idle sweep.
func (c *client) Hibernate(ctx context.Context, req *shimv1.HibernateRequest) (*shimv1.HibernateResponse, error) {
	return unary(ctx, c, "hibernate", req, validateHibernateRequest, c.rpc.Hibernate)
}

// KillSession ends the session, gracefully unless forced.
func (c *client) KillSession(ctx context.Context, req *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error) {
	return unary(ctx, c, "kill_session", req, validateKillSessionRequest, c.rpc.KillSession)
}

// StartTurn opens a turn with the daemon's minted TurnId and its origin.
func (c *client) StartTurn(ctx context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	return unary(ctx, c, "start_turn", req, validateStartTurnRequest, c.rpc.StartTurn)
}

// WatchAgent opens one agent's frame stream, opening with a catch-up page.
func (c *client) WatchAgent(ctx context.Context, req *shimv1.WatchAgentRequest) (Stream[*shimv1.WatchAgentResponse], error) {
	return openStream(ctx, c, "watch_agent", req, validateWatchAgentRequest, c.rpc.WatchAgent,
		func(resp *shimv1.WatchAgentResponse) (*shimv1.WatchAgentResponse, error) {
			if resp.GetFrame() == nil {
				return nil, invalid("WatchAgentResponse", "WatchAgentResponse.frame", "oneof is unset on a pushed frame")
			}
			return resp, nil
		})
}

// UpdateAgent delivers an answer, a consent, a prompt or a stop to an agent.
func (c *client) UpdateAgent(ctx context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error) {
	return unary(ctx, c, "update_agent", req, validateUpdateAgentRequest, c.rpc.UpdateAgent)
}

// KillTurn interrupts the open turn.
func (c *client) KillTurn(ctx context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	return unary(ctx, c, "kill_turn", req, validateKillTurnRequest, c.rpc.KillTurn)
}

// WatchBash opens one detached shell's stream.
func (c *client) WatchBash(ctx context.Context, work *conversationv1.DetachedWorkId) (Stream[*conversationv1.AgentBash], error) {
	if err := validateDetachedWorkID("WatchBashRequest.work", work); err != nil {
		c.log.Error("daemon.shimclient.watch_bash", "invalid request", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, err
	}
	return openStream(ctx, c, "watch_bash", &shimv1.WatchBashRequest{Work: work}, nil, c.rpc.WatchBash,
		func(resp *shimv1.WatchBashResponse) (*conversationv1.AgentBash, error) {
			bash := resp.GetBash()
			if bash == nil {
				return nil, invalid("WatchBashResponse", "WatchBashResponse.bash", "is unset on a pushed frame")
			}
			return bash, nil
		})
}

// StopBash stops one detached shell.
func (c *client) StopBash(ctx context.Context, req *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error) {
	return unary(ctx, c, "stop_bash", req, validateStopBashRequest, c.rpc.StopBash)
}

// DetachForeground detaches a running foreground unit.
func (c *client) DetachForeground(ctx context.Context, req *shimv1.DetachForegroundRequest) (*shimv1.DetachForegroundResponse, error) {
	return unary(ctx, c, "detach_foreground", req, validateDetachForegroundRequest, c.rpc.DetachForeground)
}

// ReadHistory pages an agent's history without opening a watch.
func (c *client) ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	return unary(ctx, c, "read_history", req, validateReadHistoryRequest, c.rpc.ReadHistory)
}

// unary is every unary verb's body: validate through the message's base
// function, log the branch, call the generated client, log the outcome.
func unary[Req any, Resp any](
	ctx context.Context,
	c *client,
	verb string,
	req *Req,
	validate func(*Req) error,
	call func(context.Context, *connect.Request[Req]) (*connect.Response[Resp], error),
) (*Resp, error) {
	operation := "daemon.shimclient." + verb
	if err := validate(req); err != nil {
		c.log.Error(operation, "invalid request", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, err
	}
	c.log.Debug(operation, "calling shim", dlog.Context{"workspace_id": string(c.ws)})
	resp, err := call(ctx, connect.NewRequest(req))
	if err != nil {
		c.log.Error(operation, "shim call failed", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
			"connect_code": connect.CodeOf(err).String(),
		})
		return nil, err
	}
	c.log.Debug(operation, "shim answered", dlog.Context{"workspace_id": string(c.ws)})
	return resp.Msg, nil
}

// openStream is every watch verb's body. A Connect error on the OPEN is
// returned as an error from the call — never a stream that fails later.
func openStream[Req any, W any, T any](
	ctx context.Context,
	c *client,
	verb string,
	req *Req,
	validate func(*Req) error,
	open func(context.Context, *connect.Request[Req]) (*connect.ServerStreamForClient[W], error),
	project func(*W) (T, error),
) (Stream[T], error) {
	operation := "daemon.shimclient." + verb
	if validate != nil {
		if err := validate(req); err != nil {
			c.log.Error(operation, "invalid request", dlog.Context{
				"workspace_id": string(c.ws), "error": err.Error(),
			})
			return nil, err
		}
	}
	c.log.Debug(operation, "opening shim stream", dlog.Context{"workspace_id": string(c.ws)})
	stream, err := open(ctx, connect.NewRequest(req))
	if err != nil {
		c.log.Error(operation, "shim stream refused", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, &StreamOpenError{Procedure: verb, Err: err}
	}
	// Connect defers the open to the first Receive, so the refusal is only
	// visible once a frame is asked for: take the first frame here, so a
	// refused open IS an error from this call.
	if !stream.Receive() {
		err := stream.Err()
		_ = stream.Close()
		if err == nil {
			err = io.EOF
		}
		c.log.Error(operation, "shim stream refused", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, &StreamOpenError{Procedure: verb, Err: err}
	}
	first, err := project(stream.Msg())
	if err != nil {
		_ = stream.Close()
		c.log.Error(operation, "shim stream opened with an illegal frame", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, &StreamOpenError{Procedure: verb, Err: err}
	}
	c.log.Debug(operation, "shim stream opened", dlog.Context{"workspace_id": string(c.ws)})
	return &firstFrameStream[W, T]{
		first: first,
		inner: &mappedStream[W, T]{procedure: verb, stream: stream, project: project},
	}, nil
}

// firstFrameStream re-serves the frame the open consumed, so a refused open is
// an error from the Watch call without the consumer losing the opening frame.
type firstFrameStream[W any, T any] struct {
	once  sync.Once
	first T
	inner *mappedStream[W, T]
}

// Recv yields the opening frame first, then the stream's own.
func (s *firstFrameStream[W, T]) Recv() (T, error) {
	served := false
	s.once.Do(func() { served = true })
	if served {
		return s.first, nil
	}
	return s.inner.Recv()
}

// Close ends the stream from this side.
func (s *firstFrameStream[W, T]) Close() { s.inner.Close() }
