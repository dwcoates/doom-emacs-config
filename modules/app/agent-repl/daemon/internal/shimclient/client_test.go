package shimclient

import (
	"context"
	"errors"
	"io"
	"os/exec"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"connectrpc.com/connect"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sourcescan"
)

// newBareClient builds a client with no process and no connection, for the
// pieces that need neither.
func newBareClient() *client {
	return newClient(dlog.NewTestLogger(), ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
}

// TestOccupyRefusesASecondHolder asserts the occupancy guard admits one holder
// and names the current one in the refusal.
func TestOccupyRefusesASecondHolder(t *testing.T) {
	// Arrange.
	c := newBareClient()
	if _, err := c.Occupy("merge"); err != nil {
		t.Fatalf("first Occupy() error = %v", err)
	}

	// Act.
	_, err := c.Occupy("drain")

	// Assert.
	var occupied *OccupiedError
	if !errors.As(err, &occupied) {
		t.Fatalf("Occupy() error = %v, want *OccupiedError", err)
	}
	if occupied.Holder != "merge" || occupied.Requested != "drain" {
		t.Fatalf("refusal = %+v, want merge holding and drain refused", occupied)
	}
}

// TestOccupyAdmitsTheNextHolderAfterRelease asserts the release function frees
// the guard.
func TestOccupyAdmitsTheNextHolderAfterRelease(t *testing.T) {
	// Arrange.
	c := newBareClient()
	release, err := c.Occupy("merge")
	if err != nil {
		t.Fatalf("first Occupy() error = %v", err)
	}

	// Act.
	release()
	_, err = c.Occupy("drain")

	// Assert.
	if err != nil {
		t.Fatalf("Occupy() after release error = %v, want nil", err)
	}
}

// TestOccupyReleaseIsIdempotent asserts a doubled release cannot hand the
// guard away twice.
func TestOccupyReleaseIsIdempotent(t *testing.T) {
	// Arrange.
	c := newBareClient()
	release, err := c.Occupy("merge")
	if err != nil {
		t.Fatalf("Occupy() error = %v", err)
	}
	release()
	if _, err := c.Occupy("drain"); err != nil {
		t.Fatalf("second Occupy() error = %v", err)
	}

	// Act: the first holder's stale release must not evict the second.
	release()
	_, err = c.Occupy("rollout")

	// Assert.
	var occupied *OccupiedError
	if !errors.As(err, &occupied) {
		t.Fatalf("Occupy() error = %v, want *OccupiedError", err)
	}
	if occupied.Holder != "drain" {
		t.Fatalf("holder = %q, want \"drain\"", occupied.Holder)
	}
}

// TestOccupyRefusesAnEmptyHolder asserts an unnamed holder cannot take the
// guard.
func TestOccupyRefusesAnEmptyHolder(t *testing.T) {
	// Arrange.
	c := newBareClient()

	// Act.
	_, err := c.Occupy("")

	// Assert.
	var invalidErr *InvalidRequestError
	if !errors.As(err, &invalidErr) {
		t.Fatalf("Occupy(\"\") error = %v, want *InvalidRequestError", err)
	}
}

// TestKillReapsAndAttributes asserts a supervised stop is decoded, attributed,
// and never misread as a crash.
func TestKillReapsAndAttributes(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	record := sink.record(t)

	// Act.
	attr := KillAttribution{Actor: "workspace.kill", Reason: "user closed the workspace"}
	if err := client.Kill(context.Background(), attr); err != nil {
		t.Fatalf("Kill() error = %v", err)
	}

	// Assert.
	info := <-client.Exited()
	if info.PID != record.PID {
		t.Fatalf("exit pid = %d, want %d", info.PID, record.PID)
	}
	if info.Attribution == nil || info.Attribution.Actor != "workspace.kill" {
		t.Fatalf("attribution = %+v, want workspace.kill", info.Attribution)
	}
	if info.Signal != syscall.SIGTERM.String() {
		t.Fatalf("signal = %q, want %q", info.Signal, syscall.SIGTERM.String())
	}
}

// TestKillEscalatesToSigkill asserts a shim that ignores SIGTERM is killed
// after the bounded grace.
func TestKillEscalatesToSigkill(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIgnoreTerm)
	client := spawnReady(t, f, spec, WithKillGrace(50*time.Millisecond))
	_ = sink.record(t)

	// Act.
	if err := client.Kill(context.Background(), KillAttribution{Actor: "drain", Reason: "shutdown"}); err != nil {
		t.Fatalf("Kill() error = %v", err)
	}

	// Assert.
	info := <-client.Exited()
	if info.Signal != syscall.SIGKILL.String() {
		t.Fatalf("signal = %q, want %q", info.Signal, syscall.SIGKILL.String())
	}
}

// TestKillFinalCloseOfExited asserts Exited yields exactly one record and then
// closes.
func TestKillFinalCloseOfExited(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	if err := client.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "one record"}); err != nil {
		t.Fatalf("Kill() error = %v", err)
	}
	<-client.Exited()

	// Act.
	_, open := <-client.Exited()

	// Assert.
	if open {
		t.Fatal("Exited() yielded a second record; it must close after one")
	}
}

// TestCrashPublishesDeadWithoutAttribution asserts a death nobody asked for is
// reported as a crash, with the stderr ring as evidence.
func TestCrashPublishesDeadWithoutAttribution(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	record := sink.record(t)

	// Act: the shim dies on its own.
	if err := syscall.Kill(record.PID, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the child: %v", err)
	}

	// Assert.
	info := <-client.Exited()
	if info.Attribution != nil {
		t.Fatalf("attribution = %+v, want nil for a crash", info.Attribution)
	}
}

// TestRedialStopsOnDeath asserts the link goes dead and the connectivity feed
// ends when the process is gone — the evidence decides, never a retry count.
func TestRedialStopsOnDeath(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	record := sink.record(t)
	if got := collectStates(t, client, 2); got[0] != LinkDialing || got[1] != LinkConnected {
		t.Fatalf("states = %v, want dialing then connected", got)
	}

	// Act.
	if err := syscall.Kill(record.PID, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the child: %v", err)
	}

	// Assert.
	if got := collectStates(t, client, 1); got[0] != LinkDead {
		t.Fatalf("state = %v, want dead", got[0])
	}
	select {
	case _, open := <-client.Connectivity():
		if open {
			t.Fatal("a state arrived after dead; redialing did not stop")
		}
	case <-time.After(10 * time.Second):
		t.Fatal("the connectivity feed did not close after dead")
	}
}

// TestRedialAfterDroppedConnection asserts a broken link to a STILL-RUNNING
// shim is redialed, and the transitions say so.
func TestRedialAfterDroppedConnection(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	if got := collectStates(t, client, 2); got[0] != LinkDialing || got[1] != LinkConnected {
		t.Fatalf("states = %v, want dialing then connected", got)
	}

	// Act: the producer ends the session stream while the process lives on.
	f.dropSessions()
	waitForSessionOpen(t, f)
	f.push(healthyUpdate())

	// Assert.
	got := collectStates(t, client, 2)
	if got[0] != LinkRedialing || got[1] != LinkConnected {
		t.Fatalf("states = %v, want redialing then connected", got)
	}
}

// TestDetachLeavesTheProcessRunning asserts the handover's transfer: the
// client stops supervising and the shim keeps serving.
func TestDetachLeavesTheProcessRunning(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	record := sink.record(t)

	// Act.
	client.Detach()

	// Assert.
	if !alive(record.PID) {
		t.Fatalf("pid %d is gone; Detach must leave the process running", record.PID)
	}
	if err := client.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "after detach"}); !errors.Is(err, ErrDetached) {
		t.Fatalf("Kill() after Detach error = %v, want ErrDetached", err)
	}
	_ = syscall.Kill(record.PID, syscall.SIGKILL)
}

// TestDetachEndsTheConnectivityFeed asserts a detached client publishes no
// further link states.
func TestDetachEndsTheConnectivityFeed(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	record := sink.record(t)
	_ = collectStates(t, client, 2)

	// Act.
	client.Detach()

	// Assert.
	select {
	case _, open := <-client.Connectivity():
		if open {
			t.Fatal("a state arrived after Detach")
		}
	case <-time.After(10 * time.Second):
		t.Fatal("the connectivity feed did not close after Detach")
	}
	_ = syscall.Kill(record.PID, syscall.SIGKILL)
}

// TestUnaryVerbReachesTheShim asserts a valid request is forwarded 1:1.
func TestUnaryVerbReachesTheShim(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	_, err := client.StartTurn(context.Background(), validStartTurnRequest())

	// Assert.
	if err != nil {
		t.Fatalf("StartTurn() error = %v", err)
	}
	if got := f.count("StartTurn"); got != 1 {
		t.Fatalf("StartTurn calls = %d, want 1", got)
	}
}

// TestGatherTitleDigestReachesTheShim asserts the new unary verb dials the
// shim like every other, so the daemon can ask for a synthesized title's
// material.
func TestGatherTitleDigestReachesTheShim(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	_, err := client.GatherTitleDigest(context.Background(), &shimv1.GatherTitleDigestRequest{})

	// Assert.
	if err != nil {
		t.Fatalf("GatherTitleDigest() error = %v", err)
	}
	if got := f.count("GatherTitleDigest"); got != 1 {
		t.Fatalf("GatherTitleDigest calls = %d, want 1", got)
	}
}

// TestInvalidRequestNeverReachesTheShim asserts base-function validation
// refuses before the wire.
func TestInvalidRequestNeverReachesTheShim(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	_, err := client.StartTurn(context.Background(), &shimv1.StartTurnRequest{})

	// Assert.
	var invalidErr *InvalidRequestError
	if !errors.As(err, &invalidErr) {
		t.Fatalf("StartTurn() error = %v, want *InvalidRequestError", err)
	}
	if got := f.count("StartTurn"); got != 0 {
		t.Fatalf("StartTurn calls = %d, want 0", got)
	}
}

// TestRefusedStreamOpenIsAnError asserts a refused watch is an ERROR from the
// call, never a stream that fails later.
func TestRefusedStreamOpenIsAnError(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	f.watchAgentRefusal = connect.NewError(connect.CodeFailedPrecondition, errors.New("no such agent"))
	client := adoptReady(t, f, dir, uds)

	// Act.
	stream, err := client.WatchAgent(context.Background(), &shimv1.WatchAgentRequest{PageSize: DefaultPageSize})

	// Assert.
	var refused *StreamOpenError
	if !errors.As(err, &refused) {
		t.Fatalf("WatchAgent() error = %v, want *StreamOpenError", err)
	}
	if stream != nil {
		t.Fatal("WatchAgent() returned a stream alongside the refusal")
	}
}

// TestStreamRecvEOFOnlyOnProducerEnd asserts Recv reports io.EOF exactly when
// the producer ended the stream, leaving the meaning to the consumer.
func TestStreamRecvEOFOnlyOnProducerEnd(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	f.watchAgentFrames = []*shimv1.WatchAgentResponse{{
		Frame: &shimv1.WatchAgentResponse_Page{Page: &conversationv1.HistoryPage{}},
	}}
	client := adoptReady(t, f, dir, uds)
	stream, err := client.WatchAgent(context.Background(), &shimv1.WatchAgentRequest{PageSize: DefaultPageSize})
	if err != nil {
		t.Fatalf("WatchAgent() error = %v", err)
	}
	t.Cleanup(stream.Close)

	// Act.
	if _, err := stream.Recv(); err != nil {
		t.Fatalf("first Recv() error = %v, want the opening page", err)
	}
	_, err = stream.Recv()

	// Assert.
	if !errors.Is(err, io.EOF) {
		t.Fatalf("second Recv() error = %v, want io.EOF", err)
	}
}

// TestWatchBashValidatesItsHandle asserts the bash watch's own argument is
// validated like every request body.
func TestWatchBashValidatesItsHandle(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	_, err := client.WatchBash(context.Background(), &conversationv1.DetachedWorkId{})

	// Assert.
	var invalidErr *InvalidRequestError
	if !errors.As(err, &invalidErr) {
		t.Fatalf("WatchBash() error = %v, want *InvalidRequestError", err)
	}
	if got := f.count("WatchBash"); got != 0 {
		t.Fatalf("WatchBash calls = %d, want 0", got)
	}
}

// TestSessionStreamRaisesOnAnUnsetPush asserts an unset non-optional field on
// a pushed frame is raised loudly rather than folded into a zero value.
func TestSessionStreamRaisesOnAnUnsetPush(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	type opened struct {
		stream Stream[*shimv1.WatchSessionResponse]
		err    error
	}
	done := make(chan opened, 1)
	go func() {
		s, err := client.WatchSession(context.Background())
		done <- opened{stream: s, err: err}
	}()
	waitForSessionOpen(t, f)
	f.push(healthyUpdate())
	r := <-done
	if r.err != nil {
		t.Fatalf("WatchSession() error = %v", r.err)
	}
	t.Cleanup(r.stream.Close)
	if _, err := r.stream.Recv(); err != nil {
		t.Fatalf("first Recv() error = %v, want the opening frame", err)
	}

	// Act: an update-less frame is illegal on this stream.
	f.push(nil)
	_, err := r.stream.Recv()

	// Assert.
	var invalidErr *InvalidRequestError
	if !errors.As(err, &invalidErr) {
		t.Fatalf("Recv() error = %v, want *InvalidRequestError", err)
	}
}

// TestAdoptedDeathIsWitnessedByTheWorkspaceLock asserts an adopted shim — one
// with no child process to reap — is declared dead only on EVIDENCE: the
// socket is gone AND the workspace lock reads free.
func TestAdoptedDeathIsWitnessedByTheWorkspaceLock(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds, WithLockProbe(func(string) (bool, error) { return true, nil }))
	if got := collectStates(t, client, 2); got[0] != LinkDialing || got[1] != LinkConnected {
		t.Fatalf("states = %v, want dialing then connected", got)
	}

	// Act: the shim's socket goes away under a client that never spawned it.
	f.stop()

	// Assert.
	got := collectStates(t, client, 2)
	if got[0] != LinkRedialing || got[1] != LinkDead {
		t.Fatalf("states = %v, want redialing then dead", got)
	}
	info := <-client.Exited()
	if info.Attribution != nil {
		t.Fatalf("attribution = %+v, want nil: nobody asked for this stop", info.Attribution)
	}
}

// TestAdoptedDeathIsNotConcludedWhileTheLockIsHeld asserts a held lock is not
// death: the client keeps redialing.
func TestAdoptedDeathIsNotConcludedWhileTheLockIsHeld(t *testing.T) {
	// Arrange.
	c := newBareClient()
	c.lockProbe = func(ids.WorkspaceID) (bool, error) { return false, nil }

	// Act.
	witnessed := c.witnessAdoptedDeath(syscall.ECONNREFUSED)

	// Assert.
	if witnessed {
		t.Fatal("witnessAdoptedDeath() = true while the lock is held")
	}
}

// TestAdoptedDeathIsNotConcludedFromAnUnreadableLock asserts a probe that
// could not tell is never read as death.
func TestAdoptedDeathIsNotConcludedFromAnUnreadableLock(t *testing.T) {
	// Arrange.
	c := newBareClient()
	c.lockProbe = func(ids.WorkspaceID) (bool, error) { return false, errors.New("permission denied") }

	// Act.
	witnessed := c.witnessAdoptedDeath(syscall.ECONNREFUSED)

	// Assert.
	if witnessed {
		t.Fatal("witnessAdoptedDeath() = true from a probe that could not tell")
	}
}

// TestAdoptedDeathNeedsTheSocketToBeGone asserts a link break that is not the
// socket disappearing is never death, however free the lock reads.
func TestAdoptedDeathNeedsTheSocketToBeGone(t *testing.T) {
	// Arrange.
	c := newBareClient()
	c.lockProbe = func(ids.WorkspaceID) (bool, error) { return true, nil }

	// Act.
	witnessed := c.witnessAdoptedDeath(io.ErrUnexpectedEOF)

	// Assert.
	if witnessed {
		t.Fatal("witnessAdoptedDeath() = true without the socket being gone")
	}
}

// TestSpawnedClientNeverWitnessesDeathFromTheLock asserts a SPAWNED shim's
// death comes from its exit, never from a lock probe: the reaper is the only
// authority when a child exists.
func TestSpawnedClientNeverWitnessesDeathFromTheLock(t *testing.T) {
	// Arrange.
	c := newBareClient()
	c.lockProbe = func(ids.WorkspaceID) (bool, error) { return true, nil }
	c.cmd = &exec.Cmd{}

	// Act.
	witnessed := c.witnessAdoptedDeath(syscall.ECONNREFUSED)

	// Assert.
	if witnessed {
		t.Fatal("witnessAdoptedDeath() = true for a spawned shim")
	}
}

// TestKillOnlyEverSignalsOurOwnChild asserts a kill can reach the supervised
// child's group and NOTHING else: a live child dies, a child that is already
// reaped is success with no failure recorded, and a pid the client never owned
// is refused loudly without a signal leaving the daemon.
func TestKillOnlyEverSignalsOurOwnChild(t *testing.T) {
	tests := []struct {
		name string
		// arrange yields the kill under test and, when the case has one, a
		// witness asserted after the kill returned.
		arrange func(t *testing.T) (kill func() error, witness func(t *testing.T))
		wantErr error
	}{
		{
			name: "a live child is killed",
			arrange: func(t *testing.T) (func() error, func(*testing.T)) {
				dir := shortDir(t)
				f, uds := startFakeShim(t, dir)
				spec, sink := newTestSpec(t, dir, uds, helperIdle)
				c := spawnReady(t, f, spec)
				record := sink.record(t)
				kill := func() error {
					return c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "live child"})
				}
				return kill, func(t *testing.T) {
					info := <-c.Exited()
					if info.PID != record.PID {
						t.Fatalf("exit pid = %d, want %d", info.PID, record.PID)
					}
				}
			},
		},
		{
			name: "an already reaped child is success",
			arrange: func(t *testing.T) (func() error, func(*testing.T)) {
				dir := shortDir(t)
				f, uds := startFakeShim(t, dir)
				spec, sink := newTestSpec(t, dir, uds, helperIdle)
				c := spawnReady(t, f, spec)
				record := sink.record(t)
				if err := syscall.Kill(record.PID, syscall.SIGKILL); err != nil {
					t.Fatalf("external SIGKILL: %v", err)
				}
				<-c.Exited()
				kill := func() error {
					return c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "already gone"})
				}
				return kill, nil
			},
		},
		{
			name: "a pid we never owned is refused",
			arrange: func(t *testing.T) (func() error, func(*testing.T)) {
				stranger := exec.Command("/bin/sh", "-c", "sleep 30")
				stranger.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
				if err := stranger.Start(); err != nil {
					t.Fatalf("start the stranger: %v", err)
				}
				t.Cleanup(func() {
					_ = syscall.Kill(-stranger.Process.Pid, syscall.SIGKILL)
					_ = stranger.Wait()
				})

				// A child of our own, so the client holds a real handle whose
				// pid is NOT the pgid it was asked to signal.
				ours := exec.Command("/bin/sh", "-c", "sleep 30")
				ours.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
				if err := ours.Start(); err != nil {
					t.Fatalf("start our own child: %v", err)
				}
				t.Cleanup(func() {
					_ = syscall.Kill(-ours.Process.Pid, syscall.SIGKILL)
					_ = ours.Wait()
				})

				c := newBareClient()
				c.cmd = ours
				c.pid = ours.Process.Pid
				c.pgid = stranger.Process.Pid

				kill := func() error {
					return c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "not ours", Force: true})
				}
				return kill, func(t *testing.T) {
					if err := syscall.Kill(stranger.Process.Pid, 0); err != nil {
						t.Fatalf("the stranger was signaled: probe error = %v", err)
					}
				}
			},
			wantErr: ErrNoProcess,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			kill, witness := tc.arrange(t)

			// Act.
			err := kill()

			// Assert.
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("Kill() error = %v, want %v", err, tc.wantErr)
			}
			if witness != nil {
				witness(t)
			}
		})
	}
}

// TestGracefulKillLeavesAHealthyShimUnescalated asserts the ordinary case the
// grace exists for: a shim that answers SIGTERM inside the grace is never
// SIGKILLed, even when the caller holds only the bound a graceful stop is
// promised — GracefulKillBound itself.
func TestGracefulKillLeavesAHealthyShimUnescalated(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	ctx, cancel := context.WithTimeout(context.Background(), GracefulKillBound)
	defer cancel()

	// Act.
	if err := client.Kill(ctx, KillAttribution{Actor: "drain", Reason: "idle sweep"}); err != nil {
		t.Fatalf("Kill() error = %v", err)
	}

	// Assert.
	info := <-client.Exited()
	if info.Signal != syscall.SIGTERM.String() {
		t.Fatalf("signal = %q, want %q: a shim that left on SIGTERM must never be escalated", info.Signal, syscall.SIGTERM.String())
	}
}

// TestGracefulKillEscalatesInsideTheCallersStandBound asserts the nesting the
// stand bound promises: a shim that ignores SIGTERM is SIGKILLed and REPORTED
// as stopped, on a caller holding drain.DefaultStandBound's own budget for the
// process stop — GracefulKillBound. The two used to be the same 5s, so the
// caller gave up at the exact instant the escalation fired and read a shim it
// was still stopping as leaked.
func TestGracefulKillEscalatesInsideTheCallersStandBound(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIgnoreTerm)
	client := spawnReady(t, f, spec)
	// THE RECORD IS THE DISPOSITION'S RECEIPT: the helper installs its SIGTERM
	// ignore before it writes, so a parent that has read it knows the ignore is
	// already in place and the SIGTERM below cannot win a race against it.
	_ = sink.record(t)
	ctx, cancel := context.WithTimeout(context.Background(), GracefulKillBound)
	defer cancel()

	// Act.
	err := client.Kill(ctx, KillAttribution{Actor: "drain", Reason: "idle sweep"})

	// Assert.
	if err != nil {
		t.Fatalf("Kill() error = %v, want nil: the escalation must land inside the caller's bound", err)
	}
	info := <-client.Exited()
	if info.Signal != syscall.SIGKILL.String() {
		t.Fatalf("signal = %q, want %q", info.Signal, syscall.SIGKILL.String())
	}
}

// TestKillAnswersItsCallerWhenTheContextEndsInsideTheGrace asserts the one
// thing that is cancellable and the one thing that is not: a caller whose bound
// expires mid-grace is answered at once, with the expiry named, AND the process
// is still SIGKILLed and still reaped. Kill took no context at all before this,
// so the caller sat through the whole grace whatever its budget said.
func TestKillAnswersItsCallerWhenTheContextEndsInsideTheGrace(t *testing.T) {
	// Arrange. The grace is far longer than the caller's bound, so an answer
	// that arrives promptly can only have come from the context.
	const grace = 30 * time.Second
	const callerBound = 50 * time.Millisecond
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIgnoreTerm)
	client := spawnReady(t, f, spec, WithKillGrace(grace))
	// See the receipt note above: the ignore is in place once the record is.
	_ = sink.record(t)
	ctx, cancel := context.WithTimeout(context.Background(), callerBound)
	defer cancel()

	// Act.
	started := time.Now()
	err := client.Kill(ctx, KillAttribution{Actor: "drain", Reason: "idle sweep"})
	answered := time.Since(started)

	// Assert.
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("Kill() error = %v, want a context.DeadlineExceeded", err)
	}
	if answered >= grace {
		t.Fatalf("Kill() answered after %v; it must answer on the caller's %v bound, not the grace", answered, callerBound)
	}
	// THE REAP IS NOT CANCELLABLE. The wait ended; the wait status is still
	// collected, or the daemon accumulates a zombie per abandoned kill.
	info := <-client.Exited()
	if info.Signal != syscall.SIGKILL.String() {
		t.Fatalf("signal = %q, want %q: the escalation must go out even when the caller has stopped waiting", info.Signal, syscall.SIGKILL.String())
	}
}

// TestFaultKindNamesEveryArm asserts the one spelling of the SessionFault arm
// names covers each arm and never drops an unrecognized one.
func TestFaultKindNamesEveryArm(t *testing.T) {
	tests := []struct {
		name  string
		fault *conversationv1.SessionFault
		want  string
	}{
		{"store", &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_StoreUnreachable{}}, "store_unreachable"},
		{"converter", &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_ConverterDefect{}}, "converter_defect"},
		{"log sink", &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_LogSinkPoisoned{}}, "log_sink_poisoned"},
		{"keepalive", &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_KeepaliveFailed{}}, "keepalive_failed"},
		{"vendor query", &conversationv1.SessionFault{Kind: &conversationv1.SessionFault_VendorQueryFailed{}}, "vendor_query_failed"},
		{"no arm set", &conversationv1.SessionFault{}, "unclassified"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act.
			got := FaultKind(tt.fault)

			// Assert.
			if got != tt.want {
				t.Fatalf("FaultKind() = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestFaultKindsJoinsEveryFault asserts the kind list and the fault count
// always agree, so a record's fault_kinds never under-reports what the shim
// is standing on.
func TestFaultKindsJoinsEveryFault(t *testing.T) {
	// Arrange.
	faults := []*conversationv1.SessionFault{
		{Kind: &conversationv1.SessionFault_StoreUnreachable{}},
		{},
		{Kind: &conversationv1.SessionFault_KeepaliveFailed{}},
	}

	// Act.
	got := FaultKinds(faults)

	// Assert.
	if want := "store_unreachable,unclassified,keepalive_failed"; got != want {
		t.Fatalf("FaultKinds() = %q, want %q", got, want)
	}
}

// TestFaultKindsOfNoFaultsIsEmpty asserts the empty case is an empty string
// rather than a stray separator.
func TestFaultKindsOfNoFaultsIsEmpty(t *testing.T) {
	// Arrange, Act.
	got := FaultKinds(nil)

	// Assert.
	if got != "" {
		t.Fatalf("FaultKinds(nil) = %q, want the empty string", got)
	}
}

// TestStandingDownIsFalseUntilAKillSessionIsAsked pins the latch's resting
// state: nothing has been asked of a fresh client, so nothing it does may be
// read as this daemon's own teardown.
func TestStandingDownIsFalseUntilAKillSessionIsAsked(t *testing.T) {
	// Arrange.
	c := newBareClient()

	// Act.
	got := c.StandingDown()

	// Assert.
	if got {
		t.Fatal("a client nobody has asked to stand down reports StandingDown() = true")
	}
}

// TestPublishExitRecordsAnAskedForCleanExitAsOrderly covers the exit route with
// NO attribution: the shim ends its own process when it answers KillSession, so
// every graceful stand-down in this daemon reaches the exit with nothing having
// called Kill. Recorded as a death, an orderly relaunch bounce cost the
// realtest sweep an ERROR for a teardown the daemon itself ordered.
func TestPublishExitRecordsAnAskedForCleanExitAsOrderly(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.standDown.Store(true)

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: 0})

	// Assert.
	if hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("a clean exit inside an asked-for stand-down was recorded as a death")
	}
	if !hasRecordAt(log, "info", "daemon.shimclient.exit") {
		t.Fatal("the orderly stand-down exit was not recorded at all")
	}
}

// TestPublishExitRecordsAnUnaskedCleanExitAsADeath is the other half of the
// distinction: a shim that exits with nobody having asked it to is gone for a
// reason this daemon does not know, whatever its exit code.
func TestPublishExitRecordsAnUnaskedCleanExitAsADeath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: 0})

	// Assert.
	if !hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("an exit nobody asked for was not recorded as a death")
	}
}

// TestPublishExitRecordsAnAskedForNonzeroExitAsADeath pins that the ASK alone
// never quiets an exit: a stood-down shim that leaves nonzero failed on its way
// out, and the failure is the whole of what the log is for.
func TestPublishExitRecordsAnAskedForNonzeroExitAsADeath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.standDown.Store(true)

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: 3})

	// Assert.
	if !hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("a nonzero exit inside a stand-down was not recorded as a death")
	}
}

// TestPublishExitRecordsAnAskedForSignalledExitAsADeath is the same rule for
// the other way an exit is not clean: a shim asked to stand down gracefully and
// then SIGKILLed did not do what it was asked.
func TestPublishExitRecordsAnAskedForSignalledExitAsADeath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.standDown.Store(true)

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: 0, Signal: "killed"})

	// Assert.
	if !hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("a signalled exit inside a stand-down was not recorded as a death")
	}
}

// hasRecordAt answers whether the test logger holds a record at one level for
// one operation.
func hasRecordAt(log *dlog.TestLogger, level, operation string) bool {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

// TestKillArmsTheStandDownLatchBeforeTheProcessIsEnded pins the invariant the
// forget path needed: a kill is a teardown THIS DAEMON ordered, so the latch
// every consumer reads is armed by the process kill and not only by the
// KillSession rpc. Forget stands a live session down through `Fleet.Stop`,
// which calls Kill, and an adopted workspace forgotten that way recorded an
// `daemon.shimclient.exit` ERROR for a departure the daemon itself asked for.
//
// Every shape Kill can meet is a row, because each of them is still an ask.
func TestKillArmsTheStandDownLatchBeforeTheProcessIsEnded(t *testing.T) {
	tests := []struct {
		name  string
		build func(t *testing.T) *client
	}{
		{
			name: "a spawned client whose process is already gone",
			build: func(t *testing.T) *client {
				c := newBareClient()
				c.mu.Lock()
				c.exited = true
				c.mu.Unlock()
				return c
			},
		},
		{
			name: "a spawned client with no process group to signal",
			build: func(t *testing.T) *client {
				c := newBareClient()
				c.cmd = &exec.Cmd{}
				return c
			},
		},
		{
			name: "an adopted client whose socket is already gone",
			build: func(t *testing.T) *client {
				return newClient(dlog.NewTestLogger(), ids.WorkspaceID("ws-1"),
					filepath.Join(shortDir(t), "absent.sock"), defaultBackoff, nil, nil)
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := tc.build(t)

			// Act.
			_ = c.Kill(context.Background(), KillAttribution{
				Actor: "workspace.stop", Reason: "the workspace was forgotten", Force: true,
			})

			// Assert.
			if !c.StandingDown() {
				t.Fatal("Kill left the stand-down latch unarmed; the departure it ordered will read as unasked")
			}
		})
	}
}

// TestKillOfADetachedClientArmsNoStandDown is the one shape that is NOT an ask.
// A handover left the process running and this client no longer supervises it,
// so the kill is refused and nothing about that process was ordered by us.
func TestKillOfADetachedClientArmsNoStandDown(t *testing.T) {
	// Arrange.
	c := newBareClient()
	c.Detach()

	// Act.
	err := c.Kill(context.Background(), KillAttribution{Actor: "workspace.stop", Reason: "forgotten"})

	// Assert.
	if !errors.Is(err, ErrDetached) {
		t.Fatalf("Kill() error = %v, want ErrDetached", err)
	}
	if c.StandingDown() {
		t.Fatal("a refused kill of a detached client armed the stand-down latch")
	}
}

// TestAnAdoptedShimsDepartureIsLoudOnlyWhenNobodyAskedForIt covers the record
// the realtest harvest failed on: forgetting a live ADOPTED workspace ends the
// very socket the redial ladder is dialing, and the witness recorded that as a
// shim that went missing. An unasked disappearance is unchanged.
func TestAnAdoptedShimsDepartureIsLoudOnlyWhenNobodyAskedForIt(t *testing.T) {
	tests := []struct {
		name      string
		standDown bool
		wantError bool
	}{
		{name: "the daemon asked this shim to go", standDown: true, wantError: false},
		{name: "nobody asked this shim to go", standDown: false, wantError: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff,
				func(ids.WorkspaceID) (bool, error) { return true, nil }, nil)
			c.standDown.Store(tc.standDown)

			// Act.
			witnessed := c.witnessAdoptedDeath(syscall.ECONNREFUSED)

			// Assert.
			if !witnessed {
				t.Fatal("witnessAdoptedDeath() = false with the socket gone and the lock free")
			}
			if got := hasRecordAt(log, "error", "daemon.shimclient.exit"); got != tc.wantError {
				t.Fatalf("an error record at daemon.shimclient.exit = %v, want %v", got, tc.wantError)
			}
		})
	}
}

// TestPublishExitRecordsAnInferredDepartureInsideAStandDownAsOrderly pins the
// half of that record that lives in publishExit. An adopted shim is not this
// daemon's child, so its exit carries the -1 SENTINEL rather than a wait
// status; read as a status it says "signalled death" about a shim that left
// exactly as asked.
func TestPublishExitRecordsAnInferredDepartureInsideAStandDownAsOrderly(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.standDown.Store(true)

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: -1, Inferred: true})

	// Assert.
	if hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("an inferred departure inside an asked-for stand-down was recorded as a death")
	}
}

// TestPublishExitRecordsAnUnaskedInferredDepartureAsADeath is that rule's other
// half: a shim that vanished with nobody having asked it to is gone for a
// reason this daemon does not know, and no absence of a wait status quiets it.
func TestPublishExitRecordsAnUnaskedInferredDepartureAsADeath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: -1, Inferred: true})

	// Assert.
	if !hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("an inferred departure nobody asked for was not recorded as a death")
	}
}

// TestTheRedialLadderEndsLoudlyOnlyWhenNobodyAskedForTheTeardown covers the
// other pair of records the harvest carried: two `daemon.shimclient.redial`
// WARNs for a stand-down the daemon ordered.
//
// THE LATCH IS RE-READ WHERE THE LADDER STOPS, and that is a different read
// from the gate at the top of the monitor loop. The break came first and the
// ask came after it, which is the ordering a forget produces: the shim link
// had already broken, the ladder was climbing against a held workspace lock,
// and the forget then stood the session down and freed that lock.
func TestTheRedialLadderEndsLoudlyOnlyWhenNobodyAskedForTheTeardown(t *testing.T) {
	tests := []struct {
		name      string
		standDown bool
		wantWarn  bool
	}{
		{name: "the daemon asked while the ladder was climbing", standDown: true, wantWarn: false},
		{name: "nobody asked at all", standDown: false, wantWarn: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the stream is already broken, and the lock probe is the
			// seam at which the teardown arrives — AFTER the top gate has been
			// passed, and freeing the very lock the ladder was waiting on.
			log := dlog.NewTestLogger()
			c := newClient(log, ids.WorkspaceID("ws-1"), filepath.Join(shortDir(t), "absent.sock"),
				backoff{Initial: time.Millisecond, Max: 2 * time.Millisecond, Factor: 1}, nil, nil)
			c.lockProbe = func(ids.WorkspaceID) (bool, error) {
				c.standDown.Store(tc.standDown)
				return true, nil
			}
			frames := make(chan *shimv1.WatchSessionResponse)
			errs := make(chan error, 1)
			errs <- io.ErrUnexpectedEOF

			// Act.
			runMonitorToCompletion(t, c, frames, errs)

			// Assert.
			if got := hasRecordSaying(log, "warn", "daemon.shimclient.redial", "redial stopped"); got != tc.wantWarn {
				t.Fatalf("a warn record saying the redial stopped = %v, want %v", got, tc.wantWarn)
			}
		})
	}
}

// runMonitorToCompletion drives the liveness monitor until it returns, bounded
// so a monitor that never stops fails the test instead of hanging the package.
// The bound is a wide multiple of a run that costs two capped 2ms backoffs and
// one refused dial to a socket that does not exist.
func runMonitorToCompletion(t *testing.T, c *client, frames chan *shimv1.WatchSessionResponse, errs chan error) {
	t.Helper()

	done := make(chan struct{})
	go func() {
		defer close(done)
		c.monitor(&inertSessionStream{}, frames, errs)
	}()
	select {
	case <-done:
	case <-time.After(5 * time.Second):
		t.Fatal("the liveness monitor never returned")
	}
}

// inertSessionStream is a broken session stream: the monitor only ever closes
// it, because the break is delivered on the error channel beside it.
type inertSessionStream struct{}

func (s *inertSessionStream) Recv() (*shimv1.WatchSessionResponse, error) { return nil, io.EOF }
func (s *inertSessionStream) Close()                                      {}

// hasRecordSaying answers whether the test logger holds a record at one level
// for one operation whose message is exactly the one named.
func hasRecordSaying(log *dlog.TestLogger, level, operation, message string) bool {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation && r.Message == message {
			return true
		}
	}
	return false
}

// ---- a call that died in a teardown this daemon ordered ----

// TestAUnaryCallInsideAnOrderedStandDownIsNotAFault covers every unary verb at
// once, because `unary` is the one body they all share. The latch is the
// shim's own record that this daemon asked it to end, so a call still in
// flight to that shim comes back unavailable for the plainest of reasons: the
// caller killed the peer. The call still FAILS -- the error is returned and
// wrapped so the caller can tell the two apart -- and only the record's level
// says which of the two happened.
//
// MEASURED, realtest run 2026-09-13T16:20:34. A deploy's SIGTERM landed inside
// the boot's own bring-up on three consecutive daemon generations; the drain
// force-stopped the workspace's shim and the in-flight StartSession answered
// `unavailable: unexpected EOF`, recorded as `daemon.shimclient.start_session:
// shim call failed` at ERROR nine milliseconds after the same process recorded
// its own `daemon.shimclient.standdown` at info.
func TestAUnaryCallInsideAnOrderedStandDownIsNotAFault(t *testing.T) {
	tests := []struct {
		name      string
		standDown bool
		wantLevel string
		wantWrap  bool
	}{
		{
			name:      "the daemon stood the shim down",
			standDown: true,
			wantLevel: "info",
			wantWrap:  true,
		},
		{
			name:      "nobody asked the shim to stand down",
			standDown: false,
			wantLevel: "error",
			wantWrap:  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a client whose socket nothing is listening on, so the
			// call fails at the transport exactly as a killed shim's does.
			log := dlog.NewTestLogger()
			c := newClient(log, ids.WorkspaceID("ws-1"),
				filepath.Join(t.TempDir(), "absent.sock"), defaultBackoff, nil, nil)
			if tt.standDown {
				c.standDown.Store(true)
			}
			req := &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{
				Fresh: &shimv1.StartSessionFresh{PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_Plan{Plan: &conversationv1.AgentPermissionModePlan{}},
				}},
			}}

			// Act.
			_, err := c.StartSession(context.Background(), req)

			// Assert.
			if err == nil {
				t.Fatal("the call answered success against a socket nothing is listening on")
			}
			if got := errors.Is(err, ErrStandDownOrdered); got != tt.wantWrap {
				t.Fatalf("errors.Is(err, ErrStandDownOrdered) = %v, want %v: %v", got, tt.wantWrap, err)
			}
			if !hasRecordAt(log, tt.wantLevel, "daemon.shimclient.start_session") {
				t.Fatalf("no %q record for the failed call: %+v", tt.wantLevel, log.Records())
			}
			other := map[bool]string{true: "error", false: "info"}[tt.standDown]
			if hasRecordAt(log, other, "daemon.shimclient.start_session") {
				t.Fatalf("the failed call was ALSO recorded at %q: %+v", other, log.Records())
			}
		})
	}
}

// TestStandDownArmsTheLatchForTheDaemonsOwnTeardown covers the ask that never
// reaches the shim. A KillSession that does not answer is escalated to a
// process stop by the daemon itself, and arming the latch is what lets the
// exit watcher, the redialer and the adopted-death witness read that departure
// as ordinary. A DETACHED client arms nothing: that process is the successor
// daemon's, so this one is ordering no teardown of it.
func TestStandDownArmsTheLatchForTheDaemonsOwnTeardown(t *testing.T) {
	tests := []struct {
		name      string
		detach    bool
		wantArmed bool
	}{
		{name: "this daemon is ordering the teardown", detach: false, wantArmed: true},
		{name: "the process belongs to the successor daemon", detach: true, wantArmed: false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c := newClient(dlog.NewTestLogger(), ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
			if tt.detach {
				c.Detach()
			}

			// Act.
			armed := c.StandDown()

			// Assert.
			if armed != tt.wantArmed {
				t.Fatalf("StandDown() = %v, want %v", armed, tt.wantArmed)
			}
			if c.StandingDown() != tt.wantArmed {
				t.Fatalf("StandingDown() = %v, want %v", c.StandingDown(), tt.wantArmed)
			}
		})
	}
}

// TestStandingDownReadsTheDaemonsLatchToo is the invariant the immediate
// shutdown needs: the question every consumer asks is whether THIS DAEMON
// ordered the departure, not whether this particular client was the one
// asked. A supervisor that has begun standing down is ending every shim it
// holds, including the clients no teardown walk can name.
func TestStandingDownReadsTheDaemonsLatchToo(t *testing.T) {
	// Arrange.
	c := newBareClient()
	daemon := false
	c.daemonStandDown = func() bool { return daemon }

	// Act.
	daemon = true

	// Assert.
	if !c.StandingDown() {
		t.Fatal("a client of a daemon that is standing down reports StandingDown() = false")
	}
}

// TestPublishExitRecordsADeparturenInsideTheDaemonsStandDownAsOrderly is the
// measured case: one shim ended up with two clients (the supervisor's spawn
// record and the fleet's adopted one) after a refused StartSession left it
// serving. The sweep armed the spawn record's own latch and recorded the exit
// at INFO; the adopted client, armed by nothing, recorded the very same
// departure as `daemon.shimclient.exit` ERROR "shim died".
func TestPublishExitRecordsADepartureInsideTheDaemonsStandDownAsOrderly(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.daemonStandDown = func() bool { return true }

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: -1, Inferred: true})

	// Assert.
	if hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("a departure inside the daemon's own stand-down was recorded as a death")
	}
}

// TestPublishExitOutsideTheDaemonsStandDownStaysADeath is that rule's other
// half, and the reason the latch is read rather than assumed: a daemon that is
// NOT standing down has ordered nothing, so an inferred departure is still a
// shim that went missing.
func TestPublishExitOutsideTheDaemonsStandDownStaysADeath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.daemonStandDown = func() bool { return false }

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: -1, Inferred: true})

	// Assert.
	if !hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("an inferred departure outside any stand-down was not recorded as a death")
	}
}

// ---- the daemon-wide latch on the adopted paths ----

// TestTheRedialLadderEndsAtDebugInsideTheDaemonsStandDown is the ladder's half
// of the measured shape: the client itself was never asked to stand down --
// nothing named it -- and the daemon's latch is the only thing that says the
// departure was ordered. Read only from the client's own latch, the ladder
// ended with `daemon.shimclient.redial` WARN "redial stopped" four times over
// one `UpdateShutdownSchedule{now}`.
func TestTheRedialLadderEndsAtDebugInsideTheDaemonsStandDown(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), filepath.Join(shortDir(t), "absent.sock"),
		backoff{Initial: time.Millisecond, Max: 2 * time.Millisecond, Factor: 1},
		func(ids.WorkspaceID) (bool, error) { return true, nil },
		func() bool { return true })
	frames := make(chan *shimv1.WatchSessionResponse)
	errs := make(chan error, 1)
	errs <- io.ErrUnexpectedEOF

	// Act.
	runMonitorToCompletion(t, c, frames, errs)

	// Assert.
	if hasRecordAt(log, "warn", "daemon.shimclient.redial") {
		t.Fatal("the redial ladder warned inside a stand-down this daemon ordered")
	}
}

// TestTheRedialLadderStillWarnsOutsideAnyStandDown is that rule's other half:
// a daemon that ordered nothing still gets the loud record.
func TestTheRedialLadderStillWarnsOutsideAnyStandDown(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), filepath.Join(shortDir(t), "absent.sock"),
		backoff{Initial: time.Millisecond, Max: 2 * time.Millisecond, Factor: 1},
		func(ids.WorkspaceID) (bool, error) { return true, nil },
		func() bool { return false })
	frames := make(chan *shimv1.WatchSessionResponse)
	errs := make(chan error, 1)
	errs <- io.ErrUnexpectedEOF

	// Act.
	runMonitorToCompletion(t, c, frames, errs)

	// Assert.
	if !hasRecordSaying(log, "warn", "daemon.shimclient.redial", "redial stopped") {
		t.Fatal("the redial ladder ended quietly with no stand-down behind it")
	}
}

// TestTheExitRecordStatesTheDaemonsLatchToo pins the FIELD, not the level: a
// reader of the measured log could see only `stand_down_asked: false`, which
// says nothing about the question that actually decided the level.
func TestTheExitRecordStatesTheDaemonsLatchToo(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil,
		func() bool { return true })

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: -1, Inferred: true})

	// Assert.
	fields := recordFields(t, log, "daemon.shimclient.exit")
	if fields["daemon_stand_down"] != true {
		t.Fatalf("the exit record's daemon_stand_down = %v, want true", fields["daemon_stand_down"])
	}
}

// TestTheExitRecordStatesTheClientsOwnLatchSeparately is the other field, and
// the reason there are two: the adopted client of a shim the daemon ordered
// away was never itself asked, and a record that folded the two into one could
// not say so.
func TestTheExitRecordStatesTheClientsOwnLatchSeparately(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil,
		func() bool { return true })

	// Act.
	c.publishExit(ExitInfo{PID: 4242, Code: -1, Inferred: true})

	// Assert.
	fields := recordFields(t, log, "daemon.shimclient.exit")
	if fields["stand_down_asked"] != false {
		t.Fatalf("the exit record's stand_down_asked = %v, want false", fields["stand_down_asked"])
	}
}

// TestTheAdoptedWitnessReadsTheDaemonsLatch covers the witness itself: the
// socket is gone and the lock is free, and the daemon's own latch is what says
// the departure was ordered rather than suffered.
func TestTheAdoptedWitnessReadsTheDaemonsLatch(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff,
		func(ids.WorkspaceID) (bool, error) { return true, nil }, func() bool { return true })

	// Act.
	c.witnessAdoptedDeath(syscall.ECONNREFUSED)

	// Assert.
	if hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("an adopted departure inside the daemon's own stand-down was recorded as a death")
	}
}

// TestAnUnaskedAdoptedDeathIsStillLoud is the invariant's floor: with no latch
// of either kind behind it, an adopted shim that went missing is a death.
func TestAnUnaskedAdoptedDeathIsStillLoud(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff,
		func(ids.WorkspaceID) (bool, error) { return true, nil }, func() bool { return false })

	// Act.
	c.witnessAdoptedDeath(syscall.ECONNREFUSED)

	// Assert.
	if !hasRecordAt(log, "error", "daemon.shimclient.exit") {
		t.Fatal("an adopted death nobody asked for was not recorded as a death")
	}
}

// recordFields returns the context of the last record at one operation.
func recordFields(t *testing.T, log *dlog.TestLogger, operation string) dlog.Context {
	t.Helper()

	for i := len(log.Records()) - 1; i >= 0; i-- {
		if r := log.Records()[i]; r.Operation == operation {
			return r.Context
		}
	}
	t.Fatalf("no record at %q", operation)
	return nil
}

// ---- a refused stream open ----

// TestARefusedStreamOpenIsRecordedAtTheLevelItsCodeMeans pins the record a
// refused WatchBash open writes. not_found and failed_precondition are a
// serving shim answering that it holds no such handle -- the session watcher
// rules on whether that was expected, and calls the ordinary case expected --
// so they are INFO here; a code that means the open itself failed stays ERROR.
// The error is returned in every case.
func TestARefusedStreamOpenIsRecordedAtTheLevelItsCodeMeans(t *testing.T) {
	tests := []struct {
		name      string
		code      connect.Code
		wantLevel string
		wrongLvl  string
	}{
		{name: "the store holds no rows for the run", code: connect.CodeNotFound, wantLevel: "info", wrongLvl: "error"},
		{name: "the shim has no such handle yet", code: connect.CodeFailedPrecondition, wantLevel: "info", wrongLvl: "error"},
		{name: "the shim failed the open", code: connect.CodeInternal, wantLevel: "error", wrongLvl: "info"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f, uds := startFakeShim(t, shortDir(t))
			f.watchBashRefusal = connect.NewError(tt.code, errors.New("the store holds no rows for shell run"))
			log := dlog.NewTestLogger()
			c := newClient(log, ids.WorkspaceID("ws-1"), uds, defaultBackoff, nil, nil)

			// Act.
			_, err := c.WatchBash(context.Background(), &conversationv1.DetachedWorkId{Value: "toolu_1"})

			// Assert.
			var refusal *StreamOpenError
			if !errors.As(err, &refusal) {
				t.Fatalf("WatchBash() error = %v, want a *StreamOpenError", err)
			}
			if !hasRecordAt(log, tt.wantLevel, "daemon.shimclient.watch_bash") {
				t.Fatalf("no %q record for the refused open: %+v", tt.wantLevel, log.Records())
			}
			if hasRecordAt(log, tt.wrongLvl, "daemon.shimclient.watch_bash") {
				t.Fatalf("the refused open was ALSO recorded at %q: %+v", tt.wrongLvl, log.Records())
			}
		})
	}
}

// TestAStreamOpenItsCallerAbandonedIsRecordedAtInfo pins that an open whose own
// caller's context ended first -- the watcher closing, the daemon tearing down
// -- is not a shim refusal: it is recorded at INFO with the cause, never at
// ERROR, and the error is still returned.
func TestAStreamOpenItsCallerAbandonedIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	f, uds := startFakeShim(t, shortDir(t))
	f.watchBashRefusal = connect.NewError(connect.CodeInternal, errors.New("never reached"))
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), uds, defaultBackoff, nil, nil)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := c.WatchBash(ctx, &conversationv1.DetachedWorkId{Value: "toolu_1"})

	// Assert.
	if err == nil {
		t.Fatalf("WatchBash() error = nil, want the abandoned open's error")
	}
	if hasRecordAt(log, "error", "daemon.shimclient.watch_bash") {
		t.Fatalf("the abandoned open was recorded at error: %+v", log.Records())
	}
	if !hasRecordAt(log, "info", "daemon.shimclient.watch_bash") {
		t.Fatalf("no info record for the abandoned open: %+v", log.Records())
	}
	if got := recordFields(t, log, "daemon.shimclient.watch_bash")["cause"]; got != context.Canceled.Error() {
		t.Fatalf("cause = %v, want %q", got, context.Canceled.Error())
	}
}

// TestAStreamOpenInsideAnOrderedStandDownIsNotARefusal pins that a stream open
// failing on a shim this daemon asked to stand down is ruled exactly as a
// unary call's failure is: recorded at INFO with both stand-down latches, never
// at ERROR, and returned as a *StreamOpenError wrapping ErrStandDownOrdered
// with the transport's error still in the chain. An open nobody stood down
// stays an ERROR refusal.
//
// MEASURED, deploy 2026-09-29T17:15:29: WatchAgent opens on six shims the
// successor's bounce had stood down came back "incomplete envelope:
// unexpected EOF" and were recorded here at ERROR.
func TestAStreamOpenInsideAnOrderedStandDownIsNotARefusal(t *testing.T) {
	tests := []struct {
		name      string
		standDown bool
		wantLevel string
		wrongLvl  string
		wantWrap  bool
	}{
		{name: "the daemon stood the shim down", standDown: true, wantLevel: "info", wrongLvl: "error", wantWrap: true},
		{name: "nobody asked the shim to stand down", standDown: false, wantLevel: "error", wrongLvl: "info", wantWrap: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f, uds := startFakeShim(t, shortDir(t))
			refusal := connect.NewError(connect.CodeInternal, errors.New("unexpected EOF"))
			f.watchBashRefusal = refusal
			log := dlog.NewTestLogger()
			c := newClient(log, ids.WorkspaceID("ws-1"), uds, defaultBackoff, nil, nil)
			if tt.standDown {
				c.standDown.Store(true)
			}

			// Act.
			_, err := c.WatchBash(context.Background(), &conversationv1.DetachedWorkId{Value: "toolu_1"})

			// Assert.
			var opened *StreamOpenError
			if !errors.As(err, &opened) {
				t.Fatalf("WatchBash() error = %v, want a *StreamOpenError", err)
			}
			if got := errors.Is(err, ErrStandDownOrdered); got != tt.wantWrap {
				t.Fatalf("errors.Is(err, ErrStandDownOrdered) = %v, want %v: %v", got, tt.wantWrap, err)
			}
			if connect.CodeOf(err) != connect.CodeInternal {
				t.Fatalf("connect.CodeOf(err) = %v, want the transport's code kept in the chain", connect.CodeOf(err))
			}
			if !hasRecordAt(log, tt.wantLevel, "daemon.shimclient.watch_bash") {
				t.Fatalf("no %q record for the failed open: %+v", tt.wantLevel, log.Records())
			}
			if hasRecordAt(log, tt.wrongLvl, "daemon.shimclient.watch_bash") {
				t.Fatalf("the failed open was ALSO recorded at %q: %+v", tt.wrongLvl, log.Records())
			}
			if tt.standDown {
				if got := recordFields(t, log, "daemon.shimclient.watch_bash")["stand_down_asked"]; got != true {
					t.Fatalf("stand_down_asked = %v, want true", got)
				}
			}
		})
	}
}

func TestABroughtUpClientCountsOneConnection(t *testing.T) {
	// Arrange
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)

	// Act
	client := spawnReady(t, f, spec)
	collectStates(t, client, 2)

	// Assert
	if got := client.Connections(); got != 1 {
		t.Fatalf("Connections() = %d after bring-up, want 1", got)
	}
}

// The count advances before LinkConnected is published, so a consumer that
// has read the redial's connected state always sees the new count.
func TestARedialCountsItsConnectionBeforeAnnouncingIt(t *testing.T) {
	// Arrange
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	client := spawnReady(t, f, spec)
	collectStates(t, client, 2)

	// Act
	f.dropSessions()
	waitForSessionOpen(t, f)
	f.push(healthyUpdate())
	got := collectStates(t, client, 2)

	// Assert
	if got[1] != LinkConnected {
		t.Fatalf("states = %v, want redialing then connected", got)
	}
	if n := client.Connections(); n != 2 {
		t.Fatalf("Connections() = %d once the redial's LinkConnected was read, want 2", n)
	}
}

// connected() is the one place a link is announced, so no site can publish
// LinkConnected without advancing the count a consumer relies on.
func TestOnlyConnectedAnnouncesALink(t *testing.T) {
	// Act
	announcements := sourcescan.Count(t, "publish(LinkConnected)")

	// Assert
	if announcements != 1 {
		t.Fatalf("production source publishes LinkConnected at %d sites; only connected() may", announcements)
	}
}
