package shimclient

import (
	"context"
	"errors"
	"io"
	"syscall"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"connectrpc.com/connect"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// newBareClient builds a client with no process and no connection, for the
// pieces that need neither.
func newBareClient() *client {
	return newClient(dlog.NewTestLogger(), ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil)
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
	if err := client.Kill(attr); err != nil {
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
	if err := client.Kill(KillAttribution{Actor: "drain", Reason: "shutdown"}); err != nil {
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
	if err := client.Kill(KillAttribution{Actor: "test", Reason: "one record"}); err != nil {
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
	if err := client.Kill(KillAttribution{Actor: "test", Reason: "after detach"}); !errors.Is(err, ErrDetached) {
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
		stream Stream[*conversationv1.SessionUpdate]
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
