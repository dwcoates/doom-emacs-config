package server

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

// TestAwaitQuietWaitsForACallStillBeingAnswered pins the whole point of the
// gate: an exit that fired while a unary call was in flight must not proceed
// until that call has been answered.
func TestAwaitQuietWaitsForACallStillBeingAnswered(t *testing.T) {
	// Arrange: a handler parked inside one unary call.
	entered := make(chan struct{})
	release := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		close(entered)
		<-release
		w.WriteHeader(http.StatusOK)
	}), nil)
	httpServer := httptest.NewServer(serving)
	defer httpServer.Close()
	answered := make(chan struct{})
	go func() {
		defer close(answered)
		resp, err := httpServer.Client().Get(httpServer.URL + "/agentrepl.v1.AgentRepl/SelectWorkspace")
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-entered

	// Act: the exit's wait, on a bound the parked call will outlast.
	quiet := make(chan int, 1)
	go func() { quiet <- serving.AwaitQuiet(time.Minute) }()

	// Assert: it does not settle while the call is being answered, and does
	// the moment it has been.
	select {
	case left := <-quiet:
		t.Fatalf("AwaitQuiet returned %d while a call was still being answered", left)
	case <-time.After(50 * time.Millisecond):
	}
	close(release)
	if left := <-quiet; left != 0 {
		t.Fatalf("AwaitQuiet = %d after the call was answered, want 0", left)
	}
	<-answered
}

// TestAwaitQuietReportsTheCallsItGaveUpOn pins the loud half: a bound that
// expires answers how many callers are about to read a cut connection.
func TestAwaitQuietReportsTheCallsItGaveUpOn(t *testing.T) {
	// Arrange
	entered := make(chan struct{})
	release := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		close(entered)
		<-release
		w.WriteHeader(http.StatusOK)
	}), nil)
	httpServer := httptest.NewServer(serving)
	// THE PARKED HANDLER IS RELEASED BEFORE THE SERVER IS CLOSED: httptest's
	// Close waits for every outstanding request, so the reverse order is a
	// deadlock rather than a teardown.
	defer httpServer.Close()
	defer close(release)
	go func() {
		resp, err := httpServer.Client().Get(httpServer.URL + "/agentrepl.v1.AgentRepl/SelectWorkspace")
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-entered

	// Act
	left := serving.AwaitQuiet(10 * time.Millisecond)

	// Assert
	if left != 1 {
		t.Fatalf("AwaitQuiet = %d after its bound expired on one parked call, want 1", left)
	}
}

// TestAwaitQuietDoesNotWaitForAStandingStream pins the exclusion. A Watch*
// handler returns only when its client goes away, so counting one would spend
// the exit's whole grace on every stop and bound nothing.
func TestAwaitQuietDoesNotWaitForAStandingStream(t *testing.T) {
	// Arrange
	entered := make(chan struct{})
	release := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		close(entered)
		<-release
		w.WriteHeader(http.StatusOK)
	}), nil)
	httpServer := httptest.NewServer(serving)
	defer httpServer.Close()
	defer close(release)
	go func() {
		resp, err := httpServer.Client().Get(httpServer.URL + "/agentrepl.v1.AgentRepl/WatchDaemon")
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-entered

	// Act, Assert
	if left := serving.AwaitQuiet(time.Minute); left != 0 {
		t.Fatalf("AwaitQuiet = %d while only a standing stream was open, want 0 at once", left)
	}
}

// TestStandingStreamPathsComeFromTheDescriptor pins that the exclusion set is
// derived rather than listed: every server-streaming verb in the service is in
// it, and no unary one is.
func TestStandingStreamPathsComeFromTheDescriptor(t *testing.T) {
	// Arrange, Act
	paths := serverStreamPaths()

	// Assert
	if !paths["/agentrepl.v1.AgentRepl/WatchDaemon"] {
		t.Fatalf("WatchDaemon is not in the standing-stream set: %v", paths)
	}
	if paths["/agentrepl.v1.AgentRepl/SelectWorkspace"] {
		t.Fatal("SelectWorkspace, a unary verb, is in the standing-stream set")
	}
}

// TestAwaitQuietWaitsForTheAnswerToLeaveNotForTheHandlerToReturn is the
// difference the first version of this gate got wrong: the handler had
// returned, the exit proceeded, and the caller still read a cut connection
// because the response had not been written yet.
func TestAwaitQuietWaitsForTheAnswerToLeaveNotForTheHandlerToReturn(t *testing.T) {
	// Arrange: a handler that has returned, on a request whose context — the
	// stream's own lifetime — has not ended.
	returned := make(chan struct{})
	streamOpen, endStream := context.WithCancel(context.Background())
	defer endStream()
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusOK)
		close(returned)
	}), nil)
	request := httptest.NewRequest(http.MethodPost, "/agentrepl.v1.AgentRepl/SelectWorkspace", nil).WithContext(streamOpen)
	go serving.ServeHTTP(httptest.NewRecorder(), request)
	<-returned

	// Act, Assert: the gate is still holding, and lets go when the stream does.
	quiet := make(chan int, 1)
	go func() { quiet <- serving.AwaitQuiet(time.Minute) }()
	select {
	case left := <-quiet:
		t.Fatalf("AwaitQuiet returned %d with the answer's stream still open", left)
	case <-time.After(50 * time.Millisecond):
	}
	endStream()
	if left := <-quiet; left != 0 {
		t.Fatalf("AwaitQuiet = %d once the stream closed, want 0", left)
	}
}
