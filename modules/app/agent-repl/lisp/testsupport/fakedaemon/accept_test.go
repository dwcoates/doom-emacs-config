package main

import (
	"bytes"
	"context"
	"encoding/binary"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"connectrpc.com/connect"
)

// connectStreamBody frames one protojson message as a connect streaming
// envelope: flag byte 0x00, a big-endian u32 length, then the payload.
func connectStreamBody(payload string) io.Reader {
	frame := make([]byte, 5, 5+len(payload))
	frame[0] = 0x00
	binary.BigEndian.PutUint32(frame[1:5], uint32(len(payload)))
	frame = append(frame, payload...)
	return bytes.NewReader(frame)
}

// openRawStream issues a connect streaming request and returns as soon as the
// RESPONSE HEADERS arrive — which is exactly the client-side observable the
// acceptance rule is about.
func openRawStream(t *testing.T, baseURL, method, payload string, deadline time.Duration) (*http.Response, context.CancelFunc) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), deadline)
	req, err := http.NewRequestWithContext(ctx, http.MethodPost,
		baseURL+"/agentrepl.v1.AgentRepl/"+method, connectStreamBody(payload))
	if err != nil {
		cancel()
		t.Fatalf("build %s request: %v", method, err)
	}
	req.Header.Set("Content-Type", "application/connect+json")
	req.Header.Set("Connect-Protocol-Version", "1")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		cancel()
		t.Fatalf("open %s: %v (headers never arrived — the stream was never accepted)", method, err)
	}
	return resp, cancel
}

func TestWatchDaemonFlushesHeadersOnAcceptance(t *testing.T) {
	// Arrange: nothing scripted, nothing pushed, no snapshot stored.
	server, baseURL := newTestServer(t)

	// Act: the open must complete on HEADERS alone.  connect-go writes them
	// on the first Send, so without the acceptance flush this call would sit
	// here until the context deadline.
	resp, cancel := openRawStream(t, baseURL, "WatchDaemon", emacsWatchDaemonJSON, 5*time.Second)
	defer cancel()
	defer resp.Body.Close()

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("accepted stream answered %d, want 200", resp.StatusCode)
	}
	if got := resp.Header.Get("Content-Type"); got != "application/connect+json" {
		t.Fatalf("Content-Type = %q, want application/connect+json", got)
	}
	// The subscription is live on the server side, and STAYS live: a standing
	// stream never ends of its own accord.
	server.mustAwaitSubscribers(t, streamDaemon, "", 1)
	if len(server.subscriberInfos()) != 1 {
		t.Fatalf("subscribers = %v, want the accepted stream still open",
			server.subscriberInfos())
	}
}

func TestWatchWorkspaceRosterFlushesHeadersOnAcceptance(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)

	// Act.
	resp, cancel := openRawStream(t, baseURL, "WatchWorkspaceRoster", "{}", 5*time.Second)
	defer cancel()
	defer resp.Body.Close()

	// Assert: the roster stream is standing too, and Emacs opens its tabs from
	// it on connect — acceptance must not wait for the first roster push.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("accepted stream answered %d, want 200", resp.StatusCode)
	}
	server.mustAwaitSubscribers(t, streamRoster, "", 1)
}

func TestWatchHostWorkspaceFlushesHeadersOnAcceptance(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)

	// Act.
	resp, cancel := openRawStream(t, baseURL, "WatchHostWorkspace",
		`{"workspace":{"id":"ws-test","dir":"/tmp/ws-test"}}`, 5*time.Second)
	defer cancel()
	defer resp.Body.Close()

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("accepted stream answered %d, want 200", resp.StatusCode)
	}
	server.mustAwaitSubscribers(t, streamHost, "ws-test", 1)
}

func TestAcceptedStreamStillDeliversItsFirstPush(t *testing.T) {
	// Arrange: headers already flushed, so the first frame arrives on a
	// response the client has long since accepted.
	server, baseURL := newTestServer(t)
	resp, cancel := openRawStream(t, baseURL, "WatchDaemon", emacsWatchDaemonJSON, 5*time.Second)
	defer cancel()
	defer resp.Body.Close()
	server.mustAwaitSubscribers(t, streamDaemon, "", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"daemon","message":{"drainCancelled":{}}}`); status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert: an ordinary envelope follows the already-sent headers.
	header := make([]byte, 5)
	if _, err := io.ReadFull(resp.Body, header); err != nil {
		t.Fatalf("read frame header: %v", err)
	}
	payload := make([]byte, binary.BigEndian.Uint32(header[1:5]))
	if _, err := io.ReadFull(resp.Body, payload); err != nil {
		t.Fatalf("read frame payload: %v", err)
	}
	if header[0] != 0x00 || !bytes.Contains(payload, []byte("drainCancelled")) {
		t.Fatalf("first frame = flags %#x payload %s", header[0], payload)
	}
}

func TestUnaryStillWritesItsOwnHeaders(t *testing.T) {
	// Arrange: the accept writer wraps EVERY response, so a unary call must be
	// unaffected — nothing calls accept, and connect-go's own WriteHeader is
	// the first one through.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	_, err := client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("DaemonHealth: %v", err)
	}
}

func TestUnaryRefusalStillCarriesItsStatus(t *testing.T) {
	// Arrange: a refusal must keep its own status rather than inherit a 200
	// from the acceptance path.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := rawUnary(t, baseURL, "SelectWorkspace", `{}`)

	// Assert.
	if status != http.StatusBadRequest {
		t.Fatalf("refused unary = %d %s, want 400", status, body)
	}
}

func TestStreamRequestRefusalIsNotAccepted(t *testing.T) {
	// Arrange: validation runs BEFORE the subscription is registered, so an
	// illegal stream request must never be accepted.
	server, baseURL := newTestServer(t)

	// Act: WatchHostWorkspace with no workspace ref at all.
	resp, cancel := openRawStream(t, baseURL, "WatchHostWorkspace", "{}", 5*time.Second)
	defer cancel()
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)

	// Assert: no subscription was ever registered, and the refusal reached the
	// client rather than a standing empty stream.
	if len(server.subscriberInfos()) != 0 {
		t.Fatalf("subscribers = %v, want none for a refused request",
			server.subscriberInfos())
	}
	if !bytes.Contains(body, []byte("workspace")) && resp.StatusCode == http.StatusOK {
		t.Fatalf("refusal = %d %s, want the invalid-argument answer", resp.StatusCode, body)
	}
}

// Once a stream is ACCEPTED, its status is settled: a later WriteHeader
// cannot retract it, because the client already read the 200 as acceptance.
func TestASecondWriteHeaderCannotRetractAnAcceptedStatus(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	writer.accept("application/connect+json")

	// Act: connect-go's own lazy header write, disagreeing with acceptance.
	writer.WriteHeader(http.StatusInternalServerError)

	// Assert.
	if recorder.Code != http.StatusOK {
		t.Fatalf("status = %d, want the accepted %d to stand", recorder.Code, http.StatusOK)
	}
}

// The ordinary case connect-go always hits: a second WriteHeader agreeing
// with the accepted status is swallowed without a word.
func TestASecondWriteHeaderAgreeingWithAcceptanceIsSwallowed(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	writer.accept("application/connect+json")

	// Act.
	writer.WriteHeader(http.StatusOK)

	// Assert.
	if recorder.Code != http.StatusOK {
		t.Fatalf("status = %d, want %d", recorder.Code, http.StatusOK)
	}
	if got := recorder.Header().Get("Content-Type"); got != "application/connect+json" {
		t.Fatalf("content type = %q, want the accepted streaming type", got)
	}
}

// Acceptance happens ONCE: a second accept must not re-send headers, or a
// stream handler that accepts defensively would corrupt its own response.
func TestAcceptIsIdempotent(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	writer.accept("application/connect+json")

	// Act.
	writer.accept("text/plain")

	// Assert.
	if got := recorder.Header().Get("Content-Type"); got != "application/connect+json" {
		t.Fatalf("content type = %q, want the first acceptance's type", got)
	}
}

// Unwrap is what lets connect-go's http.NewResponseController set write
// deadlines through this wrapper; without it every streaming deadline would
// silently be a no-op.
func TestUnwrapReachesTheRealResponseWriter(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	got := writer.Unwrap()

	// Assert.
	if got != http.ResponseWriter(recorder) {
		t.Fatalf("Unwrap returned %T, want the wrapped recorder", got)
	}
}

func TestAcceptWriterFromAnswersNothingForAnUnwrappedContext(t *testing.T) {
	// Arrange: a context that never passed through withAcceptWriter, which is
	// what a unary handler's context is.
	ctx := context.Background()

	// Act.
	got := acceptWriterFrom(ctx)

	// Assert.
	if got != nil {
		t.Fatalf("acceptWriterFrom = %v, want nil outside a wrapped request", got)
	}
}
