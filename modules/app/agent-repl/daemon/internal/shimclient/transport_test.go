package shimclient

import (
	"context"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"
)

// TestBackoffGrowsAndCaps asserts the schedule grows by its factor and stops
// at the cap.
func TestBackoffGrowsAndCaps(t *testing.T) {
	tests := []struct {
		name    string
		attempt int
		want    time.Duration
	}{
		{name: "first", attempt: 0, want: 100 * time.Millisecond},
		{name: "second", attempt: 1, want: 200 * time.Millisecond},
		{name: "third", attempt: 2, want: 400 * time.Millisecond},
		{name: "capped", attempt: 20, want: time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			b := backoff{Initial: 100 * time.Millisecond, Max: time.Second, Factor: 2}

			// Act.
			got := b.delay(tc.attempt)

			// Assert.
			if got != tc.want {
				t.Fatalf("delay(%d) = %v, want %v", tc.attempt, got, tc.want)
			}
		})
	}
}

// TestBackoffWaitStopsOnDeath asserts a wait ends the moment the process is
// known dead, rather than sitting out the delay.
func TestBackoffWaitStopsOnDeath(t *testing.T) {
	// Arrange.
	b := backoff{Initial: time.Hour, Max: time.Hour, Factor: 1}
	dead := make(chan struct{})
	close(dead)

	// Act.
	err := b.wait(context.Background(), dead, 0)

	// Assert.
	if !errors.Is(err, errProcessDead) {
		t.Fatalf("wait() = %v, want errProcessDead", err)
	}
}

// TestBackoffWaitStopsOnContext asserts a canceled supervision ends the wait.
func TestBackoffWaitStopsOnContext(t *testing.T) {
	// Arrange.
	b := backoff{Initial: time.Hour, Max: time.Hour, Factor: 1}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := b.wait(ctx, make(chan struct{}), 0)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("wait() = %v, want context.Canceled", err)
	}
}

// releasablePayload is connect-go's `payloadCloser` in the one respect this
// suite is about: it serves its bytes until it is RELEASED, and answers EOF at
// offset zero forever after. connect-go releases as soon as `Do` returns,
// which over HTTP/2 can be while the body is still being written.
type releasablePayload struct {
	mu       sync.Mutex
	body     []byte
	offset   int
	released bool
}

func (p *releasablePayload) Read(dst []byte) (int, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.released || p.offset >= len(p.body) {
		return 0, io.EOF
	}
	n := copy(dst, p.body[p.offset:])
	p.offset += n
	return n, nil
}

func (p *releasablePayload) Close() error { return nil }

func (p *releasablePayload) release() {
	p.mu.Lock()
	p.released = true
	p.mu.Unlock()
}

// lateBodyTransport reads the request body only when told to, standing in for
// http2's `writeRequestBody` goroutine, which keeps running after RoundTrip has
// answered the peer's response head.
type lateBodyTransport struct {
	body io.ReadCloser
	read chan struct{}
	done chan []byte
}

func newLateBodyTransport() *lateBodyTransport {
	return &lateBodyTransport{read: make(chan struct{}), done: make(chan []byte, 1)}
}

func (t *lateBodyTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	t.body = req.Body
	go func() {
		<-t.read
		raw, _ := io.ReadAll(t.body)
		t.done <- raw
	}()
	return &http.Response{StatusCode: http.StatusOK, Body: http.NoBody, Request: req}, nil
}

// TestOwnedRequestBodySurvivesAReleaseAfterTheResponseHead asserts the promised
// bytes still reach the wire when the caller releases its payload the instant
// the response head arrives — the race that made the shim RST_STREAM a fresh
// WatchSession with PROTOCOL_ERROR.
func TestOwnedRequestBodySurvivesAReleaseAfterTheResponseHead(t *testing.T) {
	// Arrange.
	payload := &releasablePayload{body: []byte{0, 0, 0, 0, 0}}
	next := newLateBodyTransport()
	req := httptest.NewRequest(http.MethodPost, "http://shim.uds/shim.v1.Shim/WatchSession", payload)
	req.ContentLength = int64(len(payload.body))

	// Act. The head answers, the caller releases, and only then does the
	// transport get around to the body.
	if _, err := (&ownedRequestBody{next: next}).RoundTrip(req); err != nil {
		t.Fatalf("RoundTrip() = error %v, want the response head", err)
	}
	payload.release()
	close(next.read)
	got := <-next.done

	// Assert.
	if len(got) != len(payload.body) {
		t.Fatalf("the transport wrote %d body bytes, want %d (content-length promised %d)",
			len(got), len(payload.body), req.ContentLength)
	}
}

// TestOwnedRequestBodyLeavesAnUndeclaredLengthAlone asserts a genuine client
// stream — whose bytes arrive over time — is passed through rather than read
// here, which would deadlock the call the caller is still writing to.
func TestOwnedRequestBodyLeavesAnUndeclaredLengthAlone(t *testing.T) {
	// Arrange.
	pipeReader, pipeWriter := io.Pipe()
	t.Cleanup(func() { _ = pipeWriter.Close() })
	next := newLateBodyTransport()
	req := httptest.NewRequest(http.MethodPost, "http://shim.uds/shim.v1.Shim/Ask", pipeReader)
	req.ContentLength = -1

	// Act.
	if _, err := (&ownedRequestBody{next: next}).RoundTrip(req); err != nil {
		t.Fatalf("RoundTrip() = error %v, want the response head", err)
	}

	// Assert.
	if next.body != pipeReader {
		t.Fatalf("the wrapper replaced an undeclared-length body; it must hand the stream through untouched")
	}
}

// TestOwnedRequestBodyRefusesABodyShorterThanItsDeclaredLength asserts a
// request that already contradicts its own content-length is refused by name
// here, rather than sent for the peer to reset.
func TestOwnedRequestBodyRefusesABodyShorterThanItsDeclaredLength(t *testing.T) {
	// Arrange.
	payload := &releasablePayload{body: []byte{0, 0, 0}}
	payload.release()
	next := newLateBodyTransport()
	req := httptest.NewRequest(http.MethodPost, "http://shim.uds/shim.v1.Shim/WatchSession", payload)
	req.ContentLength = 5

	// Act.
	_, err := (&ownedRequestBody{next: next}).RoundTrip(req)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "contradicts its own length") {
		t.Fatalf("RoundTrip() = %v, want a refusal naming the length contradiction", err)
	}
}
