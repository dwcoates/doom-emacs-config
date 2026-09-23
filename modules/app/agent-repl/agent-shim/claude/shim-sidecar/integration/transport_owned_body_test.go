package integration

import (
	"bytes"
	"errors"
	"io"
	"net/http"
	"strings"
	"sync"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/testclose"
)

// transport_owned_body_test.go — the guard that keeps a declared-length request
// body from being withdrawn mid-flight. Each subject is ONE edge of
// {@link ownedRequestBody}; the defect it exists for is stated in full there.

// releasingBody is connect-go's `payloadCloser`, reduced to the one behavior
// that matters here: after Release it answers io.EOF no matter how much of its
// declared length is still unread. connect-go releases it with a `defer` in
// `duplexHTTPCall.sendUnary`, so the release lands the instant `Do` returns.
type releasingBody struct {
	mu       sync.Mutex
	reader   *bytes.Reader
	released bool
	closed   bool
}

func newReleasingBody(payload []byte) *releasingBody {
	return &releasingBody{reader: bytes.NewReader(payload)}
}

func (b *releasingBody) Read(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.released {
		return 0, io.EOF
	}
	return b.reader.Read(p)
}

func (b *releasingBody) Close() error {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.closed = true
	return nil
}

func (b *releasingBody) release() {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.released = true
}

func (b *releasingBody) wasClosed() bool {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.closed
}

// recordingRoundTripper stands in for the real transport: it records what the
// delegate was handed and answers a bare 200, so a subject can assert on the
// request the wrapper actually sends rather than on a wire.
type recordingRoundTripper struct {
	req  *http.Request
	body []byte
	err  error
	// afterRead runs once the delegate has consumed the body it was given,
	// which is where the real transport's read of a released payload lands.
	afterRead func()
}

func (r *recordingRoundTripper) RoundTrip(req *http.Request) (*http.Response, error) {
	r.req = req
	if req.Body != nil {
		r.body, r.err = io.ReadAll(req.Body)
	}
	if r.afterRead != nil {
		r.afterRead()
	}
	return &http.Response{
		StatusCode: http.StatusOK,
		Body:       io.NopCloser(strings.NewReader("")),
		Request:    req,
	}, r.err
}

// TestOwnedRequestBodySurvivesARelease: the whole point. The body is released
// the way connect-go releases it — as soon as the call returns — and the bytes
// the delegate saw are still the bytes the request declared.
func TestOwnedRequestBodySurvivesARelease(t *testing.T) {
	t.Parallel()
	// Arrange.
	payload := []byte("the request message this call promised to send")
	body := newReleasingBody(payload)
	delegate := &recordingRoundTripper{}
	req, err := http.NewRequest(http.MethodPost, "http://shim/shim.v1.Shim/WatchAgent", body)
	if err != nil {
		t.Fatalf("building the request: %v", err)
	}
	req.ContentLength = int64(len(payload))
	transport := &ownedRequestBody{next: delegate}

	// Act. The release races the send in production; here it is made to WIN,
	// which is the arrangement the wrapper must survive.
	delegate.afterRead = body.release
	resp, err := transport.RoundTrip(req)

	// Assert.
	if err != nil {
		t.Fatalf("RoundTrip: %v", err)
	}
	defer testclose.OrFail(t, resp.Body)
	if !bytes.Equal(delegate.body, payload) {
		t.Errorf("the delegate was handed %q, want the declared %q", delegate.body, payload)
	}
}

// TestOwnedRequestBodyClosesWhatItConsumed: the RoundTripper contract makes
// closing the request body the transport's job, and this wrapper is the layer
// that read it.
func TestOwnedRequestBodyClosesWhatItConsumed(t *testing.T) {
	t.Parallel()
	// Arrange.
	payload := []byte("a body")
	body := newReleasingBody(payload)
	req, err := http.NewRequest(http.MethodPost, "http://shim/x", body)
	if err != nil {
		t.Fatalf("building the request: %v", err)
	}
	req.ContentLength = int64(len(payload))
	transport := &ownedRequestBody{next: &recordingRoundTripper{}}

	// Act.
	resp, err := transport.RoundTrip(req)

	// Assert.
	if err != nil {
		t.Fatalf("RoundTrip: %v", err)
	}
	defer testclose.OrFail(t, resp.Body)
	if !body.wasClosed() {
		t.Error("the wrapper consumed the request body and did not close it")
	}
}

// TestOwnedRequestBodyRefusesAShortBody: a body ALREADY short of its own
// declared length arrives too early to be repaired, so the call is refused by
// name here instead of reaching a transport that would cut the connection.
func TestOwnedRequestBodyRefusesAShortBody(t *testing.T) {
	t.Parallel()
	// Arrange.
	body := newReleasingBody([]byte("four"))
	req, err := http.NewRequest(http.MethodPost, "http://shim/short", body)
	if err != nil {
		t.Fatalf("building the request: %v", err)
	}
	req.ContentLength = 48 // the length the request announces, and does not have
	delegate := &recordingRoundTripper{}
	transport := &ownedRequestBody{next: delegate}

	// Act.
	resp, err := transport.RoundTrip(req)

	// Assert.
	if err == nil {
		testclose.OrFail(t, resp.Body)
		t.Fatal("a request that contradicts its own content-length was sent anyway")
	}
	if !strings.Contains(err.Error(), "contradicts its own length") {
		t.Errorf("the refusal reads %q, which does not name the contradiction", err)
	}
	if delegate.req != nil {
		t.Error("the contradicting request reached the delegate")
	}
}

// TestOwnedRequestBodyPassesAnUndeclaredLengthThrough: a genuine client stream
// declares no length and its bytes arrive over time, so reading it here would
// deadlock the very call the caller is still writing to.
func TestOwnedRequestBodyPassesAnUndeclaredLengthThrough(t *testing.T) {
	t.Parallel()
	// Arrange.
	pipeReader, pipeWriter := io.Pipe()
	t.Cleanup(func() { _ = pipeWriter.Close() })
	req, err := http.NewRequest(http.MethodPost, "http://shim/stream", pipeReader)
	if err != nil {
		t.Fatalf("building the request: %v", err)
	}
	req.ContentLength = -1
	delegate := &recordingRoundTripper{}
	transport := &ownedRequestBody{next: delegate}

	// Act. The delegate reads the pipe; a wrapper that had read it first would
	// never have reached the delegate at all.
	go func() {
		_, _ = pipeWriter.Write([]byte("streamed"))
		_ = pipeWriter.Close()
	}()
	resp, err := transport.RoundTrip(req)

	// Assert.
	if err != nil {
		t.Fatalf("RoundTrip: %v", err)
	}
	defer testclose.OrFail(t, resp.Body)
	if string(delegate.body) != "streamed" {
		t.Errorf("the delegate read %q, want the streamed bytes", delegate.body)
	}
}

// TestOwnedRequestBodyPassesABodilessRequestThrough: a request with no body has
// nothing to own, and the wrapper must not manufacture one.
func TestOwnedRequestBodyPassesABodilessRequestThrough(t *testing.T) {
	t.Parallel()
	// Arrange.
	req, err := http.NewRequest(http.MethodPost, "http://shim/empty", http.NoBody)
	if err != nil {
		t.Fatalf("building the request: %v", err)
	}
	delegate := &recordingRoundTripper{}
	transport := &ownedRequestBody{next: delegate}

	// Act.
	resp, err := transport.RoundTrip(req)

	// Assert.
	if err != nil {
		t.Fatalf("RoundTrip: %v", err)
	}
	defer testclose.OrFail(t, resp.Body)
	if delegate.req == nil {
		t.Fatal("the bodiless request never reached the delegate")
	}
	if len(delegate.body) != 0 {
		t.Errorf("the delegate was handed %q for a bodiless request", delegate.body)
	}
}

// TestOwnedRequestBodyReplaysOnRetry: the transport retries an idempotent
// request through GetBody, and the copy this wrapper owns is what makes that
// replay possible after the original payload is gone.
func TestOwnedRequestBodyReplaysOnRetry(t *testing.T) {
	t.Parallel()
	// Arrange.
	payload := []byte("replayable")
	body := newReleasingBody(payload)
	req, err := http.NewRequest(http.MethodPost, "http://shim/retry", body)
	if err != nil {
		t.Fatalf("building the request: %v", err)
	}
	req.ContentLength = int64(len(payload))
	delegate := &recordingRoundTripper{}
	transport := &ownedRequestBody{next: delegate}

	// Act.
	resp, err := transport.RoundTrip(req)
	if err != nil {
		t.Fatalf("RoundTrip: %v", err)
	}
	defer testclose.OrFail(t, resp.Body)
	body.release()
	replay, replayErr := delegate.req.GetBody()

	// Assert.
	if replayErr != nil {
		t.Fatalf("GetBody after the release: %v", replayErr)
	}
	defer testclose.OrFail(t, replay)
	got, readErr := io.ReadAll(replay)
	if readErr != nil && !errors.Is(readErr, io.EOF) {
		t.Fatalf("reading the replayed body: %v", readErr)
	}
	if !bytes.Equal(got, payload) {
		t.Errorf("the replayed body held %q, want the declared %q", got, payload)
	}
}
