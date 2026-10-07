package chessboard

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"google.golang.org/protobuf/encoding/protowire"
)

// fakeWebapp is an httptest cee-webapp answering the two procedures.
type fakeWebapp struct {
	server *httptest.Server
	// requests holds each procedure's last request body.
	requests map[string][]byte
	// answers maps a procedure to its status and body.
	answers map[string]func() (int, []byte)
}

func newFakeWebapp(t *testing.T) *fakeWebapp {
	t.Helper()
	w := &fakeWebapp{requests: map[string][]byte{}, answers: map[string]func() (int, []byte){}}
	w.server = httptest.NewServer(http.HandlerFunc(func(rw http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		w.requests[r.URL.Path] = body
		answer, ok := w.answers[r.URL.Path]
		if !ok || r.Header.Get("Content-Type") != "application/proto" {
			rw.WriteHeader(http.StatusNotFound)
			return
		}
		status, out := answer()
		rw.WriteHeader(status)
		_, _ = rw.Write(out)
	}))
	t.Cleanup(w.server.Close)
	return w
}

// answerWidget makes GetCeeWebWidget answer widget.
func (w *fakeWebapp) answerWidget(widget []byte) {
	w.answers[procGetCeeWebWidget] = func() (int, []byte) {
		b := protowire.AppendTag(nil, fieldResponseWidget, protowire.BytesType)
		return http.StatusOK, protowire.AppendBytes(b, widget)
	}
}

// refuse makes procedure answer a Connect error with code.
func (w *fakeWebapp) refuse(procedure, code string) {
	w.answers[procedure] = func() (int, []byte) {
		body, _ := json.Marshal(connectError{Code: code, Message: "session x holds no live game"})
		return http.StatusBadRequest, body
	}
}

// decodeFields reads a message's top-level fields: varints as uint64, bytes as
// []byte.
func decodeFields(t *testing.T, b []byte) map[protowire.Number]any {
	t.Helper()
	out := map[protowire.Number]any{}
	for len(b) > 0 {
		num, typ, n := protowire.ConsumeTag(b)
		b = b[n:]
		switch typ {
		case protowire.VarintType:
			v, m := protowire.ConsumeVarint(b)
			out[num], b = v, b[m:]
		case protowire.BytesType:
			v, m := protowire.ConsumeBytes(b)
			out[num], b = v, b[m:]
		default:
			t.Fatalf("unexpected wire type %v", typ)
		}
	}
	return out
}

func TestGetWidgetSendsTheSessionAndAnswersTheWidgetBytes(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.answerWidget([]byte{0x0a, 0x00})

	// Act.
	got, err := getWidget(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "agent-a", GameID: "g-1"})

	// Assert.
	if err != nil || string(got) != string([]byte{0x0a, 0x00}) {
		t.Fatalf("getWidget() = %v, %v; want the widget bytes", got, err)
	}
	metadata := decodeFields(t, decodeFields(t, w.requests[procGetCeeWebWidget])[fieldRequestSession].([]byte))
	if string(metadata[fieldMetadataSessionID].([]byte)) != "agent-a" || string(metadata[fieldMetadataGameID].([]byte)) != "g-1" {
		t.Fatalf("sent metadata = %v, want agent-a / g-1", metadata)
	}
}

func TestGetWidgetAnswersAnEmptyWidgetForAnAbsentField(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.answers[procGetCeeWebWidget] = func() (int, []byte) { return http.StatusOK, nil }

	// Act.
	got, err := getWidget(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "a", GameID: "g"})

	// Assert.
	if err != nil || got == nil || len(got) != 0 {
		t.Fatalf("getWidget() = %v, %v; want empty, non-nil widget bytes", got, err)
	}
}

func TestGetWidgetReadsFailedPreconditionAsTheSessionGone(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.refuse(procGetCeeWebWidget, "failed_precondition")

	// Act.
	_, err := getWidget(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "a", GameID: "g"})

	// Assert.
	if !errors.Is(err, errSessionGone) {
		t.Fatalf("getWidget() error = %v, want errSessionGone", err)
	}
}

func TestGetWidgetReadsAnyOtherRefusalAsAFailedCall(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.refuse(procGetCeeWebWidget, "internal")

	// Act.
	_, err := getWidget(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "a", GameID: "g"})

	// Assert.
	if !errors.Is(err, errCall) || errors.Is(err, errSessionGone) {
		t.Fatalf("getWidget() error = %v, want errCall alone", err)
	}
}

func TestGetWidgetReadsAnUnreadableRefusalAsAFailedCall(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.answers[procGetCeeWebWidget] = func() (int, []byte) { return http.StatusBadGateway, []byte("<html>") }

	// Act.
	_, err := getWidget(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "a", GameID: "g"})

	// Assert.
	if !errors.Is(err, errCall) {
		t.Fatalf("getWidget() error = %v, want errCall", err)
	}
}

func TestGetWidgetReadsAMalformedAnswerAsAFailedCall(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.answers[procGetCeeWebWidget] = func() (int, []byte) { return http.StatusOK, []byte{0x0a, 0x05, 0x01} }

	// Act.
	_, err := getWidget(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "a", GameID: "g"})

	// Assert.
	if !errors.Is(err, errCall) {
		t.Fatalf("getWidget() error = %v, want errCall", err)
	}
}

func TestGetWidgetReadsAnUnreachableBackendAsAFailedCall(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	url := w.server.URL
	w.server.Close()

	// Act.
	_, err := getWidget(context.Background(), http.DefaultClient, url, Session{ID: "a", GameID: "g"})

	// Assert.
	if !errors.Is(err, errCall) {
		t.Fatalf("getWidget() error = %v, want errCall", err)
	}
}

func TestGetSquareEventsSendsThePositionAndSquareAndAnswersWhole(t *testing.T) {
	// Arrange.
	w := newFakeWebapp(t)
	w.answers[procGetSquareEvents] = func() (int, []byte) { return http.StatusOK, []byte{0x08, 0x1c} }

	// Act.
	got, err := getSquareEvents(context.Background(), http.DefaultClient, w.server.URL, Session{ID: "a", GameID: "g"}, 42, 28)

	// Assert.
	if err != nil || string(got) != string([]byte{0x08, 0x1c}) {
		t.Fatalf("getSquareEvents() = %v, %v; want the answer whole", got, err)
	}
	sent := decodeFields(t, w.requests[procGetSquareEvents])
	if sent[fieldRequestGamePoint] != uint64(42) || sent[fieldRequestSquare] != uint64(28) {
		t.Fatalf("sent fields = %v, want game point 42 and square 28", sent)
	}
}
