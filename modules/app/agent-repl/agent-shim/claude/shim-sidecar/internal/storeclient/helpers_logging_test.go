package storeclient

import (
	"context"
	"encoding/json"
	"io"
	"net"
	"net/http"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// The unit harness reads its own log the way the integration suite reads the
// sidecar's durable one: as TYPED RECORDS, never as a blob of text.
//
// A subject that greps `h.logText()` for a sentence asserts the PROSE of a
// branch rather than the branch. The prose is not the contract — the operation,
// the level and the dedicated correlation keys are (AGENTS.md "Logging") — and a
// substring subject fails on a reworded message while passing on a record that
// dropped every key a reader joins on. Everything below addresses records by
// those fields instead.

// logRecord mirrors internal/logging's canonical JSON record.
type logRecord struct {
	Timestamp string         `json:"timestamp"`
	Runtime   string         `json:"runtime"`
	PID       int            `json:"pid"`
	Level     string         `json:"level"`
	Verbosity string         `json:"verbosity"`
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	RequestID string         `json:"request_id"`
	Context   map[string]any `json:"context"`
}

// parseLogLines parses captured log lines STRICTLY: the contract is JSONL, so a
// line that is not a JSON object is a defect rather than a line to skip.
func parseLogLines(t *testing.T, lines []string) []logRecord {
	t.Helper()
	var out []logRecord
	for i, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var rec logRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("sidecar log line %d is not JSON: %v\n%s", i+1, err, line)
		}
		out = append(out, rec)
	}
	return out
}

// opsAt keeps the records one operation wrote at one level. An empty level
// keeps every level.
func opsAt(records []logRecord, operation, level string) []logRecord {
	var out []logRecord
	for _, r := range records {
		if r.Operation != operation {
			continue
		}
		if level != "" && r.Level != level {
			continue
		}
		out = append(out, r)
	}
	return out
}

// requireOnceIn states that a branch was reached EXACTLY once and answers its
// record. Exactly-once is part of the contract ("every error is logged EXACTLY
// ONCE by its owning layer"), so a helper that accepted "at least one" would let
// a double-logged failure pass.
func requireOnceIn(t *testing.T, records []logRecord, operation, level string) logRecord {
	t.Helper()
	got := opsAt(records, operation, level)
	if len(got) != 1 {
		t.Fatalf("operation %q at level %q was recorded %d times, want exactly once; the log held %v",
			operation, level, len(got), operationLevels(records))
	}
	return got[0]
}

// operationLevels renders what the log actually holds, so a failure names
// branches rather than dumping prose.
func operationLevels(records []logRecord) []string {
	var out []string
	for _, r := range records {
		out = append(out, r.Operation+"/"+r.Level)
	}
	return out
}

// ctxString reads one correlation key as a string, failing when the record does
// not carry it. A missing key is the defect the subject exists to catch, so it
// is never defaulted away.
func ctxString(t *testing.T, r logRecord, key string) string {
	t.Helper()
	raw, ok := r.Context[key]
	if !ok {
		t.Fatalf("record %q carries no %q; its context was %v", r.Operation, key, r.Context)
	}
	value, ok := raw.(string)
	if !ok {
		t.Fatalf("record %q carries %q as %T, want a string", r.Operation, key, raw)
	}
	return value
}

// serveLogged is serve with a CAPTURING logger, for the subjects that are about
// the records the client writes rather than the value it returns.
func serveLogged(t *testing.T, store *fakeStore) (*Client, *[]string) {
	t.Helper()
	socket := shortSocket(t)
	listener, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listening on %s: %v", socket, err)
	}
	mux := http.NewServeMux()
	mux.Handle(storev1connect.NewShimStoreHandler(store))
	server := &http.Server{Handler: mux}
	served := make(chan struct{})
	go func() {
		defer close(served)
		_ = server.Serve(listener)
	}()
	t.Cleanup(func() {
		if err := server.Close(); err != nil {
			t.Errorf("closing the fake store's server: %v", err)
		}
		<-served
	})
	var lines []string
	log := logging.New(sliceWriter{lines: &lines}, io.Discard).With(logging.Context{Component: "storeclient-test"})
	return New(socket, log), &lines
}

// sliceWriter collects each written record as one line.
type sliceWriter struct{ lines *[]string }

func (w sliceWriter) Write(p []byte) (int, error) {
	*w.lines = append(*w.lines, string(p))
	return len(p), nil
}

// TestARefusedWriteNamesBothTheRefusalsKindAndItsSite asserts the two refusal
// keys ride together.
//
// THEY ANSWER DIFFERENT QUESTIONS. The KIND is the store's oneof arm and says
// whether a retry can help; the SITE is which call was refused and is what joins
// this record to the store's own record of the same refusal. A reader with only
// the kind knows the verdict but not what it was about.
func TestARefusedWriteNamesBothTheRefusalsKindAndItsSite(t *testing.T) {
	// Arrange.
	client, logs := serveLogged(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{
				Detail: "entry 0 carries no upsert_key",
				Kind: &storev1.WriteBatchFailure_InvalidRequest{
					InvalidRequest: &storev1.WriteBatchInvalidRequest{Field: "batch.entries[0].upsert_key"},
				},
			},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	if err == nil {
		t.Fatal("a refused write returned no error")
	}
	rec := requireOnceIn(t, parseLogLines(t, *logs), "storeclient-write-batch", "error")
	if got := ctxString(t, rec, "refusal_kind"); got != string(RefusalInvalidRequest) {
		t.Errorf("refusal_kind = %q, want the arm the store answered with", got)
	}
	if got := ctxString(t, rec, "refusal_site"); got != WriteBatchSite {
		t.Errorf("refusal_site = %q, want the write call that was refused", got)
	}
	if got := ctxString(t, rec, "field"); got != "batch.entries[0].upsert_key" {
		t.Errorf("field = %q, want the offending field the store named", got)
	}
}

// TestAWithdrawnWriteIsNotStatedAsATransportFailure separates the two ways a
// WriteBatch can come back without an answer. Cancellation is this process
// withdrawing the request on the way out — nothing committed, cursor not
// advanced, the same durable bytes re-read on the next boot, which is exactly
// what the contract promises. A deadline is the store failing to answer, which
// is a fact about the store.
//
// The error itself is returned unchanged in both cases; only the record moves.
// The cancelled case drops to the per-call DETAIL level because the sidecar's
// own storeWrite states the one normal-level `shutdown` record for that fact.
func TestAWithdrawnWriteIsNotStatedAsATransportFailure(t *testing.T) {
	cases := []struct {
		name    string
		callCtx func(*testing.T) context.Context
		// wantErrors is how many ERROR records the call is allowed to leave.
		wantErrors int
	}{
		{
			name: "this process cancelled its own cycle context",
			callCtx: func(t *testing.T) context.Context {
				t.Helper()
				c, cancel := context.WithCancel(context.Background())
				cancel()
				return c
			},
			wantErrors: 0,
		},
		{
			name: "the store did not answer within the deadline",
			callCtx: func(t *testing.T) context.Context {
				t.Helper()
				c, cancel := context.WithDeadline(context.Background(), time.Now().Add(-time.Second))
				t.Cleanup(cancel)
				return c
			},
			wantErrors: 1,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			client, logs := serveLogged(t, &fakeStore{})

			// Act.
			_, err := client.WriteBatch(tc.callCtx(t), &storev1.EntryBatch{}, nil)

			// Assert: the caller is told either way.
			if err == nil {
				t.Fatal("a write that never reached the store returned no error")
			}
			records := parseLogLines(t, *logs)
			if got := len(opsAt(records, "storeclient-write-batch", "error")); got != tc.wantErrors {
				t.Fatalf("error records = %d, want %d: %v", got, tc.wantErrors, operationLevels(records))
			}
		})
	}
}

// TestAWithdrawnCursorRecoveryIsNotStatedAsATransportFailure is the same
// separation on the OTHER verb. Cancellation is this process withdrawing the
// request on the way out — no position recovered, so nothing is read and no
// tailer is built, and the next boot asks again. A deadline is the store failing
// to answer, which is a fact about the store.
func TestAWithdrawnCursorRecoveryIsNotStatedAsATransportFailure(t *testing.T) {
	cases := []struct {
		name    string
		callCtx func(*testing.T) context.Context
		// wantErrors is how many ERROR records the call is allowed to leave.
		wantErrors int
	}{
		{
			name: "this process cancelled its own cycle context",
			callCtx: func(t *testing.T) context.Context {
				t.Helper()
				c, cancel := context.WithCancel(context.Background())
				cancel()
				return c
			},
			wantErrors: 0,
		},
		{
			name: "the store did not answer within the deadline",
			callCtx: func(t *testing.T) context.Context {
				t.Helper()
				c, cancel := context.WithDeadline(context.Background(), time.Now().Add(-time.Second))
				t.Cleanup(cancel)
				return c
			},
			wantErrors: 1,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			client, logs := serveLogged(t, &fakeStore{})

			// Act.
			_, err := client.Cursors(tc.callCtx(t), "")

			// Assert: the caller is told either way.
			if err == nil {
				t.Fatal("a cursor recovery that never reached the store returned no error")
			}
			records := parseLogLines(t, *logs)
			if got := len(opsAt(records, "storeclient-cursors", "error")); got != tc.wantErrors {
				t.Fatalf("error records = %d, want %d: %v", got, tc.wantErrors, operationLevels(records))
			}
		})
	}
}
