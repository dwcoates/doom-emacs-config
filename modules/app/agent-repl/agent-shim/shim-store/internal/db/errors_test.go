package db

import (
	"errors"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

func TestInvalidfIsErrInvalid(t *testing.T) {
	// Arrange, Act
	err := invalidf("field %s is empty", "write_id")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if !strings.Contains(err.Error(), "field write_id is empty") {
		t.Fatalf("detail lost: %v", err)
	}
}

func TestStalePointerfIsErrStalePointer(t *testing.T) {
	// Arrange, Act
	err := stalePointerf("after", "after %q names no line of book %q", "sip1-1", "agent-1")

	// Assert
	if !errors.Is(err, ErrStalePointer) {
		t.Fatalf("error = %v, want ErrStalePointer", err)
	}
}

func TestStoragefKeepsTheDriverCauseReachable(t *testing.T) {
	// Arrange: the driver's own classification must survive the wrapping, or
	// an operator loses the only account of what the database actually said.
	cause := errors.New("disk I/O error")

	// Act
	err := storagef(cause, "writing entry %s", "u1")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	if !errors.Is(err, cause) {
		t.Fatalf("driver cause is unreachable through %v", err)
	}
}

func TestSentinelsAreDistinct(t *testing.T) {
	// Arrange, Act, Assert: a refusal must never satisfy two classes at once,
	// or the server would map one refusal to two failure arms.
	if errors.Is(invalidf("x"), ErrStorage) {
		t.Fatal("an invalid request satisfied ErrStorage")
	}
	if errors.Is(stalePointerf("after", "x"), ErrInvalid) {
		t.Fatal("a stale pointer satisfied ErrInvalid")
	}
}

// ---- the structured refusal detail ----

func TestRefusalSiteNamesTheGenericStoreRefusal(t *testing.T) {
	// Arrange: a validation refusal with no finer site to name.

	// Act
	got := RefusalSite(invalidFieldf("producer", "producer is empty"))

	// Assert
	if got != SiteStoreRefusedRequest {
		t.Fatalf("RefusalSite = %q, want %q", got, SiteStoreRefusedRequest)
	}
}

func TestRefusalSiteNamesASpecificSite(t *testing.T) {
	// Arrange

	// Act
	got := RefusalSite(invalidSitef(SitePageBookMismatch, "entries[0]", "the envelope and the frame disagree"))

	// Assert
	if got != SitePageBookMismatch {
		t.Fatalf("RefusalSite = %q, want %q", got, SitePageBookMismatch)
	}
}

func TestRefusalSiteNamesTheStalePointerSite(t *testing.T) {
	// Arrange

	// Act
	got := RefusalSite(stalePointerf("after", "after names no line"))

	// Assert
	if got != SiteStalePointer {
		t.Fatalf("RefusalSite = %q, want %q", got, SiteStalePointer)
	}
}

func TestRefusalFieldCarriesTheStoresOwnFieldName(t *testing.T) {
	// Arrange: the server never opens a frame, so without this the producer
	// would receive an unnamed "invalid request" for everything inside one.

	// Act
	got := RefusalField(invalidFieldf("entries[1].upsert_key", "upsert_key is empty"))

	// Assert
	if got != "entries[1].upsert_key" {
		t.Fatalf("RefusalField = %q, want entries[1].upsert_key", got)
	}
}

func TestRefusalFieldIsEmptyWhenNoSingleFieldIsAtFault(t *testing.T) {
	// Arrange

	// Act
	got := RefusalField(invalidf("the batch cannot be re-serialized"))

	// Assert
	if got != "" {
		t.Fatalf("RefusalField = %q, want an empty name", got)
	}
}

func TestRefusalSiteIsEmptyForAStorageFailure(t *testing.T) {
	// Arrange: a storage failure names no request field and no refusal site;
	// the server classifies it by its sentinel.

	// Act
	got := RefusalSite(storagef(errors.New("disk"), "writing entry"))

	// Assert
	if got != "" {
		t.Fatalf("RefusalSite = %q, want an empty site", got)
	}
}

func TestAStructuredRefusalIsStillItsSentinel(t *testing.T) {
	// Arrange: no caller should have to learn the refusal type to classify one.
	tests := []struct {
		name string
		err  error
		want error
	}{
		{name: "invalid", err: invalidFieldf("producer", "empty"), want: ErrInvalid},
		{name: "stale pointer", err: stalePointerf("after", "gone"), want: ErrStalePointer},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := errors.Is(tc.err, tc.want)

			// Assert
			if !got {
				t.Fatalf("errors.Is(%v, %v) = false, want true", tc.err, tc.want)
			}
		})
	}
}

func TestEntryFieldSpellsThePathTheFailureArmReports(t *testing.T) {
	// Arrange
	tests := []struct {
		name  string
		index int
		path  string
		want  string
	}{
		{name: "a named field", index: 1, path: "upsert_key", want: "entries[1].upsert_key"},
		{name: "the entry itself", index: 3, path: "", want: "entries[3]"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := entryField(tc.index, tc.path)

			// Assert
			if got != tc.want {
				t.Fatalf("entryField = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestQueryErrorLogsAndReturnsTheDriverFailure(t *testing.T) {
	// Arrange: every lifecycle write funnels a driver failure through
	// queryError, but the happy-path suites never provoke a real driver
	// error, so this pins the method directly.
	d, s := newStore(t)
	cause := errors.New("disk I/O error")

	// Act
	got := d.queryError("store.db.write-batch", "agent", logging.Fields{AgentID: "agent-1"}, cause)

	// Assert: the caller gets back exactly the error it handed in.
	if !errors.Is(got, cause) {
		t.Fatalf("queryError returned %v, want it to wrap %v", got, cause)
	}
	records := s.records(t)
	if len(records) == 0 {
		t.Fatal("no records logged")
	}
	record := records[len(records)-1]
	if record["operation"] != "store.db.write-batch" {
		t.Fatalf("operation = %v, want store.db.write-batch", record["operation"])
	}
	if record["level"] != "error" {
		t.Fatalf("level = %v, want error", record["level"])
	}
	context, ok := record["context"].(map[string]any)
	if !ok {
		t.Fatalf("context = %v, want a map", record["context"])
	}
	if context["table"] != "agent" {
		t.Fatalf("context.table = %v, want agent", context["table"])
	}
	if context["error"] != cause.Error() {
		t.Fatalf("context.error = %v, want %v", context["error"], cause.Error())
	}
}

func TestAWriteBatchRefusalNamesTheOffendingEntrysField(t *testing.T) {
	// Arrange: the detail names the index, and the FIELD names it in a form the
	// producer can act on without parsing prose.
	d, _ := newStore(t)
	good := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	bad := &storev1.StoreEntry{Plane: streamPlane(), WriteId: "w2"}

	// Act
	_, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(good, bad), nil)

	// Assert
	if RefusalField(err) != "entries[1].upsert_key" {
		t.Fatalf("RefusalField = %q, want entries[1].upsert_key (error: %v)", RefusalField(err), err)
	}
}
