package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — the api_error TAXONOMY table, end to end.
//
// apiErrorKind maps the vendor's own declared error type onto ApiRequestFailed's
// closed kind set, and a type this schema does not model arrives as `unmodeled`
// CARRYING THE NAME rather than as a silently mishandled value. This is the
// table that walks that mapping through the real binary.
//
// GROUNDING LIMIT — READ BEFORE ADDING A ROW. The checked-in corpus holds
// EXACTLY ONE api_error record: a CONNECTION failure
// (`error.connection.code = "StreamSuspended"`, with no vendor `type` at all),
// which is why the one grounded row below expects the `unmodeled` arm naming
// `connection/StreamSuspended`. rate_limit_error, overloaded_error,
// authentication_error, the numeric-status-only shape and an unknown type
// string HAVE NO FIXTURE. A row for any of them would have to be a record this
// project INVENTED and called a vendor capture, which is precisely the thing the
// golden corpus exists to prevent — so they are not written here. When a capture
// run records one, drop the fixture under
// testdata/corpus/transcript-lines/ and add its row to the table; nothing else
// about this subject changes. The unit-level table in
// internal/convert/system_test.go is where a mapping is exercised without a
// capture.

// apiErrorCase is one row of the taxonomy table: a captured api_error record,
// and what its converted kind must be.
type apiErrorCase struct {
	name string
	// fixture is a corpus path, relative to testdata/corpus. A row MUST name a
	// real captured record; see the grounding limit above.
	fixture string
	// wantKind states the arm and its contents, by inspecting the converted
	// failure rather than by comparing prose.
	wantKind func(t *testing.T, failed *conversationv1.ApiRequestFailed)
}

// TestApiErrorTaxonomyTable walks every GROUNDED api_error shape the corpus
// holds through the sidecar and asserts the kind arm it lands on.
func TestApiErrorTaxonomyTable(t *testing.T) {
	t.Parallel()
	cases := []apiErrorCase{
		{
			name:    "a connection failure has no vendor type and is named rather than guessed",
			fixture: "transcript-lines/system-api_error.jsonl",
			wantKind: func(t *testing.T, failed *conversationv1.ApiRequestFailed) {
				t.Helper()
				unmodeled := failed.GetUnmodeled()
				if unmodeled == nil {
					t.Fatalf("a connection failure carries no vendor type, so it must land on `unmodeled`; it landed on %v", failed.GetKind())
				}
				if got := unmodeled.GetType(); got != "connection/StreamSuspended" {
					t.Errorf("the unmodeled type is %q, wanted the vendor's own connection code %q",
						got, "connection/StreamSuspended")
				}
			},
		},
	}

	// NOTE: a subtest NAME becomes part of t.TempDir()'s path, and
	// --config-roots is COMMA-SEPARATED — so a row name carrying a comma splits
	// the config root and the sidecar discovers nothing. Keep row names
	// comma-free.
	sessions := []string{"a1a1a1a1-a1a1-4a1a-8a1a-a1a1a1a1a1a1"}
	for i, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ctx, cancel := testContext(t)
			defer cancel()
			fake := startFakeStore(t)
			tree := newVendorTree(t)
			cwd := "/work/api-error-table-probe"
			session := sessions[i]
			g, uuid := seedApiErrorFixture(t, tree, cwd, session, tc.fixture)

			// Act.
			startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
			wantKey := "session:api_error:" + uuid
			fake.awaitEntry(ctx, t, "the api_error record", func(e *storev1.StoreEntry) bool {
				return e.GetUpsertKey() == wantKey
			})
			awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

			// Assert.
			failed := apiErrorOf(entryByUpsertKey(fake.Entries(), wantKey).GetAgentUpdate().GetServeableFrame())
			if failed == nil {
				t.Fatalf("the api_error produced no ApiRequestFailed")
			}
			tc.wantKind(t, failed)
		})
	}
}

// seedApiErrorFixture is seedApiError over an arbitrary corpus api_error
// fixture, so the taxonomy table can grow a row per captured shape.
func seedApiErrorFixture(t *testing.T, tree *vendorTree, cwd, session, fixture string) (*growingFile, string) {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)
	rec := retargetSession(t, decodeRecord(t, corpusLine(t, fixture, 0)), session, cwd)
	uuid, _ := rec["uuid"].(string)
	if uuid == "" {
		t.Fatalf("the api_error fixture %s carries no uuid", fixture)
	}
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, rec))
	return g, uuid
}
