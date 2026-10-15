package storewrite

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func interactive() *storev1.WriteClass {
	return &storev1.WriteClass{WriteClass: &storev1.WriteClass_Interactive{Interactive: &storev1.WriteClassInteractive{}}}
}

func entry() *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:     &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}},
		WriteId:   "w1",
		UpsertKey: "k1",
		Entry:     &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{}},
	}
}

func request(batch *storev1.EntryBatch) *storev1.WriteBatchRequest {
	return &storev1.WriteBatchRequest{Producer: "sidecar", WriteClass: interactive(), Batch: batch}
}

func shape() *storev1.ShapeObservation {
	return &storev1.ShapeObservation{ShapeHash: "h", Kind: "k", KeyStructure: "s", SeenMs: 1}
}

func TestValidateWriteBatchRequest(t *testing.T) {
	tests := []struct {
		name      string
		req       *storev1.WriteBatchRequest
		wantSite  string
		wantField string
	}{
		{name: "a well-formed write", req: request(&storev1.EntryBatch{Entries: []*storev1.StoreEntry{entry()}})},
		{name: "a cursor-only batch is legal", req: request(&storev1.EntryBatch{CursorAdvance: &storev1.CursorState{FileId: "f"}})},
		{name: "a claims-only batch is legal", req: request(&storev1.EntryBatch{ShellRunClaims: []*storev1.ShellRunClaim{{VendorTaskId: "t"}}})},
		{name: "a locator-only batch is legal", req: request(&storev1.EntryBatch{AgentLocators: []*storev1.AgentLocator{{}}})},
		{name: "a shapes-only write is legal", req: func() *storev1.WriteBatchRequest {
			r := request(&storev1.EntryBatch{})
			r.Shapes = []*storev1.ShapeObservation{shape()}
			return r
		}()},
		{name: "no producer", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); r.Producer = ""; return r }(), wantSite: SiteProducerEmpty, wantField: "producer"},
		{name: "no write class", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); r.WriteClass = nil; return r }(), wantSite: SiteWriteClassUnset, wantField: "write_class"},
		{name: "no batch", req: request(nil), wantSite: SiteBatchMissing, wantField: "batch"},
		{name: "a batch stating nothing", req: request(&storev1.EntryBatch{}), wantSite: SiteBatchEmpty, wantField: "batch"},
		{name: "an entry with no plane", req: request(&storev1.EntryBatch{Entries: []*storev1.StoreEntry{func() *storev1.StoreEntry { e := entry(); e.Plane = nil; return e }()}}), wantSite: SiteEntryPlaneUnset, wantField: "entries[0].plane"},
		{name: "an entry with no write id", req: request(&storev1.EntryBatch{Entries: []*storev1.StoreEntry{func() *storev1.StoreEntry { e := entry(); e.WriteId = ""; return e }()}}), wantSite: SiteEntryWriteIDEmpty, wantField: "entries[0].write_id"},
		{name: "an entry with no upsert key", req: request(&storev1.EntryBatch{Entries: []*storev1.StoreEntry{func() *storev1.StoreEntry { e := entry(); e.UpsertKey = ""; return e }()}}), wantSite: SiteEntryUpsertKeyEmpty, wantField: "entries[0].upsert_key"},
		{name: "an entry with no arm", req: request(&storev1.EntryBatch{Entries: []*storev1.StoreEntry{func() *storev1.StoreEntry { e := entry(); e.Entry = nil; return e }()}}), wantSite: SiteEntryArmUnset, wantField: "entries[0].entry"},
		{name: "a nil entry", req: request(&storev1.EntryBatch{Entries: []*storev1.StoreEntry{nil}}), wantSite: SiteEntryArmUnset, wantField: "entries[0]"},
		{name: "a cursor naming no file", req: request(&storev1.EntryBatch{CursorAdvance: &storev1.CursorState{}}), wantSite: SiteCursorFileIDEmpty, wantField: "cursor_advance.file_id"},
		{name: "an unset shape", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); r.Shapes = []*storev1.ShapeObservation{nil}; return r }(), wantSite: SiteShapeUnset, wantField: "shapes[0]"},
		{name: "a shape with no hash", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); s := shape(); s.ShapeHash = ""; r.Shapes = []*storev1.ShapeObservation{s}; return r }(), wantSite: SiteShapeHashEmpty, wantField: "shapes[0].shape_hash"},
		{name: "a shape with no kind", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); s := shape(); s.Kind = ""; r.Shapes = []*storev1.ShapeObservation{s}; return r }(), wantSite: SiteShapeKindEmpty, wantField: "shapes[0].kind"},
		{name: "a shape with no structure", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); s := shape(); s.KeyStructure = ""; r.Shapes = []*storev1.ShapeObservation{s}; return r }(), wantSite: SiteShapeStructureEmpty, wantField: "shapes[0].key_structure"},
		{name: "a shape with no instant", req: func() *storev1.WriteBatchRequest { r := request(&storev1.EntryBatch{}); s := shape(); s.SeenMs = 0; r.Shapes = []*storev1.ShapeObservation{s}; return r }(), wantSite: SiteShapeSeenMsUnset, wantField: "shapes[0].seen_ms"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := ValidateWriteBatchRequest(tt.req)

			// Assert.
			if tt.wantSite == "" {
				if got != nil {
					t.Fatalf("refused %+v, want accepted", got)
				}
				return
			}
			if got == nil || got.Site != tt.wantSite || got.Field != tt.wantField || got.Detail == "" {
				t.Fatalf("refusal = %+v, want site %s field %s with a detail", got, tt.wantSite, tt.wantField)
			}
		})
	}
}
