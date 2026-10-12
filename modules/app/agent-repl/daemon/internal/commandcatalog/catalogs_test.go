package commandcatalog

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/commandcatalog/catalogtest"
	"agent-repld/internal/ids"
)

func TestCatalog(t *testing.T) {
	tests := []struct {
		name      string
		statedTo  map[ids.WorkspaceID][][]string
		ask       ids.WorkspaceID
		want      *conversationv1.SessionCommandCatalog
		wantKnown bool
	}{
		{
			name:      "a workspace whose session stated nothing is unknown",
			statedTo:  nil,
			ask:       "ws-1",
			wantKnown: false,
		},
		{
			name:      "a stated catalog is answered",
			statedTo:  map[ids.WorkspaceID][][]string{"ws-1": {{"merge-queue", "compact"}}},
			ask:       "ws-1",
			want:      catalogtest.Named("merge-queue", "compact"),
			wantKnown: true,
		},
		{
			name:      "a later statement replaces the earlier one whole",
			statedTo:  map[ids.WorkspaceID][][]string{"ws-1": {{"compact"}, {"new-skill"}}},
			ask:       "ws-1",
			want:      catalogtest.Named("new-skill"),
			wantKnown: true,
		},
		{
			name:      "an empty catalog is a known answer",
			statedTo:  map[ids.WorkspaceID][][]string{"ws-1": {{}}},
			ask:       "ws-1",
			want:      catalogtest.Named(),
			wantKnown: true,
		},
		{
			name:      "another workspace's catalog is not this one's",
			statedTo:  map[ids.WorkspaceID][][]string{"ws-2": {{"compact"}}},
			ask:       "ws-1",
			wantKnown: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			catalogs := New()
			for ws, statements := range tt.statedTo {
				for _, names := range statements {
					catalogs.OnCommandCatalog(ws, catalogtest.Named(names...))
				}
			}

			// Act.
			got, known := catalogs.Catalog(tt.ask)

			// Assert.
			if known != tt.wantKnown {
				t.Fatalf("Catalog(%s) known = %v, want %v", tt.ask, known, tt.wantKnown)
			}
			if tt.wantKnown && !proto.Equal(got, tt.want) {
				t.Fatalf("Catalog(%s) = %v, want %v", tt.ask, got, tt.want)
			}
		})
	}
}

func TestOnCommandCatalogPanicsOnANilCatalog(t *testing.T) {
	// Arrange.
	catalogs := New()
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("OnCommandCatalog accepted a nil catalog")
		}
		if _, known := catalogs.Catalog("ws-1"); known {
			t.Fatal("a refused nil catalog was recorded")
		}
	}()

	// Act.
	catalogs.OnCommandCatalog("ws-1", nil)
}
