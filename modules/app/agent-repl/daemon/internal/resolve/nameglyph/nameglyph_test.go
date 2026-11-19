package nameglyph

import (
	"testing"

	"agent-repld/internal/wsm"
)

func TestOf(t *testing.T) {
	tests := []struct {
		name      string
		ws        wsm.Workspace
		wantGlyph string
		wantOK    bool
	}{
		{name: "an emoji-ack workspace wears the thumbs up", ws: wsm.Workspace{EmojiAck: true}, wantGlyph: "👍", wantOK: true},
		{name: "any other workspace wears none", ws: wsm.Workspace{}, wantGlyph: "", wantOK: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			glyph, ok := Of(tt.ws)

			// Assert
			if glyph != tt.wantGlyph || ok != tt.wantOK {
				t.Fatalf("Of = %q, %v; want %q, %v", glyph, ok, tt.wantGlyph, tt.wantOK)
			}
		})
	}
}
