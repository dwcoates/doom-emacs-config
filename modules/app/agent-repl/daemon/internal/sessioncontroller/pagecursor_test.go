package sessioncontroller

import (
	"encoding/base64"
	"errors"
	"strings"
	"testing"
)

// THE CURSOR IS THE DAEMON'S OWN HANDWRITING, and every one of these cases is
// about refusing something that is not it. A cursor a client edited, kept
// across a daemon whose layout changed, or replayed against a conversation it
// was never minted for must come back as a REFUSAL — a re-anchored tail read
// or a silently clamped bound would serve plausible-looking history from the
// wrong place, which is the one failure a paging feed cannot detect for itself.

func TestAMintedCursorRoundTripsUnderItsOwnSession(t *testing.T) {
	// Arrange — the bound a page would hand back after serving its oldest
	// event at seq 4,096.
	want := pageCursor{sessionID: "session-a", beforeSeq: 4096}

	// Act.
	got, err := decodePageCursor(encodePageCursor(want), "session-a")

	// Assert.
	if err != nil {
		t.Fatalf("decodePageCursor: %v", err)
	}
	if got != want {
		t.Fatalf("round-tripped cursor = %+v, want %+v", got, want)
	}
}

func TestACursorIsNotThePlainSeqItCarries(t *testing.T) {
	// Arrange — the contract says a client cannot parse the token. The
	// cheapest way that promise breaks is a token that simply IS the number.
	//
	// Act.
	token := encodePageCursor(pageCursor{sessionID: "session-a", beforeSeq: 4096})

	// Assert.
	if token == "4096" || strings.Contains(token, "4096") {
		t.Fatalf("cursor %q exposes its seq verbatim, which invites a client to do arithmetic on it", token)
	}
}

func TestEveryMalformedCursorIsRefused(t *testing.T) {
	// Arrange — each row is a token that is not this daemon's handwriting, and
	// the reason it is not.
	valid := encodePageCursor(pageCursor{sessionID: "session-a", beforeSeq: 7})
	tests := []struct {
		name    string
		token   string
		session string
	}{
		{
			name:    "empty token",
			token:   "",
			session: "session-a",
		},
		{
			name:    "not the encoding",
			token:   "not-base64-!!",
			session: "session-a",
		},
		{
			name:    "wrong field count",
			token:   base64.RawURLEncoding.EncodeToString([]byte(pageCursorVersion + pageCursorFieldSep + "session-a")),
			session: "session-a",
		},
		{
			name:    "layout from another daemon",
			token:   base64.RawURLEncoding.EncodeToString([]byte("cp0" + pageCursorFieldSep + "session-a" + pageCursorFieldSep + "7")),
			session: "session-a",
		},
		{
			name:    "minted for another conversation",
			token:   valid,
			session: "session-b",
		},
		{
			name:    "bound is not a seq",
			token:   base64.RawURLEncoding.EncodeToString([]byte(pageCursorVersion + pageCursorFieldSep + "session-a" + pageCursorFieldSep + "later")),
			session: "session-a",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := decodePageCursor(tt.token, tt.session)

			// Assert.
			if !errors.Is(err, ErrPageCursorUnreadable) {
				t.Fatalf("decodePageCursor(%q, %q) error = %v, want ErrPageCursorUnreadable", tt.token, tt.session, err)
			}
		})
	}
}

func TestClampingResolvesEveryRequestedLimit(t *testing.T) {
	// Arrange — the three things a client can ask for.
	tests := []struct {
		name      string
		requested uint32
		want      uint32
	}{
		{name: "zero takes the daemon default", requested: 0, want: pageDefaultLimit},
		{name: "an ordinary limit is served as asked", requested: 12, want: 12},
		{name: "the ceiling is a clamp, not a refusal", requested: 5000, want: pageMaxLimit},
		{name: "exactly the ceiling is unchanged", requested: pageMaxLimit, want: pageMaxLimit},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := clampPageLimit(tt.requested)

			// Assert.
			if got != tt.want {
				t.Fatalf("clampPageLimit(%d) = %d, want %d", tt.requested, got, tt.want)
			}
		})
	}
}
