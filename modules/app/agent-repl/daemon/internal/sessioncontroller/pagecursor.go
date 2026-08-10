package sessioncontroller

import (
	"encoding/base64"
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// The CONTINUATION CURSOR a ConversationPage hands back, and the only thing
// that can read it.
//
// # Why it is opaque, and why that is enforced rather than merely documented
//
// The wire contract says a client copies the cursor back BYTE-FOR-BYTE and
// never parses it (conversation-page.proto). That is a promise the daemon has
// to be able to keep on its own: a token that looks like a seq invites a
// client to do arithmetic on it, and the first client that does becomes a
// second authority on where a page starts. So the encoding is deliberately not
// a bare number — it is a versioned, session-bound blob behind base64url — and
// every field it carries is CHECKED on the way back in.
//
// # What it binds, and why each binding exists
//
//   - The SESSION. A cursor names a position in one conversation's seq space.
//     Seq spaces are per vendor session and RESTART at one when a session
//     rotates (a /clear, a compaction, a fresh conversation), so seq 4,000 of
//     the session that minted a cursor is a completely different event from
//     seq 4,000 of the session a stale tab replays it against. Without the
//     binding that stale tab silently pages someone else's history; with it,
//     the cursor is refused.
//   - The BEFORE-SEQ. The exclusive upper bound of the next page: the seq of
//     the oldest event the page that minted this cursor served.
//
// # Every malformed cursor is a REFUSAL
//
// Not a re-anchored tail read, not an empty page, not a silently clamped
// bound. A cursor the daemon cannot read means the client and the daemon
// disagree about what was minted, and serving a guess would hide that
// disagreement behind plausible-looking history. The refusal carries
// ErrPageCursorUnreadable, so a caller can tell "this client's cursor is junk"
// apart from "the store could not be read".

// pageCursorVersion prefixes every minted cursor.
//
// It is a VERSION, not decoration: the fields below are the daemon's own
// business and may change, and a cursor minted by a daemon that has since been
// replaced must be refused rather than misread under the new layout. A client
// holding a cursor across a daemon upgrade is the ordinary case (the tab
// outlives the bounce), so this is the edge that decides between a clean
// refusal and a wrong page.
const pageCursorVersion = "cp1"

// pageCursorFieldSep separates the encoded fields inside the blob. It cannot
// appear in either field: the version is a constant, and a session id is a
// uuid.
const pageCursorFieldSep = "\x1f"

// ErrPageCursorUnreadable reports a continuation cursor this daemon did not
// mint, cannot decode, or that names a different conversation.
var ErrPageCursorUnreadable = errors.New("session-controller: conversation page cursor is unreadable")

// pageCursor is the decoded continuation handle.
type pageCursor struct {
	// sessionID is the daemon session whose seq space beforeSeq is expressed
	// in. A cursor replayed against any other session is refused.
	sessionID string
	// beforeSeq is the EXCLUSIVE upper bound of the page this cursor asks for:
	// the seq of the oldest event the minting page served.
	beforeSeq uint64
}

// encodePageCursor mints the opaque token for one continuation.
func encodePageCursor(c pageCursor) string {
	raw := strings.Join([]string{
		pageCursorVersion,
		c.sessionID,
		strconv.FormatUint(c.beforeSeq, 10),
	}, pageCursorFieldSep)
	return base64.RawURLEncoding.EncodeToString([]byte(raw))
}

// decodePageCursor reads a cursor back, refusing anything this daemon did not
// mint for THIS session.
//
// expectSessionID is the session the page is being served for. It is a
// parameter rather than a field the caller compares afterwards, so the check
// cannot be forgotten at a new call site: there is no way to obtain a decoded
// cursor without having stated which conversation it must belong to.
func decodePageCursor(token, expectSessionID string) (pageCursor, error) {
	if token == "" {
		return pageCursor{}, fmt.Errorf("%w: the before anchor carried no cursor at all",
			ErrPageCursorUnreadable)
	}
	blob, err := base64.RawURLEncoding.DecodeString(token)
	if err != nil {
		return pageCursor{}, fmt.Errorf("%w: cursor is not the encoding this daemon mints: %w",
			ErrPageCursorUnreadable, err)
	}
	fields := strings.Split(string(blob), pageCursorFieldSep)
	if len(fields) != 3 {
		return pageCursor{}, fmt.Errorf("%w: cursor carries %d field(s), not the 3 this daemon mints",
			ErrPageCursorUnreadable, len(fields))
	}
	if fields[0] != pageCursorVersion {
		return pageCursor{}, fmt.Errorf("%w: cursor version %q is not %q, so it was minted by a daemon whose layout this one cannot read",
			ErrPageCursorUnreadable, fields[0], pageCursorVersion)
	}
	if fields[1] != expectSessionID {
		return pageCursor{}, fmt.Errorf("%w: cursor names session %q but the page is being served for session %q, whose seq space is a different one entirely",
			ErrPageCursorUnreadable, fields[1], expectSessionID)
	}
	beforeSeq, err := strconv.ParseUint(fields[2], 10, 64)
	if err != nil {
		return pageCursor{}, fmt.Errorf("%w: cursor bound %q is not a seq: %w",
			ErrPageCursorUnreadable, fields[2], err)
	}
	return pageCursor{sessionID: fields[1], beforeSeq: beforeSeq}, nil
}
