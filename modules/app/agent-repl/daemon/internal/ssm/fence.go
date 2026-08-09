package ssm

import "strings"

// Fence mints a workspace's STALENESS TOKEN — the single opaque string every
// per-workspace push carries so a renderer can tell a current push from a stale
// one without holding any session vocabulary at all.
//
// It lives here because the SSM is where a workspace's identity is composed:
// the two things whose rotation makes a push stale are the owning session and
// the controller generation that serves it, and this is the one place both are
// authoritative. Minting it anywhere else would be a second answer to the same
// question.
//
// THE TOKEN IS OPAQUE TO EVERY CONSUMER. It is compared BYTE-WISE and never
// parsed, split or interpreted, on either side of the wire — the composition
// below is free to change, and the only property anything may depend on is that
// two fences are equal exactly when neither identity has rotated.
//
// The separator is a PRINTABLE character neither identity can contain — both
// are an ASCII prefix followed by hex — so no pair of distinct identities can
// compose to one token, and the token survives every hand-written JSON encoder
// without an escape rule. A control character composed just as unambiguously
// and then broke exactly that: %q-quoting one into a JSON command produced an
// escape the wire's own decoder rejected.
const fenceSeparator = '|'

func Fence(sessionID, controllerGenerationID string) string {
	return sessionID + string(fenceSeparator) + controllerGenerationID
}

// SplitFence is Fence's INVERSE, and it is the daemon's alone.
//
// A fence crosses the wire as one opaque token precisely so no client has to
// hold two identities in agreement; the daemon still needs both, because its
// resync eligibility ladder distinguishes a stale SESSION under a current
// generation (a rebind it serves) from a stale generation (a rejection). This
// is the only place the token is ever read, and it lives beside the mint so the
// two can never drift.
//
// A token with no separator is one this daemon did not mint — an out-of-date
// client, or a value invented somewhere. It yields two empty identities, which
// the ladder rejects loudly, rather than being guessed at.
func SplitFence(fence string) (sessionID, controllerGenerationID string) {
	sep := strings.IndexByte(fence, fenceSeparator)
	if sep < 0 {
		return "", ""
	}
	return fence[:sep], fence[sep+1:]
}
