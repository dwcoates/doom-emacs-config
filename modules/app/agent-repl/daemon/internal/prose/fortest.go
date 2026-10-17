package prose

import frontendv1 "agentrepl/proto/frontend/v1"

// ParseForTest is TEXT parsed as a settled text, for a test that builds a
// served row from plain text: the one way a test turns a string into the
// ParsedProse a row carries. A failure panics, exactly as Parse's does.
func ParseForTest(text string) *frontendv1.ParsedProse {
	return parse(text, Settled)
}
