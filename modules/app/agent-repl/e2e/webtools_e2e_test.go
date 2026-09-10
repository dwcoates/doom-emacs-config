// webtools_e2e_test.go — the web tools' edge shapes. The happy paths
// (`!web-fetch`, `!web-search`) are remainder_e2e_test.go's; this file owns
// the redirected fetch, whose whole point is that a non-2xx answer is a
// SERVED answer rather than a tool failure.
//
// CONTRACT GROUNDING (read in this worktree):
//
//   - proto/src/conversation/v1/agent_activity.proto — AgentWebFetch's
//     result oneof: "The fetch ran and answered. An HTTP error page is still
//     this arm — the status says how the server answered", against
//     `failure`, which is "The fetch could not run at all". Its
//     AgentWebFetchTarget is "What was fetched... The URL, as the caller
//     gave it", carried on EVERY frame, and AgentWebFetchSuccess.status is
//     "How the server answered. A 404 is a served answer, not a tool
//     failure".
//   - agent-shim/claude/shim/src/convert/tools/web-fetch.ts, whose own
//     header states both rules this test pins: "The target is the CALL'S,
//     never the result's — a redirect makes the two differ: the corpus's one
//     observed fetch asked for `api.slack.com/methods` and the result
//     describes a 302 to another host", and "An HTTP error is a SUCCESS. A
//     302 and a 404 are served answers; the status carries them."
//   - proto/src/frontend/v1/feed.proto — FeedSimpleToolCall /
//     FeedToolCallReturned's succeeded verdict and text form, and
//     FeedToolCallInputLink, the input line drawn as a hyperlink
//     (daemon/internal/resolve/feed/toolcall.go drawWebFetch composes the
//     body as "<code> <codeText>\n\n<result>" and links the input to the
//     TARGET, i.e. the URL the agent asked for).
//
// The fake-SDK vendor rides the real shim in --fake mode against the
// scripted fake git (harness.NewRepo): no real git, no vendor binary, no
// network — the "redirect" is a scripted toolUseResult, never an HTTP hop.
package e2e

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
)

// wtNewWorkspace builds one World and registers a fresh scripted-fake-git
// repository as its workspace.
func wtNewWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return w, ws
}

// wtSettledToolCall matches a settled SimpleToolCall row for the turn and
// tool name.
func wtSettledToolCall(turn *conversationv1.TurnId, toolName string) func(*frontendv1.FeedRow) bool {
	return func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return row.GetTurn().GetValue() == turn.GetValue() &&
			call.GetName().GetText() == toolName &&
			call.GetReturned() != nil
	}
}

// TestWebFetchRedirect drives the `web-fetch-redirect` fake scenario
// (web.ts WEB_FETCH_REDIRECT: a WebFetch of api.example.com/methods answered
// with code 302 / "Found" and the vendor's redirect instruction as the
// result body).
//
// Four facts are pinned, each named by the contract above:
//
//  1. the SUCCEEDED verdict — a 302 is a served answer, and the daemon
//     draws the failed verdict only at code >= 400 (drawWebFetch);
//  2. the status the server answered with, drawn as the body's opening
//     line — this is the only place FeedToolCallReturned carries the code,
//     since the frontend has no typed http-status field of its own;
//  3. the vendor's redirect instruction kept VERBATIM in the same body,
//     rather than folded away into a "redirected" summary the producer
//     never wrote;
//  4. the input link naming the URL THE AGENT ASKED FOR, not the redirect
//     destination — the rule the converter's own header calls out, and the
//     one a redirect is the only way to test.
func TestWebFetchRedirect(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := wtNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "web-fetch-redirect")

	// Assert
	row := awaitFeedRow(t, w, ws, "the redirected WebFetch's settled tool card", wtSettledToolCall(turn, "WebFetch"))
	returned := row.GetActivity().GetSimpleToolCall().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("the redirected WebFetch returned = %v, want the succeeded verdict: a 302 is a served answer, not a tool failure", returned)
	}
	if returned.GetFailed() != nil {
		t.Errorf("the redirected WebFetch carries the failed verdict alongside succeeded: %v", returned)
	}

	body := returned.GetText().GetText()
	if body == "" {
		t.Fatalf("the redirected WebFetch returned = %v, want a text output form carrying the server's answer", returned)
	}
	if !strings.HasPrefix(body, "302 Found\n\n") {
		t.Errorf("the redirected WebFetch's text output = %q, want it to open with the status the server answered (\"302 Found\")", body)
	}
	if !strings.Contains(body, "REDIRECT DETECTED: The URL redirects to a different host.") {
		t.Errorf("the redirected WebFetch's text output = %q, want the vendor's redirect instruction kept verbatim", body)
	}

	const asked = "https://api.example.com/methods"
	if got := row.GetActivity().GetSimpleToolCall().GetInput().GetLink().GetUrl(); got != asked {
		t.Errorf("the redirected WebFetch's input link = %q, want the URL the agent asked for (%q) — the target is the call's, never the result's, so a redirect must not relabel the unit",
			got, asked)
	}
	if strings.Contains(row.GetActivity().GetSimpleToolCall().GetInput().GetLink().GetUrl(), "docs.example.com") {
		t.Errorf("the redirected WebFetch's input link names the redirect DESTINATION, want the asked-for host")
	}
}
