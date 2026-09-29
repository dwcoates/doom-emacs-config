package main

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// workspaceRef is the one echo token the tests hand back verbatim.  Its id is
// opaque by contract: nothing here parses or constructs it from the dir.
var workspaceRef = workspacev1.WorkspaceRef{Id: "ws-test", Dir: "/tmp/ws-test"}

// completeSubmit is the smallest SubmitPromptRequest that satisfies the
// validation invariant: the workspace ref (landing 2), the said content, an
// idempotency key, and an origin that is not UNSPECIFIED.  `feed` stays
// unset — the root composer submits to the session, not to a sub-feed.
func completeSubmit(key string) *agentreplv1.SubmitPromptRequest {
	return &agentreplv1.SubmitPromptRequest{
		Workspace:      &workspaceRef,
		Said:           &conversationv1.UserSaid{Content: &conversationv1.UserContent{}},
		IdempotencyKey: key,
		Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}
}

// emacsWatchDaemon is the WatchDaemon request Emacs sends: the client arm is
// REQUIRED, and the Emacs arm must state the elisp it has loaded and whether
// Emacs is focused.
func emacsWatchDaemon() *agentreplv1.WatchDaemonRequest {
	return &agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Emacs{
			Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "fixture-elisp-build", Focus: unfocused()},
		},
	}
}

// unfocused is the EditorFocus an unfocused Emacs states.
func unfocused() *agentreplv1.EditorFocus {
	return &agentreplv1.EditorFocus{Focus: &agentreplv1.EditorFocus_Unfocused{Unfocused: &agentreplv1.EditorFocusUnfocused{}}}
}

// emacsWatchDaemonJSON is emacsWatchDaemon's protojson spelling, for the tests
// that speak the wire by hand.
const emacsWatchDaemonJSON = `{"emacs":{"elispBuild":"fixture-elisp-build","focus":{"unfocused":{}}}}`
