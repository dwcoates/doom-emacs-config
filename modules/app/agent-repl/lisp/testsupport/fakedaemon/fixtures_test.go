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
