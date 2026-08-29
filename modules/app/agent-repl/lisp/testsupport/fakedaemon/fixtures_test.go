package main

import workspacev1 "agentrepl/proto/workspace/v1"

// workspaceRef is the one echo token the tests hand back verbatim.  Its id is
// opaque by contract: nothing here parses or constructs it from the dir.
var workspaceRef = workspacev1.WorkspaceRef{Id: "ws-test", Dir: "/tmp/ws-test"}
