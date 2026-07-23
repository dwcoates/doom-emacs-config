// Package protodeps pins the agent-shim dependencies into the daemon module
// before the farm-out groups (ssm, shimclient, workspace/merge, frontend)
// land their importers, so ten concurrent worktrees never conflict on
// go.mod. Stitch-phase cleanup deletes this file once real importers exist.
package protodeps

import (
	_ "agentrepl/proto/agentshim/core/v1"
	_ "agentrepl/wire"
	_ "google.golang.org/protobuf/proto"
	_ "modernc.org/sqlite"
)
