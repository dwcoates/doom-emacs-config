package gomeasure

import "agentrepl/testrun/internal/redundancy/proc"

// The process runner is proc's, shared with every adapter.
type (
	Cmd    = proc.Cmd
	Line   = proc.Line
	Result = proc.Result
	Exec   = proc.Exec
)

// OSExec is the real Exec.
var OSExec = proc.OSExec
