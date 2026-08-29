module claude-repld

go 1.24

require (
	agentrepl/proto v0.0.0
	google.golang.org/protobuf v1.36.11
)

replace agentrepl/proto => ../proto/gen/go

replace agentrepl/wire => ../agent-shim/wire

replace agentrepl/logging => ../agent-shim/logging/go
