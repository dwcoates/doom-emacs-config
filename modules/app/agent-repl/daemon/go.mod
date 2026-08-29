module claude-repld

go 1.24

require (
	agentrepl/proto v0.0.0
	connectrpc.com/connect v1.17.0
	golang.org/x/net v0.43.0
	google.golang.org/protobuf v1.36.11
)

require golang.org/x/text v0.28.0 // indirect

replace agentrepl/proto => ../proto/gen/go

replace agentrepl/wire => ../agent-shim/wire

replace agentrepl/logging => ../agent-shim/logging/go
