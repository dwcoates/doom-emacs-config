module claude-repld

go 1.24

require (
	agentrepl/logging v0.0.0
	agentrepl/proto v0.0.0
	agentrepl/wire v0.0.0
	connectrpc.com/connect v1.18.1
	github.com/creack/pty v1.1.24
	github.com/google/uuid v1.6.0
	golang.org/x/net v0.50.0
	google.golang.org/protobuf v1.36.11
	modernc.org/sqlite v1.34.4
)

replace agentrepl/proto => ../proto/gen/go

replace agentrepl/wire => ../agent-shim/wire

replace agentrepl/logging => ../agent-shim/logging/go
