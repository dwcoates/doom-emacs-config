module agentrepl/testservice

go 1.24.0

require (
	agentrepl/logging v0.0.0
	agentrepl/proto v0.0.0
	connectrpc.com/connect v1.17.0
	golang.org/x/net v0.43.0
)

require (
	golang.org/x/sys v0.37.0 // indirect
	golang.org/x/text v0.28.0 // indirect
	google.golang.org/protobuf v1.36.11 // indirect
)

replace agentrepl/logging => ../agent-shim/logging/go

replace agentrepl/proto => ../proto/gen/go
