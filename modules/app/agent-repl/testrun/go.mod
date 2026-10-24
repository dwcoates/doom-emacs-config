module agentrepl/testrun

go 1.24.0

require (
	agentrepl/logging v0.0.0-00010101000000-000000000000
	agentrepl/proto v0.0.0
	golang.org/x/sys v0.37.0
	google.golang.org/protobuf v1.36.11
)

replace agentrepl/logging => ../agent-shim/logging/go

replace agentrepl/proto => ../proto/gen/go
