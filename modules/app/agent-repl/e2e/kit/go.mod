module agentrepl/e2e/kit

go 1.24.0

require (
	agentrepl/proto v0.0.0
	agentrepl/testrun v0.0.0
	connectrpc.com/connect v1.17.0
)

require google.golang.org/protobuf v1.36.11 // indirect

replace agentrepl/proto => ../../proto/gen/go

replace agentrepl/testrun => ../../testrun
