module agentrepl/fakedaemon

go 1.25.0

require (
	agentrepl/proto v0.0.0-00010101000000-000000000000
	connectrpc.com/connect v1.17.0
	golang.org/x/net v0.55.0
	google.golang.org/protobuf v1.36.11
)

require golang.org/x/text v0.37.0 // indirect

replace agentrepl/proto => ../../../proto/gen/go
