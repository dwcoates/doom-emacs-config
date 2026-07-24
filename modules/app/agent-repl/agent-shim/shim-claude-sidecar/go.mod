module agentrepl/shim-claude-sidecar

go 1.23

require (
	agentrepl/proto v0.0.0
	google.golang.org/protobuf v1.36.11
)

replace agentrepl/proto => ../../proto/gen/go

replace agentrepl/wire => ../wire
