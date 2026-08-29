module agentrepl/shim-claude-sidecar

go 1.23.0

require (
	agentrepl/logging v0.0.0
	agentrepl/proto v0.0.0
	connectrpc.com/connect v1.17.0
	github.com/fsnotify/fsnotify v1.9.0
	golang.org/x/sys v0.35.0
	google.golang.org/protobuf v1.36.11
)

require golang.org/x/net v0.43.0 // indirect

replace agentrepl/proto => ../../../proto/gen/go

replace agentrepl/logging => ../../logging/go
