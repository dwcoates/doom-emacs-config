module agentrepl/testservice

go 1.24.0

require agentrepl/logging v0.0.0

require golang.org/x/sys v0.37.0 // indirect

replace agentrepl/logging => ../agent-shim/logging/go

replace agentrepl/proto => ../proto/gen/go
