module agent-repld

go 1.24.0

require (
	agentrepl/logging v0.0.0-00010101000000-000000000000
	agentrepl/proto v0.0.0
	agentrepl/protohelpers v0.0.0
	agentrepl/testrun v0.0.0
	connectrpc.com/connect v1.17.0
	github.com/creack/pty v1.1.24
	github.com/google/uuid v1.6.0
	github.com/prometheus/client_golang v1.23.2
	github.com/tree-sitter/go-tree-sitter v0.25.0
	go.yaml.in/yaml/v3 v3.0.4
	golang.org/x/net v0.43.0
	golang.org/x/sys v0.37.0
	golang.org/x/text v0.28.0
	google.golang.org/protobuf v1.36.11
	modernc.org/sqlite v1.46.1
)

require github.com/mattn/go-pointer v0.0.1 // indirect

require (
	github.com/beorn7/perks v1.0.1 // indirect
	github.com/cespare/xxhash/v2 v2.3.0 // indirect
	github.com/munnerz/goautoneg v0.0.0-20191010083416-a7dc8b61c822 // indirect
	github.com/prometheus/client_model v0.6.2
	github.com/prometheus/common v0.66.1 // indirect
	github.com/prometheus/procfs v0.16.1 // indirect
	go.yaml.in/yaml/v2 v2.4.2 // indirect
)

require (
	agentrepl/tldr v0.0.0
	agentrepl/tldrprompt v0.0.0
	github.com/dustin/go-humanize v1.0.1 // indirect
	github.com/mattn/go-isatty v0.0.20 // indirect
	github.com/ncruces/go-strftime v1.0.0 // indirect
	github.com/remyoudompheng/bigfft v0.0.0-20230129092748-24d4a6f8daec // indirect
	golang.org/x/exp v0.0.0-20251023183803-a4bb9ffd2546 // indirect
	modernc.org/libc v1.67.6 // indirect
	modernc.org/mathutil v1.7.1 // indirect
	modernc.org/memory v1.11.0 // indirect
)

replace agentrepl/proto => ../proto/gen/go

replace agentrepl/protohelpers => ../proto/helpers/go

replace agentrepl/logging => ../agent-shim/logging/go

replace agentrepl/testrun => ../testrun

replace agentrepl/tldr => ../tree-sitter-tldr/tldr/src

replace agentrepl/tldrprompt => ../tree-sitter-tldr/tldr_prompt/src
