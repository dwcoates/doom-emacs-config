module agent-repld/reposcans

go 1.24.0

require (
	agent-repld v0.0.0
	agentrepl/testrun v0.0.0
)

require (
	agentrepl/logging v0.0.0-00010101000000-000000000000 // indirect
	agentrepl/proto v0.0.0 // indirect
	agentrepl/protohelpers v0.0.0 // indirect
	connectrpc.com/connect v1.17.0 // indirect
	github.com/dustin/go-humanize v1.0.1 // indirect
	github.com/google/uuid v1.6.0 // indirect
	github.com/mattn/go-isatty v0.0.20 // indirect
	github.com/ncruces/go-strftime v1.0.0 // indirect
	github.com/remyoudompheng/bigfft v0.0.0-20230129092748-24d4a6f8daec // indirect
	golang.org/x/exp v0.0.0-20251023183803-a4bb9ffd2546 // indirect
	golang.org/x/net v0.43.0 // indirect
	golang.org/x/sys v0.37.0 // indirect
	golang.org/x/text v0.28.0 // indirect
	google.golang.org/protobuf v1.36.11 // indirect
	modernc.org/libc v1.67.6 // indirect
	modernc.org/mathutil v1.7.1 // indirect
	modernc.org/memory v1.11.0 // indirect
	modernc.org/sqlite v1.46.1 // indirect
)

replace agent-repld => ../

replace agentrepl/proto => ../../proto/gen/go

replace agentrepl/protohelpers => ../../proto/helpers/go

replace agentrepl/logging => ../../agent-shim/logging/go

replace agentrepl/testrun => ../../testrun
