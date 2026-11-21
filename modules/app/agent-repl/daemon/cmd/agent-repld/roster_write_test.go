package main

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// recordedWrite is one call rosterWrite's write was given.
type recordedWrite struct {
	ws ids.WorkspaceID
	v  int
}

func TestRosterWrite(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantError bool
	}{
		{name: "a written change records nothing", err: nil, wantError: false},
		{name: "a failed write is recorded at ERROR", err: errors.New("disk full"), wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			var writes []recordedWrite
			sink := rosterWrite(log, "daemon.cmd.test_write", "could not keep the test value durable",
				func(_ context.Context, ws ids.WorkspaceID, v int) error {
					writes = append(writes, recordedWrite{ws: ws, v: v})
					return tt.err
				},
				func(v int) dlog.Context { return dlog.Context{"value": v} })

			// Act
			sink("ws-1", 7)

			// Assert
			if !reflect.DeepEqual(writes, []recordedWrite{{ws: "ws-1", v: 7}}) {
				t.Fatalf("writes = %v, want exactly the change", writes)
			}
			record, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.cmd.test_write"})
			if found != tt.wantError {
				t.Fatalf("ERROR record found = %v, want %v (records %+v)", found, tt.wantError, log.Records())
			}
			if tt.wantError && (record.Message != "could not keep the test value durable" ||
				record.Context["workspace"] != "ws-1" || record.Context["cause"] != "disk full" || record.Context["value"] != 7) {
				t.Fatalf("record = %+v, want the message, the workspace, the cause and the change's fields", record)
			}
		})
	}
}

// TestEveryRosterSinkWritesThroughRosterWrite pins the extraction: each
// roster sink that keeps a change durable is built by rosterWrite and records
// nothing itself, so a hand-rolled sink that logs its own failure fails here.
func TestEveryRosterSinkWritesThroughRosterWrite(t *testing.T) {
	for _, file := range []string{"roster_results.go", "roster_status_changes.go", "roster_visibilities.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			parsed, err := parser.ParseFile(token.NewFileSet(), file, nil, 0)
			if err != nil {
				t.Fatalf("parse %s: %v", file, err)
			}

			// Act
			calls := map[string]int{}
			ast.Inspect(parsed, func(n ast.Node) bool {
				call, ok := n.(*ast.CallExpr)
				if !ok {
					return true
				}
				switch fn := call.Fun.(type) {
				case *ast.Ident:
					calls[fn.Name]++
				case *ast.SelectorExpr:
					calls["."+fn.Sel.Name]++
				}
				return true
			})

			// Assert
			if calls["rosterWrite"] != 1 || calls[".Error"] != 0 {
				t.Fatalf("%s calls rosterWrite %d times and .Error %d times, want once and never",
					file, calls["rosterWrite"], calls[".Error"])
			}
		})
	}
}
