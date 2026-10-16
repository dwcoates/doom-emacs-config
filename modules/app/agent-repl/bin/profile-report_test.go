package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"
	"time"
)

func TestSpreadOf(t *testing.T) {
	ms := time.Millisecond
	tests := []struct {
		name string
		ds   []time.Duration
		want spread
	}{
		{"one duration is its own median, 90th percentile and maximum", []time.Duration{7 * ms}, spread{1, 7 * ms, 7 * ms, 7 * ms}},
		{"unsorted durations are sorted before the percentiles are read", []time.Duration{50 * ms, 10 * ms, 40 * ms, 20 * ms, 30 * ms}, spread{5, 30 * ms, 40 * ms, 50 * ms}},
		{"the 90th percentile of ten durations is the ninth", []time.Duration{1 * ms, 2 * ms, 3 * ms, 4 * ms, 5 * ms, 6 * ms, 7 * ms, 8 * ms, 9 * ms, 10 * ms}, spread{10, 5 * ms, 9 * ms, 10 * ms}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ds := append([]time.Duration(nil), tt.ds...)

			// Act
			got := spreadOf(ds)

			// Assert
			if got != tt.want {
				t.Fatalf("spreadOf(%v) = %+v, want %+v", tt.ds, got, tt.want)
			}
		})
	}
}

func TestSpreadOfPanicsOnNoDurations(t *testing.T) {
	defer func() {
		if r := recover(); r != "spreadOf: no durations" {
			t.Fatalf("recovered %v, want the no-durations panic", r)
		}
	}()
	spreadOf(nil)
}

// TestOnlySpreadOfReadsPercentiles keeps every latency table on the one
// helper: a table that sorts and reads its own percentiles fails here.
func TestOnlySpreadOfReadsPercentiles(t *testing.T) {
	// Arrange
	file, err := parser.ParseFile(token.NewFileSet(), "profile-report.go", nil, 0)
	if err != nil {
		t.Fatalf("parse profile-report.go: %v", err)
	}
	callers := map[string]bool{}

	// Act
	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			if call, ok := n.(*ast.CallExpr); ok {
				if id, ok := call.Fun.(*ast.Ident); ok && id.Name == "percentile" {
					callers[fn.Name.Name] = true
				}
			}
			return true
		})
	}

	// Assert
	if len(callers) != 1 || !callers["spreadOf"] {
		t.Fatalf("percentile is called from %v, want spreadOf alone", callers)
	}
}

func TestNextForNeverAnswersAnEdgeBeforeItsStart(t *testing.T) {
	// Arrange: a matching record sorted after the start's index, but stamped
	// before the start, then the real edge.
	base := time.Unix(1000, 0)
	match := func(r record) bool { return r.Message == "edge" }
	records := []record{
		{Message: "edge", WorkspaceID: "A", at: base.Add(5 * time.Millisecond)},
		{Message: "edge", WorkspaceID: "A", at: base.Add(10 * time.Millisecond)},
	}

	// Act
	got, ok := nextFor(records, 0, "A", base.Add(7*time.Millisecond), match)

	// Assert
	if !ok || !got.at.Equal(base.Add(10*time.Millisecond)) {
		t.Fatalf("nextFor = %v at %v, want the edge at +10ms", ok, got.at)
	}
}
