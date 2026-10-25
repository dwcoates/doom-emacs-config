package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
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

func TestLoadBudgetsReadsEveryProfileKindFromTheVocabulary(t *testing.T) {
	// Act
	budgets, err := loadBudgets("../proto/vocab/latencies.json")

	// Assert
	if err != nil {
		t.Fatalf("loadBudgets: %v", err)
	}
	if len(budgets) != len(profileKinds) {
		t.Fatalf("budgets = %d, want one per profile kind (%d)", len(budgets), len(profileKinds))
	}
	if b := budgets["switch_repeat"]; b.limit != 100*time.Millisecond || b.name == "" || b.reason == "" {
		t.Fatalf("switch_repeat = %+v, want a 100ms budget with its name and reason", b)
	}
}

func TestLoadBudgetsRefusesAVocabularyMissingAKind(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "latencies.json")
	if err := os.WriteFile(path, []byte(`{"latencies":[{"name":"switch_repeat","threshold_ms":100}]}`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := loadBudgets(path)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has no budget for prompt_echo") {
		t.Fatalf("err = %v, want the missing kind named", err)
	}
}

func TestLoadBudgetsRefusesAnUnreadableVocabulary(t *testing.T) {
	// Act
	_, err := loadBudgets(filepath.Join(t.TempDir(), "absent.json"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the latency vocabulary") {
		t.Fatalf("err = %v, want the read refused", err)
	}
}

func TestSampleTop(t *testing.T) {
	report := "Call graph:\n    10 Thread_1\n\nTotal number in stack (recursive counted multiple, when >=5):\n\n" +
		"Sort by top of stack, same collapsed (when >= 5):\n" +
		"        mach_msg2_trap  (in libsystem_kernel.dylib)        900\n" +
		"        process_mark_stack  (in Emacs)        80\n" +
		"        exec_byte_code  (in Emacs)        20\n" +
		"\nBinary Images:\n    0x1 Emacs\n"
	tests := []struct {
		name    string
		content string
		n       int
		want    string
		wantErr string
	}{
		{name: "every leaf of the section", content: report, n: 15,
			want: "mach_msg2_trap  (in libsystem_kernel.dylib)        900\nprocess_mark_stack  (in Emacs)        80\nexec_byte_code  (in Emacs)        20\n"},
		{name: "the first n leaves", content: report, n: 2,
			want: "mach_msg2_trap  (in libsystem_kernel.dylib)        900\nprocess_mark_stack  (in Emacs)        80\n"},
		{name: "a report without the section", content: "Call graph:\n    10 Thread_1\n", n: 15,
			wantErr: `has no "Sort by top of stack" section`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "emacs-1.cpu.sample")
			if err := os.WriteFile(path, []byte(tt.content), 0o644); err != nil {
				t.Fatal(err)
			}
			// Act
			got, err := sampleTop(path, tt.n)
			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("sampleTop error = %v, want it to contain %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("sampleTop: %v", err)
			}
			if got != tt.want {
				t.Fatalf("sampleTop = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSampleTopRefusesAMissingFile(t *testing.T) {
	// Act
	_, err := sampleTop(filepath.Join(t.TempDir(), "absent.cpu.sample"), 15)
	// Assert
	if err == nil {
		t.Fatal("sampleTop of an absent file answered no error")
	}
}
