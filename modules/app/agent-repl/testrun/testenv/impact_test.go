package testenv

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func getenvOf(m map[string]string) func(string) string { return func(k string) string { return m[k] } }

func TestImpactProgramNamesTheCoverBuild(t *testing.T) {
	if got := ImpactProgram("agent-repld"); got != "agent-repld-impact" {
		t.Fatalf("ImpactProgram = %q", got)
	}
}

func TestAppendThenReadImpactWindowsMergesByTest(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "windows.jsonl")
	env := getenvOf(map[string]string{ImpactOut: path})
	for _, w := range []ImpactWindow{
		{Test: "", Executed: []string{"p/boot.go"}, CPUSeconds: 0.5},
		{Test: "TestA", Executed: []string{"p/a.go", "p/b.go"}, CPUSeconds: 0.25},
		{Test: "TestA", Executed: []string{"p/b.go", "p/c.go"}, CPUSeconds: 0.25},
		{Test: "TestB", Executed: nil, Incomplete: "a daemon was killed"},
		{Test: "TestB", Executed: nil, Incomplete: "a daemon was killed"},
	} {
		// Act
		if err := AppendImpactWindow(env, w); err != nil {
			t.Fatalf("AppendImpactWindow: %v", err)
		}
	}
	got, err := ReadImpactWindows(path)
	// Assert
	if err != nil {
		t.Fatalf("ReadImpactWindows: %v", err)
	}
	a := got["TestA"]
	if strings.Join(a.Executed, ",") != "p/a.go,p/b.go,p/c.go" || a.Windows != 2 || a.CPUSeconds != 0.5 {
		t.Fatalf("TestA = %+v, want the union of its two windows", a)
	}
	if len(got["TestB"].Incomplete) != 1 || got["TestB"].Incomplete[0] != "a daemon was killed" || len(a.Incomplete) != 0 {
		t.Fatalf("incomplete reasons: TestB = %v, TestA = %v, want TestB's one distinct reason", got["TestB"].Incomplete, a.Incomplete)
	}
	if got[""].Windows != 1 || got["TestB"] == nil || len(got["TestB"].Executed) != 0 {
		t.Fatalf("boot and TestB = %+v, %+v", got[""], got["TestB"])
	}
}

func TestAppendImpactWindowDoesNothingUnlessAsked(t *testing.T) {
	if err := AppendImpactWindow(getenvOf(nil), ImpactWindow{Test: "TestA"}); err != nil {
		t.Fatalf("AppendImpactWindow with no file named = %v, want nothing done", err)
	}
}

func TestAppendImpactWindowRefusesAnUnwritablePath(t *testing.T) {
	// Arrange
	env := getenvOf(map[string]string{ImpactOut: filepath.Join(t.TempDir(), "absent", "w.jsonl")})
	// Act
	err := AppendImpactWindow(env, ImpactWindow{Test: "TestA"})
	// Assert
	if err == nil || !strings.Contains(err.Error(), "open the impact windows") {
		t.Fatalf("AppendImpactWindow = %v, want a refusal naming the file", err)
	}
}

func TestReadImpactWindowsCases(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	bad := filepath.Join(dir, "bad.jsonl")
	if err := os.WriteFile(bad, []byte("{\"test\":\"TestA\"}\nnot json\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name, path, wantErr string
		wantNil             bool
	}{
		{"an absent file answers none", filepath.Join(dir, "absent"), "", true},
		{"a line that is no window is refused", bad, "line 2 is not an impact window", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ReadImpactWindows(tt.path)
			// Assert
			switch {
			case tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)):
				t.Fatalf("ReadImpactWindows = %v, want %q", err, tt.wantErr)
			case tt.wantErr == "" && (err != nil || (tt.wantNil && got != nil)):
				t.Fatalf("ReadImpactWindows = (%v, %v)", got, err)
			}
		})
	}
}

func TestParseCovTextfmt(t *testing.T) {
	tests := []struct {
		name, text, want, wantErr string
	}{
		{"only blocks that ran count", "mode: atomic\nm/a.go:1.1,2.2 1 3\nm/b.go:1.1,2.2 1 0\nm/a.go:4.1,5.2 2 0\nm/c.go:1.1,2.2 1 1\n", "m/a.go,m/c.go", ""},
		{"an empty profile is no files", "mode: set\n", "", ""},
		{"a malformed line is refused", "mode: set\nnot a block\n", "", "is not <file>:<range>"},
		{"a missing count is refused", "m/a.go:1.1,2.2 1 x\n", "", "has no count"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseCovTextfmt([]byte(tt.text))
			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("ParseCovTextfmt = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || strings.Join(got, ",") != tt.want {
				t.Fatalf("ParseCovTextfmt = (%v, %v), want %q", got, err, tt.want)
			}
		})
	}
}

func fakeCovdata(t *testing.T, body string) CovReducer {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "covdata")
	script := "#!/bin/sh\nfor a in \"$@\"; do case \"$a\" in -o=*) out=\"${a#-o=}\";; esac; done\n" + body + "\n"
	if err := os.WriteFile(bin, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
	return CovReducer{Bin: bin}
}

func TestCovReducerReduces(t *testing.T) {
	// Arrange
	r := fakeCovdata(t, `printf 'mode: atomic\nm/a.go:1.1,2.2 1 2\nm/b.go:1.1,2.2 1 0\n' > "$out"`)
	// Act
	files, cpu, err := r.Reduce("/meta", "/c1", "/c2")
	// Assert
	if err != nil || strings.Join(files, ",") != "m/a.go" || cpu < 0 {
		t.Fatalf("Reduce = (%v, %v, %v), want the one executed file", files, cpu, err)
	}
}

func TestCovReducerReportsAFailingTool(t *testing.T) {
	// Arrange
	r := fakeCovdata(t, `echo "no meta-data" >&2; exit 3`)
	// Act
	_, _, err := r.Reduce("/meta", "/c1")
	// Assert
	if err == nil || !strings.Contains(err.Error(), "covdata textfmt -i=/meta,/c1 failed") || !strings.Contains(err.Error(), "no meta-data") {
		t.Fatalf("Reduce = %v, want the failed invocation with the tool's stderr", err)
	}
}

func TestCovReducerReportsAnUnreadableReduction(t *testing.T) {
	// Arrange: a tool that succeeds and writes nothing.
	r := fakeCovdata(t, `true`)
	// Act
	_, _, err := r.Reduce("/meta", "/c1")
	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the reduction") {
		t.Fatalf("Reduce = %v, want a refusal naming the missing output", err)
	}
}

func TestDefaultCovReducerFindsTheToolchainsCovdata(t *testing.T) {
	// Act
	r, err := DefaultCovReducer()
	// Assert
	if err != nil || !strings.HasSuffix(r.Bin, "covdata") {
		t.Fatalf("DefaultCovReducer = (%+v, %v)", r, err)
	}
}
