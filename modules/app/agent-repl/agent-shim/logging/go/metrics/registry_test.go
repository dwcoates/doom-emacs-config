package metrics

import (
	"strings"
	"testing"
)

func TestNewRegistry(t *testing.T) {
	tests := []struct {
		name    string
		program string
		build   string
		wantErr string
	}{
		{name: "known program and build", program: ProgramStore, build: "abc123"},
		{name: "unknown program is refused", program: "emacs", build: "abc123", wantErr: `unknown program "emacs"`},
		{name: "empty build is refused", program: ProgramDaemon, build: "", wantErr: "has no build hash"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			reg, err := NewRegistry(tt.program, tt.build)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to contain %q", err, tt.wantErr)
				}
				if reg != nil {
					t.Fatalf("a refused registry was still answered")
				}
				return
			}
			if err != nil {
				t.Fatalf("NewRegistry: %v", err)
			}
			families, err := reg.Gather()
			if err != nil {
				t.Fatalf("Gather: %v", err)
			}
			var info, goRuntime bool
			for _, f := range families {
				if f.GetName() == BuildInfoName {
					labels := map[string]string{}
					for _, l := range f.GetMetric()[0].GetLabel() {
						labels[l.GetName()] = l.GetValue()
					}
					if labels["program"] != tt.program || labels["build"] != tt.build {
						t.Fatalf("build info labels = %v", labels)
					}
					if v := f.GetMetric()[0].GetGauge().GetValue(); v != 1 {
						t.Fatalf("build info value = %v, want 1", v)
					}
					info = true
				}
				if f.GetName() == "go_goroutines" {
					goRuntime = true
				}
			}
			if !info || !goRuntime {
				t.Fatalf("build info present = %v, go runtime present = %v", info, goRuntime)
			}
		})
	}
}

func TestValidProgramCoversExactlyTheClosedSet(t *testing.T) {
	// Arrange
	want := []string{"daemon", "store", "sidecar"}

	// Act / Assert
	if len(Programs) != len(want) {
		t.Fatalf("Programs = %v, want %v", Programs, want)
	}
	for i, p := range want {
		if Programs[i] != p || !ValidProgram(p) {
			t.Fatalf("Programs[%d] = %q, want %q (valid=%v)", i, Programs[i], p, ValidProgram(p))
		}
	}
	if ValidProgram("shim") {
		t.Fatalf("shim is not a scraped program")
	}
}
