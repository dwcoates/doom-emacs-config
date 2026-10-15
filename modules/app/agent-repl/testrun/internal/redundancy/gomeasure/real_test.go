package gomeasure

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"agentrepl/testrun/internal/redundancy"
)

// The real toolchain, end to end, on a module small enough to build twice
// in a few seconds: TestSignAll catches everything the other two catch.
var realModule = map[string]string{
	"go.mod": "module sign\n\ngo 1.24\n",
	"sign.go": `package sign

func Sign(n int) int {
	if n < 0 {
		return -1
	}
	if n > 0 {
		return 1
	}
	return 0
}
`,
	"sign_test.go": `package sign

import "testing"

func TestSignAll(t *testing.T) {
	for n, want := range map[int]int{-5: -1, 0: 0, 5: 1} {
		if got := Sign(n); got != want {
			t.Fatalf("Sign(%d) = %d, want %d", n, got, want)
		}
	}
}

func TestSignPositive(t *testing.T) {
	if Sign(5) != 1 {
		t.Fatal("Sign(5) is not 1")
	}
}

func TestSignNegative(t *testing.T) {
	t.Run("one", func(t *testing.T) {
		if Sign(-1) != -1 {
			t.Fatal("Sign(-1) is not -1")
		}
	})
}
`,
}

func TestMeasureEndToEndWithTheRealToolchain(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	for name, src := range realModule {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	cfg := Config{Module: dir, Package: ".", Tests: []string{"sign_test.go"}, Mutate: []string{"."}, Runs: 2, Scratch: t.TempDir(), Go: "go"}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, OSExec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	r, err := redundancy.Minimize(m.Cases, redundancy.KillsOnly, 0)
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(r.Kept, []string{"TestSignAll"}) {
		t.Fatalf("kept %v, want TestSignAll alone; cases %+v", r.Kept, m.Cases)
	}
	for _, rm := range r.Removed {
		if rm.Case != "TestSignPositive" && rm.Case != "TestSignNegative/one" {
			t.Errorf("removed %s, a case this fixture does not have", rm.Case)
		}
		if rm.DominatedBy != "TestSignAll" {
			t.Errorf("%s dominated by %q, want TestSignAll", rm.Case, rm.DominatedBy)
		}
	}
	for _, c := range m.Cases {
		if c.Cost <= 0 {
			t.Errorf("%s measured %v", c.ID, c.Cost)
		}
	}
}
