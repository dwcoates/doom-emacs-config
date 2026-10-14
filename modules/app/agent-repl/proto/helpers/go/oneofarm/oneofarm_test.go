package oneofarm

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestName(t *testing.T) {
	tests := []struct {
		name    string
		status  *frontendv1.FooterStatus
		want    string
		wantSet bool
	}{
		{name: "a set arm is named by its proto field name", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Working{Working: &frontendv1.FooterStatusWorking{}}}, want: "working", wantSet: true},
		{name: "a multi-word arm keeps its snake_case name", status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_MergeFailed{MergeFailed: &frontendv1.FooterStatusMergeFailed{}}}, want: "merge_failed", wantSet: true},
		{name: "an unset oneof names nothing", status: &frontendv1.FooterStatus{}, want: "", wantSet: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, set := Name(tt.status, "status")

			// Assert.
			if got != tt.want || set != tt.wantSet {
				t.Fatalf("Name = %q, %v; want %q, %v", got, set, tt.want, tt.wantSet)
			}
		})
	}
}

func TestFieldAnswersTheSetArmsField(t *testing.T) {
	// Arrange.
	status := &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Idle{Idle: &frontendv1.FooterStatusIdle{}}}

	// Act.
	got := Field(status.ProtoReflect(), "status")

	// Assert.
	if got == nil || got.Name() != "idle" {
		t.Fatalf("Field = %v, want the idle field", got)
	}
}

func TestFieldAnswersNilForAnUnsetOneof(t *testing.T) {
	// Act.
	got := Field((&frontendv1.FooterStatus{}).ProtoReflect(), "status")

	// Assert.
	if got != nil {
		t.Fatalf("Field = %v, want nil", got)
	}
}

func TestNamePanicsOnAOneofTheMessageDoesNotDeclare(t *testing.T) {
	// Arrange.
	defer func() {
		r := recover()
		if r == nil || !strings.Contains(r.(string), `declares no oneof "nope"`) {
			t.Fatalf("recover() = %v, want a panic naming the missing oneof", r)
		}
	}()

	// Act.
	Name(&frontendv1.FooterStatus{}, "nope")
}

// TestTestsNameArmsOnlyThroughOneofarm pins the call sites: a test under e2e/
// or daemon/integration/ that reads WhichOneof by hand is a drifted copy of
// Name.
func TestTestsNameArmsOnlyThroughOneofarm(t *testing.T) {
	// Arrange: the agent-repl root, four directories up.
	root, err := filepath.Abs(filepath.Join("..", "..", "..", ".."))
	if err != nil {
		t.Fatalf("resolve the agent-repl root: %v", err)
	}
	var offenders []string

	// Act.
	for _, dir := range []string{"e2e", filepath.Join("daemon", "integration")} {
		err := filepath.WalkDir(filepath.Join(root, dir), func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() || !strings.HasSuffix(path, "_test.go") {
				return nil
			}
			body, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			if strings.Contains(string(body), "WhichOneof(") {
				offenders = append(offenders, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", dir, err)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these tests read WhichOneof by hand; read the arm through oneofarm.Field or oneofarm.Name instead: %v", offenders)
	}
}
