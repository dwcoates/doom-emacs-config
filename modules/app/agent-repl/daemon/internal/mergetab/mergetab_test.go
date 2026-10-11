package mergetab

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// tabsOfEveryArm answers one empty tab per arm of FeedMergeTab.kind.
func tabsOfEveryArm() map[protoreflect.Name]*frontendv1.FeedMergeTab {
	out := map[protoreflect.Name]*frontendv1.FeedMergeTab{}
	oneof := (&frontendv1.FeedMergeTab{}).ProtoReflect().Descriptor().Oneofs().ByName("kind")
	for i := 0; i < oneof.Fields().Len(); i++ {
		field := oneof.Fields().Get(i)
		tab := &frontendv1.FeedMergeTab{}
		m := tab.ProtoReflect()
		m.Set(field, m.NewField(field))
		out[field.Name()] = tab
	}
	return out
}

func TestEveryArmCarryingASwitchIsAgentic(t *testing.T) {
	for name, tab := range tabsOfEveryArm() {
		t.Run(string(name), func(t *testing.T) {
			// Arrange: whether the arm's message declares a workspace switch.
			oneof := tab.ProtoReflect().Descriptor().Oneofs().ByName("kind")
			arm := oneof.Fields().ByName(name).Message()
			declares := arm.Fields().ByName("workspace_switch") != nil

			// Act
			set := SetSwitch(tab, &frontendv1.FeedMergeTabWorkspaceSwitch{})

			// Assert: exactly the arms that declare a switch are agentic, and a
			// switch set is the switch read back.
			if set != declares {
				t.Fatalf("SetSwitch = %v, want %v for an arm that declares a switch: %v", set, declares, declares)
			}
			if (Switch(tab) != nil) != declares {
				t.Fatalf("Switch = %v, want one exactly when the arm declares it", Switch(tab))
			}
		})
	}
}

func TestSwitchIsNilOnAnAgenticTabThatCarriesNone(t *testing.T) {
	// Arrange
	tab := &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Conflicts{Conflicts: &frontendv1.FeedMergeTabConflicts{}}}

	// Act / Assert
	if Switch(tab) != nil {
		t.Fatal("Switch answered a switch the tab does not carry")
	}
}

// agenticArmSwitch matches a read or a write of one agentic arm's switch.
var agenticArmSwitch = regexp.MustCompile(`\.(PrePrompt|Conflicts|Fixes|PostPrompt)\.(Get)?WorkspaceSwitch`)

func TestTheAgenticArmsAreSpelledOnlyHere(t *testing.T) {
	// Arrange: every daemon source outside this package and the generated code.
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || strings.Contains(path, string(filepath.Separator)+"mergetab"+string(filepath.Separator)) {
			return nil
		}
		source, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if agenticArmSwitch.Match(source) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert: no other file reaches an agentic tab's switch arm by arm.
	if err != nil {
		t.Fatalf("WalkDir: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("%v set an agentic tab's switch themselves, want mergetab.SetSwitch", offenders)
	}
}
