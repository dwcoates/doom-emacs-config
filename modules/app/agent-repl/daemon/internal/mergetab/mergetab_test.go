package mergetab

import (
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

// classified is every arm of FeedMergeTab.kind and whether its content is a
// session's conversation. An arm missing here fails the test below, so a new
// arm is classified the moment it lands.
var classified = map[protoreflect.Name]bool{
	"queue":         false,
	"pick":          true,
	"pre_prompt":    true,
	"rebasing":      false,
	"conflicts":     true,
	"tests":         false,
	"fixes":         true,
	"committing":    false,
	"updating_main": false,
	"post_prompt":   true,
}

func TestAgenticClassifiesEveryArm(t *testing.T) {
	for name, tab := range tabsOfEveryArm() {
		t.Run(string(name), func(t *testing.T) {
			// Arrange
			want, known := classified[name]
			if !known {
				t.Fatalf("arm %q is not classified as agentic or resolved", name)
			}

			// Act
			got := Agentic(tab)

			// Assert
			if got != want {
				t.Fatalf("Agentic(%s) = %v, want %v", name, got, want)
			}
		})
	}
}

func TestAgenticIsFalseForATabWithNoArm(t *testing.T) {
	// Arrange
	tab := &frontendv1.FeedMergeTab{}

	// Act / Assert
	if Agentic(tab) {
		t.Fatal("Agentic answered true for a tab with no arm set")
	}
}
