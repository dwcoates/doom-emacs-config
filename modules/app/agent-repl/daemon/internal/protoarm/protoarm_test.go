package protoarm

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"
)

func TestSet(t *testing.T) {
	tests := []struct {
		name    string
		m       protoreflect.Message
		oneof   protoreflect.Name
		want    protoreflect.Name
		wantSet bool
	}{
		{
			name:    "answers the set arm's field",
			m:       (&frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}}).ProtoReflect(),
			oneof:   "result",
			want:    "update",
			wantSet: true,
		},
		{
			name:    "answers false when no arm is set",
			m:       (&frontendv1.FeedMerge{}).ProtoReflect(),
			oneof:   "result",
			wantSet: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			field, ok := Set(tt.m, tt.oneof)

			// Assert
			if ok != tt.wantSet {
				t.Fatalf("Set ok = %v, want %v", ok, tt.wantSet)
			}
			if ok && field.Name() != tt.want {
				t.Fatalf("Set field = %s, want %s", field.Name(), tt.want)
			}
			if !ok && field != nil {
				t.Fatalf("Set field = %v, want nil when no arm is set", field)
			}
		})
	}
}

func TestSetPanicsNamingAOneofTheMessageDoesNotDeclare(t *testing.T) {
	// Arrange
	m := (&frontendv1.FeedMerge{}).ProtoReflect()
	defer func() {
		// Assert
		got, _ := recover().(string)
		if !strings.Contains(got, "frontend.v1.FeedMerge") || !strings.Contains(got, `"nope"`) {
			t.Fatalf("panic = %q, want it to name the message and the oneof", got)
		}
	}()

	// Act
	Set(m, "nope")
	t.Fatal("Set returned for an undeclared oneof, want a panic")
}

func TestName(t *testing.T) {
	tests := []struct {
		name    string
		m       protoreflect.Message
		want    protoreflect.Name
		wantSet bool
	}{
		{
			name:    "answers the set arm's name",
			m:       (&frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}}).ProtoReflect(),
			want:    "update",
			wantSet: true,
		},
		{
			name:    "answers an empty name and false when no arm is set",
			m:       (&frontendv1.FeedMerge{}).ProtoReflect(),
			want:    "",
			wantSet: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := Name(tt.m, "result")

			// Assert
			if got != tt.want || ok != tt.wantSet {
				t.Fatalf("Name = %q, %v, want %q, %v", got, ok, tt.want, tt.wantSet)
			}
		})
	}
}

func TestMustAnswersTheSetArmsName(t *testing.T) {
	// Arrange
	m := (&frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}}).ProtoReflect()

	// Act
	got := Must(m, "result", "test.op")

	// Assert
	if got != "update" {
		t.Fatalf("Must = %q, want update", got)
	}
}

func TestMustPanicsNamingTheMessageTheOneofAndTheOperationWhenNoArmIsSet(t *testing.T) {
	// Arrange
	m := (&frontendv1.FeedMerge{}).ProtoReflect()
	defer func() {
		// Assert
		got, _ := recover().(string)
		for _, want := range []string{"frontend.v1.FeedMerge", `"result"`, "test.op"} {
			if !strings.Contains(got, want) {
				t.Fatalf("panic = %q, want it to name %s", got, want)
			}
		}
	}()

	// Act
	Must(m, "result", "test.op")
	t.Fatal("Must returned for an unset required oneof, want a panic")
}

func TestMustPanicsNamingAOneofTheMessageDoesNotDeclare(t *testing.T) {
	// Arrange
	m := (&frontendv1.FeedMerge{}).ProtoReflect()
	defer func() {
		// Assert
		got, _ := recover().(string)
		if !strings.Contains(got, `"nope"`) {
			t.Fatalf("panic = %q, want it to name the undeclared oneof", got)
		}
	}()

	// Act
	Must(m, "nope", "test.op")
	t.Fatal("Must returned for an undeclared oneof, want a panic")
}
