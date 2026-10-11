package protoarm

import (
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
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
			m:       (&frontendv1.HeldOffer{Offer: &frontendv1.HeldOffer_MergeDequeue{MergeDequeue: &frontendv1.HeldOfferMergeDequeue{}}}).ProtoReflect(),
			oneof:   "offer",
			want:    "merge_dequeue",
			wantSet: true,
		},
		{
			name:    "answers false when no arm is set",
			m:       (&frontendv1.HeldOffer{}).ProtoReflect(),
			oneof:   "offer",
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
	m := (&frontendv1.HeldOffer{}).ProtoReflect()
	defer func() {
		// Assert
		got, _ := recover().(string)
		if !strings.Contains(got, "frontend.v1.HeldOffer") || !strings.Contains(got, `"nope"`) {
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
			m:       (&frontendv1.HeldOffer{Offer: &frontendv1.HeldOffer_MergeDequeue{MergeDequeue: &frontendv1.HeldOfferMergeDequeue{}}}).ProtoReflect(),
			want:    "merge_dequeue",
			wantSet: true,
		},
		{
			name:    "answers an empty name and false when no arm is set",
			m:       (&frontendv1.HeldOffer{}).ProtoReflect(),
			want:    "",
			wantSet: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := Name(tt.m, "offer")

			// Assert
			if got != tt.want || ok != tt.wantSet {
				t.Fatalf("Name = %q, %v, want %q, %v", got, ok, tt.want, tt.wantSet)
			}
		})
	}
}

// TestNoSiteResolvesANamedOneofByHand holds every production site in the
// daemon to the one shape: an arm of a oneof named in the source is read
// through Set or Name, never by handing WhichOneof a ByName lookup.
func TestNoSiteResolvesANamedOneofByHand(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "WhichOneof(") && strings.Contains(line, "Oneofs().ByName(") {
				offenders = append(offenders, path+":"+strconv.Itoa(i+1))
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("named oneofs resolved by hand, want protoarm.Set or protoarm.Name: %v", offenders)
	}
}
