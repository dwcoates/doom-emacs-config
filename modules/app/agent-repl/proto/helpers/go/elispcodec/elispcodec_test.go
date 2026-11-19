package elispcodec

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/reflect/protoregistry"

	_ "agentrepl/proto/agentrepl/v1"
	_ "agentrepl/proto/conversation/v1"
	_ "agentrepl/proto/frontend/v1"
	_ "agentrepl/proto/repoconfig/v1"
	_ "agentrepl/proto/shim/v1"
	_ "agentrepl/proto/store/v1"
	_ "agentrepl/proto/storeread/v1"
	_ "agentrepl/proto/testrun/v1"
	_ "agentrepl/proto/testservice/v1"
	_ "agentrepl/proto/workspace/v1"
	_ "agentrepl/proto/zettel/v1"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// lispDir is the module's elisp, from this package's directory.
const lispDir = "../../../../lisp"

// narrowings are the declared fields the codec leaves out on purpose, by
// message, each with the proto's reason Emacs never receives it.
var narrowings = map[string]map[string]string{
	// Sent ONLY on a webview stream (endpoint_watch_daemon.proto).
	"WatchDaemonResponse": {"newsDigest": "a webview-only push"},
	// The Recently Merged and Merge Queue headers carry no count
	// (wire-roster.el's agent-repl-wire-decode-roster-merged-section-header).
	"RosterSectionHeader": {"count": "the merged and queue headers' variant"},
}

// narrowed answers MESSAGE's narrowed fields as a set.
func narrowed(message string) map[string]bool {
	out := map[string]bool{}
	for f := range narrowings[message] {
		out[f] = true
	}
	return out
}

// byName answers every registered message, keyed by its bare name.
func byName() map[string][]protoreflect.MessageDescriptor {
	out := map[string][]protoreflect.MessageDescriptor{}
	protoregistry.GlobalFiles.RangeFiles(func(fd protoreflect.FileDescriptor) bool {
		var walk func(protoreflect.MessageDescriptors)
		walk = func(mds protoreflect.MessageDescriptors) {
			for i := 0; i < mds.Len(); i++ {
				md := mds.Get(i)
				out[string(md.Name())] = append(out[string(md.Name())], md)
				walk(md.Messages())
			}
		}
		walk(fd.Messages())
		return true
	})
	return out
}

func TestClaims(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   []Claim
	}{
		{
			name:   "an allowlist is the fields it names",
			source: `(agent-repl-wire--check-keys "RosterMergeQueueMode" object '(sync async))`,
			want:   []Claim{{File: "f.el", Message: "RosterMergeQueueMode", Fields: []string{"sync", "async"}}},
		},
		{
			name:   "an allowlist spans lines",
			source: "(agent-repl-wire--check-keys \"Row\" object\n  '(workspace\n    name))",
			want:   []Claim{{File: "f.el", Message: "Row", Fields: []string{"workspace", "name"}}},
		},
		{
			name:   "an empty allowlist lets nothing through",
			source: `(agent-repl-wire--check-keys "Empty" object '())`,
			want:   []Claim{{File: "f.el", Message: "Empty", Fields: []string{}}},
		},
		{
			name:   "an empty decode lets nothing through",
			source: `(lambda (v) (agent-repl-wire--decode-empty "RosterMergeQueueModeSync" v))`,
			want:   []Claim{{File: "f.el", Message: "RosterMergeQueueModeSync", Fields: []string{}}},
		},
		{
			name:   "a helper's own call names no literal message and is not read",
			source: `(agent-repl-wire--check-keys message-name object fields)`,
			want:   nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Claims("f.el", tc.source)

			// Assert
			if len(got) != len(tc.want) {
				t.Fatalf("Claims = %#v, want %#v", got, tc.want)
			}
			for i := range got {
				if got[i].File != tc.want[i].File || got[i].Message != tc.want[i].Message ||
					strings.Join(got[i].Fields, ",") != strings.Join(tc.want[i].Fields, ",") {
					t.Errorf("Claims[%d] = %#v, want %#v", i, got[i], tc.want[i])
				}
			}
		})
	}
}

func TestMismatch(t *testing.T) {
	asyncMode := (&frontendv1.RosterMergeQueueModeAsync{}).ProtoReflect().Descriptor()
	mode := (&frontendv1.RosterMergeQueueMode{}).ProtoReflect().Descriptor()
	cases := []struct {
		name       string
		claim      Claim
		candidates []protoreflect.MessageDescriptor
		narrowed   map[string]bool
		wantMatch  bool
		wantIn     string
	}{
		{
			name:       "the declared fields match",
			claim:      Claim{File: "f.el", Message: "RosterMergeQueueMode", Fields: []string{"async", "sync"}},
			candidates: []protoreflect.MessageDescriptor{mode},
			wantMatch:  true,
		},
		{
			name:       "an empty decode of a message with a field is the 2026-11-19 freeze",
			claim:      Claim{File: "f.el", Message: "RosterMergeQueueModeAsync", Fields: []string{}},
			candidates: []protoreflect.MessageDescriptor{asyncMode},
			wantIn:     "lacks [requester]",
		},
		{
			name:       "a retired field the codec still names is extra",
			claim:      Claim{File: "f.el", Message: "RosterMergeQueueMode", Fields: []string{"async", "sync", "racing"}},
			candidates: []protoreflect.MessageDescriptor{mode},
			wantIn:     "names [racing]",
		},
		{
			name:       "one matching package of several is enough",
			claim:      Claim{File: "f.el", Message: "RosterMergeQueueMode", Fields: []string{"async", "sync"}},
			candidates: []protoreflect.MessageDescriptor{asyncMode, mode},
			wantMatch:  true,
		},
		{
			name:       "a narrowed field the codec leaves out still matches",
			claim:      Claim{File: "f.el", Message: "RosterMergeQueueMode", Fields: []string{"sync"}},
			candidates: []protoreflect.MessageDescriptor{mode},
			narrowed:   map[string]bool{"async": true},
			wantMatch:  true,
		},
		{
			name:       "a narrowing excuses only the field it names",
			claim:      Claim{File: "f.el", Message: "RosterMergeQueueMode", Fields: []string{}},
			candidates: []protoreflect.MessageDescriptor{mode},
			narrowed:   map[string]bool{"async": true},
			wantIn:     "lacks [sync]",
		},
		{
			name:   "a name no proto declares is a mismatch",
			claim:  Claim{File: "f.el", Message: "NoSuchMessage", Fields: []string{}},
			wantIn: "declared by no .proto message",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Mismatch(tc.claim, tc.candidates, tc.narrowed)

			// Assert
			if tc.wantMatch {
				if got != "" {
					t.Fatalf("Mismatch = %q, want a match", got)
				}
				return
			}
			if !strings.Contains(got, tc.wantIn) {
				t.Fatalf("Mismatch = %q, want it to contain %q", got, tc.wantIn)
			}
		})
	}
}

// TestTheElispCodecNamesEveryDeclaredField reads every codec file and holds
// each literal claim to the proto it decodes.
func TestTheElispCodecNamesEveryDeclaredField(t *testing.T) {
	// Arrange
	files, err := filepath.Glob(filepath.Join(lispDir, "*.el"))
	if err != nil {
		t.Fatalf("glob the elisp: %v", err)
	}
	messages := byName()
	var claims []Claim
	for _, path := range files {
		if strings.HasPrefix(filepath.Base(path), "test-") {
			continue
		}
		source, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		claims = append(claims, Claims(filepath.Base(path), string(source))...)
	}
	if len(claims) < 200 {
		t.Fatalf("read only %d claims from %s; the codec's call shape changed under the reader", len(claims), lispDir)
	}

	// Act and Assert
	for _, claim := range claims {
		if why := Mismatch(claim, messages[claim.Message], narrowed(claim.Message)); why != "" {
			t.Error(why)
		}
	}
}
