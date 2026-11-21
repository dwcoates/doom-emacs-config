package reposcans

// EVERY EMACS DECODER REFUSES A FIELD IT DOES NOT LIST
// (agent-repl-wire--check-keys in lisp/wire-*.el), so a field the daemon
// starts sending that Emacs's list lacks drops the whole message it rides. On
// 2026-10-29 the roster gained `cold_gates`, Emacs refused every roster that
// carried one, and switching workspaces broke until the decoder was taught it.
// These tests compare each decoder's list with its message's fields in the
// generated schema, so the omission fails the suite instead of the editor.
//
// Run by hand with -count=1: the files read here are outside this module, so
// Go's test cache does not see them change (the suite runner never caches).

import (
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strings"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/reflect/protoregistry"

	// The daemon's API, which registers every package its messages import.
	_ "agentrepl/proto/agentrepl/v1"
)

// checkKeys matches one key check with a literal message name and a literal
// key list: (agent-repl-wire--check-keys "Name" OBJECT '(key ...)), or nil.
var checkKeys = regexp.MustCompile(`\(agent-repl-wire--check-keys\s+"([^"]+)"\s+\S+\s+(?:'\(([^)]*)\)|nil\))`)

// anyCheck matches every key check, literal or not.
var anyCheck = regexp.MustCompile(`\(agent-repl-wire--check-keys\b`)

// computed are the key checks whose message name or key list is built in code,
// so this test cannot read them: by file, how many. A new computed check fails
// the test until it is counted here and its decoder is checked by hand (or,
// better, written with a literal list).
var computed = map[string]int{
	"wire-common.el": 3,
	"wire-host.el":   16,
	"wire-roster.el": 18,
	"wire-verbs.el":  1,
}

// narrowed are the fields a decoder refuses ON PURPOSE, each with the
// contract that says the daemon never sends it there: by file, message and
// field. A decoder may narrow only what is listed here.
var narrowed = map[string]string{
	"wire-host.el/WatchDaemonResponse/newsDigest": "a webview stream's alone (endpoint_watch_daemon.proto); never sent to Emacs",
	"wire-roster.el/RosterSectionHeader/count":    "the Deploys section's header carries no count (wire-roster.el, roster-merged-section-header)",
}

// decoder is one literal key check.
type decoder struct {
	file    string
	message string
	keys    []string
}

// readDecoders answers every literal key check in the wire files, and how
// many checks each file has that are not literal.
func readDecoders(t *testing.T) ([]decoder, map[string]int) {
	t.Helper()
	lispDir := filepath.Join(moduleRootDir(t), "lisp")
	files, err := filepath.Glob(filepath.Join(lispDir, "wire*.el"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no wire files under %s: %v", lispDir, err)
	}
	var out []decoder
	other := map[string]int{}
	for _, path := range files {
		text, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		file := filepath.Base(path)
		all := len(anyCheck.FindAllIndex(text, -1))
		literal := checkKeys.FindAllSubmatch(text, -1)
		for _, m := range literal {
			out = append(out, decoder{file: file, message: string(m[1]), keys: strings.Fields(string(m[2]))})
		}
		if n := all - len(literal); n > 0 {
			other[file] = n
		}
	}
	return out, other
}

// schema answers every message the daemon's API registers, by its short name.
func schema() map[string][]protoreflect.MessageDescriptor {
	out := map[string][]protoreflect.MessageDescriptor{}
	var walk func(protoreflect.MessageDescriptors)
	walk = func(ms protoreflect.MessageDescriptors) {
		for i := 0; i < ms.Len(); i++ {
			m := ms.Get(i)
			out[string(m.Name())] = append(out[string(m.Name())], m)
			walk(m.Messages())
		}
	}
	protoregistry.GlobalFiles.RangeFiles(func(fd protoreflect.FileDescriptor) bool {
		walk(fd.Messages())
		return true
	})
	return out
}

// jsonNames answers a message's fields as protojson names them.
func jsonNames(m protoreflect.MessageDescriptor) []string {
	fields := m.Fields()
	out := make([]string, 0, fields.Len())
	for i := 0; i < fields.Len(); i++ {
		out = append(out, fields.Get(i).JSONName())
	}
	sort.Strings(out)
	return out
}

func TestEveryEmacsDecoderListsExactlyItsMessagesFields(t *testing.T) {
	// Arrange
	decoders, _ := readDecoders(t)
	messages := schema()

	for _, d := range decoders {
		t.Run(d.file+"/"+d.message, func(t *testing.T) {
			// Act
			found := messages[d.message]

			// Assert
			if len(found) == 0 {
				t.Fatalf("%s checks the keys of %q, which the schema has no message named", d.file, d.message)
			}
			// A short name several packages share is matched by fields: the
			// decoder must match one of them exactly.
			var mismatches []string
			for _, m := range found {
				want := jsonNames(m)
				got := slices.Clone(d.keys)
				sort.Strings(got)
				if slices.Equal(got, want) {
					return
				}
				var missing, extra []string
				for _, k := range want {
					if !slices.Contains(got, k) && narrowed[d.file+"/"+d.message+"/"+k] == "" {
						missing = append(missing, k)
					}
				}
				for _, k := range got {
					if !slices.Contains(want, k) {
						extra = append(extra, k)
					}
				}
				if len(missing) == 0 && len(extra) == 0 {
					return
				}
				mismatches = append(mismatches, string(m.FullName())+": refuses "+strings.Join(missing, ", ")+"; lists unknown "+strings.Join(extra, ", "))
			}
			t.Fatalf("%s's decoder of %q does not list exactly its fields:\n  %s", d.file, d.message, strings.Join(mismatches, "\n  "))
		})
	}
}

// rosterRowOwnKeys matches the literal list of RosterRow's own fields in
// agent-repl-wire--roster-row-keys, and rosterRowArms the status arm table
// whose keys the list appends (wire-roster.el).
var (
	rosterRowOwnKeys = regexp.MustCompile(`\(defconst agent-repl-wire--roster-row-keys\s+\(append '\(([^)]*)\)\s+\(mapcar #'car agent-repl-wire-roster-row-status-arms\)\)`)
	rosterRowArms    = regexp.MustCompile(`(?s)\(defconst agent-repl-wire-roster-row-status-arms\s+'\((.*?)\)\s+"`)
	rosterRowArmKey  = regexp.MustCompile(`\((\w+)\s+:`)
)

// rosterRowKeys answers the keys SOURCE's RosterRow decoder lets through:
// the row's own fields and every status arm's key. It fails when either
// definition no longer has the shape this test reads.
func rosterRowKeys(t *testing.T, source string) []string {
	t.Helper()
	own := rosterRowOwnKeys.FindStringSubmatch(source)
	arms := rosterRowArms.FindStringSubmatch(source)
	if own == nil || arms == nil {
		t.Fatalf("wire-roster.el no longer defines agent-repl-wire--roster-row-keys as a literal list appended to the status arms' keys; teach this test its new shape")
	}
	keys := strings.Fields(own[1])
	for _, arm := range rosterRowArmKey.FindAllStringSubmatch(arms[1], -1) {
		keys = append(keys, arm[1])
	}
	sort.Strings(keys)
	return keys
}

// TestTheRosterRowDecoderListsExactlyItsMessagesFields reads the one key
// check whose list is built in code and rides every roster push. On
// 2026-11-20 the row gained `open_owned_pull_request`, the list lacked it,
// and Emacs refused every roster until the decoder was taught the field:
// the literal scan above passes a computed list over.
func TestTheRosterRowDecoderListsExactlyItsMessagesFields(t *testing.T) {
	// Arrange
	source, err := os.ReadFile(filepath.Join(moduleRootDir(t), "lisp", "wire-roster.el"))
	if err != nil {
		t.Fatalf("read wire-roster.el: %v", err)
	}
	var want []string
	for _, m := range schema()["RosterRow"] {
		if m.ParentFile().Package() == "frontend.v1" {
			want = jsonNames(m)
		}
	}
	if want == nil {
		t.Fatalf("the schema has no frontend.v1.RosterRow")
	}

	// Act
	got := rosterRowKeys(t, string(source))

	// Assert
	if !slices.Equal(got, want) {
		t.Fatalf("wire-roster.el's RosterRow decoder does not list exactly its fields:\n  lists   %v\n  message %v", got, want)
	}
}

func TestRosterRowKeysReadsTheOwnFieldsAndEveryStatusArm(t *testing.T) {
	tests := []struct {
		name   string
		source string
		want   []string
	}{
		{
			name: "own fields and arms, sorted",
			source: `(defconst agent-repl-wire-roster-row-status-arms
  '((thinking :thinking decode-thinking)
    (vendorSilent :vendor-silent
                  decode-vendor-silent))
  "Arms.")
(defconst agent-repl-wire--roster-row-keys
  (append '(workspace name)
          (mapcar #'car agent-repl-wire-roster-row-status-arms))
  "Keys.")`,
			want: []string{"name", "thinking", "vendorSilent", "workspace"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := rosterRowKeys(t, tc.source)

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("rosterRowKeys = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestEveryNarrowingNamesAFieldItsMessageHas(t *testing.T) {
	// Arrange
	messages := schema()

	for key := range narrowed {
		t.Run(key, func(t *testing.T) {
			// Act
			parts := strings.Split(key, "/")
			found := false
			for _, m := range messages[parts[1]] {
				if m.Fields().ByJSONName(parts[2]) != nil {
					found = true
				}
			}

			// Assert
			if len(parts) != 3 || !found {
				t.Fatalf("narrowing %q names no field of its message", key)
			}
		})
	}
}

func TestEveryComputedEmacsKeyCheckIsCounted(t *testing.T) {
	// Arrange / Act
	_, other := readDecoders(t)

	// Assert
	for file, n := range other {
		if computed[file] != n {
			t.Errorf("%s has %d key checks this test cannot read, %d counted: count a new one in `computed` and check its decoder by hand", file, n, computed[file])
		}
	}
	for file, n := range computed {
		if other[file] != n {
			t.Errorf("`computed` counts %d unreadable key checks in %s, which has %d", n, file, other[file])
		}
	}
}
