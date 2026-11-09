package usersetup_test

import (
	"os"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/usersetup"
)

func TestPairs(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    []usersetup.Pair
		wantErr string
	}{
		{name: "blank is no pairs", raw: "  ", want: nil},
		{name: "one pair", raw: "a=b", want: []usersetup.Pair{{Key: "a", Value: "b", Entry: "a=b"}}},
		{
			name: "pairs keep their order and lose their padding",
			raw:  " a = b , c=d ",
			want: []usersetup.Pair{{Key: "a", Value: "b", Entry: "a = b "}, {Key: "c", Value: "d", Entry: " c=d"}},
		},
		{name: "an empty value is the caller's to judge", raw: "a=", want: []usersetup.Pair{{Key: "a", Value: "", Entry: "a="}}},
		{name: "an entry with no equals sign", raw: "a=b,c", wantErr: "owner: NAME pair \"c\" is not `<k>=<v>`"},
		{name: "an empty key", raw: "=b", wantErr: "owner: NAME pair \"=b\" is not `<k>=<v>`"},
		{name: "a trailing comma leaves an empty entry", raw: "a=b,", wantErr: "owner: NAME pair \"\" is not `<k>=<v>`"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := usersetup.Pairs("owner", "NAME", "<k>=<v>", tc.raw)

			// Assert.
			if tc.wantErr != "" {
				if err == nil || !strings.HasPrefix(err.Error(), tc.wantErr) || !strings.Contains(err.Error(), usersetup.Doc) {
					t.Fatalf("Pairs(%q) = %v, want %q pointing at %s", tc.raw, err, tc.wantErr, usersetup.Doc)
				}
				return
			}
			if err != nil {
				t.Fatalf("Pairs(%q) = %v, want nil", tc.raw, err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Pairs(%q) = %v, want %v", tc.raw, got, tc.want)
			}
		})
	}
}

// TestPairsIsTheOnePairParser pins that every per-user pair variable is read
// through Pairs: a reader that splits its own pairs would drift from the
// shared error and trimming rules.
func TestPairsIsTheOnePairParser(t *testing.T) {
	for _, path := range []string{"../pullrefs/pullrefs.go"} {
		t.Run(path, func(t *testing.T) {
			// Arrange.
			src, err := os.ReadFile(path) //nolint:gosec // repository source
			if err != nil {
				t.Fatal(err)
			}
			body := string(src)

			// Assert.
			if !strings.Contains(body, "usersetup.Pairs(") {
				t.Fatalf("%s does not read its pairs through usersetup.Pairs", path)
			}
			if strings.Contains(body, `strings.Cut(`) && strings.Contains(body, `"="`) {
				t.Fatalf("%s splits `<key>=<value>` pairs by hand", path)
			}
		})
	}
}
