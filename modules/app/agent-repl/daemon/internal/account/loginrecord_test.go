package account_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/account"
)

// rootWithIdentity makes a config root whose identity file holds body.
func rootWithIdentity(t *testing.T, body string) string {
	t.Helper()
	dir := t.TempDir()
	writeIdentity(t, dir, body)
	return dir
}

func TestReadLoginRecordReadsTheAccountBlock(t *testing.T) {
	cases := []struct {
		name      string
		body      string
		wantBlock string
		wantStamp time.Time
	}{
		{
			name:      "a logged-in root carries its block and its profile stamp",
			body:      `{"numStartups":3,"oauthAccount":{"emailAddress":"a@b.c","profileFetchedAt":1791267808251}}`,
			wantBlock: `{"emailAddress":"a@b.c","profileFetchedAt":1791267808251}`,
			wantStamp: time.UnixMilli(1791267808251).UTC(),
		},
		{
			name:      "a block without a profile stamp has a zero stamp",
			body:      `{"oauthAccount":{"emailAddress":"a@b.c"}}`,
			wantBlock: `{"emailAddress":"a@b.c"}`,
		},
		{
			name: "a root naming no account has an empty record",
			body: `{"numStartups":3}`,
		},
		{
			name: "a null account block is no account",
			body: `{"oauthAccount":null}`,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := rootWithIdentity(t, tc.body)

			// Act.
			got, err := account.ReadLoginRecord(dir)

			// Assert.
			if err != nil {
				t.Fatalf("ReadLoginRecord: %v", err)
			}
			if got.Block != tc.wantBlock || !got.ProfileFetchedAt.Equal(tc.wantStamp) {
				t.Fatalf("ReadLoginRecord = %+v, want block %q stamp %s", got, tc.wantBlock, tc.wantStamp)
			}
		})
	}
}

func TestReadLoginRecordOfARootWithNoIdentityFileIsEmpty(t *testing.T) {
	// Act.
	got, err := account.ReadLoginRecord(t.TempDir())

	// Assert.
	if err != nil || got != (account.LoginRecord{}) {
		t.Fatalf("ReadLoginRecord = (%+v, %v), want an empty record", got, err)
	}
}

func TestReadLoginRecordRefusesAMalformedFile(t *testing.T) {
	cases := []struct {
		name string
		body string
		want string
	}{
		{name: "a file that is not JSON", body: `{`, want: "parsing"},
		{name: "a stamp that is not a number", body: `{"oauthAccount":{"profileFetchedAt":"soon"}}`, want: "oauthAccount block"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := rootWithIdentity(t, tc.body)

			// Act.
			_, err := account.ReadLoginRecord(dir)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("ReadLoginRecord error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestReadLoginRecordRefusesAnEmptyConfigDir(t *testing.T) {
	// Act.
	_, err := account.ReadLoginRecord("")

	// Assert.
	if err == nil {
		t.Fatal("ReadLoginRecord(\"\") = nil, want a refusal")
	}
}

func TestReadLoginRecordRefusesAnUnreadableFile(t *testing.T) {
	// Arrange: the identity file's path is a directory, which cannot be read.
	dir := t.TempDir()
	if err := os.Mkdir(filepath.Join(dir, ".claude.json"), 0o700); err != nil {
		t.Fatalf("Mkdir: %v", err)
	}

	// Act.
	_, err := account.ReadLoginRecord(dir)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "reading") {
		t.Fatalf("ReadLoginRecord error = %v, want the read failure", err)
	}
}
