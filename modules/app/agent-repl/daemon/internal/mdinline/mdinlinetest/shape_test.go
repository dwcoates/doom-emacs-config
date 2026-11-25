package mdinlinetest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const source = `package x

import "agent-repld/internal/mdinline"

func Shared(md string) string {
	return mdinline.RewriteProse(md, false, nil)
}

func HandRolled(md string) string {
	doc := mdinline.Read(md)
	_ = doc
	return mdinline.RewriteProse(md, false, nil)
}

func Neither(md string) string { return md }
`

func TestRewriteShape(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "x.go")
	if err := os.WriteFile(file, []byte(source), 0o600); err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name    string
		file    string
		fn      string
		wantErr string
	}{
		{name: "a function walking through RewriteProse has the shape", file: file, fn: "Shared"},
		{name: "a function reading lines itself is refused", file: file, fn: "HandRolled", wantErr: "walks mdinline.Read's lines itself"},
		{name: "a function never calling RewriteProse is refused", file: file, fn: "Neither", wantErr: "never calls mdinline.RewriteProse"},
		{name: "a missing function is refused", file: file, fn: "Absent", wantErr: "declares no function Absent"},
		{name: "an unparsable file is refused", file: filepath.Join(dir, "missing.go"), fn: "Shared", wantErr: "parse"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := RewriteShape(tc.file, tc.fn)

			// Assert
			switch {
			case tc.wantErr == "" && err != nil:
				t.Fatalf("RewriteShape() = %v, want nil", err)
			case tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)):
				t.Fatalf("RewriteShape() = %v, want an error holding %q", err, tc.wantErr)
			}
		})
	}
}

func TestHandWrittenLinks(t *testing.T) {
	dir := t.TempDir()
	write := func(name, src string) string {
		file := filepath.Join(dir, name)
		if err := os.WriteFile(file, []byte(src), 0o600); err != nil {
			t.Fatal(err)
		}
		return file
	}
	cases := []struct {
		name    string
		file    string
		wantErr string
	}{
		{name: "a file writing links through WriteLink passes",
			file: write("ok.go", "package x\n\nfunc f() { mdinline.WriteLink(nil, \"#1\", \"u\") }\n")},
		{name: "a file spelling a link by hand is refused",
			file: write("bad.go", "package x\n\nvar s = \"[\" + \"t\" + \"](\" + \"u\" + \")\"\n"), wantErr: "spells a markdown link by hand"},
		{name: "a raw regular expression is not a link",
			file: write("regexp.go", "package x\n\nvar r = `^[a-z](?:x)$`\n")},
		{name: "an unparsable file is refused", file: filepath.Join(dir, "missing.go"), wantErr: "parse"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := HandWrittenLinks(tc.file)

			// Assert
			switch {
			case tc.wantErr == "" && err != nil:
				t.Fatalf("HandWrittenLinks() = %v, want nil", err)
			case tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)):
				t.Fatalf("HandWrittenLinks() = %v, want an error holding %q", err, tc.wantErr)
			}
		})
	}
}
