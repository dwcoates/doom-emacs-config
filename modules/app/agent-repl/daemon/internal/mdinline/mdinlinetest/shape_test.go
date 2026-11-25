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
