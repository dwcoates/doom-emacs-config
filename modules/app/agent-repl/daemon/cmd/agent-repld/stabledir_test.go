package main

import (
	"errors"
	"strings"
	"testing"
)

func TestEnterStableDir(t *testing.T) {
	cases := []struct {
		name      string
		opts      options
		getwdErr  error
		chdirErr  error
		want      string
		wantErr   string
		wantMoved bool
	}{
		{name: "absolute path flags move the daemon and answer where it started", opts: options{stateDir: "/state", shim: "/shim/main.js"}, want: "/worktree", wantMoved: true},
		{name: "no path flags move the daemon", want: "/worktree", wantMoved: true},
		{name: "a deleted start directory is named, and the daemon still moves", getwdErr: errors.New("no such file or directory"), want: "unreadable: no such file or directory", wantMoved: true},
		{name: "a relative path flag is refused before any move", opts: options{webapp: "dist"}, wantErr: `--webapp-dist "dist" is relative`},
		{name: "a refused move fails the boot", chdirErr: errors.New("permission denied"), wantErr: "move to /: permission denied", wantMoved: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getwd := func() (string, error) { return "/worktree", tc.getwdErr }
			var moved []string
			chdir := func(dir string) error { moved = append(moved, dir); return tc.chdirErr }

			// Act
			got, err := enterStableDir(tc.opts, getwd, chdir)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("enterStableDir = %q, %v; want an error containing %q", got, err, tc.wantErr)
				}
			} else if err != nil || got != tc.want {
				t.Fatalf("enterStableDir = %q, %v; want %q", got, err, tc.want)
			}
			if wantMoves := map[bool]int{true: 1, false: 0}[tc.wantMoved]; len(moved) != wantMoves || (wantMoves == 1 && moved[0] != stableDir) {
				t.Fatalf("moved to %v, want %d move(s) to %s", moved, wantMoves, stableDir)
			}
		})
	}
}

func TestPathFlagsNameEveryPathFlag(t *testing.T) {
	// Arrange: every path flag set to a relative path.
	for _, name := range []string{"state-dir", "webapp-dist", "shim-main", "prompts-dir", "multi-repo-config-dir", "default-config-dir", "self-repo"} {
		t.Run(name, func(t *testing.T) {
			opts, err := parseFlags("agent-repld", []string{"--" + name + "=relative/path"})
			if err != nil {
				t.Fatalf("parseFlags: %v", err)
			}

			// Act
			_, err = enterStableDir(opts, func() (string, error) { return "/worktree", nil }, func(string) error { return nil })

			// Assert
			if err == nil || !strings.Contains(err.Error(), "--"+name) {
				t.Fatalf("enterStableDir = %v, want --%s refused as relative", err, name)
			}
		})
	}
}
