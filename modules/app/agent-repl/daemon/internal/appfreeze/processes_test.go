package appfreeze

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"testing"

	"agentrepl/logging/procstat"
)

type fakeRunner struct {
	out  string
	code int
	err  error
	argv []string
	dir  string
}

func (f *fakeRunner) Run(_ context.Context, dir string, argv []string) (string, int, error) {
	f.dir, f.argv = dir, argv
	return f.out, f.code, f.err
}

func TestPSLister(t *testing.T) {
	tests := []struct {
		name    string
		runner  *fakeRunner
		want    []Process
		wantErr bool
	}{
		{name: "paths keep their spaces", runner: &fakeRunner{out: "  1     0 /sbin/launchd\n 300     1 /Applications/Google Chrome.app/Contents/MacOS/Google Chrome\n\n"},
			want: []Process{{PID: 1, PPID: 0, Path: "/sbin/launchd"}, {PID: 300, PPID: 1, Path: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"}}},
		{name: "a tool that cannot run is an error", runner: &fakeRunner{err: errors.New("no such file")}, wantErr: true},
		{name: "a failing tool is an error", runner: &fakeRunner{out: "ps: bad option", code: 1}, wantErr: true},
		{name: "a line without a path is refused", runner: &fakeRunner{out: "1 0\n"}, wantErr: true},
		{name: "a line whose pid is no number is refused", runner: &fakeRunner{out: "x 0 /bin/x\n"}, wantErr: true},
		{name: "a line whose parent is no number is refused", runner: &fakeRunner{out: "1 y /bin/x\n"}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := PSLister(tt.runner, "/bin/ps")(context.Background())

			// Assert
			if !reflect.DeepEqual(tt.runner.argv, []string{"/bin/ps", "-axo", "pid=,ppid=,comm="}) || tt.runner.dir != "/" {
				t.Fatalf("ran %q in %q, want ps's whole table in /", tt.runner.argv, tt.runner.dir)
			}
			if tt.wantErr {
				if err == nil {
					t.Fatalf("PSLister() = %v, want an error", got)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("PSLister() = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestExecutableOf(t *testing.T) {
	tests := []struct {
		path, app string
		want      bool
	}{
		{path: "/usr/bin/slack", app: "slack", want: true},
		{path: "/opt/google/chrome/chrome_crashpad_handler", app: "chrome", want: true},
		{path: "/usr/bin/slack-desktop-helper", app: "slack", want: false},
		{path: "", app: "slack", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.path+"/"+tt.app, func(t *testing.T) {
			// Act
			got := executableOf(tt.path, tt.app)

			// Assert
			if got != tt.want {
				t.Fatalf("executableOf(%q, %q) = %v, want %v", tt.path, tt.app, got, tt.want)
			}
		})
	}
}

func TestAppMatcherForEachPlatform(t *testing.T) {
	// Act
	mac, linux := AppMatcherFor("darwin"), AppMatcherFor("linux")

	// Assert
	if !mac("/Applications/Slack.app/Contents/MacOS/Slack", "Slack") || mac("/usr/bin/slack", "slack") ||
		!linux("/usr/bin/slack", "slack") || linux("/Applications/Slack.app/Contents/MacOS/Slack", "slack") {
		t.Fatal("each platform must match its own naming of an application, and only it")
	}
}

func TestProcListerReadsTheTreeAndHidesAnotherUsersExecutable(t *testing.T) {
	// Arrange
	root := t.TempDir()
	write := func(path, content string) {
		t.Helper()
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	write(filepath.Join(root, "stat"), "btime 1\n")
	write(filepath.Join(root, "10", "stat"), "10 (slack) S 1 10 10 0 -1 0 0 0 0 0 1 1 0 0 20 0 1 0 5 0 0")
	if err := os.Symlink("/usr/bin/slack", filepath.Join(root, "10", "exe")); err != nil {
		t.Fatal(err)
	}
	write(filepath.Join(root, "11", "stat"), "11 (systemd) S 0 11 11 0 -1 0 0 0 0 0 1 1 0 0 20 0 1 0 5 0 0")

	// Act
	got, err := ProcLister(procstat.New(root))(context.Background())

	// Assert
	sort.Slice(got, func(i, j int) bool { return got[i].PID < got[j].PID })
	want := []Process{{PID: 10, PPID: 1, Path: "/usr/bin/slack"}, {PID: 11, PPID: 0, Path: ""}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("ProcLister() = (%+v, %v), want %+v", got, err, want)
	}
}
