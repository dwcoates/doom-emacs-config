package appfreeze

import (
	"context"
	"errors"
	"reflect"
	"testing"
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
