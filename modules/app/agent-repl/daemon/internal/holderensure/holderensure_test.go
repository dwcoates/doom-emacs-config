package holderensure

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"agentrepl/logging/sockethold"

	"agent-repld/internal/dlog"
)

// writeManifests writes the root manifest and its includes under a fresh
// module root, answering it.
func writeManifests(t *testing.T, files map[string]string) string {
	t.Helper()
	root := t.TempDir()
	for rel, body := range files {
		path := filepath.Join(root, rel)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

func TestDeclaredReadsEveryHolderTheManifestsDeclareForThisPlatform(t *testing.T) {
	other := "linux"
	if runtime.GOOS == "linux" {
		other = "darwin"
	}
	tests := []struct {
		name    string
		files   map[string]string
		want    []Holder
		wantErr bool
	}{
		{name: "a holder in an included manifest is read with its control expanded", files: map[string]string{
			"requirements.json":   `{"includes": ["d/requirements.json"], "requirements": []}`,
			"d/requirements.json": `{"requirements": [{"id": "h", "kind": "socket_holder", "control": "~/c.sock"}]}`,
		}, want: []Holder{{ID: "h", Control: "/home/me/c.sock"}}},
		{name: "a holder in the root manifest is read", files: map[string]string{
			"requirements.json": `{"requirements": [{"id": "h", "kind": "socket_holder", "platforms": ["` + runtime.GOOS + `"], "control": "/c.sock"}]}`,
		}, want: []Holder{{ID: "h", Control: "/c.sock"}}},
		{name: "every other kind is passed over", files: map[string]string{
			"requirements.json": `{"requirements": [{"id": "s", "kind": "service", "control": "/c.sock"}]}`,
		}},
		{name: "a holder of another platform is passed over", files: map[string]string{
			"requirements.json": `{"requirements": [{"id": "h", "kind": "socket_holder", "platforms": ["` + other + `"], "control": "/c.sock"}]}`,
		}},
		{name: "an include that does not exist is an error", files: map[string]string{
			"requirements.json": `{"includes": ["gone/requirements.json"]}`,
		}, wantErr: true},
		{name: "a manifest that is no JSON is an error", files: map[string]string{
			"requirements.json": `{`,
		}, wantErr: true},
		{name: "a relative control socket is an error", files: map[string]string{
			"requirements.json": `{"requirements": [{"id": "h", "kind": "socket_holder", "control": "c.sock"}]}`,
		}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := writeManifests(t, tt.files)

			// Act
			got, err := Declared(root, "/home/me")

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("Declared = (%v, %v), want %v (error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestDeclaredReadsWithNoRootManifestIsAnError(t *testing.T) {
	// Act
	_, err := Declared(t.TempDir(), "/home/me")

	// Assert
	if err == nil {
		t.Fatal("Declared = nil, want the missing root manifest")
	}
}

func TestControlOfFindsTheHolderByID(t *testing.T) {
	holders := []Holder{{ID: "a", Control: "/a.sock"}, {ID: "b", Control: "/b.sock"}}
	tests := []struct {
		name    string
		id      string
		want    string
		wantErr bool
	}{
		{name: "a declared holder answers its control socket", id: "b", want: "/b.sock"},
		{name: "an undeclared holder is an error", id: "c", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ControlOf(holders, tt.id)

			// Assert
			if got != tt.want || (err != nil) != tt.wantErr {
				t.Fatalf("ControlOf = (%q, %v), want %q (error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestEnsure(t *testing.T) {
	missing := fmt.Errorf("%w: dial", sockethold.ErrNoHolder)
	hung := errors.New("read: i/o timeout")
	tests := []struct {
		name        string
		control     string
		probe       error
		install     error
		wantErr     bool
		wantInstall string
		wantRecord  dlog.RecordQuery
	}{
		{name: "a served control socket installs nothing", control: "/d.sock",
			wantRecord: dlog.RecordQuery{Level: "debug", Message: "a holder serves the control socket"}},
		{name: "a declared holder no holder serves is installed", control: "/d.sock", probe: missing, wantInstall: "daemon-holder",
			wantRecord: dlog.RecordQuery{Level: "info", Message: "installed the holder"}},
		{name: "an install that fails fails the ensure", control: "/d.sock", probe: missing, install: errors.New("exit 1"), wantErr: true, wantInstall: "daemon-holder",
			wantRecord: dlog.RecordQuery{Level: "error", Message: "the holder could not be installed"}},
		{name: "an undeclared control socket no holder serves is never installed", control: "/test.sock", probe: missing, wantErr: true,
			wantRecord: dlog.RecordQuery{Level: "error", Message: "no holder serves the control socket, and no requirement declares a holder there, so none is installed"}},
		{name: "a holder that fails its probe is never reinstalled", control: "/d.sock", probe: hung, wantErr: true,
			wantRecord: dlog.RecordQuery{Level: "error", Message: "the holder's control socket did not answer its probe; it is the holder's failure, and nothing was installed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			var installed string
			var bounded bool
			e := &Ensurer{
				Holders: []Holder{{ID: "daemon-holder", Control: "/d.sock"}},
				Probe:   func(context.Context, string) error { return tt.probe },
				Install: func(ctx context.Context, id string) error {
					_, bounded = ctx.Deadline()
					installed = id
					return tt.install
				},
				Log: log,
			}

			// Act
			err := e.Ensure(context.Background(), tt.control)

			// Assert
			if (err != nil) != tt.wantErr || installed != tt.wantInstall || (installed != "" && !bounded) {
				t.Fatalf("Ensure = %v, installed %q (bounded %v); want error %v, installed %q", err, installed, bounded, tt.wantErr, tt.wantInstall)
			}
			q := tt.wantRecord
			q.Operation = op
			rec, ok := dlog.FindRecord(log.Records(), q)
			if !ok || rec.Context["control"] != tt.control {
				t.Fatalf("records = %+v, want %+v naming the control socket", log.Records(), q)
			}
		})
	}
}
