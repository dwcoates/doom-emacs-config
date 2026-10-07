package chessboard

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// ranCommand is one command the fake runner was asked to run.
type ranCommand struct {
	dir  string
	argv []string
}

// fakeRunner answers commands by their argv's leading words, records every
// run, and lets a handler stand in for a command's effect on disk.
type fakeRunner struct {
	mu       sync.Mutex
	ran      []ranCommand
	handlers map[string]func(dir string, argv []string) (string, int, error)
}

func newFakeRunner() *fakeRunner {
	return &fakeRunner{handlers: map[string]func(string, []string) (string, int, error){}}
}

// on installs the handler for commands whose argv starts with prefix (words
// joined by spaces).
func (r *fakeRunner) on(prefix string, handler func(dir string, argv []string) (string, int, error)) {
	r.handlers[prefix] = handler
}

func (r *fakeRunner) Run(_ context.Context, dir string, argv []string) (string, int, error) {
	r.mu.Lock()
	r.ran = append(r.ran, ranCommand{dir: dir, argv: append([]string(nil), argv...)})
	r.mu.Unlock()
	line := strings.Join(argv, " ")
	best := ""
	for prefix := range r.handlers {
		if strings.HasPrefix(line, prefix) && len(prefix) > len(best) {
			best = prefix
		}
	}
	if best == "" {
		return "", 0, nil
	}
	return r.handlers[best](dir, argv)
}

// commands answers every run's argv, joined, in order.
func (r *fakeRunner) commands() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := make([]string, 0, len(r.ran))
	for _, c := range r.ran {
		out = append(out, strings.Join(c.argv, " "))
	}
	return out
}

// fakeCheckout makes an explanation-engine checkout holding the widget's
// inputs, and answers the checkout and its CLI directory.
func fakeCheckout(t *testing.T) (string, string) {
	t.Helper()
	checkout := t.TempDir()
	cli := filepath.Join(checkout, cliDir)
	for rel, body := range map[string]string{
		"web/package.json":                           `{"name":"cee-web"}`,
		"web/package-lock.json":                      `{}`,
		"web/gen/chesscom/a_pb.ts":                   "export {}",
		"web/packages/cee-web-widget/package.json":   `{"name":"@chesscom/cee-web-widget"}`,
		"web/packages/cee-web-widget/src/index.ts":   "export {}",
		"web/packages/cee-web-widget/src/mount.ts":   "export {}",
		"web/packages/cee-web-widget/vite.config.ts": "export default {}",
	} {
		writeFile(t, filepath.Join(cli, rel), body)
	}
	return checkout, cli
}

// writeFile writes body to path, making its directory.
func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("make %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// buildsTheWidget installs a widget build that writes both bundle files.
func buildsTheWidget(r *fakeRunner, cli string) {
	r.on("npm run build", func(string, []string) (string, int, error) {
		dist := widgetDist(cli)
		if err := os.MkdirAll(dist, 0o755); err != nil {
			return "", 0, err
		}
		for _, name := range []string{WidgetScript, WidgetStylesheet} {
			if err := os.WriteFile(filepath.Join(dist, name), []byte("built"), 0o644); err != nil {
				return "", 0, err
			}
		}
		return "built", 0, nil
	})
}

// buildsTheWebapp installs a cee-webapp build that writes body to the -o path.
func buildsTheWebapp(r *fakeRunner, body string) {
	r.on("go build", func(_ string, argv []string) (string, int, error) {
		return "", 0, os.WriteFile(argv[3], []byte(body), 0o755)
	})
}

// servesAt installs a `gns cee debug webapp` that answers url.
func servesAt(r *fakeRunner, url string) {
	r.on("env", func(string, []string) (string, int, error) {
		return `{"url":"` + url + `"}`, 0, nil
	})
}

// removeFile deletes path.
func removeFile(t *testing.T, path string) {
	t.Helper()
	if err := os.Remove(path); err != nil {
		t.Fatalf("remove %s: %v", path, err)
	}
}
