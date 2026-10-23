package storeimport

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// fakeStore is an import endpoint answering STATUS and keeping what it got.
type fakeStore struct {
	srv    *httptest.Server
	labels []string
	body   string
	path   string
}

func newFakeStore(t *testing.T, status int) *fakeStore {
	t.Helper()
	fs := &fakeStore{}
	fs.srv = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		fs.body, fs.path, fs.labels = string(raw), r.URL.Path, r.URL.Query()["extra_label"]
		w.WriteHeader(status)
		_, _ = w.Write([]byte("nope"))
	}))
	t.Cleanup(fs.srv.Close)
	return fs
}

func writeStore(t *testing.T, root, content string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(root, File), []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestPostSendsTheBodyLabeledWithTheJobAndTheCallersLabels(t *testing.T) {
	// Arrange
	root := t.TempDir()
	store := newFakeStore(t, http.StatusNoContent)
	writeStore(t, root, `{"import_url":"`+store.srv.URL+`/api/v1/import/prometheus","job":"agent-repl"}`)

	// Act
	err := Post(context.Background(), http.DefaultClient, root, []byte("x 1\n"), "instance=testrun", "suite=go")

	// Assert
	if err != nil {
		t.Fatalf("Post = %v", err)
	}
	if store.path != "/api/v1/import/prometheus" || store.body != "x 1\n" {
		t.Fatalf("store got %q at %q", store.body, store.path)
	}
	if want := []string{"job=agent-repl", "instance=testrun", "suite=go"}; !slices.Equal(store.labels, want) {
		t.Fatalf("labels = %v, want %v", store.labels, want)
	}
}

func TestPostAnswersEveryFailure(t *testing.T) {
	refusing := newFakeStore(t, http.StatusBadRequest)
	tests := []struct {
		name  string
		store string
		want  string
	}{
		{name: "no store file", want: "read the store's address"},
		{name: "a store file without an address", store: `{"job":"agent-repl"}`, want: "names no import_url and job"},
		{name: "a store file without a job", store: `{"import_url":"http://127.0.0.1:1/"}`, want: "names no import_url and job"},
		{name: "an unreadable store file", store: `{`, want: "names no import_url and job"},
		{name: "an import_url that is no URL", store: `{"import_url":"::nope","job":"j"}`, want: "import_url"},
		{name: "a store that does not answer", store: `{"import_url":"http://127.0.0.1:1/x","job":"j"}`, want: "reach http://127.0.0.1:1/x"},
		{name: "a store that refuses", store: `{"import_url":"` + refusing.srv.URL + `/x","job":"j"}`, want: "the samples were refused by"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			if tt.store != "" {
				writeStore(t, root, tt.store)
			}

			// Act
			err := Post(context.Background(), http.DefaultClient, root, []byte("x 1\n"))

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Post = %v, want an error saying %q", err, tt.want)
			}
		})
	}
}
