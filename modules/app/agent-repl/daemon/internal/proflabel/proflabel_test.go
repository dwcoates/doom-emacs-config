package proflabel

import (
	"bytes"
	"context"
	"runtime/pprof"
	"strings"
	"testing"

	"agent-repld/internal/ids"
)

func TestDo(t *testing.T) {
	tests := []struct {
		name      string
		ws        ids.WorkspaceID
		wantLabel bool
	}{
		{name: "work for a workspace is labeled with its id", ws: "ws-1", wantLabel: true},
		{name: "work for no workspace is unlabeled", ws: "", wantLabel: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var got ids.WorkspaceID
			var labeled, ran bool

			// Act
			Do(context.Background(), tt.ws, func(ctx context.Context) {
				ran = true
				got, labeled = Of(ctx)
			})

			// Assert
			if !ran || labeled != tt.wantLabel || got != tt.ws {
				t.Fatalf("ran %v, Of = %q, %v; want %q, %v", ran, got, labeled, tt.ws, tt.wantLabel)
			}
		})
	}
}

// A GOROUTINE INHERITS ITS CREATOR'S LABELS, which is what labels everything
// a request or a watcher starts: a goroutine profile taken while one waits
// shows it under the workspace's label.
func TestAGoroutineStartedUnderALabelCarriesIt(t *testing.T) {
	// Arrange
	waiting := make(chan struct{})
	release := make(chan struct{})
	done := make(chan struct{})
	Do(context.Background(), "ws-child", func(context.Context) {
		go func() {
			defer close(done)
			close(waiting)
			<-release
		}()
	})
	<-waiting

	// Act
	var buf bytes.Buffer
	err := pprof.Lookup("goroutine").WriteTo(&buf, 1)
	close(release)
	<-done

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(buf.String(), `labels: {"`+Key+`":"ws-child"}`) {
		t.Fatalf("no goroutine carries %s=ws-child in:\n%s", Key, buf.String())
	}
}

func TestDoRestoresTheCallersLabels(t *testing.T) {
	// Arrange
	outer := pprof.WithLabels(context.Background(), pprof.Labels("other", "x"))

	// Act
	var inner, after ids.WorkspaceID
	Do(outer, "ws-1", func(ctx context.Context) { inner, _ = Of(ctx) })
	after, labeled := Of(outer)

	// Assert
	if inner != "ws-1" || labeled || after != "" {
		t.Fatalf("inner %q, after %q (%v); want the label inside alone", inner, after, labeled)
	}
}
