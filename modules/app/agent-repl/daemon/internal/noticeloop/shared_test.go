package noticeloop_test

import (
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/mergenotice"
	"agent-repld/internal/workspacenotice"
	"agent-repld/internal/workspacesubs"
)

// TestEveryNotifierDeliversThroughTheSharedLoop pins that each notifier of an
// agent delivers through noticeloop.Loop rather than a loop of its own, so the
// ordering and drain-on-exit guarantees cannot drift between them.
func TestEveryNotifierDeliversThroughTheSharedLoop(t *testing.T) {
	notifiers := map[string]reflect.Type{
		"mergenotice.Service":     reflect.TypeOf(mergenotice.Service{}),
		"workspacenotice.Service": reflect.TypeOf(workspacenotice.Service{}),
		"workspacesubs.Service":   reflect.TypeOf(workspacesubs.Service{}),
	}
	for name, typ := range notifiers {
		t.Run(name, func(t *testing.T) {
			// Act
			var loops int
			for i := range typ.NumField() {
				field := typ.Field(i).Type
				if field.Kind() == reflect.Pointer && strings.HasPrefix(field.Elem().String(), "noticeloop.Loop[") {
					loops++
				}
			}

			// Assert
			if loops != 1 {
				t.Errorf("%s holds %d noticeloop.Loop fields, want exactly 1", name, loops)
			}
		})
	}
}
