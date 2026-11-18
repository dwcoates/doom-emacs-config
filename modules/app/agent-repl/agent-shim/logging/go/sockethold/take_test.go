package sockethold

import (
	"context"
	"errors"
	"net"
	"testing"
	"time"
)

func TestTakeTimedBoundsTheTakeAndAnswersItsTime(t *testing.T) {
	refused := errors.New("refused")
	tests := []struct {
		name    string
		take    TakeFunc
		wantErr error
	}{
		{name: "a take that answers is handed back with its listener", take: func(context.Context, string, string) (net.Listener, error) {
			return nil, nil
		}},
		{name: "a take that fails is handed back with its error", take: func(context.Context, string, string) (net.Listener, error) {
			return nil, refused
		}, wantErr: refused},
		{name: "a take that outlasts the bound is cut at it", take: func(ctx context.Context, _, _ string) (net.Listener, error) {
			<-ctx.Done()
			return nil, ctx.Err()
		}, wantErr: context.DeadlineExceeded},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, took, err := TakeTimed(tt.take, "/control", "name")

			// Assert
			if !errors.Is(err, tt.wantErr) || took < 0 || took > TakeBound+time.Second {
				t.Fatalf("TakeTimed = (%s, %v), want %v within the bound", took, err, tt.wantErr)
			}
		})
	}
}

func TestTakeTimedPassesTheControlAndNameThrough(t *testing.T) {
	// Arrange
	var gotControl, gotName string
	var hadDeadline bool
	take := func(ctx context.Context, control, name string) (net.Listener, error) {
		_, hadDeadline = ctx.Deadline()
		gotControl, gotName = control, name
		return nil, nil
	}

	// Act
	if _, _, err := TakeTimed(take, "/control", "store"); err != nil {
		t.Fatal(err)
	}

	// Assert
	if gotControl != "/control" || gotName != "store" || !hadDeadline {
		t.Fatalf("take saw (%q, %q, deadline %v), want the control, the name and the bound", gotControl, gotName, hadDeadline)
	}
}
