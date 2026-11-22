package desktopnotify

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// endingDB is the slice of a state client DropBannersOnEnd wraps: the writes
// it observes.
type endingDB struct {
	wsm.DB
	err error
}

func (e *endingDB) MarkClosed(context.Context, wsm.WorkspaceID, wsm.Closure) error { return e.err }

func (e *endingDB) Forget(context.Context, wsm.WorkspaceID) (wsm.ForgetReport, error) {
	return wsm.ForgetReport{}, e.err
}

type droppedBanners struct{ dropped []string }

func (d *droppedBanners) Drop(ws ids.WorkspaceID, reason string) {
	d.dropped = append(d.dropped, string(ws)+": "+reason)
}

func TestDropBannersOnEnd(t *testing.T) {
	writes := []struct {
		name   string
		write  func(wsm.DB) error
		reason string
	}{
		{name: "a close", write: func(db wsm.DB) error {
			return db.MarkClosed(context.Background(), "ws1", wsm.Closure{At: time.Unix(0, 0), By: wsm.CloseByUser})
		}, reason: "ws1: the workspace was closed"},
		{name: "a forget", write: func(db wsm.DB) error {
			_, err := db.Forget(context.Background(), "ws1")
			return err
		}, reason: "ws1: the workspace was forgotten"},
	}
	refused := errors.New("database is locked")
	for _, w := range writes {
		for _, o := range []struct {
			name string
			err  error
		}{{name: "recorded drops the banner"}, {name: "refused drops nothing", err: refused}} {
			t.Run(w.name+" "+o.name, func(t *testing.T) {
				// Arrange
				banners := &droppedBanners{}
				db := DropBannersOnEnd(&endingDB{err: o.err}, banners)

				// Act
				err := w.write(db)

				// Assert
				if !errors.Is(err, o.err) || (o.err == nil) != (err == nil) {
					t.Fatalf("write = %v, want %v", err, o.err)
				}
				var want []string
				if o.err == nil {
					want = []string{w.reason}
				}
				if !slices.Equal(banners.dropped, want) {
					t.Fatalf("dropped %v, want %v", banners.dropped, want)
				}
			})
		}
	}
}
