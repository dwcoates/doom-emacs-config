package desktopnotify

import (
	"context"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// BannerDropper lets go of one workspace's held banner (Notifier.Drop).
type BannerDropper interface {
	Drop(ws ids.WorkspaceID, reason string)
}

// DropBannersOnEnd wraps a state client so every close (MarkClosed) and
// forget (Forget) it records lets go of the workspace's held banner, after
// the write. Nothing else about the client changes.
//
// EVERY END REACHES THE BANNERS, NOT ONLY THE ONES A CALLER REMEMBERS. A
// workspace closes from the user's close, a kill, a landed merge and a boot,
// and a landed merge's own banner is raised after its close, so the forget
// that follows lets go of it too. A write that failed recorded nothing, so
// the banner is kept.
func DropBannersOnEnd(db wsm.DB, banners BannerDropper) wsm.DB {
	return &bannerDroppingDB{DB: db, banners: banners}
}

type bannerDroppingDB struct {
	wsm.DB
	banners BannerDropper
}

// MarkClosed records the close, then drops the workspace's banner.
func (o *bannerDroppingDB) MarkClosed(ctx context.Context, id wsm.WorkspaceID, closure wsm.Closure) error {
	if err := o.DB.MarkClosed(ctx, id, closure); err != nil {
		return err
	}
	o.banners.Drop(id, "the workspace was closed")
	return nil
}

// Forget records the forget, then drops the workspace's banner.
func (o *bannerDroppingDB) Forget(ctx context.Context, id wsm.WorkspaceID) (wsm.ForgetReport, error) {
	report, err := o.DB.Forget(ctx, id)
	if err != nil {
		return report, err
	}
	o.banners.Drop(id, "the workspace was forgotten")
	return report, nil
}
