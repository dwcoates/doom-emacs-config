package newsdigest

import (
	"context"
	"errors"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
)

// Republish reads the standing digest from the store and publishes it (or
// none), so a daemon that inherited a digest draws it and a digest another
// daemon made while this one was not serving reaches this one's webviews.
func (d *Digester) Republish(ctx context.Context) error {
	d.standingMu.Lock()
	defer d.standingMu.Unlock()
	state, err := d.deps.Store.NewsDigestState(ctx)
	if err != nil {
		d.deps.Log.Error(opStanding, "the standing news digest could not be read", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("newsdigest: read the standing digest: %w", err)
	}
	overlay, err := decodeStanding(state.Standing)
	if err != nil {
		d.deps.Log.Error(opStanding, "the standing news digest did not decode", dlog.Context{
			"digest": state.LatestID, "cause": err.Error(),
		})
		return err
	}
	d.publishLocked(overlay)
	d.deps.Log.Debug(opStanding, "published the standing news digest", dlog.Context{
		"digest": state.LatestID, "shown": overlay != nil,
	})
	return nil
}

// Dismiss answers DismissNewsDigest: the standing digest is taken down in
// every webview when the request names it (or names it again); any other id
// is unknown_digest and changes nothing. An error is the store failing.
func (d *Digester) Dismiss(ctx context.Context, req *agentreplv1.DismissNewsDigestRequest) (*agentreplv1.DismissNewsDigestResponse, error) {
	id := req.GetId().GetValue()
	d.standingMu.Lock()
	defer d.standingMu.Unlock()
	matched, err := d.deps.Store.DismissNewsDigest(ctx, id)
	if err != nil {
		d.deps.Log.Error(opDismiss, "the news digest could not be dismissed", dlog.Context{"digest": id, "cause": err.Error()})
		return nil, fmt.Errorf("newsdigest: dismiss %q: %w", id, err)
	}
	if !matched {
		d.deps.Log.Info(opDismiss, "a dismiss named no standing news digest", dlog.Context{"digest": id})
		return &agentreplv1.DismissNewsDigestResponse{Result: &agentreplv1.DismissNewsDigestResponse_Error{
			Error: &agentreplv1.DismissNewsDigestError{Cause: &agentreplv1.DismissNewsDigestError_UnknownDigest{
				UnknownDigest: &agentreplv1.DismissNewsDigestUnknownDigest{},
			}},
		}}, nil
	}
	d.publishLocked(nil)
	d.deps.Log.Info(opDismiss, "dismissed the news digest in every webview", dlog.Context{"digest": id})
	return &agentreplv1.DismissNewsDigestResponse{Result: &agentreplv1.DismissNewsDigestResponse_Success{
		Success: &agentreplv1.DismissNewsDigestSuccess{},
	}}, nil
}

// Redisplay answers a FULL EMACS RESTART (a WatchDaemon carrying an Emacs
// process identity the daemon has not seen last): the day's digest returns
// even if it was dismissed (owner, 2026-10-02: "not a redigest, just the same
// digest redisplayed for the day"). It stands the newest digest again only
// when it was minted TODAY, on the local calendar, and is down; a digest still
// standing, one from an earlier day, or none at all changes nothing. No run
// starts and no model is called. An error is the store failing.
func (d *Digester) Redisplay(ctx context.Context) error {
	d.standingMu.Lock()
	defer d.standingMu.Unlock()
	state, err := d.deps.Store.NewsDigestState(ctx)
	if err != nil {
		d.deps.Log.Error(opRestand, "the news digest could not be read to redisplay it", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("newsdigest: read the digest to redisplay: %w", err)
	}
	fields := dlog.Context{"digest": state.LatestID}
	switch {
	case state.Standing != nil:
		d.deps.Log.Debug(opRestand, "the day's digest already stands; an Emacs restart changes nothing", fields)
		return nil
	case state.LatestOverlay == nil:
		d.deps.Log.Debug(opRestand, "no digest is kept to redisplay", fields)
		return nil
	case !sameLocalDay(state.LatestMadeAt, d.deps.Clock.Now()):
		fields["made_at"] = state.LatestMadeAt
		d.deps.Log.Debug(opRestand, "the newest digest was made on an earlier day; it is not redisplayed", fields)
		return nil
	}
	overlay, err := decodeStanding(state.LatestOverlay)
	if err != nil {
		d.deps.Log.Error(opRestand, "the kept news digest did not decode; it is not redisplayed", dlog.Context{
			"digest": state.LatestID, "cause": err.Error(),
		})
		return err
	}
	stood, err := d.deps.Store.RestandNewsDigest(ctx, state.LatestID)
	if err != nil {
		d.deps.Log.Error(opRestand, "the day's digest could not be stood again", dlog.Context{
			"digest": state.LatestID, "cause": err.Error(),
		})
		return fmt.Errorf("newsdigest: restand %q: %w", state.LatestID, err)
	}
	if !stood {
		d.deps.Log.Error(opRestand, "the store refused to stand the day's digest again", dlog.Context{
			"digest":              state.LatestID,
			"invariant_violation": "a dismissed digest read under the standing lock can be stood again",
		})
		return fmt.Errorf("newsdigest: the store did not stand %q again", state.LatestID)
	}
	d.publishLocked(overlay)
	d.deps.Log.Info(opRestand, "an Emacs restart stood the day's dismissed digest again in every webview", fields)
	return nil
}

// sameLocalDay reports whether two instants fall on one local calendar day. A
// zero instant (a digest minted before its instant was kept) is never today.
func sameLocalDay(a, b time.Time) bool {
	if a.IsZero() {
		return false
	}
	ay, am, ad := a.Local().Date()
	by, bm, bd := b.Local().Date()
	return ay == by && am == bm && ad == bd
}

// publishLocked publishes overlay as the standing, or none when it is nil.
// standingMu is held.
func (d *Digester) publishLocked(overlay *frontendv1.NewsDigestOverlay) {
	if overlay == nil {
		d.topic.Publish(&agentreplv1.NewsDigestStanding{Standing: &agentreplv1.NewsDigestStanding_None{
			None: &agentreplv1.NewsDigestNone{},
		}})
		return
	}
	d.topic.Publish(&agentreplv1.NewsDigestStanding{Standing: &agentreplv1.NewsDigestStanding_Shown{Shown: overlay}})
}

// decodeStanding decodes a stored overlay; nil stays nil (none stands). An
// overlay that does not decode, or whose sections are not ones this daemon
// makes, is refused rather than drawn.
func decodeStanding(stored []byte) (*frontendv1.NewsDigestOverlay, error) {
	if stored == nil {
		return nil, nil
	}
	overlay := &frontendv1.NewsDigestOverlay{}
	if err := proto.Unmarshal(stored, overlay); err != nil {
		return nil, fmt.Errorf("newsdigest: the stored overlay did not decode: %w", err)
	}
	if overlay.GetId().GetValue() == "" {
		return nil, errors.New("newsdigest: the stored overlay carries no id")
	}
	for i, section := range overlay.GetSections() {
		if section.GetKind().GetKind() == nil {
			return nil, fmt.Errorf("newsdigest: the stored overlay's section %d carries no kind", i)
		}
		if len(section.GetItems()) == 0 {
			return nil, fmt.Errorf("newsdigest: the stored overlay's section %d has no items", i)
		}
	}
	// A digest made before the weekly section existed carries none; one that
	// carries it carries it whole.
	if week := overlay.GetWeek(); week != nil {
		switch outcome := week.GetOutcome().(type) {
		case *frontendv1.NewsDigestWeek_Quiet:
		case *frontendv1.NewsDigestWeek_Risks:
			if len(outcome.Risks.GetItems()) == 0 {
				return nil, errors.New("newsdigest: the stored overlay's week holds no risks")
			}
		default:
			return nil, errors.New("newsdigest: the stored overlay's week carries no outcome")
		}
	}
	return overlay, nil
}
