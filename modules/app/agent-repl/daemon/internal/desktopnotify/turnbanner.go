package desktopnotify

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/wsm"
)

// opTurn is the operation the turn banner's records carry.
const opTurn = "daemon.desktopnotify.turn_ended"

// KindTurnEnded is the banner kind a turn end raises.
const KindTurnEnded = "turn_ended"

// Endings takes what a live turn's end said (feed.Resolver.TakeTurnEnding).
type Endings interface {
	TakeTurnEnding(ws ids.WorkspaceID, turn ids.TurnID) (feed.TurnEnding, bool)
}

// Poster raises one banner (Notifier.Post).
type Poster interface {
	Post(ws ids.WorkspaceID, kind string, compose Compose)
}

// Summaries writes a completed turn's body (Summarizer.Summarize).
type Summaries interface {
	Summarize(ctx context.Context, ws ids.WorkspaceID, answer string) string
}

// TurnDeps are the turn banner's collaborators.
type TurnDeps struct {
	Endings   Endings
	Poster    Poster
	Summaries Summaries
	// Now stamps the banner with the turn's end.
	Now func() time.Time
	Log dlog.Logger
}

// TurnBanners raises the banner a turn end earns. The prompt queue's turn end
// calls it — the same edge that installs the turn's close on the roster — so a
// banner is raised exactly for the turns whose end the clients are shown, and
// never for a replayed one.
type TurnBanners struct {
	deps TurnDeps
}

// NewTurnBanners builds the turn banner. It panics on a missing collaborator.
func NewTurnBanners(deps TurnDeps) *TurnBanners {
	if deps.Endings == nil || deps.Poster == nil || deps.Summaries == nil || deps.Now == nil || deps.Log == nil {
		panic("desktopnotify: NewTurnBanners requires Endings, Poster, Summaries, Now and Log")
	}
	return &TurnBanners{deps: deps}
}

// OnTurnEnded raises the banner for a turn that ended with how. It reads the
// turn's end through ladder.ResolveTurnEnd, the table the roster's colour is
// drawn from: `done` and `interrupted` (green) raise ✅ with a summary of the
// final answer, `turn_failed` (turquoise, or blue once the vendor blocks)
// raises ❌ with the errored ending's line.
func (t *TurnBanners) OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how wsm.TurnClose) {
	log := t.deps.Log.With(dlog.Context{"workspace": string(ws), "turn": string(turn)})
	ending, ok := t.deps.Endings.TakeTurnEnding(ws, turn)
	if !ok {
		log.Info(opTurn, "the turn drew no live ending (a /clear, or a turn this feed never saw); no banner", nil)
		return
	}
	end, known := ladder.ResolveTurnEnd(how, ending.Failure)
	if !known {
		log.Error(opTurn, "a turn close has no turn end; no banner", dlog.Context{
			"close": int(how), "invariant_violation": "every turn close resolves to a turn end",
		})
		return
	}
	at := t.deps.Now()
	log.Debug(opTurn, "raising the turn's banner", dlog.Context{"turn_end": end.String()})
	t.deps.Poster.Post(ws, KindTurnEnded, func(ctx context.Context, name string) Banner {
		if end == ladder.TurnEndFailed {
			return Banner{Title: TurnTitle(end, name, at), Body: ending.Error}
		}
		return Banner{Title: TurnTitle(end, name, at), Body: t.deps.Summaries.Summarize(ctx, ws, ending.Answer)}
	})
}

// TurnTitle is a turn banner's first line: the outcome's mark, the
// workspace's name, what happened, and the local time the turn ended.
func TurnTitle(end ladder.TurnEnd, name string, at time.Time) string {
	if end == ladder.TurnEndFailed {
		return fmt.Sprintf("❌ %s turn errored %s", name, at.Format("15:04"))
	}
	return fmt.Sprintf("✅ %s turn completed %s", name, at.Format("15:04"))
}
