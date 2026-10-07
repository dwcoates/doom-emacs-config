package chessboard

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// callTimeout bounds one CEE call: a widget resolution walks the whole game
// tree, which a large analysis makes slow but never this slow.
const callTimeout = 2 * time.Minute

// Deps is what Boards needs from the daemon.
type Deps struct {
	// Log is the daemon's global logger. Required.
	Log dlog.Logger
	// Run runs the backend's commands (npm, go, pkill, gns). Required;
	// production binds internal/scriptrunner.
	Run Runner
	// Getenv reads the daemon's environment, for the checkout. Required.
	Getenv func(string) string
	// PluginDir is the gns cee plugin's install directory. Required.
	PluginDir string
	// HTTP makes the CEE calls. Required.
	HTTP *http.Client
	// Life is the daemon's lifetime: it bounds every readying run and CEE
	// call, and ends Changed. Required.
	Life context.Context
}

// Boards is every board's state, keyed by its session. A board is resolved
// when it is first drawn and again on every later draw that finds it at rest,
// so a board drawn from history after its session has gone shows as
// unavailable rather than as the last widget it had.
type Boards struct {
	deps    Deps
	backend *backend

	mu sync.Mutex
	// boards is each session's state.
	boards map[Session]*board
	// changed is every session whose bubble changed since Changed last
	// answered.
	changed map[Session]struct{}
	// wake is signalled when changed gains a session.
	wake chan struct{}
	// settled is told each session whose resolution ended, after its body is
	// recorded. nil in production; a test waits on it, because a resolution
	// that changes nothing publishes nothing to wait on.
	settled func(Session)
}

// board is one session's state.
type board struct {
	// body is the bubble's state arm as it stands.
	body isBody
	// resolving is set while a resolution of this board is under way.
	resolving bool
}

// isBody is one of the bubble's three state arms: *FeedChessBoardPreparing,
// *FeedChessBoardUnavailable or *FeedChessBoardReady. The generated oneof
// interface is unexported, so the arms are held as their messages and
// bubble() refuses anything else.
type isBody = any

// New builds the boards.
func New(deps Deps) (*Boards, error) {
	switch {
	case deps.Log == nil:
		return nil, errors.New("chessboard: Deps.Log is required")
	case deps.Run == nil:
		return nil, errors.New("chessboard: Deps.Run is required")
	case deps.Getenv == nil:
		return nil, errors.New("chessboard: Deps.Getenv is required")
	case deps.PluginDir == "":
		return nil, errors.New("chessboard: Deps.PluginDir is required")
	case deps.HTTP == nil:
		return nil, errors.New("chessboard: Deps.HTTP is required")
	case deps.Life == nil:
		return nil, errors.New("chessboard: Deps.Life is required")
	}
	b := &Boards{
		deps:    deps,
		boards:  map[Session]*board{},
		changed: map[Session]struct{}{},
		wake:    make(chan struct{}, 1),
	}
	b.backend = &backend{
		log:       deps.Log,
		run:       deps.Run,
		getenv:    deps.Getenv,
		pluginDir: deps.PluginDir,
		onStep:    b.onStep,
		life:      deps.Life,
	}
	return b, nil
}

// Board answers the bubble for a session as it stands, and starts resolving
// the board when no resolution of it is under way.
func (b *Boards) Board(s Session) *frontendv1.FeedChessBoard {
	b.mu.Lock()
	defer b.mu.Unlock()
	e, ok := b.boards[s]
	if !ok {
		e = &board{body: preparing(stepGettingReady)}
		b.boards[s] = e
	}
	if !e.resolving {
		e.resolving = true
		go b.resolve(s)
	}
	return bubble(headingFor(s), e.body)
}

// View answers a session's bubble as it stands without starting a resolution:
// what a re-publication after a change draws. A session never drawn is an
// invariant violation (only drawn boards change), and panics.
func (b *Boards) View(s Session) *frontendv1.FeedChessBoard {
	b.mu.Lock()
	defer b.mu.Unlock()
	e, ok := b.boards[s]
	if !ok {
		panic(fmt.Sprintf("chessboard: View of session %v, which was never drawn", s))
	}
	return bubble(headingFor(s), e.body)
}

// FailedRequest answers the bubble for an agent's board call that failed:
// unavailable, saying what the call said. named may name no session.
func FailedRequest(named *conversationv1.AgentChessBoardSession, said string) *frontendv1.FeedChessBoard {
	heading := "Chess board"
	if s, ok := SessionOf(named); ok {
		heading = headingFor(s).GetText()
	}
	reason := "The agent's board request failed."
	if said != "" {
		reason = "The agent's board request failed: " + said
	}
	return bubble(&frontendv1.FeedChessBoardHeading{Text: heading}, unavailable(reason))
}

// Changed blocks until at least one board's bubble has changed, and answers
// every changed session. It answers ctx's error when ctx ends first.
func (b *Boards) Changed(ctx context.Context) ([]Session, error) {
	for {
		b.mu.Lock()
		if len(b.changed) > 0 {
			out := make([]Session, 0, len(b.changed))
			for s := range b.changed {
				out = append(out, s)
			}
			b.changed = map[Session]struct{}{}
			b.mu.Unlock()
			sort.Slice(out, func(i, j int) bool {
				if out[i].ID != out[j].ID {
					return out[i].ID < out[j].ID
				}
				return out[i].GameID < out[j].GameID
			})
			return out, nil
		}
		b.mu.Unlock()
		select {
		case <-b.wake:
		case <-ctx.Done():
			return nil, ctx.Err()
		}
	}
}

// Run hands every change to apply until ctx ends: the loop the feed resolver
// is driven by.
func (b *Boards) Run(ctx context.Context, apply func([]Session)) {
	for {
		sessions, err := b.Changed(ctx)
		if err != nil {
			b.deps.Log.Info(opBoard, "stopped re-publishing chess boards: the daemon is standing down", dlog.Context{"cause": err.Error()})
			return
		}
		apply(sessions)
	}
}

// resolve readies the backend and fetches the session's widget data, then
// settles the board.
func (b *Boards) resolve(s Session) {
	log := b.deps.Log.With(dlog.Context{"cee_session_id": s.ID, "cee_game_id": s.GameID})
	srv, f := b.backend.ensure(b.deps.Life)
	if f != nil {
		b.settle(s, unavailable(f.reason))
		return
	}
	b.setIfPreparing(s, stepLoadingGame)
	ctx, cancel := context.WithTimeout(b.deps.Life, callTimeout)
	defer cancel()
	widget, err := getWidget(ctx, b.deps.HTTP, srv.baseURL, s)
	switch {
	case errors.Is(err, errSessionGone):
		log.Info(opBoard, "the board's CEE session no longer holds its game; the board is unavailable", dlog.Context{"cause": err.Error()})
		b.settle(s, unavailable(fmt.Sprintf("CEE session %s no longer holds game %s.", s.ID, s.GameID)))
	case err != nil:
		log.Error(opBoard, "cee-webapp did not answer the board's widget request", dlog.Context{"cause": err.Error(), "url": srv.baseURL})
		b.settle(s, unavailable("The chess widget's backend did not answer: "+err.Error()))
	default:
		log.Info(opBoard, "the board's widget data arrived; the board is ready", dlog.Context{"widget_bytes": len(widget), "widget_stamp": srv.widgetStamp})
		b.settle(s, &frontendv1.FeedChessBoardReady{
			Widget:      &frontendv1.FeedChessBoardWidget{CeeWebWidget: widget},
			Bundle:      bundleFor(srv.widgetStamp),
			SquareToken: s.Token(),
		})
	}
}

// settle records a board's resolved body, ending its resolution.
func (b *Boards) settle(s Session, body isBody) {
	b.mu.Lock()
	e := b.boards[s]
	e.resolving = false
	b.setLocked(s, e, body)
	settled := b.settled
	b.mu.Unlock()
	if settled != nil {
		settled(s)
	}
}

// setIfPreparing shows a step on a board that is still preparing; a board that
// is already drawn keeps its body while it is re-resolved.
func (b *Boards) setIfPreparing(s Session, step string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	e := b.boards[s]
	if _, ok := e.body.(*frontendv1.FeedChessBoardPreparing); ok {
		b.setLocked(s, e, preparing(step))
	}
}

// onStep shows a readying run's step on every board still preparing.
func (b *Boards) onStep(step string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	for s, e := range b.boards {
		if _, ok := e.body.(*frontendv1.FeedChessBoardPreparing); ok && e.resolving {
			b.setLocked(s, e, preparing(step))
		}
	}
}

// setLocked replaces a board's body and, when that changes its bubble, marks
// the board changed. Called with mu held.
func (b *Boards) setLocked(s Session, e *board, body isBody) {
	before := bubble(headingFor(s), e.body)
	e.body = body
	if proto.Equal(before, bubble(headingFor(s), body)) {
		return
	}
	b.changed[s] = struct{}{}
	select {
	case b.wake <- struct{}{}:
	default:
	}
}

// ErrSessionGone is a square click on a board whose session no longer holds
// its game.
var ErrSessionGone = errSessionGone

// ErrBackend is a square click the backend gave no answer to.
var ErrBackend = errors.New("chessboard: the chess widget's backend gave no answer")

// InspectSquare asks cee-webapp what the engine says about one square of a
// board's game, and answers the GetSquareEventsResponse whole. The singleton
// is ensured on every click (it is reused when live, which is the common
// case), so a stopped backend is restarted by the click rather than failing
// it. A failure wraps ErrSessionGone or ErrBackend.
func (b *Boards) InspectSquare(ctx context.Context, s Session, gamePoint int64, square uint32) ([]byte, error) {
	log := b.deps.Log.With(dlog.Context{"cee_session_id": s.ID, "cee_game_id": s.GameID, "game_point": gamePoint, "square": square})
	url, f := b.backend.webappURL(ctx)
	if f != nil {
		return nil, fmt.Errorf("%w: %s", ErrBackend, f.reason)
	}
	ctx, cancel := context.WithTimeout(ctx, callTimeout)
	defer cancel()
	answer, err := getSquareEvents(ctx, b.deps.HTTP, url, s, gamePoint, square)
	switch {
	case errors.Is(err, errSessionGone):
		log.Info(opSquare, "a square was clicked on a board whose CEE session no longer holds its game", dlog.Context{"cause": err.Error()})
		return nil, err
	case err != nil:
		log.Error(opSquare, "cee-webapp did not answer a square click", dlog.Context{"cause": err.Error(), "url": url})
		return nil, fmt.Errorf("%w: %v", ErrBackend, err)
	}
	log.Debug(opSquare, "cee-webapp answered a square click", dlog.Context{"answer_bytes": len(answer)})
	return answer, nil
}

// bundleFor answers a widget build's bundle URLs.
func bundleFor(stamp string) *frontendv1.FeedChessBoardBundle {
	return &frontendv1.FeedChessBoardBundle{
		ScriptUrl:     BundleRoute + stamp + "/" + WidgetScript,
		StylesheetUrl: BundleRoute + stamp + "/" + WidgetStylesheet,
	}
}

// BundleRoute is where the daemon serves the widget bundle, on its own origin.
const BundleRoute = "/chess-widget/"

// ServeHTTP serves the widget bundle: BundleRoute, a build stamp, and one of
// the two bundle files. Only the build the backend last served is answered; a
// stale stamp is 404, which a page re-reading its board's current URLs never
// asks for.
func (b *Boards) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	rest := r.URL.Path[len(BundleRoute):]
	stamp, name, _ := strings.Cut(rest, "/")
	built, ok := b.backend.builtWidget()
	if !ok || stamp != built.widgetStamp || (name != WidgetScript && name != WidgetStylesheet) {
		b.deps.Log.Info(opBundle, "refused a chess widget bundle request for no current build file", dlog.Context{
			"path": r.URL.Path, "current_stamp": built.widgetStamp,
		})
		http.NotFound(w, r)
		return
	}
	path := filepath.Join(built.widgetDist, name)
	if _, err := os.Stat(path); err != nil {
		b.deps.Log.Error(opBundle, "the current chess widget build is missing a bundle file", dlog.Context{"path": path, "cause": err.Error()})
		http.NotFound(w, r)
		return
	}
	w.Header().Set("Cache-Control", "no-cache")
	http.ServeFile(w, r, path)
}

// headingFor composes a board's heading.
func headingFor(s Session) *frontendv1.FeedChessBoardHeading {
	return &frontendv1.FeedChessBoardHeading{Text: "Chess board · CEE session " + s.ID}
}

// preparing composes the progress arm.
func preparing(step string) *frontendv1.FeedChessBoardPreparing {
	return &frontendv1.FeedChessBoardPreparing{Step: &frontendv1.FeedChessBoardPreparingStep{Text: step}}
}

// unavailable composes the unavailable arm.
func unavailable(reason string) *frontendv1.FeedChessBoardUnavailable {
	return &frontendv1.FeedChessBoardUnavailable{Reason: &frontendv1.FeedChessBoardUnavailableReason{Text: reason}}
}

// bubble composes the whole bubble from its heading and state arm.
func bubble(heading *frontendv1.FeedChessBoardHeading, body isBody) *frontendv1.FeedChessBoard {
	out := &frontendv1.FeedChessBoard{Heading: heading}
	switch arm := body.(type) {
	case *frontendv1.FeedChessBoardPreparing:
		out.State = &frontendv1.FeedChessBoard_Preparing{Preparing: arm}
	case *frontendv1.FeedChessBoardUnavailable:
		out.State = &frontendv1.FeedChessBoard_Unavailable{Unavailable: arm}
	case *frontendv1.FeedChessBoardReady:
		out.State = &frontendv1.FeedChessBoard_Ready{Ready: arm}
	default:
		panic(fmt.Sprintf("chessboard: a board body of type %T has no bubble arm", body))
	}
	return out
}
