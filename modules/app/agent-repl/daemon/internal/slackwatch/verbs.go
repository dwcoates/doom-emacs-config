package slackwatch

import (
	"context"
	"errors"
	"fmt"
	"slices"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gnsclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/slacklink"
	"agent-repld/internal/wsm"
)

// RefusalKind is why a verb was refused; the server answers each as its
// rpc's error arm.
type RefusalKind int

// The refusals.
const (
	// RefusedNotSubscribed is a verb that needs a subscription the
	// workspace does not hold.
	RefusedNotSubscribed RefusalKind = iota + 1
	// RefusedNoEmojiPrefix is a post that does not open with one shortcode.
	RefusedNoEmojiPrefix
	// RefusedReservedEmoji is a post opening with an announcement's shortcode.
	RefusedReservedEmoji
	// RefusedListenMode is an unforced post in listen mode.
	RefusedListenMode
	// RefusedPostLimit is an unforced post past the post limit.
	RefusedPostLimit
	// RefusedSecret is a post carrying a secret.
	RefusedSecret
	// RefusedUnreachable is Slack failing, through `gns`.
	RefusedUnreachable
)

// Refusal is a verb the watcher refused, with what the refusal names.
type Refusal struct {
	Kind RefusalKind
	// Emoji is the reserved shortcode (RefusedReservedEmoji).
	Emoji string
	// Secrets are what each secret reads as (RefusedSecret).
	Secrets []string
	// Posts are the subscription's posts (RefusedPostLimit).
	Posts Posts
	// Detail is what `gns` said (RefusedUnreachable).
	Detail string
}

func (r *Refusal) Error() string {
	switch r.Kind {
	case RefusedNotSubscribed:
		return "the workspace holds no subscription to the thread"
	case RefusedNoEmojiPrefix:
		return "a post opens with exactly one emoji shortcode and a space"
	case RefusedReservedEmoji:
		return fmt.Sprintf("the shortcode :%s: is reserved for the daemon's announcements", r.Emoji)
	case RefusedListenMode:
		return "the subscription is in listen mode"
	case RefusedPostLimit:
		return fmt.Sprintf("the subscription made %d of its %d posts", r.Posts.Made, r.Posts.Limit)
	case RefusedSecret:
		return fmt.Sprintf("the post carries what reads as a secret (%v)", r.Secrets)
	case RefusedUnreachable:
		return "Slack could not be reached: " + r.Detail
	}
	return fmt.Sprintf("slackwatch: refusal %d", int(r.Kind))
}

// RefusalOf answers ERR's refusal, nil when it is none.
func RefusalOf(err error) *Refusal {
	var r *Refusal
	if errors.As(err, &r) {
		return r
	}
	return nil
}

// unreachable is a `gns` failure as the refusal a verb answers. The failure
// itself was recorded by gnsclient.
func unreachable(err error) error {
	var f *gnsclient.Failure
	if errors.As(err, &f) {
		return &Refusal{Kind: RefusedUnreachable, Detail: f.Error()}
	}
	return err
}

// Posts are a subscription's posts against its limit.
type Posts struct {
	Made  int
	Limit int
}

// PostsOf answers SUB's posts.
func PostsOf(sub wsm.SlackThreadSubscription) Posts {
	return Posts{Made: sub.PostsMade, Limit: DefaultPostLimit}
}

// Workspace names the workspace a verb acts for.
type Workspace struct {
	ID ids.WorkspaceID
	// Name is how its posts' banner and footer name it.
	Name string
}

// parse reads URL, which the server validated, into a thread.
func parse(url string) (slacklink.Thread, error) {
	thread, err := slacklink.ParseThread(url)
	if err != nil {
		return slacklink.Thread{}, fmt.Errorf("slackwatch: %w", err)
	}
	return thread, nil
}

// find answers WS's subscription to the thread URL names: the one rooted at
// the link's message, or the one whose own link names that message.
func (w *Watcher) find(ctx context.Context, ws ids.WorkspaceID, url string) (wsm.SlackThreadSubscription, bool, error) {
	thread, err := parse(url)
	if err != nil {
		return wsm.SlackThreadSubscription{}, false, err
	}
	subs, err := w.deps.Store.WorkspaceSlackThreadSubscriptions(ctx, ws)
	if err != nil {
		return wsm.SlackThreadSubscription{}, false, err
	}
	sub, found := wsm.FindSlackThreadSubscription(subs, thread)
	return sub, found, nil
}

// Subscribe subscribes WS to the thread URL names, in MODE, answering the
// subscription and whether WS already held one. A fresh subscription is
// announced in the thread, and follows it from now on: from the head of the
// channel's echo pipe when GNS keeps one, and otherwise by polling the thread
// from its latest message (a direct message, whose channel GNS keeps no echo
// pipe for). A held one changes its mode only, announced when it changed. A
// closed or unknown workspace is the store's error.
func (w *Watcher) Subscribe(ctx context.Context, ws Workspace, url string, mode wsm.SlackThreadMode, origin wsm.SlackThreadOrigin) (wsm.SlackThreadSubscription, bool, error) {
	fields := dlog.Context{"subscriber": string(ws.ID), "thread_url": url, "mode": string(mode), "origin": string(origin)}
	thread, err := parse(url)
	if err != nil {
		return wsm.SlackThreadSubscription{}, false, err
	}
	sub := wsm.SlackThreadSubscription{Subscriber: ws.ID, URL: url, Mode: mode, Origin: origin, SubscribedAt: w.deps.Clock.Now()}
	held, found, err := w.find(ctx, ws.ID, url)
	if err != nil {
		return wsm.SlackThreadSubscription{}, false, err
	}
	if found {
		sub.Thread = held.Thread
	} else {
		root, err := w.deps.GNS.ThreadRoot(ctx, thread.Channel, thread.TS)
		if err != nil {
			return wsm.SlackThreadSubscription{}, false, unreachable(err)
		}
		sub.Thread = wsm.SlackThreadKey{Channel: thread.Channel, ThreadTS: root}
		if err := w.deps.GNS.SubscribeChannel(ctx, thread.Channel); err != nil {
			return wsm.SlackThreadSubscription{}, false, unreachable(err)
		}
		if err := w.startingPoint(ctx, &sub); err != nil {
			return wsm.SlackThreadSubscription{}, false, unreachable(err)
		}
	}
	previous, wasHeld, err := w.deps.Store.SubscribeSlackThread(ctx, sub)
	if err != nil {
		return wsm.SlackThreadSubscription{}, false, err
	}
	fields["thread"], fields["held"] = sub.Thread.String(), wasHeld
	w.deps.Log.Info(opVerb, "subscribed the workspace to a Slack thread", fields)
	switch {
	case !wasHeld:
		w.announce(ctx, ws, sub, "`"+ws.Name+"`\n"+attachAnnouncement(mode))
	case previous != mode:
		w.announce(ctx, ws, sub, modeChangeAnnouncement(mode))
	}
	w.startWorker(keyOf(sub))
	if !wasHeld {
		if err := w.republishLinks(ctx, ws.ID, sub.Thread, "subscribe"); err != nil {
			return wsm.SlackThreadSubscription{}, false, err
		}
	}
	stored, _, err := w.lookup(ctx, keyOf(sub))
	return stored, wasHeld, err
}

// startingPoint sets the source and cursor SUB, a fresh subscription, follows
// its thread from: the head of its channel's echo pipe when GNS keeps one,
// else the thread's latest message, for a poll.
func (w *Watcher) startingPoint(ctx context.Context, sub *wsm.SlackThreadSubscription) error {
	username, err := w.login(ctx)
	if err != nil {
		return err
	}
	pipe := gnsclient.PipeFor(username, sub.Thread.Channel)
	head, kept, err := w.deps.GNS.PipeHead(ctx, pipe)
	if err != nil {
		return err
	}
	if kept {
		sub.Source, sub.Cursor = wsm.SlackThreadSourceEchoPipe, head
		return nil
	}
	later, err := w.deps.GNS.ThreadAfter(ctx, sub.Thread.Channel, sub.Thread.ThreadTS, sub.Thread.ThreadTS)
	if err != nil {
		return err
	}
	sub.Source, sub.CursorTS = wsm.SlackThreadSourceThreadPoll, sub.Thread.ThreadTS
	if len(later) > 0 {
		sub.CursorTS = later[len(later)-1].TS
	}
	w.deps.Log.Info(opVerb, "GNS keeps no echo pipe for the thread's channel; the thread is followed by polling it",
		dlog.Context{"subscriber": string(sub.Subscriber), "thread": sub.Thread.String(), "channel": sub.Thread.Channel, "pipe": pipe,
			"cursor_ts": sub.CursorTS, "poll_every": ThreadPollEvery.String()})
	return nil
}

// Unsubscribe ends WS's subscription to the thread URL names, telling the
// thread first, and answers whether one stood and whether the thread was
// told.
func (w *Watcher) Unsubscribe(ctx context.Context, ws Workspace, url string) (bool, bool, error) {
	sub, found, err := w.find(ctx, ws.ID, url)
	if err != nil || !found {
		return false, false, err
	}
	announced := w.announce(ctx, ws, sub, detachAnnouncement())
	ended, err := w.deps.Store.UnsubscribeSlackThread(ctx, ws.ID, sub.Thread)
	if err != nil {
		return false, announced, err
	}
	w.stopWorker(keyOf(sub))
	w.deps.Log.Info(opVerb, "ended the workspace's Slack-thread subscription", dlog.Context{"subscriber": string(ws.ID), "thread": sub.Thread.String(), "announced": announced})
	if ended {
		if err := w.republishLinks(ctx, ws.ID, sub.Thread, "unsubscribe"); err != nil {
			return false, announced, err
		}
	}
	return ended, announced, nil
}

// republishLinks republishes WS's links after its subscription to THREAD
// started or ended (VERB), so a link to the thread draws its "subscribed"
// badge exactly while the subscription stands. A failure is recorded here,
// once, and returned.
func (w *Watcher) republishLinks(ctx context.Context, ws ids.WorkspaceID, thread wsm.SlackThreadKey, verb string) error {
	if err := w.deps.Links.RepublishLinks(ctx, ws); err != nil {
		w.deps.Log.Error(opVerb, "could not republish the workspace's links after its Slack-thread subscription changed",
			dlog.WithCause(dlog.Context{"subscriber": string(ws), "thread": thread.String(), "verb": verb}, err))
		return fmt.Errorf("slackwatch: republish the links of %s after %s: %w", ws, verb, err)
	}
	return nil
}

// SetMode sets the mode of WS's subscription to the thread URL names,
// resetting its post count for any mode but listen, and announces a change.
func (w *Watcher) SetMode(ctx context.Context, ws Workspace, url string, mode wsm.SlackThreadMode) (wsm.SlackThreadSubscription, error) {
	sub, found, err := w.find(ctx, ws.ID, url)
	if err != nil {
		return wsm.SlackThreadSubscription{}, err
	}
	if !found {
		return wsm.SlackThreadSubscription{}, &Refusal{Kind: RefusedNotSubscribed}
	}
	previous, err := w.deps.Store.SetSlackThreadMode(ctx, ws.ID, sub.Thread, mode, mode != wsm.SlackThreadModeListen)
	if err != nil {
		return wsm.SlackThreadSubscription{}, err
	}
	if previous != mode {
		w.announce(ctx, ws, sub, modeChangeAnnouncement(mode))
	}
	w.deps.Log.Info(opVerb, "set the Slack-thread subscription's mode", dlog.Context{"subscriber": string(ws.ID), "thread": sub.Thread.String(), "mode": string(mode), "previous": string(previous)})
	updated, _, err := w.lookup(ctx, keyOf(sub))
	return updated, err
}

// Post posts TEXT into the thread URL names for WS, answering the posted
// message's timestamp and the subscription's posts. Reaching the post limit
// switches the subscription to listen, and says so in the thread.
func (w *Watcher) Post(ctx context.Context, ws Workspace, url, text string, force bool) (string, Posts, error) {
	sub, found, err := w.find(ctx, ws.ID, url)
	if err != nil {
		return "", Posts{}, err
	}
	if !found {
		return "", Posts{}, &Refusal{Kind: RefusedNotSubscribed}
	}
	switch emoji := emojiRefusal(text); emoji {
	case "":
	case "-":
		return "", Posts{}, &Refusal{Kind: RefusedNoEmojiPrefix}
	default:
		return "", Posts{}, &Refusal{Kind: RefusedReservedEmoji, Emoji: emoji}
	}
	if secrets := Secrets(text); len(secrets) > 0 {
		return "", Posts{}, &Refusal{Kind: RefusedSecret, Secrets: secrets}
	}
	posts := PostsOf(sub)
	if !force && sub.Mode == wsm.SlackThreadModeListen {
		return "", Posts{}, &Refusal{Kind: RefusedListenMode}
	}
	if !force && posts.Made >= posts.Limit {
		return "", Posts{}, &Refusal{Kind: RefusedPostLimit, Posts: posts}
	}
	ts, made, err := w.post(ctx, ws, sub, text, true)
	if err != nil {
		return "", Posts{}, err
	}
	posts.Made = made
	w.deps.Log.Info(opVerb, "posted into the Slack thread for the workspace", dlog.Context{"subscriber": string(ws.ID), "thread": sub.Thread.String(), "ts": ts, "posts": made, "forced": force})
	if made >= posts.Limit && sub.Mode != wsm.SlackThreadModeListen {
		if _, err := w.deps.Store.SetSlackThreadMode(ctx, ws.ID, sub.Thread, wsm.SlackThreadModeListen, false); err != nil {
			w.deps.Log.Error(opVerb, "could not switch the subscription to listen at its post limit; its next post is refused by the limit", dlog.WithCause(keyOf(sub).fields(), err))
		} else {
			w.announce(ctx, ws, sub, limitAnnouncement())
		}
	}
	return ts, posts, nil
}

// post reserves a nonce and posts TEXT under it, answering the timestamp and
// the posts made since the count was last reset.
func (w *Watcher) post(ctx context.Context, ws Workspace, sub wsm.SlackThreadSubscription, text string, counted bool) (string, int, error) {
	nonce, err := w.deps.NewNonce()
	if err != nil {
		return "", 0, fmt.Errorf("mint a post's nonce: %w", err)
	}
	if err := w.deps.Store.ReserveSlackThreadPost(ctx, ws.ID, sub.Thread, nonce, w.deps.Clock.Now()); err != nil {
		return "", 0, err
	}
	ts, err := w.deps.GNS.Post(ctx, sub.Thread.Channel, sub.Thread.ThreadTS, postBlocks(ws.Name, nonce, text))
	if err != nil {
		return "", 0, unreachable(err)
	}
	made, err := w.deps.Store.RecordSlackThreadPost(ctx, ws.ID, sub.Thread, nonce, ts, counted)
	return ts, made, err
}

// announce posts one of the daemon's announcements, answering whether it was
// made. A failure is recorded and never fails the verb that announced: the
// subscription's change stands whether or not the thread heard of it.
func (w *Watcher) announce(ctx context.Context, ws Workspace, sub wsm.SlackThreadSubscription, text string) bool {
	if _, _, err := w.post(ctx, ws, sub, text, false); err != nil {
		w.deps.Log.Error(opVerb, "could not announce the subscription's change in the thread; the change stands", dlog.WithCause(keyOf(sub).fields(), err))
		return false
	}
	return true
}

// Message is one message a read answers.
type Message struct {
	TS      string
	Author  string
	Text    string
	OwnPost bool
}

// Read reads up to LIMIT of the newest messages of the thread URL names, for
// WS, marking WS's own posts.
func (w *Watcher) Read(ctx context.Context, ws ids.WorkspaceID, url string, limit int) (wsm.SlackThreadKey, []Message, error) {
	thread, err := parse(url)
	if err != nil {
		return wsm.SlackThreadKey{}, nil, err
	}
	root, err := w.deps.GNS.ThreadRoot(ctx, thread.Channel, thread.TS)
	if err != nil {
		return wsm.SlackThreadKey{}, nil, unreachable(err)
	}
	key := wsm.SlackThreadKey{Channel: thread.Channel, ThreadTS: root}
	msgs, err := w.deps.GNS.Thread(ctx, key.Channel, key.ThreadTS, limit)
	if err != nil {
		return wsm.SlackThreadKey{}, nil, unreachable(err)
	}
	nonces, err := w.deps.Store.SlackThreadPostNonces(ctx, ws, key)
	if err != nil {
		return wsm.SlackThreadKey{}, nil, err
	}
	out := make([]Message, len(msgs))
	for i, m := range msgs {
		out[i] = Message{TS: m.TS, Author: m.Author, Text: m.Text, OwnPost: wsm.CarriesNonce(m.Text, nonces)}
	}
	return key, out, nil
}

// Login is the GNS login as it last stood: the username, or why it could not
// be read; both empty before any read.
type Login struct {
	Username string
	Failure  string
}

// Status answers the login, every subscription and the reaction initiator.
func (w *Watcher) Status(ctx context.Context) (Login, []wsm.SlackThreadSubscription, wsm.SlackReactionSettings, PollState, error) {
	if _, err := w.login(ctx); err != nil && ctx.Err() != nil {
		return Login{}, nil, wsm.SlackReactionSettings{}, PollState{}, ctx.Err()
	}
	w.mu.Lock()
	login := Login{Username: w.username}
	if w.loginErr != nil {
		login.Failure = w.loginErr.Error()
	}
	polls := w.polls.snapshot()
	w.mu.Unlock()
	subs, err := w.deps.Store.SlackThreadSubscriptions(ctx)
	if err != nil {
		return Login{}, nil, wsm.SlackReactionSettings{}, PollState{}, err
	}
	settings, err := w.deps.Store.SlackReactionSettings(ctx)
	if err != nil {
		return Login{}, nil, wsm.SlackReactionSettings{}, PollState{}, err
	}
	if missing := settings.Missing(); len(missing) > 0 {
		polls = PollState{Missing: slices.Clone(missing)}
	}
	return login, subs, settings, polls, nil
}

// SubscribeAtCreation subscribes the workspace a create is making to the
// thread it is created for (workspace.SlackThreadSubscriber).
func (w *Watcher) SubscribeAtCreation(ctx context.Context, ws ids.WorkspaceID, name, url string, mode wsm.SlackThreadMode) error {
	_, _, err := w.Subscribe(ctx, Workspace{ID: ws, Name: name}, url, mode, wsm.SlackThreadOriginCreation)
	return err
}
