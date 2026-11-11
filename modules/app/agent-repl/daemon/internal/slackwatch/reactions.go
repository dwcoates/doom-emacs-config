package slackwatch

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// THE REACTION INITIATOR (agentrepl.v1 slack_reactions.proto): an emoji the
// watch user types as a `:shortcode:` in their own message, or adds as a
// reaction onto any message, creates the workspace its binding names. It
// polls Slack's API through `gns`, because reactions do not travel on a
// channel's echo pipe, and only while its settings are complete.

// The poll's cadence and windows, the gns-sockets plugin's defaults. They are
// how far behind a trigger may be noticed, not bounds on an operation.
const (
	// PollEvery is the gap between polls.
	PollEvery = 3 * time.Minute
	// Lookback is how far back a poll's search reads the watch user's
	// messages; it overlaps the previous poll, and the fired ledger drops a
	// message read twice.
	Lookback = 10 * time.Minute
	// StartupLookback is the first poll's, so a restart sweeps up triggers
	// typed while the daemon was down.
	StartupLookback = 8 * time.Hour
	// searchPageCap bounds a poll's search to this many pages of 100, so a
	// busy history cannot spin one poll forever.
	searchPageCap = 20
)

// The briefs a fired binding's intake is composed from (prompts/).
const (
	BriefIntake           = "slack-reaction-intake"
	BriefDirectives       = "slack-reaction-directives"
	BriefSubscribedStep   = "slack-reaction-subscribed"
	BriefPullAnnounced    = "slack-reaction-pr-announced"
	BriefPullOffered      = "slack-reaction-pr-offered"
	BriefOneShotDirective = "slack-reaction-one-shot"
)

// IntakeRequest is one fired binding's intake workspace: a hidden peer of
// the settings' repository, with a fresh context, whose one turn names the
// task and forks itself into the workspace that does it.
type IntakeRequest struct {
	// RepoID is the repository it is created in.
	RepoID string
	// Name is its name, unique per trigger.
	Name string
	// Prompt is its first prompt.
	Prompt string
}

// IntakeCreator creates a fired binding's intake workspace.
type IntakeCreator interface {
	CreateSlackIntake(ctx context.Context, req IntakeRequest) error
}

// PollState is the reaction initiator's latest poll, as the status answers
// it. Exactly one of its shapes holds: Missing set (inactive); Polled false
// (not polled yet); Failure set (the latest poll failed); else the latest
// poll succeeded, firing Fired.
type PollState struct {
	Missing []string
	Polled  bool
	At      time.Time
	Fired   int
	Failure string
}

func (p PollState) snapshot() PollState {
	p.Missing = append([]string(nil), p.Missing...)
	return p
}

// DispatchID is a trigger's identity in the fired ledger: the first 16 hex
// digits of sha256(channel:ts:emoji), the gns-sockets plugin's, so one
// (message, emoji) fires once whether it was typed, reacted, or both.
func DispatchID(channel, ts, emoji string) string {
	sum := sha256.Sum256([]byte(channel + ":" + ts + ":" + emoji))
	return hex.EncodeToString(sum[:])[:16]
}

// runReactions polls on the cadence until ctx ends: the first poll at once
// with the startup lookback.
func (w *Watcher) runReactions(ctx context.Context) {
	lookback := StartupLookback
	for {
		if w.gate.Admits() {
			if polled := w.PollReactions(ctx, lookback); polled {
				lookback = Lookback
			}
		}
		select {
		case <-ctx.Done():
			return
		case <-w.deps.Clock.After(PollEvery):
		}
	}
}

// PollReactions runs one poll reading LOOKBACK of the watch user's messages,
// and answers whether the settings were complete enough to poll.
func (w *Watcher) PollReactions(ctx context.Context, lookback time.Duration) bool {
	settings, err := w.deps.Store.SlackReactionSettings(ctx)
	if err != nil {
		if ctx.Err() == nil {
			w.deps.Log.Error(opReactions, "could not read the reaction settings; nothing is polled this time", dlog.WithCause(nil, err))
		}
		return false
	}
	if missing := settings.Missing(); len(missing) > 0 {
		w.setPolls(PollState{Missing: missing})
		return false
	}
	fired := 0
	searchErr := w.pollSearch(ctx, settings, lookback, &fired)
	reactionsErr := w.pollReacted(ctx, settings, &fired)
	state := PollState{Polled: true, At: w.deps.Clock.Now(), Fired: fired}
	if err := errors.Join(searchErr, reactionsErr); err != nil {
		state.Failure = err.Error()
		w.deps.Log.Info(opReactions, "a reaction poll failed in part; the next poll reads again", dlog.WithCause(dlog.Context{"fired": fired}, err))
	} else {
		w.deps.Log.Debug(opReactions, "polled the watch user's messages and reactions", dlog.Context{"fired": fired})
	}
	w.setPolls(state)
	return true
}

func (w *Watcher) setPolls(p PollState) {
	w.mu.Lock()
	w.polls = p
	w.mu.Unlock()
}

// pollSearch fires every bound shortcode typed in the watch user's messages
// read within LOOKBACK.
func (w *Watcher) pollSearch(ctx context.Context, settings wsm.SlackReactionSettings, lookback time.Duration, fired *int) error {
	name, err := w.deps.GNS.SearchName(ctx, settings.WatchUser)
	if err != nil {
		return fmt.Errorf("read the watch user's name: %w", err)
	}
	oldest := w.deps.Clock.Now().Add(-lookback)
	for page := 1; page <= searchPageCap; page++ {
		res, err := w.deps.GNS.Search(ctx, "from:@"+name, page)
		if err != nil {
			return fmt.Errorf("search the watch user's messages: %w", err)
		}
		pastWindow := false
		for _, m := range res.Messages {
			at, ok := slackTime(m.TS)
			if !ok || at.Before(oldest) {
				pastWindow = pastWindow || ok
				continue
			}
			for _, b := range settings.Bindings {
				if strings.Contains(m.Text, ":"+b.Emoji+":") {
					w.fire(ctx, settings, b, m.Channel, m.TS, m.Permalink, fired)
				}
			}
		}
		if len(res.Messages) == 0 || pastWindow || page >= res.Pages {
			return nil
		}
	}
	return nil
}

// pollReacted fires every bound emoji the watch user reacted with, after the
// baseline; the first read records the standing ones without firing.
func (w *Watcher) pollReacted(ctx context.Context, settings wsm.SlackReactionSettings, fired *int) error {
	reacted, err := w.deps.GNS.ListReacted(ctx, settings.WatchUser)
	if err != nil {
		return fmt.Errorf("list the watch user's reactions: %w", err)
	}
	if settings.BaselinedAt == nil {
		var standing []wsm.SlackReactionTrigger
		for _, m := range reacted {
			for _, emoji := range m.Emojis {
				if _, bound := settings.Binding(emoji); bound {
					standing = append(standing, wsm.SlackReactionTrigger{DispatchID: DispatchID(m.Channel, m.TS, emoji), Channel: m.Channel, TS: m.TS, Emoji: emoji})
				}
			}
		}
		if err := w.deps.Store.RecordSlackReactionBaseline(ctx, standing, w.deps.Clock.Now()); err != nil {
			return fmt.Errorf("record the reaction baseline: %w", err)
		}
		w.deps.Log.Info(opReactions, "recorded the standing reactions as the baseline; only reactions added from now on fire", dlog.Context{"standing": len(standing)})
		return nil
	}
	for _, m := range reacted {
		for _, emoji := range m.Emojis {
			if b, bound := settings.Binding(emoji); bound {
				w.fire(ctx, settings, b, m.Channel, m.TS, m.Permalink, fired)
			}
		}
	}
	return nil
}

// slackTime reads a Slack timestamp (`seconds.micros`).
func slackTime(ts string) (time.Time, bool) {
	secs, micros, _ := strings.Cut(ts, ".")
	s, err := strconv.ParseInt(secs, 10, 64)
	if err != nil || s <= 0 {
		return time.Time{}, false
	}
	us, _ := strconv.ParseInt((micros + "000000")[:6], 10, 64)
	return time.Unix(s, us*1000), true
}

// fire creates B's intake for the message at TS in CHANNEL, once ever: the
// trigger is recorded fired before the intake is asked for, so a failure
// after the mark is never fired twice, and is recorded.
func (w *Watcher) fire(ctx context.Context, settings wsm.SlackReactionSettings, b wsm.SlackReactionBinding, channel, ts, link string, fired *int) {
	trigger := wsm.SlackReactionTrigger{DispatchID: DispatchID(channel, ts, b.Emoji), Channel: channel, TS: ts, Emoji: b.Emoji}
	fields := dlog.Context{"dispatch_id": trigger.DispatchID, "emoji": b.Emoji, "channel": channel, "ts": ts, "link": link}
	if link == "" {
		w.deps.Log.Error(opReactions, "Slack named no link for a triggering message; it is not fired", fields)
		return
	}
	fresh, err := w.deps.Store.MarkSlackReactionFired(ctx, trigger, w.deps.Clock.Now())
	if err != nil {
		w.deps.Log.Error(opReactions, "could not record a trigger as fired; it is not fired, and the next poll reads it again", dlog.WithCause(fields, err))
		return
	}
	if !fresh {
		return
	}
	prompt, err := w.intakePrompt(b, channel, ts, link)
	if err != nil {
		w.deps.Log.Error(opReactions, "could not compose a fired binding's intake prompt; the trigger is recorded fired and creates nothing", dlog.WithCause(fields, err))
		return
	}
	req := IntakeRequest{RepoID: settings.RepoID, Name: "slack-intake-" + b.Emoji + "-" + strings.ReplaceAll(ts, ".", "-"), Prompt: prompt}
	if err := w.deps.Intakes.CreateSlackIntake(ctx, req); err != nil {
		w.deps.Log.Error(opReactions, "could not create a fired binding's intake workspace; the trigger is recorded fired and creates nothing", dlog.WithCause(dlog.Merge(fields, dlog.Context{"intake": req.Name}), err))
		return
	}
	*fired++
	w.deps.Log.Info(opReactions, "a bound emoji fired; its intake workspace was created", dlog.Merge(fields, dlog.Context{"intake": req.Name}))
}

// ForkCommand is the create an intake runs to fork itself into B's
// workspace for the message LINK, with only <NAME> and <PRESCRIPTION> left
// for the intake to fill.
func ForkCommand(b wsm.SlackReactionBinding, link string) string {
	cmd := "agent-repld workspace create -repository here -relationship peer -lands-into main -origin-master -context fork -fork here -name <NAME> -prompt <PRESCRIPTION>"
	if b.Priority != nil {
		cmd += " -priority " + PriorityFlag(*b.Priority)
	}
	switch b.Thread {
	case wsm.SlackReactionAnnounced:
		cmd += " -slack-thread " + link + " -originator emoji-ack"
	case wsm.SlackReactionSubscribed:
		cmd += " -slack-thread " + link + " -slack-subscribe " + string(b.Mode)
	default:
		panic(fmt.Sprintf("slackwatch: a binding of %q relates to its thread as %q", b.Emoji, b.Thread))
	}
	if b.OneShot {
		cmd += " -one-shot"
	}
	return cmd
}

// PriorityFlag spells P as `agent-repld`'s -priority flag does.
func PriorityFlag(p wsm.Priority) string {
	switch p {
	case wsm.PriorityP05:
		return "p05"
	case wsm.PriorityP1:
		return "p1"
	case wsm.PriorityP2:
		return "p2"
	case wsm.PriorityP3:
		return "p3"
	}
	panic(fmt.Sprintf("slackwatch: no priority %d", int(p)))
}

// intakePrompt composes B's intake prompt for the message at TS in CHANNEL.
// Each brief is handed exactly the values its placeholders name.
func (w *Watcher) intakePrompt(b wsm.SlackReactionBinding, channel, ts, link string) (string, error) {
	subscribed := ""
	if b.Thread == wsm.SlackReactionSubscribed {
		var err error
		if subscribed, err = w.deps.Briefs(BriefSubscribedStep, map[string]string{"link": link, "mode": string(b.Mode)}); err != nil {
			return "", err
		}
	}
	pullBrief := BriefPullOffered
	if b.Thread == wsm.SlackReactionAnnounced {
		pullBrief = BriefPullAnnounced
	}
	pulls, err := w.deps.Briefs(pullBrief, map[string]string{"link": link})
	if err != nil {
		return "", err
	}
	oneShot := ""
	if b.OneShot {
		if oneShot, err = w.deps.Briefs(BriefOneShotDirective, map[string]string{"emoji": b.Emoji}); err != nil {
			return "", err
		}
	}
	directives, err := w.deps.Briefs(BriefDirectives, map[string]string{"link": link, "channel": channel, "ts": ts, "emoji": b.Emoji,
		"subscribed": subscribed, "pull_requests": pulls, "one_shot": oneShot})
	if err != nil {
		return "", err
	}
	return w.deps.Briefs(BriefIntake, map[string]string{"link": link, "channel": channel, "ts": ts, "emoji": b.Emoji,
		"fork_command": ForkCommand(b, link), "directives": directives})
}
