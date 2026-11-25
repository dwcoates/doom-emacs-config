// Package slackchannels holds the Slack channels the user is a member of on
// their work Slack, which the Emacs composer completes after a `#`
// (agentrepl.v1.ListSlackChannels) and the feed links a `#<channel-name>` of
// a workspace's text to (ChannelIDs, slackrefs).
//
// THE LIST IS THE USER'S. One Directory serves every composer of the daemon:
// channel membership belongs to the person, never to a workspace.
//
// IT IS READ ON DEMAND, ONE READ AT A TIME. The first ask starts a read, and
// so does the first ask once the list is MaxAge old; an ask never waits on
// one, because a read takes seconds. Reads never start at boot, so a daemon
// no composer or feed asks never reaches Slack. The Directory alone decides whether a
// read starts, under its own lock, and marks it in flight before releasing
// it, so two reads can never run at once however many composers ask.
//
// A FAILED READ IS LOGGED HERE, ONCE. A failed refresh leaves the earlier list
// standing, since it is still the best completion source; a failed read with
// no earlier list is answered as the error arm until a read succeeds.
package slackchannels

import (
	"context"
	"fmt"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gnsclient"
	"agent-repld/internal/inflight"
)

// opRead is the operation of every record about a read of the channels.
const opRead = "daemon.slackchannels.read"

// MaxAge is how old a list may grow before the next ask reads it again.
// Channel membership changes on the scale of days, so an hour keeps a newly
// joined channel offered the same working session while costing one read an
// hour at most.
const MaxAge = time.Hour

// ReadBound bounds one read: gns's own bound on the channel list
// (gnsclient.ChannelsBound), where it is measured.
const ReadBound = gnsclient.ChannelsBound

// Reader reads every channel the user is a member of, once. It honors ctx,
// which carries ReadBound.
type Reader func(ctx context.Context) ([]*agentreplv1.SlackChannel, error)

// Deps is what a Directory reads with.
type Deps struct {
	// Read is one read of the channels (GNSReader in production).
	Read Reader
	// Now is the clock the list's age is measured on.
	Now func() time.Time
	// Life bounds every read beyond ReadBound: a read outlives the ask that
	// started it, never the daemon.
	Life context.Context
	// Log records every read's outcome.
	Log dlog.Logger
}

// Directory is the daemon's one list of the user's channels. Build it with
// New.
type Directory struct {
	deps Deps

	mu sync.Mutex
	// channels is the last successful read's answer; listed says whether
	// any read has succeeded (an empty membership is still a list).
	channels []*agentreplv1.SlackChannel
	listed   bool
	// ids is each channel's id keyed by its name, rebuilt by every
	// successful read and never written after: ChannelIDs hands it out.
	ids map[string]string
	// attemptedAt is when the last read ENDED, successful or not, which is
	// what the list's age is measured from: a failed refresh waits MaxAge
	// like a successful one, so a broken gns is not run at every ask.
	attemptedAt time.Time
	// failure is the last read's failure, nil once a read succeeds.
	failure error
	// reading is true while a read is in flight.
	reading bool
}

// New builds an empty Directory. Every Deps field is required.
func New(deps Deps) (*Directory, error) {
	switch {
	case deps.Read == nil:
		return nil, fmt.Errorf("slackchannels: a reader is required")
	case deps.Now == nil:
		return nil, fmt.Errorf("slackchannels: a clock is required")
	case deps.Life == nil:
		return nil, fmt.Errorf("slackchannels: a lifetime context is required")
	case deps.Log == nil:
		return nil, fmt.Errorf("slackchannels: a logger is required")
	}
	return &Directory{deps: deps}, nil
}

// Response answers what the Directory knows of the channels, as
// ListSlackChannels answers it, and starts a read when none is in flight and
// the list is missing, failed with nothing to fall back on, or MaxAge old.
func (d *Directory) Response() *agentreplv1.ListSlackChannelsResponse {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.startDueLocked()
	switch {
	case d.listed:
		return &agentreplv1.ListSlackChannelsResponse{Result: &agentreplv1.ListSlackChannelsResponse_Success{
			Success: &agentreplv1.ListSlackChannelsSuccess{Listing: &agentreplv1.ListSlackChannelsSuccess_Listed{
				Listed: &agentreplv1.ListSlackChannelsListed{Channels: d.channels},
			}},
		}}
	case d.failure != nil:
		return &agentreplv1.ListSlackChannelsResponse{Result: &agentreplv1.ListSlackChannelsResponse_Error{
			Error: &agentreplv1.ListSlackChannelsError{Cause: &agentreplv1.ListSlackChannelsError_ReadFailed{
				ReadFailed: &agentreplv1.ListSlackChannelsReadFailed{Detail: d.failure.Error()},
			}},
		}}
	default:
		return &agentreplv1.ListSlackChannelsResponse{Result: &agentreplv1.ListSlackChannelsResponse_Success{
			Success: &agentreplv1.ListSlackChannelsSuccess{Listing: &agentreplv1.ListSlackChannelsSuccess_Pending{
				Pending: &agentreplv1.ListSlackChannelsPending{},
			}},
		}}
	}
}

// ChannelIDs answers each channel's id keyed by its name, from the last
// successful read, nil while none has succeeded. The map is never written
// once answered. Like Response it starts a read when one is due, and never
// waits on it.
func (d *Directory) ChannelIDs() map[string]string {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.startDueLocked()
	return d.ids
}

// startDueLocked starts a read when one is due (dueLocked). d.mu is held.
func (d *Directory) startDueLocked() {
	if d.dueLocked() {
		d.reading = true
		inflight.Go(d.read)
	}
}

// dueLocked reports whether an ask should start a read. d.mu is held.
func (d *Directory) dueLocked() bool {
	switch {
	case d.reading:
		return false
	case d.listed:
		return d.deps.Now().Sub(d.attemptedAt) >= MaxAge
	default:
		// Never read, or every read so far failed: the ask that finds no
		// list starts another read.
		return true
	}
}

// read runs one read and installs its outcome. It is started only by
// Response, with d.reading already set.
func (d *Directory) read() {
	ctx, cancel := context.WithTimeout(d.deps.Life, ReadBound)
	defer cancel()
	started := d.deps.Now()
	channels, err := d.deps.Read(ctx)
	ended := d.deps.Now()

	d.mu.Lock()
	defer d.mu.Unlock()
	d.reading = false
	d.attemptedAt = ended
	took := ended.Sub(started).Round(time.Millisecond).String()
	if err != nil {
		d.failure = err
		d.deps.Log.Error(opRead, "the Slack channels could not be read", dlog.Context{
			"error":      err.Error(),
			"took":       took,
			"bound":      ReadBound.String(),
			"had_list":   d.listed,
			"kept_count": len(d.channels),
		})
		return
	}
	d.channels = channels
	d.ids = idsByName(channels)
	d.listed = true
	d.failure = nil
	d.deps.Log.Info(opRead, "read the Slack channels", dlog.Context{
		"count": len(channels),
		"took":  took,
	})
}

// idsByName answers each of CHANNELS' ids keyed by its name. A name Slack
// answers twice (a channel shared in from another organization) keeps its
// first channel, in Slack's order.
func idsByName(channels []*agentreplv1.SlackChannel) map[string]string {
	ids := make(map[string]string, len(channels))
	for _, c := range channels {
		if _, seen := ids[c.GetName()]; !seen {
			ids[c.GetName()] = c.GetId()
		}
	}
	return ids
}
