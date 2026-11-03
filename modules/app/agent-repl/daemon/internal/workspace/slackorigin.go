package workspace

import (
	"context"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/slackfmt"
	"agent-repld/internal/slackpost"
	"agent-repld/internal/wsm"
)

// THE DAEMON SPEAKS FOR A WORKSPACE IN THE SLACK THREAD IT WAS CREATED FOR
// (agentrepl.v1.CreateWorkspaceSlackOrigin). No agent writes or sends these
// posts: the daemon composes each body and posts it through slackpost, whose
// formatter is the send-to-slack skill's. Three posts and one reaction:
//
//   - THE OPENING, the workspace's task in its own words: its one-sentence
//     summary (internal/titlesynth), posted once that summary first exists.
//     The two edges it needs (the thread recorded, adoptSlackOrigin; a summary
//     installed, SummaryInstalled) each try it, and the store's claim
//     (wsm ClaimSlackOpening) lets exactly one of them post, whichever order
//     they come in. Nothing is posted in the summary's place while it is
//     missing.
//   - EVERY OWNED LINK, as it is stored (SetLink): a pull request by its label
//     and its `#<number>` link, as send-to-slack names one, anything else by
//     its label. A link is claimed before it is posted (wsm
//     ClaimSlackLinkPost), so setting it again, a relabel among them, never
//     posts it twice. The posted message's ts is recorded with it.
//   - THE :merged: REACTION on a posted pull request's message once the pull
//     request merges (PullRequestMerged, from the pull-request watcher). The
//     merge and the ts can arrive in either order, so each write answers
//     whether the other has already landed (wsm RecordSlackLinkPost,
//     MarkSlackLinkMerged), and exactly one of them reacts.
//   - A FAILED DEPLOY, once a finish's deploy leaves the queue failed
//     (MergeEnded), with where to look: the workspace, left where it stopped,
//     and every link it owns. A landing posts nothing more, since its links
//     were posted as they were stored and its pull request is reacted to as
//     it merges.
//
// A POST OR A REACTION THAT FAILS FAILS NOTHING ELSE. The create, the link,
// the merge and the deploy stand whatever Slack answered, so a failure is
// recorded here, its one ownership point, and drawn as a transient footer
// line on the workspace (FaultKindSlackPostFailed); it is never retried.

// opSlackOrigin is the operation every slack-origin record is made under.
const opSlackOrigin = "daemon.workspace.slack_origin"

// FaultKindSlackPostFailed is the transient footer line a failed post raises:
// non-escalating, so the status stands as it was.
const FaultKindSlackPostFailed = "slack_post_failed"

// SlackOriginLabel is the label the originating thread is stored under among
// the workspace's links.
const SlackOriginLabel = "Originating Slack thread"

// MergedReaction is the reaction a posted pull request's message gets once
// the pull request merges.
const MergedReaction = "merged"

// slackPostTimeout bounds one post or reaction. UNMEASURED: no healthy `gns
// slack send` latency has been recorded yet, so every post records its
// elapsed time (`elapsed_ms`) for the bound to be set from (org/todo.md).
const slackPostTimeout = 20 * time.Second

// The failed deploy's post, as Markdown for slackfmt.
const (
	slackFailedLead    = "Work on this stopped: its deploy failed."
	slackWhereToLook   = "Where to look:"
	slackWorkspaceLeft = "workspace, left open where it stopped"
)

// adoptSlackOrigin takes up the Slack thread a newly created workspace was
// created for: it records the thread, stores it among the workspace's links as
// a reference, and posts the opening when the workspace's summary already
// stands. LINK empty is a create no Slack message started, and does nothing.
func (v *verbs) adoptSlackOrigin(ctx context.Context, created wsm.Workspace, link string) {
	if link == "" {
		return
	}
	ctx = context.WithoutCancel(ctx)
	log := v.deps.Log.WorkspaceOrCentral(created.Dir).With(dlog.Context{"workspace": string(created.ID), "thread_url": link})
	if _, err := slackpost.ParseThread(link); err != nil {
		// The request's validation parsed the link, so this is a defect.
		v.slackFailed(log, created.ID, "the originating thread's link does not parse", err)
		return
	}
	if err := v.deps.DB.SetWorkspaceSlackOrigin(ctx, created.ID, link); err != nil {
		v.slackFailed(log, created.ID, "could not record the originating thread", err)
		return
	}
	if _, err := v.SetLink(ctx, created.ID, link, SlackOriginLabel, false); err != nil {
		v.slackFailed(log, created.ID, "could not store the originating thread among the links", err)
		return
	}
	summary, found, err := v.deps.DB.WorkspaceSummaryOf(ctx, created.ID)
	switch {
	case err != nil:
		v.slackFailed(log, created.ID, "could not read the summary the opening post tells the thread", err)
	case !found:
		log.Info(opSlackOrigin, "the opening post waits for the workspace's first summary", nil)
	default:
		v.postOpening(ctx, log, created.ID, summary.Title)
	}
}

// SummaryInstalled implements titlesynth.SummaryObserver: the first summary
// of a workspace created for a Slack thread is posted into it as the opening.
// Every later summary, and every summary of a workspace created for none,
// posts nothing.
func (v *verbs) SummaryInstalled(ctx context.Context, ws ids.WorkspaceID, title string) {
	ctx = context.WithoutCancel(ctx)
	v.postOpening(ctx, v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}), ws, title)
}

// postOpening claims the opening post and, when this call took the claim,
// posts TITLE into the thread as it.
func (v *verbs) postOpening(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, title string) {
	link, claimed, err := v.deps.DB.ClaimSlackOpening(ctx, ws)
	switch {
	case err != nil:
		v.slackFailed(log, ws, "could not claim the opening post", err)
		return
	case !claimed:
		log.Debug(opSlackOrigin, "no opening post is owed: the workspace was created for no thread, or its opening is claimed", nil)
		return
	}
	log = log.With(dlog.Context{"thread_url": link})
	thread, err := slackpost.ParseThread(link)
	if err != nil {
		v.slackFailed(log, ws, "the recorded originating thread's link does not parse", err)
		return
	}
	v.postToSlack(ctx, log, ws, thread, "opening", title+"\n")
}

// postOwnedLink posts the owned link URL, labelled LABEL, into the thread WS
// was created for, once ever, and records the posted message's ts with it. A
// pull request that already merged is reacted to at once.
func (v *verbs) postOwnedLink(ctx context.Context, ws ids.WorkspaceID, url, label string) {
	ctx = context.WithoutCancel(ctx)
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "url": url})
	link, claimed, err := v.deps.DB.ClaimSlackLinkPost(ctx, ws, url)
	switch {
	case err != nil:
		v.slackFailed(log, ws, "could not claim the owned link's post", err)
		return
	case !claimed:
		log.Debug(opSlackOrigin, "the owned link is not posted: the workspace was created for no thread, or the link is posted already", nil)
		return
	}
	log = log.With(dlog.Context{"thread_url": link})
	thread, err := slackpost.ParseThread(link)
	if err != nil {
		v.slackFailed(log, ws, "the recorded originating thread's link does not parse", err)
		return
	}
	posted, ok := v.postToSlack(ctx, log, ws, thread, "link", LinkMarkdown(label, url)+"\n")
	if !ok {
		return
	}
	log = log.With(dlog.Context{"message_ts": posted.TS})
	merged, err := v.deps.DB.RecordSlackLinkPost(ctx, ws, url, posted.TS)
	switch {
	case err != nil:
		v.slackFailed(log, ws, "could not record the owned link's posted message; its merge is not reacted to", err)
	case merged:
		v.reactMerged(ctx, log, ws, thread, posted.TS)
	}
}

// PullRequestMerged implements prwatch.MergeObserver: the message that
// posted PR into the thread WS was created for gets the :merged: reaction.
// A pull request WS never posted is no thread's concern.
func (v *verbs) PullRequestMerged(ctx context.Context, ws ids.WorkspaceID, pr wsm.PullRequestKey) {
	ctx = context.WithoutCancel(ctx)
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "pull_request": pr.String()})
	posts, err := v.deps.DB.WorkspaceSlackLinkPosts(ctx, ws)
	if err != nil {
		v.slackFailed(log, ws, "could not read the posted links of a workspace whose pull request merged", err)
		return
	}
	var thread *slackpost.Thread
	for _, post := range posts {
		if key, ok := prwatch.ParseURL(post.URL); !ok || key != pr {
			continue
		}
		log := log.With(dlog.Context{"url": post.URL})
		ts, err := v.deps.DB.MarkSlackLinkMerged(ctx, ws, post.URL)
		if err != nil {
			v.slackFailed(log, ws, "could not mark the posted pull request merged; its merge is not reacted to", err)
			continue
		}
		if ts == "" {
			log.Info(opSlackOrigin, "the merged pull request's post has no message to react to yet: its post's record reacts, or the post failed", nil)
			continue
		}
		if thread == nil {
			if thread, err = v.originThread(ctx, ws); err != nil {
				v.slackFailed(log, ws, "could not read the originating thread of a merged pull request's post", err)
				return
			}
		}
		v.reactMerged(ctx, log.With(dlog.Context{"thread_url": thread.URL, "message_ts": ts}), ws, *thread, ts)
	}
}

// originThread answers the thread WS was created for. A workspace with a
// posted link was created for one, so none is an error.
func (v *verbs) originThread(ctx context.Context, ws ids.WorkspaceID) (*slackpost.Thread, error) {
	link, found, err := v.deps.DB.WorkspaceSlackOrigin(ctx, ws)
	if err != nil {
		return nil, err
	}
	if !found {
		return nil, fmt.Errorf("workspace %s posted a link into no originating thread", ws)
	}
	thread, err := slackpost.ParseThread(link)
	if err != nil {
		return nil, err
	}
	return &thread, nil
}

// MergeEnded implements merge.EndObserver: a finish's deploy in WS that
// failed is told to the Slack thread WS was created for, with where to look.
// A workspace created for no thread, a landing, and a merge abandoned off the
// queue post nothing.
func (v *verbs) MergeEnded(ctx context.Context, ws ids.WorkspaceID, out merge.DetachedOutcome) {
	ctx = context.WithoutCancel(ctx)
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "outcome": out.Kind.String()})
	link, found, err := v.deps.DB.WorkspaceSlackOrigin(ctx, ws)
	switch {
	case err != nil:
		v.slackFailed(log, ws, "could not read the originating thread of a workspace whose merge ended", err)
		return
	case !found:
		return
	case out.Kind == merge.DetachedAbandoned:
		log.Info(opSlackOrigin, "a merge was taken off the queue before it ended; the originating thread is not told", dlog.Context{"thread_url": link})
		return
	case out.Kind == merge.DetachedLanded:
		log.Info(opSlackOrigin, "the work landed; the originating thread already has every owned link, and a merged pull request's reaction", dlog.Context{"thread_url": link})
		return
	}
	log = log.With(dlog.Context{"thread_url": link})
	thread, err := slackpost.ParseThread(link)
	if err != nil {
		v.slackFailed(log, ws, "the recorded originating thread's link does not parse", err)
		return
	}
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		v.slackFailed(log, ws, "could not read the workspace to tell the originating thread where to look", err)
		return
	}
	links, err := v.deps.DB.WorkspaceLinks(ctx, ws)
	if err != nil {
		v.slackFailed(log, ws, "could not read the links to tell the originating thread", err)
		return
	}
	v.postToSlack(ctx, log, ws, thread, "failed", FailedBody(record.Name, links))
}

// FailedBody composes the failed deploy's post: that the work stopped, and
// where to look: the workspace NAME, and every owned link as a bullet, most
// recently set first.
func FailedBody(name string, links []wsm.WorkspaceLink) string {
	where := []string{fmt.Sprintf("- The `%s` %s", name, slackWorkspaceLeft)}
	for _, link := range links {
		if link.Owned {
			where = append(where, "- "+LinkMarkdown(link.Label, link.URL))
		}
	}
	return slackFailedLead + " " + slackWhereToLook + "\n\n" + strings.Join(where, "\n") + "\n"
}

// LinkMarkdown names a link as send-to-slack does: a pull request by LABEL
// followed by its `#<number>` link, anything else as LABEL linked.
func LinkMarkdown(label, url string) string {
	if number, ok := slackfmt.PullNumber(url); ok {
		return fmt.Sprintf("%s [#%s](%s)", label, number, url)
	}
	return fmt.Sprintf("[%s](%s)", label, url)
}

// postToSlack posts BODY into THREAD and records how it went, answering the
// posted message and whether it was posted.
func (v *verbs) postToSlack(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, thread slackpost.Thread, post, body string) (slackpost.Posted, bool) {
	var posted slackpost.Posted
	elapsed, err := v.timedSlack(ctx, func(ctx context.Context) (err error) {
		posted, err = v.deps.Slack.Reply(ctx, thread, body)
		return err
	})
	if err != nil {
		v.slackFailed(log.With(dlog.Context{"post": post, "elapsed_ms": elapsed}), ws, "the originating thread was not posted to", err)
		return slackpost.Posted{}, false
	}
	log.Info(opSlackOrigin, "posted to the originating thread", dlog.Context{"post": post, "message_ts": posted.TS, "permalink": posted.Permalink, "elapsed_ms": elapsed})
	return posted, true
}

// reactMerged adds the :merged: reaction to the message TS in THREAD and
// records how it went.
func (v *verbs) reactMerged(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, thread slackpost.Thread, ts string) {
	elapsed, err := v.timedSlack(ctx, func(ctx context.Context) error {
		return v.deps.Slack.React(ctx, thread.Channel, ts, MergedReaction)
	})
	fields := dlog.Context{"reaction": MergedReaction, "message_ts": ts, "elapsed_ms": elapsed}
	if err != nil {
		v.slackFailed(log.With(fields), ws, "the merged pull request's post was not reacted to", err)
		return
	}
	log.Info(opSlackOrigin, "reacted to the merged pull request's post", fields)
}

// timedSlack makes one Slack CALL, bounded by slackPostTimeout, answering
// its elapsed milliseconds with its error: every post and reaction is made
// through it, so each is bounded and timed alike.
func (v *verbs) timedSlack(ctx context.Context, call func(context.Context) error) (int64, error) {
	ctx, cancel := context.WithTimeout(ctx, slackPostTimeout)
	defer cancel()
	began := v.now()
	err := call(ctx)
	return v.now().Sub(began).Milliseconds(), err
}

// slackFailed records a slack-origin failure once and draws it on the
// workspace's footer as a transient line.
func (v *verbs) slackFailed(log dlog.Logger, ws ids.WorkspaceID, what string, err error) {
	log.Error(opSlackOrigin, what, dlog.Context{"cause": err.Error()})
	v.deps.Footer.OpenFault(ws, footer.Fault{
		ID:     FaultKindSlackPostFailed + ":" + string(ws),
		Kind:   FaultKindSlackPostFailed,
		Detail: what,
		At:     v.now(),
	})
}
