package workspace

import (
	"context"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/slackfmt"
	"agent-repld/internal/slackpost"
	"agent-repld/internal/wsm"
)

// THE DAEMON SPEAKS FOR A WORKSPACE IN THE SLACK THREAD IT WAS CREATED FOR
// (agentrepl.v1.CreateWorkspaceSlackOrigin). No agent writes or sends these
// posts: the daemon composes each body and posts it through slackpost, whose
// formatter is the send-to-slack skill's. Two posts, and only two:
//
//   - THE BEGINNING, once the workspace is created (adoptSlackOrigin): the
//     thread is recorded, stored among the workspace's links as a reference,
//     and told work began.
//   - THE END, once a finish's deploy leaves the queue for good (MergeEnded):
//     the thread is told whether the work landed or failed, with every link
//     the workspace OWNS as a bullet. The deploy is what opens a pull request
//     in a repository that lands through one, so the end post waits for it
//     rather than going out when the finish verb is called.
//
// A POST THAT FAILS FAILS NOTHING ELSE. The create succeeded and the deploy
// ended whatever Slack answered, so a failure is recorded here, its one
// ownership point, and drawn as a transient footer line on the workspace
// (FaultKindSlackPostFailed); it is never retried.

// opSlackOrigin is the operation every slack-origin record is made under.
const opSlackOrigin = "daemon.workspace.slack_origin"

// FaultKindSlackPostFailed is the transient footer line a failed post raises:
// non-escalating, so the status stands as it was.
const FaultKindSlackPostFailed = "slack_post_failed"

// SlackOriginLabel is the label the originating thread is stored under among
// the workspace's links.
const SlackOriginLabel = "Originating Slack thread"

// slackPostTimeout bounds one post. UNMEASURED: no healthy `gns slack send`
// latency has been recorded yet, so every post records its elapsed time
// (`elapsed_ms`) for the bound to be set from (org/todo.md).
const slackPostTimeout = 20 * time.Second

// The post bodies, as Markdown for slackfmt.
const (
	slackBegunBody    = "Work on this has begun."
	slackLandedLead   = "Work on this is done."
	slackFailedLead   = "Work on this stopped: its deploy failed."
	slackOwnedHeading = "What it produced:"
)

// adoptSlackOrigin takes up the Slack thread a newly created workspace was
// created for: it records the thread, stores it among the workspace's links as
// a reference, and posts that work began. LINK empty is a create no Slack
// message started, and does nothing.
func (v *verbs) adoptSlackOrigin(ctx context.Context, created wsm.Workspace, link string) {
	if link == "" {
		return
	}
	ctx = context.WithoutCancel(ctx)
	log := v.deps.Log.WorkspaceOrCentral(created.Dir).With(dlog.Context{"workspace": string(created.ID), "thread_url": link})
	thread, err := slackpost.ParseThread(link)
	if err != nil {
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
	v.postToSlack(ctx, log, created.ID, thread, "begun", slackBegunBody)
}

// MergeEnded implements merge.EndObserver: a merge run in WS that landed or
// failed is told to the Slack thread WS was created for, with every link WS
// owns. A workspace created for no thread, and a merge abandoned off the
// queue, post nothing.
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
	}
	log = log.With(dlog.Context{"thread_url": link})
	thread, err := slackpost.ParseThread(link)
	if err != nil {
		v.slackFailed(log, ws, "the recorded originating thread's link does not parse", err)
		return
	}
	links, err := v.deps.DB.WorkspaceLinks(ctx, ws)
	if err != nil {
		v.slackFailed(log, ws, "could not read the links to tell the originating thread", err)
		return
	}
	v.postToSlack(ctx, log, ws, thread, "ended", EndBody(out, links))
}

// EndBody composes the end post: whether the work landed or failed, and every
// owned link as a bullet, most recently set first. A pull request is named by
// its label followed by its `#<number>` link, as send-to-slack names one.
func EndBody(out merge.DetachedOutcome, links []wsm.WorkspaceLink) string {
	var body strings.Builder
	if out.Kind == merge.DetachedFailed {
		body.WriteString(slackFailedLead)
	} else {
		body.WriteString(slackLandedLead)
	}
	var owned []string
	for _, link := range links {
		if !link.Owned {
			continue
		}
		if number, ok := slackfmt.PullNumber(link.URL); ok {
			owned = append(owned, fmt.Sprintf("- %s [#%s](%s)", link.Label, number, link.URL))
		} else {
			owned = append(owned, fmt.Sprintf("- [%s](%s)", link.Label, link.URL))
		}
	}
	if len(owned) > 0 {
		body.WriteString(" " + slackOwnedHeading + "\n\n" + strings.Join(owned, "\n"))
	}
	return body.String() + "\n"
}

// postToSlack posts BODY into THREAD and records how it went.
func (v *verbs) postToSlack(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, thread slackpost.Thread, post, body string) {
	ctx, cancel := context.WithTimeout(ctx, slackPostTimeout)
	defer cancel()
	began := v.now()
	permalink, err := v.deps.Slack.Reply(ctx, thread, body)
	elapsed := v.now().Sub(began).Milliseconds()
	if err != nil {
		v.slackFailed(log.With(dlog.Context{"post": post, "elapsed_ms": elapsed}), ws, "the originating thread was not posted to", err)
		return
	}
	log.Info(opSlackOrigin, "posted to the originating thread", dlog.Context{"post": post, "permalink": permalink, "elapsed_ms": elapsed})
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
