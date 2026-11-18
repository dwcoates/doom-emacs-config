package workspace

import (
	"context"
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/ids"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/textline"
	"agent-repld/internal/wsm"
)

// rpcChangeWorkspaceLinks is the rpc every links verb answers for.
const rpcChangeWorkspaceLinks = "ChangeWorkspaceLinks"

// The bounds of a link's label, in words.
const (
	minLinkLabelWords = 2
	maxLinkLabelWords = 8
)

// PullRequestFollower answers the subscription an owned link takes, which
// the pull-request watcher's first read anchors
// (prwatch.Watcher.FollowFromFirstRead): storing a link never waits on
// GitHub.
type PullRequestFollower interface {
	FollowFromFirstRead(subscriber ids.WorkspaceID, pr wsm.PullRequestKey, events wsm.PullRequestEvents, origin wsm.PullRequestOrigin) wsm.PullRequestSubscription
}

// SetLink stores URL under LABEL on the workspace, owned or a reference, or
// relabels it and sets its ownership anew when the workspace already holds it
// (agentrepl.v1.ChangeWorkspaceLinksSet), and republishes the topbar title
// that opens the shown links. An OWNED GitHub pull request the workspace
// holds no subscription to is subscribed in the same write
// (wsm.LinkSet.Subscribe), unread: the watcher's first read of it anchors the
// subscription, so the verb answers at once whether GitHub is up or not. A url the
// external browser would refuse to open is refused invalid_link, and a label
// that is not two to eight words in one sentence invalid_label, before
// anything is written. An owned link may name ANNOUNCES, a link the workspace
// already holds that it announces (agentrepl.v1.WorkspaceLinkOwned.announces);
// an announced url it does not hold is refused unknown_announced_link. An
// owned GNS subpage of a GNS page the workspace already owns is refused
// subpage_of_owned_link (wsm.SubpageOfOwnedLinkError), nothing stored. It
// answers every link the workspace then holds.
func (v *verbs) SetLink(ctx context.Context, ws ids.WorkspaceID, url, label string, owned bool, announces string) ([]wsm.WorkspaceLink, error) {
	fields := dlog.Context{"url": url, "label": label, "owned": owned, "announces": announces}
	return v.changeLinks(ctx, ws, fields, "set a link", func(log dlog.Logger) ([]wsm.WorkspaceLink, error) {
		if err := externalbrowser.Validate(url); err != nil {
			return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmInvalidLink,
				fmt.Sprintf("%q is not a link the external browser opens: %v", url, err), false,
				map[string]any{"url": url, "reason": err.Error()})
		}
		if err := validLinkLabel(label); err != nil {
			return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmInvalidLabel,
				fmt.Sprintf("%q is not a label the menu draws: %v", label, err), false,
				map[string]any{"label": label, "reason": err.Error()})
		}
		set := wsm.LinkSet{URL: url, Label: label, Owned: owned, Announces: announces}
		if pr, ok := prwatch.ParseURL(url); ok {
			set.PullRequest = &pr
			fields["pull_request"] = pr.String()
			if owned {
				sub, err := v.ownedSubscription(ctx, log, ws, pr, fields)
				if err != nil {
					return nil, err
				}
				set.Subscribe = sub
			}
		}
		return v.deps.DB.SetWorkspaceLink(ctx, ws, set)
	})
}

// ownedSubscription answers the subscription an owned link to PR takes: nil
// when the workspace already holds one, so storing the link again reads
// nothing.
func (v *verbs) ownedSubscription(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, pr wsm.PullRequestKey, fields dlog.Context) (*wsm.PullRequestSubscription, error) {
	held, err := v.deps.DB.WorkspacePullRequestSubscriptions(ctx, ws)
	if err != nil {
		return nil, err
	}
	for _, sub := range held {
		if sub.PullRequest == pr {
			return nil, nil
		}
	}
	sub := v.deps.PullRequests.FollowFromFirstRead(ws, pr, wsm.AllPullRequestEvents(), wsm.PullRequestOriginOwnedLink)
	log.Debug(opChangeLinks, "the owned pull request is followed from the watcher's first read of it", fields)
	return &sub, nil
}

// RemoveLink removes one stored link (agentrepl.v1.ChangeWorkspaceLinksRemove);
// a url the workspace does not hold is refused unknown_link.
func (v *verbs) RemoveLink(ctx context.Context, ws ids.WorkspaceID, url string) ([]wsm.WorkspaceLink, error) {
	return v.changeLinks(ctx, ws, dlog.Context{"url": url}, "removed a link", func(dlog.Logger) ([]wsm.WorkspaceLink, error) {
		return v.deps.DB.RemoveWorkspaceLink(ctx, ws, url)
	})
}

// ShowLinks names exactly the links the summary's menu shows
// (agentrepl.v1.ChangeWorkspaceLinksShow). More than the menu shows is refused
// too_many_shown, a url the workspace does not hold unknown_link, and a show
// leaving an owned link out while it shows a reference owned_link_unshown
// (wsm.OwnedLinkUnshownError), with nothing changed.
func (v *verbs) ShowLinks(ctx context.Context, ws ids.WorkspaceID, urls []string) ([]wsm.WorkspaceLink, error) {
	return v.changeLinks(ctx, ws, dlog.Context{"urls": urls}, "chose the shown links", func(log dlog.Logger) ([]wsm.WorkspaceLink, error) {
		if len(urls) > wsm.ShownLinkLimit {
			return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmTooManyShown,
				fmt.Sprintf("a show names at most %d links, not %d", wsm.ShownLinkLimit, len(urls)), false,
				map[string]any{"named": uint32(len(urls)), "limit": uint32(wsm.ShownLinkLimit)})
		}
		return v.deps.DB.ShowWorkspaceLinks(ctx, ws, urls)
	})
}

// ClearLinks removes every stored link (agentrepl.v1.ChangeWorkspaceLinksClear).
func (v *verbs) ClearLinks(ctx context.Context, ws ids.WorkspaceID) ([]wsm.WorkspaceLink, error) {
	return v.changeLinks(ctx, ws, dlog.Context{}, "cleared the links", func(dlog.Logger) ([]wsm.WorkspaceLink, error) {
		return nil, v.deps.DB.ClearWorkspaceLinks(ctx, ws)
	})
}

// changeLinks is every links verb's one sequence: resolve the served
// workspace, run CHANGE (which refuses or writes), turn the store's unknown
// link into the rpc's arm, republish the topbar with the shown links, and
// record the outcome once.
func (v *verbs) changeLinks(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context, done string,
	change func(log dlog.Logger) ([]wsm.WorkspaceLink, error)) ([]wsm.WorkspaceLink, error) {
	_, log, err := v.owned(ctx, rpcChangeWorkspaceLinks, ws)
	if err != nil {
		return nil, err
	}
	links, err := change(log)
	var unknown *wsm.UnknownLinkError
	var unannounced *wsm.UnknownAnnouncedLinkError
	var ownedElsewhere *wsm.PullRequestOwnedElsewhereError
	var subpage *wsm.SubpageOfOwnedLinkError
	var ownedUnshown *wsm.OwnedLinkUnshownError
	_, refused := AsRefusal(err)
	switch {
	case errors.As(err, &ownedElsewhere):
		return nil, v.refuseOwnedElsewhere(ctx, log, fields, ownedElsewhere)
	case errors.As(err, &unknown):
		return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmUnknownLink,
			fmt.Sprintf("the workspace holds no link %q", unknown.URL), false, map[string]any{"url": unknown.URL})
	case errors.As(err, &unannounced):
		return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmUnknownAnnouncedLink,
			fmt.Sprintf("the workspace holds no link %q for this one to announce", unannounced.URL), false, map[string]any{"url": unannounced.URL})
	case errors.As(err, &subpage):
		return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmSubpageOfOwnedLink,
			fmt.Sprintf("%q is a GNS subpage of the owned link %q, which already stands for it", subpage.URL, subpage.OwnedPageURL), false,
			map[string]any{"url": subpage.URL, "owned_page_url": subpage.OwnedPageURL})
	case errors.As(err, &ownedUnshown):
		return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmOwnedLinkUnshown,
			fmt.Sprintf("the show leaves the owned link %q unshown while it shows the reference %q; owned links come first", ownedUnshown.OwnedURL, ownedUnshown.ReferenceURL), false,
			map[string]any{"owned_url": ownedUnshown.OwnedURL, "reference_url": ownedUnshown.ReferenceURL})
	case refused:
		return nil, err
	case err != nil:
		log.Error(opChangeLinks, "could not change the links", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("change the links of %q: %w", ws, err)
	}
	v.publishLinks(ws, links)
	fields["links"], fields["shown"] = len(links), len(ShownLinks(links))
	log.Info(opChangeLinks, done, fields)
	return links, nil
}

// publishLinks hands every view of a workspace's links what it draws: the
// topbar title the shown links, and the sidebar row whether the workspace
// owns a pull request still open (wsm.OwnedPullRequestOpen). Every verb that
// changes the links, or what their pull requests' subscriptions record, ends
// here, so the two views never disagree about the links.
func (v *verbs) publishLinks(ws ids.WorkspaceID, links []wsm.WorkspaceLink) {
	v.deps.Topbar.SetLinks(ws, ShownLinks(links))
	v.deps.Sidebar.SetOpenOwnedPullRequest(ws, wsm.OwnedPullRequestOpen(links))
	v.deps.Merge.WorkspaceLinksChanged(ws)
}

// RepublishLinks reads the workspace's links again and publishes them
// (publishLinks), for a change that reached their pull requests'
// subscriptions without a links verb: a subscription taken or ended by its
// own rpc, or a followed pull request's end.
func (v *verbs) RepublishLinks(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.record(ctx, rpcChangeWorkspaceLinks, ws)
	if err != nil {
		return err
	}
	links, err := v.deps.DB.WorkspaceLinks(ctx, ws)
	if err != nil {
		log.Error(opRepublishLinks, "could not read the workspace's links to republish them", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("republish the links of %q: %w", ws, err)
	}
	v.publishLinks(ws, links)
	log.Debug(opRepublishLinks, "republished the workspace's links", dlog.Context{"links": len(links), "open_owned_pull_request": wsm.OwnedPullRequestOpen(links)})
	return nil
}

// PullRequestEnded takes the end of a pull request WS follows: its links are
// republished, so the row stops drawing an owned pull request that is no
// longer open, and a merge may tell the workspace its owned pull requests
// all merged (TellOwnedPullRequestsMerged).
func (v *verbs) PullRequestEnded(ctx context.Context, ws ids.WorkspaceID, merged bool) error {
	if err := v.RepublishLinks(ctx, ws); err != nil {
		return err
	}
	if !merged {
		return nil
	}
	return v.TellOwnedPullRequestsMerged(ctx, ws)
}

// refuseOwnedElsewhere refuses a set owning a pull request another open
// workspace owns (agentrepl.v1.ChangeWorkspaceLinksPullRequestOwnedElsewhere),
// naming the owner. An owner whose row cannot be read is a store failure.
func (v *verbs) refuseOwnedElsewhere(ctx context.Context, log dlog.Logger, fields dlog.Context, e *wsm.PullRequestOwnedElsewhereError) error {
	owner, err := v.deps.DB.Workspace(ctx, e.Owner)
	if err != nil {
		log.Error(opChangeLinks, "could not read the workspace that owns the pull request, so the refusal cannot name it",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"owner": string(e.Owner), "pull_request": e.PullRequest.String()}), err))
		return fmt.Errorf("read the owner %q of %s: %w", e.Owner, e.PullRequest, err)
	}
	return refuseWith(log, rpcChangeWorkspaceLinks, ArmPullRequestOwnedElsewhere,
		fmt.Sprintf("the pull request %s is owned by workspace %q; a pull request has one owning workspace", e.PullRequest, owner.Name), false,
		map[string]any{"url": fields["url"], "owner": owner.Ref(), "owner_name": owner.Name})
}

// validLinkLabel answers why LABEL is not one the summary's menu draws: not
// two to eight words, more than one sentence, a line break, or whitespace at
// either end (textline.OneSentence). Nil for a label the menu draws.
func validLinkLabel(label string) error {
	return textline.OneSentence("label", label, minLinkLabelWords, maxLinkLabelWords)
}

// LinkOwnershipView renders whether a link is owned
// (agentrepl.v1.WorkspaceLinkOwnership).
func LinkOwnershipView(owned bool) *agentreplv1.WorkspaceLinkOwnership {
	if owned {
		return &agentreplv1.WorkspaceLinkOwnership{Ownership: &agentreplv1.WorkspaceLinkOwnership_Owned{Owned: &agentreplv1.WorkspaceLinkOwned{}}}
	}
	return &agentreplv1.WorkspaceLinkOwnership{Ownership: &agentreplv1.WorkspaceLinkOwnership_Reference{Reference: &agentreplv1.WorkspaceLinkReference{}}}
}

// ShownLinks is the links the summary's menu shows, in the order given (most
// recently set first, as the store reads them).
func ShownLinks(links []wsm.WorkspaceLink) []topbar.Link {
	var shown []topbar.Link
	for _, link := range links {
		if link.Shown {
			shown = append(shown, topbar.Link{Label: link.Label, URL: link.URL, Owned: link.Owned, State: link.State})
		}
	}
	return shown
}

// LinksView renders a workspace's stored links as an agent reads them
// (agentrepl.v1.WorkspaceLinks): every link with its placement, and whether
// the menu has room for another.
func LinksView(links []wsm.WorkspaceLink) *agentreplv1.WorkspaceLinks {
	out := &agentreplv1.WorkspaceLinks{Links: make([]*agentreplv1.WorkspaceLink, len(links))}
	var shown uint32
	for i, link := range links {
		placement := &agentreplv1.WorkspaceLinkPlacement{}
		if link.Shown {
			shown++
			placement.Placement = &agentreplv1.WorkspaceLinkPlacement_Shown{Shown: &agentreplv1.WorkspaceLinkPlacementShown{}}
		} else {
			placement.Placement = &agentreplv1.WorkspaceLinkPlacement_Unshown{Unshown: &agentreplv1.WorkspaceLinkPlacementUnshown{}}
		}
		out.Links[i] = &agentreplv1.WorkspaceLink{Url: link.URL, Label: link.Label, Placement: placement, Ownership: ownershipView(link)}
	}
	if shown < wsm.ShownLinkLimit {
		out.Menu = &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Room{
			Room: &agentreplv1.WorkspaceLinksMenuRoom{Free: wsm.ShownLinkLimit - shown}}}
	} else {
		out.Menu = &agentreplv1.WorkspaceLinksMenu{Standing: &agentreplv1.WorkspaceLinksMenu_Full{
			Full: &agentreplv1.WorkspaceLinksMenuFull{Limit: wsm.ShownLinkLimit, Unshown: uint32(len(links)) - shown}}}
	}
	return out
}

// ownershipView renders whether the workspace's work made what LINK points
// at, and for an owned link how the daemon follows it.
func ownershipView(link wsm.WorkspaceLink) *agentreplv1.WorkspaceLinkOwnership {
	view := LinkOwnershipView(link.Owned)
	if owned := view.GetOwned(); owned != nil {
		owned.Follow = followView(link)
		if link.Announces != "" {
			owned.Announces = &agentreplv1.WorkspaceLinkAnnouncement{Url: link.Announces}
		}
	}
	return view
}

// followView renders how the daemon follows an owned LINK.
func followView(link wsm.WorkspaceLink) *agentreplv1.WorkspaceLinkOwnedFollow {
	follow := &agentreplv1.WorkspaceLinkOwnedFollow{}
	switch sub := link.Follow; {
	case link.PullRequest == nil:
		follow.Standing = &agentreplv1.WorkspaceLinkOwnedFollow_NotFollowable{NotFollowable: &agentreplv1.WorkspaceLinkOwnedFollowNotFollowable{}}
	case sub == nil:
		follow.Standing = &agentreplv1.WorkspaceLinkOwnedFollow_Unsubscribed{Unsubscribed: &agentreplv1.WorkspaceLinkOwnedFollowUnsubscribed{}}
	case sub.Ended != nil:
		state := &agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Closed{Closed: &agentreplv1.PullRequestStateClosed{}}}
		if sub.Ended.Merged {
			state.State = &agentreplv1.PullRequestState_Merged{Merged: &agentreplv1.PullRequestStateMerged{MergeCommitSha: sub.Ended.MergeSHA}}
		}
		follow.Standing = &agentreplv1.WorkspaceLinkOwnedFollow_Ended{Ended: &agentreplv1.WorkspaceLinkOwnedFollowEnded{State: state}}
	default:
		follow.Standing = &agentreplv1.WorkspaceLinkOwnedFollow_Following{Following: &agentreplv1.WorkspaceLinkOwnedFollowFollowing{Subscription: prwatch.SubscriptionProto(*sub)}}
	}
	return follow
}
