package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
)

// rpcChangeWorkspaceLinks is the rpc every links verb answers for.
const rpcChangeWorkspaceLinks = "ChangeWorkspaceLinks"

// The bounds of a link's label, in words.
const (
	minLinkLabelWords = 2
	maxLinkLabelWords = 8
)

// SetLink stores URL under LABEL on the workspace, or relabels it when the
// workspace already holds it (agentrepl.v1.ChangeWorkspaceLinksSet), and
// republishes the topbar title that opens the shown links. A url the external
// browser would refuse to open is refused invalid_link, and a label that is
// not two to eight words in one sentence invalid_label, before anything is
// written. It answers every link the workspace then holds.
func (v *verbs) SetLink(ctx context.Context, ws ids.WorkspaceID, url, label string) ([]wsm.WorkspaceLink, error) {
	fields := dlog.Context{"url": url, "label": label}
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
		return v.deps.DB.SetWorkspaceLink(ctx, ws, url, label)
	})
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
// too_many_shown, and a url the workspace does not hold unknown_link, with
// nothing changed.
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
	_, refused := AsRefusal(err)
	switch {
	case errors.As(err, &unknown):
		return nil, refuseWith(log, rpcChangeWorkspaceLinks, ArmUnknownLink,
			fmt.Sprintf("the workspace holds no link %q", unknown.URL), false, map[string]any{"url": unknown.URL})
	case refused:
		return nil, err
	case err != nil:
		log.Error(opChangeLinks, "could not change the links", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("change the links of %q: %w", ws, err)
	}
	v.deps.Topbar.SetLinks(ws, ShownLinks(links))
	fields["links"], fields["shown"] = len(links), len(ShownLinks(links))
	log.Info(opChangeLinks, done, fields)
	return links, nil
}

// validLinkLabel answers why LABEL is not one the summary's menu draws: not
// two to eight words, more than one sentence, a line break, or whitespace at
// either end. Nil for a label the menu draws.
func validLinkLabel(label string) error {
	if strings.ContainsAny(label, "\r\n") {
		return errors.New("a label is one line")
	}
	if strings.TrimSpace(label) != label {
		return errors.New("a label carries no whitespace at either end")
	}
	words := strings.Fields(label)
	if len(words) < minLinkLabelWords || len(words) > maxLinkLabelWords {
		return fmt.Errorf("a label is %d to %d words, not %d", minLinkLabelWords, maxLinkLabelWords, len(words))
	}
	for _, word := range words[:len(words)-1] {
		if last, _ := utf8.DecodeLastRuneInString(word); isSentenceEnd(last) {
			return fmt.Errorf("a label is one sentence, and %q ends one before its last word", word)
		}
	}
	return nil
}

// isSentenceEnd reports whether R ends a sentence.
func isSentenceEnd(r rune) bool {
	return r == '.' || r == '!' || r == '?' || unicode.Is(unicode.Sentence_Terminal, r)
}

// ShownLinks is the links the summary's menu shows, in the order given (most
// recently set first, as the store reads them).
func ShownLinks(links []wsm.WorkspaceLink) []topbar.Link {
	var shown []topbar.Link
	for _, link := range links {
		if link.Shown {
			shown = append(shown, topbar.Link{Label: link.Label, URL: link.URL})
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
		out.Links[i] = &agentreplv1.WorkspaceLink{Url: link.URL, Label: link.Label, Placement: placement}
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
