package server

import (
	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/zettel/internal/graph"
)

// THE ONE MAPPING between zettel.v1 (the protocol) and package graph (the
// persistence model). mapping_test.go fails when a field is added on one side
// and not mapped.

// ParentFromProto translates a request's parent, answering the offending
// field's name when it is malformed: an unset parent or origin, an empty node
// id, a zero revision, or an empty quote.
func ParentFromProto(p *zettelv1.NodeParent) (graph.Parent, string, bool) {
	switch origin := p.GetOrigin().(type) {
	case *zettelv1.NodeParent_ThreadStart:
		return graph.Parent{}, "", true
	case *zettelv1.NodeParent_Node:
		of := origin.Node
		if blank(of.GetNode().GetId()) {
			return graph.Parent{}, "parent.node.node", false
		}
		if of.GetRevision().GetNumber() == 0 {
			return graph.Parent{}, "parent.node.revision", false
		}
		asked := &graph.AskedOf{Node: graph.NodeID(of.GetNode().GetId()), Revision: of.GetRevision().GetNumber()}
		if of.Quote != nil {
			if blank(of.GetQuote().GetText()) {
				return graph.Parent{}, "parent.node.quote.text", false
			}
			asked.Quote = &graph.Quote{Text: of.GetQuote().GetText()}
			if of.GetQuote().QuestionContext != nil {
				context := of.GetQuote().GetQuestionContext().GetText()
				asked.Quote.Context = &context
			}
		}
		return graph.Parent{Of: asked}, "", true
	default:
		return graph.Parent{}, "parent", false
	}
}

// ParentToProto translates a stored parent.
func ParentToProto(p graph.Parent) *zettelv1.NodeParent {
	if p.Of == nil {
		return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}
	}
	node := &zettelv1.NodeParentNode{
		Node:     &zettelv1.NodeId{Id: string(p.Of.Node)},
		Revision: &zettelv1.RevisionNumber{Number: p.Of.Revision},
	}
	if p.Of.Quote != nil {
		node.Quote = &zettelv1.NodeQuote{Text: p.Of.Quote.Text}
		if p.Of.Quote.Context != nil {
			node.Quote.QuestionContext = &zettelv1.NodeQuoteContext{Text: *p.Of.Quote.Context}
		}
	}
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: node}}
}

// NodeToProto translates a stored node.
func NodeToProto(n graph.Node) *zettelv1.Node {
	return &zettelv1.Node{
		Id:       &zettelv1.NodeId{Id: string(n.ID)},
		Thread:   &zettelv1.ThreadId{Id: string(n.Thread)},
		Parent:   ParentToProto(n.Parent),
		Question: &zettelv1.NodeQuestion{Text: n.Question, Abbreviation: n.Abbreviation},
		Answer: &zettelv1.NodeRevision{
			Number:      &zettelv1.RevisionNumber{Number: n.Answer.Number},
			Markdown:    n.Answer.Markdown,
			WrittenAtMs: n.Answer.WrittenAt.UnixMilli(),
		},
		CreatedAtMs: n.CreatedAt.UnixMilli(),
		Sightings:   &zettelv1.NodeSightings{Count: n.Sightings},
		Turn:        &zettelv1.ExchangeTurn{TurnId: n.Turn},
	}
}

// MatchToProto renders a search match.
func MatchToProto(m graph.Match) *zettelv1.SearchThreadsMatch {
	snippet := &zettelv1.Snippet{}
	for _, part := range m.Snippet {
		if part.Hit {
			snippet.Parts = append(snippet.Parts, &zettelv1.SnippetPart{Part: &zettelv1.SnippetPart_Hit{Hit: part.Text}})
		} else {
			snippet.Parts = append(snippet.Parts, &zettelv1.SnippetPart{Part: &zettelv1.SnippetPart_Plain{Plain: part.Text}})
		}
	}
	return &zettelv1.SearchThreadsMatch{
		Thread:  &zettelv1.ThreadId{Id: string(m.Thread)},
		Node:    &zettelv1.NodeId{Id: string(m.Node)},
		Snippet: snippet,
	}
}
