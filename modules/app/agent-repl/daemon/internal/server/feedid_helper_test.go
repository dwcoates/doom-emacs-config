package server

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// feedIDFor encodes a root-feed row id for one workspace, which is how a test
// hands the handlers a well-formed but FOREIGN feed address.
func feedIDFor(ws ids.WorkspaceID) *frontendv1.FeedId {
	return feedid.Encode(feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "row-1"},
	})
}

// permissionIDFor encodes a permission row's id, whose row key holds the ask id
// the answer verbs address.
func permissionIDFor(ws ids.WorkspaceID, ask string) *frontendv1.FeedId {
	return feedid.Encode(feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindPermission, ID: ask},
	})
}

// questionIDFor encodes a question row's id.
func questionIDFor(ws ids.WorkspaceID, ask string) *frontendv1.FeedId {
	return feedid.Encode(feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindQuestion, ID: ask},
	})
}

// coldGateIDFor encodes the cold gate's row id.
func coldGateIDFor(ws ids.WorkspaceID) *frontendv1.FeedId {
	return feedid.Encode(feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindColdGate, ID: "gate"},
	})
}

// feedAddressFor encodes a FEED's own address for one workspace, which is what
// OpenFeed's optional field carries.
func feedAddressFor(ws ids.WorkspaceID) *frontendv1.FeedId {
	return feedid.EncodeFeed(ws, feedid.Feed{Root: true})
}
