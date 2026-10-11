// Package catalogtest builds slash-command catalogs for tests: the one
// fixture every package that handles a conversation.v1.SessionCommandCatalog
// states its catalogs with.
package catalogtest

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// Named answers a catalog of one entry per name, in order, each carrying its
// name alone. No names answers an empty catalog, which is a real catalog.
func Named(names ...string) *conversationv1.SessionCommandCatalog {
	catalog := &conversationv1.SessionCommandCatalog{}
	for _, name := range names {
		catalog.Commands = append(catalog.Commands, &conversationv1.SessionCommandCatalogEntry{Name: name})
	}
	return catalog
}
