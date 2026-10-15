package feed

import (
	"slices"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
)

// ① THE PROMPT'S KNOWN COMMANDS. FeedUserPrompt.commands names every command
// of the workspace's standing catalog the prompt's text holds in slashed form,
// so the bubble marks it as the Emacs composer marks it while it is typed. It
// has two edges, the same two the `working` flag has:
//
//   - STAMPED on every upsert of a user-prompt row, by stampPromptCommands,
//     from the catalog in hand (Deps.Commands);
//   - RE-PUBLISHED when the session states a catalog, by restampPromptCommands,
//     so a row drawn before any catalog was stated (a replay at daemon start)
//     or before a skill was added gains its marks then.
//
// The list is the CANDIDATES: the client finds the slash words in the drawn
// text and marks one exactly when its name is listed, so the word rule lives
// with the drawing and this side only says which names are known.

// stampPromptCommands states on a user-prompt row the commands of CATALOG its
// text blocks name. A row that is not a user prompt is untouched; a nil
// catalog (none stated yet) names none.
func stampPromptCommands(row *frontendv1.FeedRow, catalog *conversationv1.SessionCommandCatalog) {
	prompt := row.GetUserPrompt()
	if prompt == nil {
		return
	}
	prompt.Commands = promptCommands(prompt, catalog)
}

// promptCommands is every name or alias of CATALOG whose slashed form occurs
// in one of PROMPT's text blocks, sorted and without repeats; nil when none.
func promptCommands(prompt *frontendv1.FeedUserPrompt, catalog *conversationv1.SessionCommandCatalog) []*frontendv1.FeedUserPromptCommand {
	var texts []string
	for _, block := range prompt.GetSuccess().GetBody().GetBlocks() {
		if text := block.GetText().GetText(); strings.Contains(text, "/") {
			texts = append(texts, text)
		}
	}
	if len(texts) == 0 {
		return nil
	}
	var names []string
	for _, entry := range catalog.GetCommands() {
		for _, name := range append([]string{entry.GetName()}, entry.GetAliases()...) {
			if name != "" && slices.ContainsFunc(texts, func(text string) bool {
				return strings.Contains(text, "/"+name)
			}) {
				names = append(names, name)
			}
		}
	}
	slices.Sort(names)
	names = slices.Compact(names)
	var commands []*frontendv1.FeedUserPromptCommand
	for _, name := range names {
		commands = append(commands, &frontendv1.FeedUserPromptCommand{Name: name})
	}
	return commands
}

// catalogOf answers the workspace's standing catalog, or nil when none is
// known (none stated yet, or no table wired).
func (r *resolver) catalogOf(s *wsState) *conversationv1.SessionCommandCatalog {
	if r.deps.Commands == nil {
		return nil
	}
	catalog, _ := r.deps.Commands(s.id)
	return catalog
}

// restampPromptCommands re-publishes every user-prompt row of the workspace,
// on every feed, so each states the commands of the catalog just stated. A row
// whose list did not change upserts to an equal snapshot, which upsert drops
// as churn, so a catalog restated unchanged re-publishes nothing.
func (r *resolver) restampPromptCommands(s *wsState) {
	considered := 0
	for key, f := range s.feeds {
		for _, id := range f.order {
			row := f.rows[id]
			if row.GetUserPrompt() == nil {
				continue
			}
			r.upsert(s, placement{feed: s.feedAddrs[key]}, row, !f.nonDurable[id])
			considered++
		}
	}
	r.logger(s.id).Debug("daemon.feed.prompt_commands_restamped",
		"the session stated its command catalog; every user-prompt row was re-stamped with the commands it names",
		dlog.Context{"rows": considered, "commands": len(r.catalogOf(s).GetCommands())})
}
