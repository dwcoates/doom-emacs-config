// Package agentverbs holds what the agent API's dedicated verbs say about
// themselves where more than one program must say it the same way: the usage
// lines of the verbs that replace a slash command, and the refusal an agent
// is told when it sends one.
package agentverbs

import "fmt"

// The slash commands an agent has a dedicated verb for.
const (
	// SlashCompact is the command `workspace compact` replaces.
	SlashCompact = "/compact"
	// SlashModel is the command `workspace model` replaces.
	SlashModel = "/model"
)

// The usage lines of the verbs that replace a slash command. The CLI's verb
// table and every refusal that names a verb read these, so a flag change
// reaches both.
const (
	// CompactUsage is `agent-repld workspace compact`'s usage.
	CompactUsage = "compact WS [-instructions TEXT] [-as-user]"
	// ModelUsage is `agent-repld workspace model`'s usage.
	ModelUsage = "model WS -model M [-as-user]"
)

// replacements are the dedicated verbs, by the slash command each replaces.
var replacements = map[string]string{
	SlashCompact: CompactUsage,
	SlashModel:   ModelUsage,
}

// ReplacementUsage answers the usage of the verb that replaces COMMAND (a
// slash command as typed, slash included), false when it has none.
func ReplacementUsage(command string) (string, bool) {
	usage, ok := replacements[command]
	return usage, ok
}

// SlashCommandRefusal is what an agent is told when a prompt it sent began
// with COMMAND: that slash commands are not supported from agents, and the
// verb that replaces it. A command with a verb names that one verb and its
// usage; any other names every replacement.
func SlashCommandRefusal(command string) string {
	if usage, ok := ReplacementUsage(command); ok {
		return fmt.Sprintf("slash commands are not supported from agents, so the prompt beginning with %s was not sent; use `agent-repld workspace %s` instead", command, usage)
	}
	return fmt.Sprintf("slash commands are not supported from agents, so the prompt beginning with %s was not sent; the dedicated verbs replace them: `agent-repld workspace compact` for %s and `agent-repld workspace model` for %s",
		command, SlashCompact, SlashModel)
}
