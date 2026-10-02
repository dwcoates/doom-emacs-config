// Package effortlevel is the vendor's spelling of a reasoning-effort level
// ("medium") against the canonical conversation.v1.AgentEffortLevel, in both
// directions. It is the ONE table: a settings file, a selector label and a
// review heading all read a level through here, so a level the vendor adds
// lands in one switch rather than drifting between near-duplicates.
package effortlevel

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// spellings is every named level and the vendor's word for it.
var spellings = []struct {
	level conversationv1.AgentEffortLevel
	word  string
}{
	{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW, "low"},
	{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM, "medium"},
	{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH, "high"},
	{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_XHIGH, "xhigh"},
	{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX, "max"},
}

// Word is the vendor's word for LEVEL, empty for UNSPECIFIED or a value the
// table does not carry.
func Word(level conversationv1.AgentEffortLevel) string {
	for _, s := range spellings {
		if s.level == level {
			return s.word
		}
	}
	return ""
}

// Parse maps the vendor's word onto the canonical level. The empty word is
// UNSPECIFIED (nothing stated); any other unknown word is an error, because
// it is a vendor spelling this daemon cannot map and must report.
func Parse(word string) (conversationv1.AgentEffortLevel, error) {
	if word == "" {
		return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED, nil
	}
	for _, s := range spellings {
		if s.word == word {
			return s.level, nil
		}
	}
	return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED,
		fmt.Errorf("%q is not an effort level this daemon knows", word)
}
