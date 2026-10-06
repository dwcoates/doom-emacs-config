package feed

import (
	"fmt"
	"sort"
	"strings"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// WHAT A LOAD DID WITH EACH ENTRY. A reader's load that draws nothing leaves
// an empty page and nothing else to read: the history_loaded record names, by
// entry kind, how many entries drew a row in the feed, were withheld for an
// older page, completed an earlier load's withheld rows, or drew no row at
// all (a settled hook, a budget warning, a row drawn into another feed), so
// an empty feed is read off the record rather than inferred.

// entryOutcome is what became of one entry a load replayed.
type entryOutcome string

const (
	// outcomeDrew: the entry drew a row in the load's feed.
	outcomeDrew entryOutcome = "drew"
	// outcomeCompleted: an entry an earlier load withheld drew its row now.
	outcomeCompleted entryOutcome = "completed"
	// outcomeWithheld: the entry waits for its turn's starting entry.
	outcomeWithheld entryOutcome = "withheld"
	// outcomeNoRow: the entry was replayed and drew no row in the load's feed.
	outcomeNoRow entryOutcome = "no_row"
)

// entryOutcomeDepth bounds how far an entry's kind follows its nested arms:
// entry → frame result → update → activity item → that item's own arm.
const entryOutcomeDepth = 5

// entryOutcomes tallies a load's entries by kind and outcome.
type entryOutcomes map[string]int

// note tallies one entry.
func (o *entryOutcomes) note(at *conversationv1.HistoryEntryAt, outcome entryOutcome) {
	if *o == nil {
		*o = entryOutcomes{}
	}
	(*o)[entryKind(at)+"="+string(outcome)]++
}

// String renders the tally sorted, "kind=outcome:count" joined by spaces.
func (o entryOutcomes) String() string {
	keys := make([]string, 0, len(o))
	for key := range o {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	parts := make([]string, 0, len(keys))
	for _, key := range keys {
		parts = append(parts, fmt.Sprintf("%s:%d", key, o[key]))
	}
	return strings.Join(parts, " ")
}

// entryKind names an entry by the arms it carries, outermost first, e.g.
// "agent_frame.update.activity.hook.succeeded"; "unset" when it carries none.
func entryKind(at *conversationv1.HistoryEntryAt) string {
	if kind := armPath(at.GetEntry().ProtoReflect(), entryOutcomeDepth); kind != "" {
		return kind
	}
	return "unset"
}

// armPath follows the first populated oneof of M, in declaration order, into
// its message for up to DEPTH arms.
func armPath(m protoreflect.Message, depth int) string {
	if depth == 0 || !m.IsValid() {
		return ""
	}
	oneofs := m.Descriptor().Oneofs()
	for i := range oneofs.Len() {
		field := m.WhichOneof(oneofs.Get(i))
		if field == nil {
			continue
		}
		name := string(field.Name())
		if field.Kind() == protoreflect.MessageKind {
			if inner := armPath(m.Get(field).Message(), depth-1); inner != "" {
				return name + "." + inner
			}
		}
		return name
	}
	return ""
}
