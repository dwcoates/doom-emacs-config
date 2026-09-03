package e2e

import (
	"fmt"
	"sort"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/reflect/protoreflect"

	"claude-repld/integration/harness"
)

func rowArm(row *frontendv1.FeedRow) string {
	m := row.ProtoReflect()
	od := m.Descriptor().Oneofs().ByName("row")
	if od == nil {
		return "?"
	}
	fd := m.WhichOneof(od)
	if fd == nil {
		return "unset"
	}
	name := string(fd.Name())
	if fd.Kind() == protoreflect.MessageKind && name == "activity" {
		act := row.GetActivity()
		ad := act.ProtoReflect()
		aod := ad.Descriptor().Oneofs().ByName("unit")
		if aod != nil {
			if afd := ad.WhichOneof(aod); afd != nil {
				return "activity." + string(afd.Name())
			}
		}
	}
	return name
}

func TestZZDiscovery(t *testing.T) {
	w := NewWorld(t, WorldOpts{})
	scenarios := []string{
		"skill", "skill-fail", "hook-blocked", "hook-failed", "hook-cancelled",
		"subagent-failed", "web-fetch", "read", "write-create", "bash-image",
		"send-message", "wakeup-schedule", "usage-full", "slash", "mcp-all",
	}
	for _, sc := range scenarios {
		repo := harness.NewRepo(t)
		ws := harness.Register(t, w.Daemon, repo.Dir)
		prompt := "!" + sc
		if sc == "" {
			prompt = "plain prose please"
		}
		func() {
			defer func() {
				if r := recover(); r != nil {
					fmt.Printf("SCEN %-28s PANIC %v\n", sc, r)
				}
			}()
			resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
				Workspace:      ws,
				Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
					{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: prompt}}},
				}}},
				IdempotencyKey: newIdempotencyKey(t),
				Origin:         e2ePromptOrigin,
			}))
			if err != nil {
				fmt.Printf("SCEN %-28s SUBMIT-ERR %v\n", sc, err)
				return
			}
			turn := resp.Msg.GetSuccess().GetTurn().GetTurn()
			if turn.GetValue() == "" {
				fmt.Printf("SCEN %-28s NO-TURN %v\n", sc, resp.Msg)
				return
			}
			// A card scenario never ends its turn until answered, so the wait
			// is for ANY row past the prompt rather than the terminal row.
			AwaitTurnEnded(t, w, ws, turn)
			arms := map[string]int{}
			for _, row := range tlOpenRows(t, w, ws) {
				arms[rowArm(row)]++
			}
			keys := []string{}
			for k := range arms {
				keys = append(keys, fmt.Sprintf("%s x%d", k, arms[k]))
			}
			sort.Strings(keys)
			fmt.Printf("SCEN %-28s %v\n", sc, keys)
		}()
	}
}
