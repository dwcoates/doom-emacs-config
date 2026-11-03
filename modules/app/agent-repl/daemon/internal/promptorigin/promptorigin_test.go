package promptorigin

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// daemonNoticeOrigins are the daemon's notices to an agent.
var daemonNoticeOrigins = map[conversationv1.PromptOrigin]bool{
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE:         true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE:           true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE:   true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_PULL_REQUEST_NOTICE:        true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE:  true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE: true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT:       true,
}

func TestOnlyTheDaemonsNoticesAreNotices(t *testing.T) {
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		t.Run(origin.String(), func(t *testing.T) {
			// Act.
			got := DaemonNotice(origin)

			// Assert.
			if want := daemonNoticeOrigins[origin]; got != want {
				t.Fatalf("DaemonNotice(%s) = %v, want %v", origin, got, want)
			}
		})
	}
}

func TestDaemonNoticeNamedReadsTheStoredSpelling(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{name: "PROMPT_ORIGIN_MERGE_QUEUE_NOTICE", want: true},
		{name: "PROMPT_ORIGIN_MERGE_HANDBACK", want: false},
		{name: "NO_SUCH_ORIGIN", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := DaemonNoticeNamed(tt.name)

			// Assert.
			if got != tt.want {
				t.Fatalf("DaemonNoticeNamed(%q) = %v, want %v", tt.name, got, tt.want)
			}
		})
	}
}

func TestCheckSender(t *testing.T) {
	agentSent := conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
	userSent := conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
		named  bool
		want   error
	}{
		{name: "an agent-sent prompt naming its sender", origin: agentSent, named: true},
		{name: "a user's prompt naming none", origin: userSent},
		{name: "an agent-sent prompt naming none", origin: agentSent, want: ErrSenderMismatch},
		{name: "a user's prompt naming a sender", origin: userSent, named: true, want: ErrSenderMismatch},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := CheckSender(tt.origin, tt.named)

			// Assert
			if !errors.Is(err, tt.want) || (tt.want == nil && err != nil) {
				t.Fatalf("CheckSender = %v, want %v", err, tt.want)
			}
		})
	}
}

func TestCheckSenderNamedReadsTheStoredSpelling(t *testing.T) {
	tests := []struct {
		name   string
		origin string
		named  bool
		want   error
	}{
		{name: "the agent-sent spelling naming its sender", origin: "PROMPT_ORIGIN_AGENT_SENT", named: true},
		{name: "the agent-sent spelling naming none", origin: "PROMPT_ORIGIN_AGENT_SENT", want: ErrSenderMismatch},
		{name: "an origin no vocabulary holds, naming a sender", origin: "NO_SUCH_ORIGIN", named: true, want: ErrSenderMismatch},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := CheckSenderNamed(tt.origin, tt.named)

			// Assert
			if !errors.Is(err, tt.want) || (tt.want == nil && err != nil) {
				t.Fatalf("CheckSenderNamed = %v, want %v", err, tt.want)
			}
		})
	}
}

// THE SENDER RULE IS WRITTEN ONCE: every layer carrying a prompt's sender asks
// CheckSender, so a hand-rolled comparison of an origin against
// PROMPT_ORIGIN_AGENT_SENT beside a sender fails here.
func TestEverySenderCheckIsCheckSender(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`PROMPT_ORIGIN_AGENT_SENT\)\s*!=\s*\(`)
	var found []string

	// Act
	err := filepath.WalkDir("../..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolled.Match(raw) && !strings.HasSuffix(path, "promptorigin.go") {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's source: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("these files compare an origin to PROMPT_ORIGIN_AGENT_SENT by hand instead of asking CheckSender: %v", found)
	}
}

func TestUserSent(t *testing.T) {
	user := map[conversationv1.PromptOrigin]bool{
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT:                 true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_AND_HIDE:        true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT: true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX:    true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_PREFIX:     true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT:          true,
	}
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		t.Run(origin.String(), func(t *testing.T) {
			// Act
			got := UserSent(origin)

			// Assert
			if got != user[origin] {
				t.Fatalf("UserSent(%s) = %v, want %v", origin, got, user[origin])
			}
		})
	}
}

func TestNamedReadsTheStoredSpelling(t *testing.T) {
	tests := []struct {
		name   string
		stored string
		want   conversationv1.PromptOrigin
	}{
		{name: "a user's prompt", stored: "PROMPT_ORIGIN_USER_SENT", want: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT},
		{name: "an agent-sent prompt", stored: "PROMPT_ORIGIN_AGENT_SENT", want: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT},
		{name: "a name no vocabulary holds", stored: "NO_SUCH_ORIGIN", want: conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED},
		{name: "the empty name", stored: "", want: conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Named(tt.stored)

			// Assert
			if got != tt.want {
				t.Fatalf("Named(%q) = %s, want %s", tt.stored, got, tt.want)
			}
		})
	}
}

// A STORED ORIGIN IS READ IN ONE PLACE: every layer reading the spelling a
// hold or a turn stores asks Named, so a hand-rolled lookup in
// conversation.v1.PromptOrigin_value outside this package fails here.
func TestEveryStoredOriginIsReadByNamed(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`PromptOrigin\(conversationv1\.PromptOrigin_value\[`)
	var found []string

	// Act
	err := filepath.WalkDir("../..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolled.Match(raw) && !strings.HasSuffix(path, "promptorigin.go") {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's source: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("these files read a stored origin by hand instead of asking Named: %v", found)
	}
}

func TestAdopted(t *testing.T) {
	adopted := map[conversationv1.PromptOrigin]bool{
		conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED: true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_REMOTE_CONTROL: true,
	}
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		t.Run(origin.String(), func(t *testing.T) {
			// Act
			got := Adopted(origin)

			// Assert
			if got != adopted[origin] {
				t.Fatalf("Adopted(%s) = %v, want %v", origin, got, adopted[origin])
			}
		})
	}
}

func TestOnlyTheNoticesAndAgentPromptsAreDeliveredOnArrival(t *testing.T) {
	onArrival := map[conversationv1.PromptOrigin]bool{
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE:         true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE:           true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE:   true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_PULL_REQUEST_NOTICE:        true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE:  true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT:       true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK:             true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_RETARGETED:       true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATE_FAILED:    true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MAIN_SYNC_REPAIR:           true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LINK_UNRESOLVED:            true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE: true,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT:                 true,
	}
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		t.Run(origin.String(), func(t *testing.T) {
			// Act
			got := DeliveredOnArrival(origin)

			// Assert
			if got != onArrival[origin] {
				t.Fatalf("DeliveredOnArrival(%s) = %v, want %v", origin, got, onArrival[origin])
			}
		})
	}
}

// EVERY NOTICE IS DELIVERED ON ARRIVAL: the held section draws no card for a
// notice, so one that could be held would wait where nothing draws it.
func TestEveryNoticeIsDeliveredOnArrival(t *testing.T) {
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		if !DaemonNotice(origin) {
			continue
		}
		t.Run(origin.String(), func(t *testing.T) {
			// Act
			got := DeliveredOnArrival(origin)

			// Assert
			if !got {
				t.Fatalf("DaemonNotice(%s) but not DeliveredOnArrival", origin)
			}
		})
	}
}

func TestDeliveredOnArrivalNamedReadsTheStoredSpelling(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{name: "PROMPT_ORIGIN_AGENT_SENT", want: true},
		{name: "PROMPT_ORIGIN_USER_SENT", want: false},
		{name: "NO_SUCH_ORIGIN", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := DeliveredOnArrivalNamed(tt.name)

			// Assert
			if got != tt.want {
				t.Fatalf("DeliveredOnArrivalNamed(%q) = %v, want %v", tt.name, got, tt.want)
			}
		})
	}
}

// noAgentTurnOrigins are the origins whose turns set no agent working: the
// re-drives of an interrupted turn and the daemon's compactions.
var noAgentTurnOrigins = map[conversationv1.PromptOrigin]bool{
	conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART:        true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME: true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT:           true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_COMPACT_AFTER:         true,
}

func TestStartsAgentTurnClassifiesEveryNamedOrigin(t *testing.T) {
	for value := range conversationv1.PromptOrigin_name {
		origin := conversationv1.PromptOrigin(value)
		if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
			continue
		}
		t.Run(origin.String(), func(t *testing.T) {
			// Act.
			got := StartsAgentTurn(origin)

			// Assert.
			if want := !noAgentTurnOrigins[origin]; got != want {
				t.Fatalf("StartsAgentTurn(%s) = %v, want %v", origin, got, want)
			}
		})
	}
}

func TestStartsAgentTurnFailsHardOnAnUnclassifiedOrigin(t *testing.T) {
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
	}{
		{name: "the unspecified origin", origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED},
		{name: "an origin the vocabulary does not name", origin: conversationv1.PromptOrigin(9999)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			defer func() {
				// Assert.
				if recovered := recover(); recovered == nil {
					t.Fatalf("StartsAgentTurn(%s) did not fail hard", tt.origin)
				}
			}()

			// Act.
			StartsAgentTurn(tt.origin)
		})
	}
}
