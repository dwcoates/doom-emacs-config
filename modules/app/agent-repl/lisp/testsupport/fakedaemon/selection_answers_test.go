package main

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"connectrpc.com/connect"
)

// rosterNaming is a roster push naming ID current, as a selection's answer.
func rosterNaming(id string) string {
	return fmt.Sprintf(`{"roster":{"current":{"workspace":{"id":%q,"dir":"/tmp/%s"}}}}`, id, id)
}

// answersBody is a /_fake/selection-answers body answering each of IDS.
func answersBody(ids ...string) string {
	parts := make([]string, 0, len(ids))
	for _, id := range ids {
		parts = append(parts, fmt.Sprintf("%q:%s", id, rosterNaming(id)))
	}
	return `{"answers":{` + strings.Join(parts, ",") + `}}`
}

func openRoster(t *testing.T, server *fakeServer, client agentreplv1connectClient) (*connect.ServerStreamForClient[agentreplv1.WatchWorkspaceRosterResponse], context.CancelFunc) {
	t.Helper()
	return openStream(t, server, streamRoster, "", func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchWorkspaceRosterResponse], error) {
		return client.WatchWorkspaceRoster(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceRosterRequest{}))
	})
}

func selectWorkspace(t *testing.T, client agentreplv1connectClient, id string) {
	t.Helper()
	if _, err := client.SelectWorkspace(context.Background(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/tmp/" + id},
	})); err != nil {
		t.Fatalf("SelectWorkspace(%s): %v", id, err)
	}
}

// rosterPushes ends the roster stream and answers the current id of every
// push it carried, in order. Ending it is what makes "nothing else arrived"
// observable without waiting on a clock.
func rosterPushes(t *testing.T, baseURL string, stream *connect.ServerStreamForClient[agentreplv1.WatchWorkspaceRosterResponse]) []string {
	t.Helper()
	if status, body := controlPost(t, baseURL, "/_fake/end", `{"stream":"roster"}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}
	var ids []string
	for stream.Receive() {
		ids = append(ids, stream.Msg().GetRoster().GetCurrent().GetWorkspace().GetId())
	}
	if stream.Err() != nil {
		t.Fatalf("roster stream ended with %v, want a clean end", stream.Err())
	}
	return ids
}

func TestSelectionAnswersPushTheRosterThatAnswersASelection(t *testing.T) {
	tests := []struct {
		name     string
		answered []string
		selected string
		want     []string
	}{
		{name: "an answered selection pushes its roster", answered: []string{"ws-a", "ws-b"}, selected: "ws-b", want: []string{"ws-b"}},
		{name: "a selection with no answer pushes nothing", answered: []string{"ws-a"}, selected: "ws-b", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			server, baseURL := newTestServer(t)
			client := newTestClient(t, baseURL)
			stream, cancel := openRoster(t, server, client)
			defer cancel()
			if status, body := controlPost(t, baseURL, "/_fake/selection-answers", answersBody(tt.answered...)); status != http.StatusOK {
				t.Fatalf("/_fake/selection-answers = %d %s", status, body)
			}

			// Act
			selectWorkspace(t, client, tt.selected)

			// Assert
			if got := rosterPushes(t, baseURL, stream); strings.Join(got, ",") != strings.Join(tt.want, ",") {
				t.Fatalf("roster pushes named %v current, want %v", got, tt.want)
			}
		})
	}
}

func TestSelectionAnswerIsPushedBeforeTheSelectionIsAnswered(t *testing.T) {
	// Arrange: the test's own push follows the selection's answer.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openRoster(t, server, client)
	defer cancel()
	controlPost(t, baseURL, "/_fake/selection-answers", answersBody("ws-a"))

	// Act: the selection returns only once its roster is pushed, so a push
	// made after it is after it on the stream too.
	selectWorkspace(t, client, "ws-a")
	if status, body := controlPost(t, baseURL, "/_fake/push", `{"stream":"roster","message":`+rosterNaming("ws-later")+`}`); status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert
	if got := rosterPushes(t, baseURL, stream); strings.Join(got, ",") != "ws-a,ws-later" {
		t.Fatalf("roster pushes named %v current, want [ws-a ws-later]", got)
	}
}

func TestSelectionAnswerIsStoredAsTheRostersSnapshot(t *testing.T) {
	// Arrange
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	controlPost(t, baseURL, "/_fake/selection-answers", answersBody("ws-a"))
	selectWorkspace(t, client, "ws-a")

	// Act: a subscriber that arrives after the selection.
	stream, cancel := openRoster(t, server, client)
	defer cancel()

	// Assert
	if got := rosterPushes(t, baseURL, stream); strings.Join(got, ",") != "ws-a" {
		t.Fatalf("a later subscriber received %v, want the answer naming ws-a", got)
	}
}

func TestSelectionAnswersReplaceTheEarlierAnswers(t *testing.T) {
	// Arrange
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openRoster(t, server, client)
	defer cancel()
	controlPost(t, baseURL, "/_fake/selection-answers", answersBody("ws-a"))
	controlPost(t, baseURL, "/_fake/selection-answers", answersBody("ws-b"))

	// Act
	selectWorkspace(t, client, "ws-a")

	// Assert
	if got := rosterPushes(t, baseURL, stream); len(got) != 0 {
		t.Fatalf("a selection the replaced answers dropped pushed %v, want nothing", got)
	}
}

func TestSelectionAnswersRefuseAnInvalidRosterAndKeepTheEarlierAnswers(t *testing.T) {
	// Arrange
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openRoster(t, server, client)
	defer cancel()
	controlPost(t, baseURL, "/_fake/selection-answers", answersBody("ws-a"))

	// Act
	status, body := controlPost(t, baseURL, "/_fake/selection-answers", `{"answers":{"ws-b":{"roster":{"noSuchField":1}}}}`)
	selectWorkspace(t, client, "ws-a")

	// Assert
	if status != http.StatusBadRequest || !strings.Contains(body, "ws-b") {
		t.Fatalf("/_fake/selection-answers = %d %s, want 400 naming ws-b", status, body)
	}
	if got := rosterPushes(t, baseURL, stream); strings.Join(got, ",") != "ws-a" {
		t.Fatalf("roster pushes named %v current, want the kept answer naming ws-a", got)
	}
}

func TestResetDropsTheSelectionAnswers(t *testing.T) {
	// Arrange
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	controlPost(t, baseURL, "/_fake/selection-answers", answersBody("ws-a", "ws-b"))

	// Act
	status, body := controlPost(t, baseURL, "/_fake/reset", `{}`)
	stream, cancel := openRoster(t, server, client)
	defer cancel()
	selectWorkspace(t, client, "ws-a")

	// Assert
	var counts resetCounts
	if err := json.Unmarshal([]byte(body), &counts); status != http.StatusOK || err != nil {
		t.Fatalf("/_fake/reset = %d %s (%v)", status, body, err)
	}
	if got := rosterPushes(t, baseURL, stream); counts.SelectionAnswers != 2 || len(got) != 0 {
		t.Fatalf("reset counted %d answers and a later selection pushed %v, want 2 and nothing", counts.SelectionAnswers, got)
	}
}

func TestOpenStreamReturnsOnceTheServerHasRegisteredIt(t *testing.T) {
	// Arrange
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act
	_, cancel := openRoster(t, server, client)
	defer cancel()

	// Assert: registered by the time it returned, with no wait of its own here.
	got := 0
	for _, sub := range server.subscriberInfos() {
		if sub.Stream == streamRoster {
			got++
		}
	}
	if got != 1 {
		t.Fatalf("roster subscribers = %d once openStream returned, want 1", got)
	}
}

func TestStreamOpenersShareOpenStream(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("control_test.go")
	if err != nil {
		t.Fatal(err)
	}
	answers, err := os.ReadFile("selection_answers_test.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act: an opener that registers on its own instead of through openStream.
	// The needle is split so this line is not one of its own matches.
	needle := "context.WithCancel(" + "context.Background())"
	handRolled := strings.Count(string(src)+string(answers), needle)

	// Assert: openStream's own is the only one.
	if handRolled != 1 {
		t.Fatalf("%d stream openers build their own context, want only openStream's", handRolled)
	}
}
