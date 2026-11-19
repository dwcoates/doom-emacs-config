package prwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/ghclient"
)

func TestAFullPageIsFollowedByTheNext(t *testing.T) {
	// Arrange
	gh := newGH()
	var full []any
	for i := 1; i <= perPage; i++ {
		full = append(full, commentJSONOf(uint64(i), "a", "b", t0, t0, nil))
	}
	gh.set(pathPull(), pullJSON("open", false, false, "h"))
	gh.set(pathComments(), mustJSON(full))
	gh.set(fmt.Sprintf("%s/issues/%d/comments?per_page=100&page=2", base, prK.Number), mustJSON([]any{commentJSONOf(101, "a", "b", t0, t0, nil)}))

	// Act
	snap, err := newReader(gh).read(context.Background(), prK, false)

	// Assert
	if err != nil || len(snap.Comments) != perPage+1 {
		t.Fatalf("read = %d comments, %v; want both pages", len(snap.Comments), err)
	}
}

func TestAnUnchangedPageIsReadFromTheCache(t *testing.T) {
	// Arrange
	gh := newGH()
	gh.set(pathComments(), mustJSON([]any{commentJSONOf(1, "a", "b", t0, t0, nil)}))
	gh.set(pathPull(), pullJSON("open", false, false, "h"))
	r := newReader(gh)
	if _, err := r.read(context.Background(), prK, false); err != nil {
		t.Fatal(err)
	}

	// Act
	snap, err := r.read(context.Background(), prK, false)

	// Assert
	if err != nil || len(snap.Comments) != 1 {
		t.Fatalf("second read = %+v, %v; want the cached comment", snap, err)
	}
}

func TestDecodePRReadsItsState(t *testing.T) {
	tests := []struct {
		name  string
		body  string
		state string
	}{
		{name: "open", body: pullJSON("open", false, false, "h"), state: "open"},
		{name: "a draft", body: pullJSON("open", true, false, "h"), state: "draft"},
		{name: "merged", body: pullJSON("closed", false, true, "h"), state: "merged"},
		{name: "closed", body: pullJSON("closed", false, false, "h"), state: "closed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			pr, err := decodePR([]byte(tt.body))

			// Assert
			if err != nil || stateOf(pr) != tt.state {
				t.Fatalf("decodePR = %+v, %v; want %s", pr, err, tt.state)
			}
		})
	}
}

func TestDecodePRRefusesAnUnknownState(t *testing.T) {
	// Act
	_, err := decodePR([]byte(`{"state":"weird"}`))

	// Assert
	if err == nil {
		t.Fatal("decodePR = nil, want refused")
	}
}

// configAnswer answers every LifecycleConfigAt call with DATA, counting them.
func configAnswer(gh *fakeGH, data string, errs ...ghclient.GraphQLError) *int {
	calls := 0
	gh.graphql = func(string, map[string]any) (ghclient.GraphQLResponse, error) {
		calls++
		return ghclient.GraphQLResponse{Data: json.RawMessage(data), Errors: errs}, nil
	}
	return &calls
}

func blob(text string, truncated bool) string {
	return mustJSON(map[string]any{"repository": map[string]any{"object": map[string]any{"__typename": "Blob", "text": text, "isTruncated": truncated}}})
}

func TestGateChecksReadsTheCommitsConfig(t *testing.T) {
	tests := []struct {
		name      string
		data      string
		errs      []ghclient.GraphQLError
		wantGates map[string]bool
		wantErr   string
	}{
		{name: "a commit with no config has no gates", data: `{"repository":{"object":null}}`, wantGates: map[string]bool{}},
		{name: "a config with no pull_requests has no gates", data: blob("pre_deploy: {}\n", false), wantGates: map[string]bool{}},
		{name: "a config names its gates", data: blob("pre_deploy: {}\npull_requests:\n  gate_checks: [a, b]\n", false), wantGates: map[string]bool{"a": true, "b": true}},
		{name: "an unusable config is refused naming the file", data: blob("pull_requests: {}\n", false), wantErr: "o/r@h:.agent-repl/lifecycle.yaml is not a usable lifecycle config"},
		{name: "a directory is refused", data: `{"repository":{"object":{"__typename":"Tree"}}}`, wantErr: "o/r@h:.agent-repl/lifecycle.yaml is a Tree, not a readable file"},
		{name: "a truncated file is refused", data: blob("pre_deploy: {}\n", true), wantErr: "GitHub truncated o/r@h:.agent-repl/lifecycle.yaml"},
		{name: "a nulled repository is its reported failure", data: `{"repository":null}`, errs: []ghclient.GraphQLError{{Type: "NOT_FOUND", Message: "gone", Path: []string{"repository"}}}, wantErr: "gone"},
		{name: "an undecodable answer is refused", data: `[]`, wantErr: "decode the LifecycleConfigAt answer"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			gh := newGH()
			configAnswer(gh, tt.data, tt.errs...)

			// Act
			gates, err := newReader(gh).gateChecks(context.Background(), "o", "r", "h")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("gateChecks = %v, %v; want an error containing %q", gates, err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(gates, tt.wantGates) {
				t.Fatalf("gateChecks = %v, %v; want %v", gates, err, tt.wantGates)
			}
		})
	}
}

func TestGateChecksAsksAboutACommitOnce(t *testing.T) {
	tests := []struct {
		name string
		data string
	}{
		{name: "a usable config", data: blob("pre_deploy: {}\n", false)},
		{name: "an unusable config", data: blob("nonsense: 1\n", false)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			gh := newGH()
			calls := configAnswer(gh, tt.data)
			r := newReader(gh)
			_, first := r.gateChecks(context.Background(), "o", "r", "h")

			// Act
			_, second := r.gateChecks(context.Background(), "o", "r", "h")

			// Assert
			if *calls != 1 || fmt.Sprint(first) != fmt.Sprint(second) {
				t.Fatalf("%d calls, errors %v then %v; want one call answering the same", *calls, first, second)
			}
		})
	}
}

func TestGateChecksAsksAgainAfterAFailedCall(t *testing.T) {
	// Arrange
	gh := newGH()
	r := newReader(gh)
	if _, err := r.gateChecks(context.Background(), "o", "r", "h"); err == nil {
		t.Fatal("an unscripted call succeeded")
	}
	configAnswer(gh, `{"repository":{"object":null}}`)

	// Act
	gates, err := r.gateChecks(context.Background(), "o", "r", "h")

	// Assert
	if err != nil || len(gates) != 0 {
		t.Fatalf("gateChecks = %v, %v; want the second call made and answered", gates, err)
	}
}
