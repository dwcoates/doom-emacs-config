package prwatch

import (
	"context"
	"fmt"
	"testing"
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
