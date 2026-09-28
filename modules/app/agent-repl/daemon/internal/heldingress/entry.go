package heldingress

import (
	"bytes"
	"encoding/json"
	"fmt"
	"path/filepath"

	"google.golang.org/protobuf/encoding/protojson"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// FormatVersion is the one entry format this ingress reads. A file naming any
// other version is malformed: a reader that guessed at a newer format could
// deliver a prompt it misread.
const FormatVersion = 1

// Entry is one held prompt, exactly as a producer wrote it:
//
//	{
//	  "version": 1,
//	  "project_dir": "/abs/path/of/the/workspace/worktree",
//	  "idempotency_key": "the key of the SubmitPrompt attempt it re-drives",
//	  "origin": "PROMPT_ORIGIN_USER_SENT",
//	  "said": { ...the SubmitPromptRequest.said UserSaid, in protojson... },
//	  "queued_at": "2026-09-28T12:00:00.000000000Z"
//	}
//
// One file holds one prompt, so a file is removed exactly when its one prompt
// was accepted. Files are ingested in NAME order, and a producer names them
// `held_<UTC timestamp>_<...>.json` so name order is the order they were
// written.
type Entry struct {
	// Version is FormatVersion.
	Version int `json:"version"`
	// ProjectDir is the workspace's worktree root, the key it is resolved by.
	ProjectDir string `json:"project_dir"`
	// IdempotencyKey is the key the prompt was first submitted under. It is
	// what makes a prompt the daemon already accepted a duplicate here.
	IdempotencyKey string `json:"idempotency_key"`
	// Origin is the PromptOrigin enum's value NAME, as protojson spells it.
	Origin string `json:"origin"`
	// Said is the composed UserSaid in protojson.
	Said json.RawMessage `json:"said"`
	// QueuedAt is when the producer wrote the entry. It is carried for the
	// record only; order is the file name's.
	QueuedAt string `json:"queued_at"`
}

// decoded is an entry with its wire fields decoded.
type decoded struct {
	Entry
	origin conversationv1.PromptOrigin
	said   *conversationv1.UserSaid
}

// parse decodes one entry file, refusing anything the ingress could only act
// on by guessing. Unknown fields are refused too: a field this reader does
// not know is a producer writing a format it does not read.
func parse(data []byte) (decoded, error) {
	var entry Entry
	decoder := json.NewDecoder(bytes.NewReader(data))
	decoder.DisallowUnknownFields()
	if err := decoder.Decode(&entry); err != nil {
		return decoded{}, fmt.Errorf("decode the entry: %w", err)
	}
	if decoder.More() {
		return decoded{}, fmt.Errorf("decode the entry: trailing data after the object")
	}
	switch {
	case entry.Version != FormatVersion:
		return decoded{}, fmt.Errorf("version %d is not the format this ingress reads (%d)", entry.Version, FormatVersion)
	case entry.ProjectDir == "" || !filepath.IsAbs(entry.ProjectDir):
		return decoded{}, fmt.Errorf("project_dir %q must be an absolute directory", entry.ProjectDir)
	case entry.IdempotencyKey == "":
		return decoded{}, fmt.Errorf("idempotency_key is required: it is what keeps an accepted prompt from being delivered twice")
	case len(entry.Said) == 0:
		return decoded{}, fmt.Errorf("said is required")
	}
	value, ok := conversationv1.PromptOrigin_value[entry.Origin]
	if !ok || conversationv1.PromptOrigin(value) == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return decoded{}, fmt.Errorf("origin %q is not a prompt origin", entry.Origin)
	}
	said := &conversationv1.UserSaid{}
	if err := protojson.Unmarshal(entry.Said, said); err != nil {
		return decoded{}, fmt.Errorf("decode said: %w", err)
	}
	if len(said.GetContent().GetBlocks()) == 0 {
		return decoded{}, fmt.Errorf("said carries no content blocks")
	}
	return decoded{Entry: entry, origin: conversationv1.PromptOrigin(value), said: said}, nil
}
