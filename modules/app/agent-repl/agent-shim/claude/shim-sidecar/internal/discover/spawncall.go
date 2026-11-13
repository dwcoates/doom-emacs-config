package discover

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// spawningTools are the vendor tools whose call spawns a subagent: a Task or
// Agent call, and a Skill whose skill runs in a subagent of its own.
var spawningTools = map[string]bool{"Agent": true, "Task": true, "Skill": true}

// errNoSpawningCall and errAmbiguousSpawningCall are why spawningCall named
// no call: none answers the subagent's prompt, or more than one does.
var (
	errNoSpawningCall        = errors.New("no unclaimed spawning call in the parent transcript answers the subagent's prompt")
	errAmbiguousSpawningCall = errors.New("more than one unclaimed spawning call in the parent transcript answers the subagent's prompt")
)

// spawningCall finds the call that spawned the subagent whose transcript is
// at PATH when its meta names none (2026-10-19: the vendor wrote a Skill
// subagent's meta without its toolUseId when the user rejected the Skill
// call as it started). The spawning call's tool_use_id IS the subagent's
// AgentId under the cross-plane minting rule, so it is read where the vendor
// did record it: the parent transcript's assistant records of the SAME prompt
// (the subagent's first record's promptId) carry the spawning tool_use, and a
// call a sibling's meta already names spawned that sibling. EXACTLY ONE
// unclaimed call answers; none or several is an error, never a guess.
func spawningCall(path string) (string, error) {
	prompt, err := firstPromptID(path)
	if err != nil {
		return "", err
	}
	subagents := filepath.Dir(path)
	claimed, err := claimedCalls(subagents)
	if err != nil {
		return "", err
	}
	parent := filepath.Dir(subagents) + ".jsonl"
	calls, err := promptSpawningCalls(parent, prompt)
	if err != nil {
		return "", err
	}
	var unclaimed []string
	for _, id := range calls {
		if !claimed[id] {
			unclaimed = append(unclaimed, id)
		}
	}
	switch len(unclaimed) {
	case 1:
		return unclaimed[0], nil
	case 0:
		return "", fmt.Errorf("%w (prompt %s in %s)", errNoSpawningCall, prompt, parent)
	}
	return "", fmt.Errorf("%w (prompt %s in %s: %s)", errAmbiguousSpawningCall, prompt, parent, strings.Join(unclaimed, ", "))
}

// transcriptRecord is the part of a transcript record spawningCall reads.
type transcriptRecord struct {
	Type     string `json:"type"`
	PromptID string `json:"promptId"`
	Message  struct {
		Content json.RawMessage `json:"content"`
	} `json:"message"`
}

// contentBlock is the part of a message content block spawningCall reads.
type contentBlock struct {
	Type string `json:"type"`
	ID   string `json:"id"`
	Name string `json:"name"`
}

// firstPromptID is the promptId of the transcript's first record.
func firstPromptID(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", fmt.Errorf("reading the subagent transcript %s: %w", path, err)
	}
	defer f.Close()
	scanner := transcriptScanner(f)
	if !scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return "", fmt.Errorf("reading the subagent transcript %s: %w", path, err)
		}
		return "", fmt.Errorf("the subagent transcript %s has no record", path)
	}
	var first transcriptRecord
	if err := json.Unmarshal(scanner.Bytes(), &first); err != nil {
		return "", fmt.Errorf("parsing the first record of %s: %w", path, err)
	}
	if first.PromptID == "" {
		return "", fmt.Errorf("the first record of %s names no promptId", path)
	}
	return first.PromptID, nil
}

// claimedCalls is every spawning call a meta in the SUBAGENTS directory names.
func claimedCalls(subagents string) (map[string]bool, error) {
	metas, err := filepath.Glob(filepath.Join(subagents, "*.meta.json"))
	if err != nil {
		return nil, err
	}
	claimed := map[string]bool{}
	for _, p := range metas {
		meta, err := ReadMeta(p)
		if err != nil {
			return nil, err
		}
		if meta.ToolUseID != "" {
			claimed[meta.ToolUseID] = true
		}
	}
	return claimed, nil
}

// promptSpawningCalls is every spawning tool_use the PARENT transcript's
// assistant records of PROMPT carry, in order. The vendor stamps promptId on
// a prompt's user records only (the prompt and its tool results), so an
// assistant record belongs to the prompt the latest stamped record named.
func promptSpawningCalls(parent, prompt string) ([]string, error) {
	f, err := os.Open(parent)
	if err != nil {
		return nil, fmt.Errorf("reading the parent transcript %s: %w", parent, err)
	}
	defer f.Close()
	scanner := transcriptScanner(f)
	var calls []string
	current := ""
	for scanner.Scan() {
		var record transcriptRecord
		if err := json.Unmarshal(scanner.Bytes(), &record); err != nil {
			return nil, fmt.Errorf("parsing a record of %s: %w", parent, err)
		}
		if record.PromptID != "" {
			current = record.PromptID
		}
		if record.Type != "assistant" || current != prompt {
			continue
		}
		var blocks []contentBlock
		if err := json.Unmarshal(record.Message.Content, &blocks); err != nil {
			continue
		}
		for _, b := range blocks {
			if b.Type == "tool_use" && spawningTools[b.Name] && b.ID != "" {
				calls = append(calls, b.ID)
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("reading the parent transcript %s: %w", parent, err)
	}
	return calls, nil
}

// transcriptScanner reads a transcript record by record: one line each, of
// up to 64 MiB, since a record carries a whole tool result.
func transcriptScanner(f *os.File) *bufio.Scanner {
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64*1024), 64*1024*1024)
	return scanner
}
