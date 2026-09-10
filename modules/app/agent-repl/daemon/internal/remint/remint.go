// Package remint re-mints every identity inside a vendor transcript under ONE
// consistent mapping, so a FORK's copy of a parent conversation is a
// conversation of the child's own rather than a second copy of the parent's.
//
// WHY A BYTE COPY IS BROKEN. The file plane keys rows by what the vendor
// records SAY, not by which file said it: the sidecar mints
// `activity:<tool_use_id>`, `terminal:<agent>:<record uuid>` and
// `residue:<record uuid>` straight out of the record's own identity fields
// (`shim-sidecar/internal/convert/keys.go`). A fork that copies the parent's
// bytes therefore hands the sidecar the SAME upsert keys under a SECOND book —
// the child's — and the store refuses the batch with "would move the row from
// book A to book B" (`shim-store/internal/db/write.go`), parks the file, and
// the child's conversation never advances again.
//
// THE MAPPING IS A PURE FUNCTION OF (old id) -> (new id), memoized, and applied
// to the whole port: the transcript AND the sidecar directory beside it. That
// is what keeps a cross-record `parentUuid` link, a tool_use/tool_result
// pairing, and a subagent's `agent-<id>` file name pointing at each other after
// the port exactly as they did before it.
//
// THE PARENT'S FILE IS NEVER TOUCHED. Re-minting happens on the way into the
// child's root; the parent keeps its own conversation, which is the whole point
// of a fork.
package remint

import (
	"bytes"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"path"
	"regexp"
	"strings"

	"github.com/google/uuid"
)

// uuidShaped spells the vendor's record-uuid form (8-4-4-4-12 hex).
var uuidShaped = regexp.MustCompile(`^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$`)

// identityKeys are the record fields whose STRING VALUE is an identity — one
// the sidecar derives an upsert key from, or one another record points at.
//
// Every `*uuid`-suffixed key is covered by rule rather than by name (see
// isIdentityKey), because the vendor keeps inventing them: `parentUuid`,
// `logicalParentUuid`, `leafUuid`, `headUuid`, `anchorUuid`, `tailUuid`,
// `refusedUserMessageUuid` are all the same kind of thing.
var identityKeys = map[string]bool{
	// The session the record belongs to. PINNED to the child's own vendor
	// session id rather than mapped, because the child's file IS that session.
	"sessionId":  true,
	"session_id": true,

	// A prompt's identity, and the message identities the file-history records
	// point back at.
	"promptId":          true,
	"prompt_id":         true,
	"messageId":         true,
	"message_id":        true,
	"snapshotMessageId": true,

	// The vendor's agent LOCATOR (`agent-<id>`), which is also the sidecar
	// transcript's file name — renamed under the same mapping by PathSegment.
	"agentId":          true,
	"agent_id":         true,
	"createdAgentId":   true,
	"created_agent_id": true,

	// The call an activity is keyed by, in every spelling the vendor uses: the
	// tool_result's back-reference, and the hook attachment's.
	"tool_use_id": true,
	"toolUseId":   true,
	"toolUseID":   true,

	// Detached work: the spool's task id and a workflow run's id, both of which
	// name rows and directories.
	"task_id":            true,
	"taskId":             true,
	"backgroundTaskId":   true,
	"background_task_id": true,
	"runId":              true,
	"run_id":             true,
}

// pinnedToSession are the keys that always become the CHILD's vendor session
// id, whatever the parent's record said. The per-record `sessionId` diverges
// from the file's own id in a fifth of real records, and a fork's copy must
// still name exactly one session: the child's.
var pinnedToSession = map[string]bool{"sessionId": true, "session_id": true}

// isIdentityKey answers whether a field's string value is an identity.
func isIdentityKey(key string) bool {
	if identityKeys[key] {
		return true
	}
	return strings.HasSuffix(strings.ToLower(key), "uuid")
}

// Mapper carries one port's whole mapping.
//
// It is NOT safe for concurrent use, and does not need to be: one port is one
// sequential walk over one transcript and its sidecar.
type Mapper struct {
	childSessionID string
	mapping        map[string]string
	taken          map[string]bool
	mint           func(old string) string
	rewrites       int
}

// New builds the mapping for one port. The parent's own vendor session id is
// seeded to the child's, so every reference to it — wherever it appears — comes
// out as the child's id.
//
// A nil mint takes DefaultMint; a test passes its own to make the mapping
// readable.
func New(parentSessionID, childSessionID string, mint func(old string) string) *Mapper {
	if mint == nil {
		mint = DefaultMint
	}
	m := &Mapper{
		childSessionID: childSessionID,
		mapping:        map[string]string{},
		taken:          map[string]bool{},
		mint:           mint,
	}
	if parentSessionID != "" && childSessionID != "" {
		m.mapping[parentSessionID] = childSessionID
		m.taken[childSessionID] = true
	}
	return m
}

// ID answers the new identity for an old one, minting it on first sight and
// answering the same value on every sight after. The empty id maps to itself:
// an identity that is absent stays absent.
func (m *Mapper) ID(old string) string {
	if old == "" {
		return ""
	}
	if got, ok := m.mapping[old]; ok {
		return got
	}
	minted := m.mint(old)
	for minted == "" || m.taken[minted] || minted == old {
		minted = m.mint(old + "\x00" + minted)
	}
	m.mapping[old] = minted
	m.taken[minted] = true
	return minted
}

// DefaultMint mints a fresh identity of the SAME SHAPE as the old one, which is
// what keeps shape-dispatching consumers working: a uuid-shaped id stays
// uuid-shaped, a `toolu_…`/`wf_…` id keeps its vendor prefix, and anything else
// keeps its length. It never derives anything from the old value's bytes — a
// derived id would be a second name for the parent's identity rather than a new
// one.
func DefaultMint(old string) string {
	switch {
	case uuidShaped.MatchString(old):
		return uuid.NewString()
	default:
		if i := strings.Index(old, "_"); i > 0 && i < len(old)-1 {
			return old[:i+1] + randomHex(len(old)-i-1)
		}
		return randomHex(len(old))
	}
}

// randomHex answers n hex characters (at least 8, so nothing degenerate is ever
// minted for a very short id).
func randomHex(n int) string {
	if n < 8 {
		n = 8
	}
	buf := make([]byte, (n+1)/2)
	if _, err := rand.Read(buf); err != nil {
		// crypto/rand.Read never fails on the platforms this daemon runs on,
		// and a silent fallback to a weaker source would mint colliding
		// identities — so this is loud rather than defaulted away.
		panic(fmt.Sprintf("remint: reading random bytes: %v", err))
	}
	return hex.EncodeToString(buf)[:n]
}

// Lines re-mints a whole JSONL transcript.
//
// EVERY LINE IS ACCOUNTED FOR. A blank line and a well-formed line that is not
// a JSON OBJECT pass through byte-for-byte; so does an object with no identity
// in it, because re-encoding one would rewrite key order and spacing for no
// gain. A MALFORMED line is an error, never a skip: silently dropping a record
// on the way into a fork loses conversation, and porting it unchanged would
// carry the parent's identity into the child's book.
func (m *Mapper) Lines(data []byte) ([]byte, error) {
	var out bytes.Buffer
	out.Grow(len(data))
	rest := data
	lineNo := 0
	for len(rest) > 0 {
		lineNo++
		line := rest
		term := []byte(nil)
		if i := bytes.IndexByte(rest, '\n'); i >= 0 {
			line, term, rest = rest[:i], rest[i:i+1], rest[i+1:]
		} else {
			rest = nil
		}
		converted, err := m.Line(line)
		if err != nil {
			return nil, fmt.Errorf("remint: line %d: %w", lineNo, err)
		}
		out.Write(converted)
		out.Write(term)
	}
	return out.Bytes(), nil
}

// Line re-mints one JSONL record, answering the line's own bytes when nothing
// in it was an identity.
func (m *Mapper) Line(line []byte) ([]byte, error) {
	if len(bytes.TrimSpace(line)) == 0 {
		return line, nil
	}
	record, err := decodeObject(line)
	if err != nil {
		return nil, err
	}
	if record == nil {
		// Well-formed JSON that is not an object: nothing here owns an
		// identity, so it survives byte-for-byte.
		return line, nil
	}
	before := m.rewrites
	m.rewriteObject(record, false)
	if m.rewrites == before {
		return line, nil
	}
	return encode(record)
}

// Document re-mints a whole-file JSON object — the subagent's
// `agent-<id>.meta.json`, whose `toolUseId` IS that agent's AgentId under the
// cross-plane minting rule.
func (m *Mapper) Document(data []byte) ([]byte, error) {
	if len(bytes.TrimSpace(data)) == 0 {
		return data, nil
	}
	document, err := decodeObject(data)
	if err != nil {
		return nil, err
	}
	if document == nil {
		return data, nil
	}
	before := m.rewrites
	m.rewriteObject(document, false)
	if m.rewrites == before {
		return data, nil
	}
	return encode(document)
}

// PathSegment re-mints the identity a sidecar path segment CARRIES, leaving
// every other segment alone: `agent-<id>.jsonl`, its `agent-<id>.meta.json`
// companion, and a workflow's `wf_<id>` directory. The locator in the name and
// the `agentId` a record states are the same identity, so they must move
// together or the parent's transcript records would point at a file the child
// does not have.
func (m *Mapper) PathSegment(segment string) string {
	if strings.HasPrefix(segment, "wf_") {
		return m.ID(segment)
	}
	if !strings.HasPrefix(segment, "agent-") {
		return segment
	}
	name := strings.TrimPrefix(segment, "agent-")
	ext := ""
	for _, candidate := range []string{".meta.json", ".jsonl", ".json"} {
		if strings.HasSuffix(name, candidate) {
			name, ext = strings.TrimSuffix(name, candidate), candidate
			break
		}
	}
	if name == "" {
		return segment
	}
	return "agent-" + m.ID(name) + ext
}

// PathRel re-mints every segment of a sidecar-relative path.
func (m *Mapper) PathRel(rel string) string {
	segments := strings.Split(rel, "/")
	for i, segment := range segments {
		segments[i] = m.PathSegment(segment)
	}
	return path.Join(segments...)
}

// rewriteObject walks one JSON object in place.
//
// bareID says whether a plain `id` field here is an identity. It is TRUE for a
// `message` object (`message.id` is what the sidecar's activity keys for text
// and thinking blocks are built from) and for each block of that message's
// `content` (a tool_use block's `id` IS the tool_use_id), and FALSE everywhere
// else — a tool INPUT is free to carry an `id` of its own that names something
// in the user's world, and rewriting that would corrupt the conversation
// instead of re-identifying it.
func (m *Mapper) rewriteObject(object map[string]any, bareID bool) {
	for key, value := range object {
		switch {
		case pinnedToSession[key]:
			if _, ok := value.(string); ok {
				if object[key] != m.childSessionID {
					m.rewrites++
				}
				object[key] = m.childSessionID
			}
		case isIdentityKey(key) || (bareID && key == "id"):
			object[key] = m.rewriteIdentity(value)
		case key == "message":
			if child, ok := value.(map[string]any); ok {
				m.rewriteObject(child, true)
			} else {
				m.rewriteValue(value)
			}
		case bareID && key == "content":
			m.rewriteContent(value)
		default:
			m.rewriteValue(value)
		}
	}
}

// rewriteContent walks a message's content blocks, each of which is message-like
// for the purpose of a bare `id`.
func (m *Mapper) rewriteContent(value any) {
	blocks, ok := value.([]any)
	if !ok {
		m.rewriteValue(value)
		return
	}
	for _, block := range blocks {
		if object, ok := block.(map[string]any); ok {
			m.rewriteObject(object, true)
			continue
		}
		m.rewriteValue(block)
	}
}

// rewriteIdentity maps an identity field's value: a string, or a list of them.
// A null stays null — an absent parent is not an identity to mint.
func (m *Mapper) rewriteIdentity(value any) any {
	switch typed := value.(type) {
	case string:
		if typed == "" {
			return typed
		}
		m.rewrites++
		return m.ID(typed)
	case []any:
		for i, element := range typed {
			typed[i] = m.rewriteIdentity(element)
		}
		return typed
	default:
		return value
	}
}

// rewriteValue recurses into anything that might hold an object.
func (m *Mapper) rewriteValue(value any) {
	switch typed := value.(type) {
	case map[string]any:
		m.rewriteObject(typed, false)
	case []any:
		for _, element := range typed {
			m.rewriteValue(element)
		}
	}
}

// decodeObject decodes one JSON value, answering nil (and no error) when the
// value is well-formed but is not an object.
//
// NUMBERS ARE DECODED AS json.Number so a re-encoded record states the token
// counts and timestamps the vendor wrote, rather than a float64's rounding of
// them.
func decodeObject(data []byte) (map[string]any, error) {
	decoder := json.NewDecoder(bytes.NewReader(data))
	decoder.UseNumber()
	var value any
	if err := decoder.Decode(&value); err != nil {
		return nil, fmt.Errorf("parsing the vendor record: %w", err)
	}
	if err := trailingIsBlank(decoder); err != nil {
		return nil, err
	}
	object, ok := value.(map[string]any)
	if !ok {
		return nil, nil
	}
	return object, nil
}

// trailingIsBlank refuses a line that carries anything after the record. Two
// records on one line, or trailing garbage, is a malformed transcript, and
// reading only the first value would drop the rest silently.
func trailingIsBlank(decoder *json.Decoder) error {
	var extra any
	err := decoder.Decode(&extra)
	switch {
	case err == nil:
		return errors.New("parsing the vendor record: a second JSON value follows it on the same line")
	case errors.Is(err, io.EOF):
		return nil
	default:
		return fmt.Errorf("parsing the vendor record: trailing content: %w", err)
	}
}

// encode renders a rewritten record back to one line, leaving HTML unescaped so
// prompt text survives the round trip as the vendor wrote it.
func encode(value any) ([]byte, error) {
	var buffer bytes.Buffer
	encoder := json.NewEncoder(&buffer)
	encoder.SetEscapeHTML(false)
	if err := encoder.Encode(value); err != nil {
		return nil, fmt.Errorf("remint: re-encoding the record: %w", err)
	}
	return bytes.TrimRight(buffer.Bytes(), "\n"), nil
}
