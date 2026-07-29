// Package convert maps the Claude harness's on-disk JSON records into the
// agentshim.data.v1 proto vocabulary under the design §5.1 validation contract:
//
//   - Missing EXPECTED field (an absent/unknown top-level type discriminator)
//     is a HARD error: the caller persists the raw record as a core.UnparsedEvent
//     and loud-logs it. Conversion NEVER silently yields a zero value for a
//     record it cannot classify.
//   - Unexpected NEW field (a JSON key the proto does not model) is captured
//     verbatim into a per-record extras Struct (dotted path → value) and
//     loud-logged ONCE per distinct field name. Nothing is dropped.
//   - string-or-object / string-or-blocks unions are modeled as explicit oneofs
//     with a raw_string / content_string arm; the converter routes by the JSON
//     value's runtime type, never erroring on the string arm.
//   - Disk camelCase (toolUseResult, sessionId, toolUseID) and stream snake_case
//     normalize to ONE proto shape via a canonical (lowercased, underscore- and
//     dash-stripped) field-name match.
//
// The converter is intentionally reflection-driven so that adding a proto field
// automatically starts populating it; only the oneof discriminators (which JSON
// does not express as a key named after the chosen arm) carry bespoke dispatch.
package convert

import (
	"encoding/base64"
	"fmt"
	"strings"

	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/known/structpb"
)

// Converter holds the once-per-distinct-field-name log-dedup set. It is NOT safe
// for concurrent use; each tailer owns its own converter.
type Converter struct {
	log  *logging.Bound
	seen map[string]bool
}

// New builds a Converter. A nil log is tolerated (discarded).
func New(log *logging.Bound) *Converter {
	return &Converter{log: log, seen: make(map[string]bool)}
}

// capture accumulates unknown fields for one record as a flat dotted-path map.
type capture struct {
	c      *Converter
	fields map[string]any
}

func (c *Converter) newCapture() *capture { return &capture{c: c, fields: map[string]any{}} }

// add records an unknown field at path with its raw JSON value and loud-logs the
// distinct leaf name once (§5.1).
func (cap *capture) add(path string, v any) {
	cap.fields[path] = v
	leaf := path
	if i := strings.LastIndex(path, "."); i >= 0 {
		leaf = path[i+1:]
	}
	if !cap.c.seen[leaf] {
		cap.c.seen[leaf] = true
		cap.c.log.With(logging.Context{Operation: "capture-extra"}).Log("unknown field %q path=%s captured into extras", leaf, path)
	}
}

// toStruct returns the accumulated extras as a Struct, or nil if none.
func (cap *capture) toStruct() *structpb.Struct {
	if len(cap.fields) == 0 {
		return nil
	}
	s, err := structpb.NewStruct(cap.fields)
	if err != nil {
		// A non-representable extras value should never happen (the inputs came
		// straight from encoding/json), but never drop it silently.
		cap.c.log.With(logging.Context{Operation: "build-extras"}).Log("failed to build extras struct: %v", err)
		return nil
	}
	return s
}

// canon normalizes a field/JSON name to its case- and separator-insensitive form
// so disk `toolUseID`, proto `tool_use_id`, and stream `toolUseId` all collide.
func canon(s string) string {
	var b strings.Builder
	for _, r := range s {
		switch {
		case r == '_' || r == '-':
			continue
		case r >= 'A' && r <= 'Z':
			b.WriteRune(r + ('a' - 'A'))
		default:
			b.WriteRune(r)
		}
	}
	return b.String()
}

// fieldIndex maps canonical names → non-oneof field descriptors of md. Oneof
// members are excluded: a stray key must never be able to select a oneof arm
// (those are chosen by explicit discriminator dispatch).
func fieldIndex(md protoreflect.MessageDescriptor) map[string]protoreflect.FieldDescriptor {
	idx := map[string]protoreflect.FieldDescriptor{}
	fs := md.Fields()
	for i := 0; i < fs.Len(); i++ {
		fd := fs.Get(i)
		if fd.ContainingOneof() != nil {
			continue
		}
		idx[canon(string(fd.Name()))] = fd
	}
	return idx
}

func isStruct(fd protoreflect.FieldDescriptor) bool {
	return fd.Kind() == protoreflect.MessageKind &&
		fd.Message().FullName() == "google.protobuf.Struct"
}

// isListValue reports whether fd is a singular google.protobuf.ListValue field
// (a schemaless array home). Repeated ListValue fields are handled by the list
// path; only the singular case needs the array-subtree absorption here.
func isListValue(fd protoreflect.FieldDescriptor) bool {
	return fd.Kind() == protoreflect.MessageKind && !fd.IsList() &&
		fd.Message().FullName() == "google.protobuf.ListValue"
}

// assign populates msg from obj, routing every key. ignore names (canonical) are
// consumed silently (structural discriminators like "type"/"role"). Unmapped
// keys land in cap under path.
func (c *Converter) assign(msg protoreflect.Message, obj map[string]any, path string, cap *capture, ignore map[string]bool) {
	idx := fieldIndex(msg.Descriptor())
	for k, v := range obj {
		ck := canon(k)
		if ignore[ck] {
			continue
		}
		fd, ok := idx[ck]
		if !ok {
			cap.add(joinPath(path, k), v)
			continue
		}
		c.setField(msg, fd, v, joinPath(path, k), cap)
	}
}

func joinPath(path, k string) string {
	if path == "" {
		return k
	}
	return path + "." + k
}

// setField assigns one field. JSON null leaves the field default. Type mismatches
// and unmodeled enum values are captured (never dropped, never hard-failed).
func (c *Converter) setField(msg protoreflect.Message, fd protoreflect.FieldDescriptor, v any, path string, cap *capture) {
	if v == nil {
		return
	}
	if fd.IsList() {
		list, ok := v.([]any)
		if !ok {
			cap.add(path, v)
			return
		}
		lv := msg.Mutable(fd).List()
		for _, el := range list {
			ev, ok := c.scalarOrMessage(fd, el, path, cap)
			if ok {
				lv.Append(ev)
			}
		}
		return
	}
	// Struct fields absorb any object subtree losslessly.
	if isStruct(fd) {
		m, ok := v.(map[string]any)
		if !ok {
			cap.add(path, v)
			return
		}
		s, err := structpb.NewStruct(m)
		if err != nil {
			cap.add(path, v)
			return
		}
		msg.Set(fd, protoreflect.ValueOfMessage(s.ProtoReflect()))
		return
	}
	// A singular google.protobuf.ListValue field absorbs a JSON array subtree
	// losslessly (e.g. ApiUsage.iterations, whose disk value is an array of
	// heterogeneous per-iteration objects with no fixed schema).
	if isListValue(fd) {
		arr, ok := v.([]any)
		if !ok {
			cap.add(path, v)
			return
		}
		lv, err := structpb.NewList(arr)
		if err != nil {
			cap.add(path, v)
			return
		}
		msg.Set(fd, protoreflect.ValueOfMessage(lv.ProtoReflect()))
		c.markSet(msg, fd)
		return
	}
	if fd.Kind() == protoreflect.MessageKind || fd.Kind() == protoreflect.GroupKind {
		m, ok := v.(map[string]any)
		if !ok {
			cap.add(path, v)
			return
		}
		sub := msg.NewField(fd)
		c.fillMessage(sub.Message(), m, path, cap, nil)
		msg.Set(fd, sub)
		c.markSet(msg, fd)
		return
	}
	pv, ok := c.scalar(fd, v, path, cap)
	if !ok {
		return
	}
	msg.Set(fd, pv)
	c.markSet(msg, fd)
}

// scalarOrMessage builds a repeated-list element value.
func (c *Converter) scalarOrMessage(fd protoreflect.FieldDescriptor, v any, path string, cap *capture) (protoreflect.Value, bool) {
	if fd.Kind() == protoreflect.MessageKind || fd.Kind() == protoreflect.GroupKind {
		m, ok := v.(map[string]any)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		sub := protoreflect.ValueOfMessage(newConcrete(fd.Message()))
		c.fillMessage(sub.Message(), m, path, cap, nil)
		return sub, true
	}
	return c.scalar(fd, v, path, cap)
}

// scalar coerces a JSON value to a proto scalar value for fd.
func (c *Converter) scalar(fd protoreflect.FieldDescriptor, v any, path string, cap *capture) (protoreflect.Value, bool) {
	switch fd.Kind() {
	case protoreflect.StringKind:
		s, ok := v.(string)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfString(s), true
	case protoreflect.BoolKind:
		b, ok := v.(bool)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfBool(b), true
	case protoreflect.BytesKind:
		s, ok := v.(string)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfBytes(decodeBytes(s)), true
	case protoreflect.EnumKind:
		n, ok := enumValue(fd.Enum(), v)
		if !ok {
			// Unmodeled enum value: capture the raw string, leave the enum at
			// its zero value (never guess a mapping).
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfEnum(n), true
	case protoreflect.Int32Kind, protoreflect.Sint32Kind, protoreflect.Sfixed32Kind:
		f, ok := asFloat(v)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfInt32(int32(f)), true
	case protoreflect.Int64Kind, protoreflect.Sint64Kind, protoreflect.Sfixed64Kind:
		f, ok := asFloat(v)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfInt64(int64(f)), true
	case protoreflect.Uint32Kind, protoreflect.Fixed32Kind:
		f, ok := asFloat(v)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfUint32(uint32(f)), true
	case protoreflect.Uint64Kind, protoreflect.Fixed64Kind:
		f, ok := asFloat(v)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfUint64(uint64(f)), true
	case protoreflect.DoubleKind:
		f, ok := asFloat(v)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfFloat64(f), true
	case protoreflect.FloatKind:
		f, ok := asFloat(v)
		if !ok {
			cap.add(path, v)
			return protoreflect.Value{}, false
		}
		return protoreflect.ValueOfFloat32(float32(f)), true
	default:
		cap.add(path, v)
		return protoreflect.Value{}, false
	}
}

func asFloat(v any) (float64, bool) {
	switch t := v.(type) {
	case float64:
		return t, true
	case float32:
		return float64(t), true
	case int:
		return float64(t), true
	case int64:
		return float64(t), true
	default:
		return 0, false
	}
}

// decodeBytes base64-decodes s; on failure (e.g. corpus-truncated image data) it
// preserves the raw string bytes rather than dropping the field.
func decodeBytes(s string) []byte {
	if b, err := base64.StdEncoding.DecodeString(s); err == nil {
		return b
	}
	if b, err := base64.RawStdEncoding.DecodeString(s); err == nil {
		return b
	}
	return []byte(s)
}

// markSet flips a companion "<field>_set" bool true when present, implementing
// the proto3 absent-vs-false convention (is_error_set, exit_code_set, …).
func (c *Converter) markSet(msg protoreflect.Message, fd protoreflect.FieldDescriptor) {
	sib := msg.Descriptor().Fields().ByName(protoreflect.Name(string(fd.Name()) + "_set"))
	if sib != nil && sib.Kind() == protoreflect.BoolKind && sib.ContainingOneof() == nil {
		msg.Set(sib, protoreflect.ValueOfBool(true))
	}
}

// enumValue resolves a JSON enum value (numeric or "kebab-or-snake string") to a
// proto enum number by canonical suffix match against the value names.
func enumValue(ed protoreflect.EnumDescriptor, v any) (protoreflect.EnumNumber, bool) {
	switch t := v.(type) {
	case float64:
		return protoreflect.EnumNumber(int32(t)), true
	case string:
		want := canon(t)
		vals := ed.Values()
		// Exact canonical match first (ENQUEUE == "enqueue").
		for i := 0; i < vals.Len(); i++ {
			vd := vals.Get(i)
			if canon(string(vd.Name())) == want {
				return vd.Number(), true
			}
		}
		// Then suffix match against the prefixed form (ENTRYPOINT_SDK_CLI ↦ "sdk-cli").
		up := strings.ToUpper(strings.ReplaceAll(t, "-", "_"))
		for i := 0; i < vals.Len(); i++ {
			vd := vals.Get(i)
			if strings.HasSuffix(string(vd.Name()), "_"+up) {
				return vd.Number(), true
			}
		}
		return 0, false
	default:
		return 0, false
	}
}

// ---------------------------------------------------------------------------
// Top-level dispatch
// ---------------------------------------------------------------------------

// TranscriptLine converts one decoded transcript/sidechain JSONL object into a
// datav1.TranscriptLine plus any captured extras. A missing or unknown top-level
// "type" is a hard error (→ UnparsedEvent per §5.1).
func (c *Converter) TranscriptLine(obj map[string]any) (*datav1.TranscriptLine, *structpb.Struct, error) {
	typ, _ := obj["type"].(string)
	c.log.With(logging.Context{Operation: "convert-transcript-line"}).LogVerbose("converting transcript line type=%q keys=%d", typ, len(obj))
	if typ == "" {
		err := fmt.Errorf("transcript line missing required %q field", "type")
		return nil, nil, err
	}
	line := &datav1.TranscriptLine{}
	lm := line.ProtoReflect()
	fd := oneofMemberByCanon(lm.Descriptor(), "line", canon(typ))
	if fd == nil {
		err := fmt.Errorf("unknown transcript line type %q", typ)
		return nil, nil, err
	}
	cap := c.newCapture()
	sub := lm.NewField(fd)
	switch string(fd.Name()) {
	case "user":
		c.userLine(sub.Message(), obj, cap)
	case "assistant":
		c.assistantLine(sub.Message(), obj, cap)
	case "system":
		if err := c.systemLine(sub.Message(), obj, cap); err != nil {
			return nil, nil, err
		}
	case "attachment":
		if err := c.attachmentLine(sub.Message(), obj, cap); err != nil {
			return nil, nil, err
		}
	default:
		// Flat metadata line (mode, permission_mode, queue_operation,
		// last_prompt, ai_title, pr_link, file_history_*, frame_link): no
		// envelope, payload fields sit at the top level.
		c.assign(sub.Message(), obj, string(fd.Name()), cap, map[string]bool{"type": true})
	}
	lm.Set(fd, sub)
	c.log.With(logging.Context{Operation: "convert-transcript-line"}).LogVerbose("converted transcript line type=%q extras=%t", typ, len(cap.fields) > 0)
	return line, cap.toStruct(), nil
}

// msgIgnore are the structural tags inside an API message object that carry no
// data the proto models (the type/role are implied by the enclosing line).
var msgIgnore = map[string]bool{"type": true, "role": true}

func (c *Converter) userLine(m protoreflect.Message, obj map[string]any, cap *capture) {
	fs := m.Descriptor().Fields()
	env := m.Mutable(fs.ByName("envelope")).Message()
	msgFd := fs.ByName("message")
	turFd := fs.ByName("tool_use_result")
	hasTurFd := fs.ByName("has_tool_use_result")
	for k, v := range obj {
		switch canon(k) {
		case "type":
			continue
		case "message":
			if mo, ok := v.(map[string]any); ok {
				um := m.NewField(msgFd)
				c.apiUserMessage(um.Message(), mo, cap)
				m.Set(msgFd, um)
			} else {
				cap.add("message", v)
			}
		case "tooluseresult":
			tur := m.NewField(turFd)
			c.toolUseResult(tur.Message(), v, cap)
			m.Set(turFd, tur)
			m.Set(hasTurFd, protoreflect.ValueOfBool(true))
		default:
			c.routeEnvelope(env, k, v, cap)
		}
	}
}

func (c *Converter) assistantLine(m protoreflect.Message, obj map[string]any, cap *capture) {
	fs := m.Descriptor().Fields()
	env := m.Mutable(fs.ByName("envelope")).Message()
	msgFd := fs.ByName("message")
	for k, v := range obj {
		switch canon(k) {
		case "type":
			continue
		case "message":
			if mo, ok := v.(map[string]any); ok {
				am := m.NewField(msgFd)
				c.apiAssistantMessage(am.Message(), mo, cap)
				m.Set(msgFd, am)
			} else {
				cap.add("message", v)
			}
		default:
			c.routeEnvelope(env, k, v, cap)
		}
	}
}

// routeEnvelope assigns one top-level key to the shared LineEnvelope.
func (c *Converter) routeEnvelope(env protoreflect.Message, k string, v any, cap *capture) {
	idx := fieldIndex(env.Descriptor())
	fd, ok := idx[canon(k)]
	if !ok {
		cap.add(joinPath("envelope", k), v)
		return
	}
	c.setField(env, fd, v, joinPath("envelope", k), cap)
}

func (c *Converter) systemLine(m protoreflect.Message, obj map[string]any, cap *capture) error {
	sub, _ := obj["subtype"].(string)
	if sub == "" {
		return fmt.Errorf("system line missing required %q field", "subtype")
	}
	env := m.Mutable(m.Descriptor().Fields().ByName("envelope")).Message()
	subFd := oneofMemberByCanon(m.Descriptor(), "subtype", canon(sub))
	if subFd == nil {
		return fmt.Errorf("unknown system subtype %q", sub)
	}
	payload := m.NewField(subFd)
	pm := payload.Message()
	pidx := fieldIndex(pm.Descriptor())
	for k, v := range obj {
		ck := canon(k)
		if ck == "type" || ck == "subtype" {
			continue
		}
		if fd, ok := pidx[ck]; ok {
			c.setField(pm, fd, v, joinPath(string(subFd.Name()), k), cap)
			continue
		}
		c.routeEnvelope(env, k, v, cap)
	}
	m.Set(subFd, payload)
	return nil
}

func (c *Converter) attachmentLine(m protoreflect.Message, obj map[string]any, cap *capture) error {
	att, ok := obj["attachment"].(map[string]any)
	if !ok {
		return fmt.Errorf("attachment line missing required %q object", "attachment")
	}
	atype, _ := att["type"].(string)
	if atype == "" {
		return fmt.Errorf("attachment missing required %q field", "type")
	}
	env := m.Mutable(m.Descriptor().Fields().ByName("envelope")).Message()
	attFd := oneofMemberByCanon(m.Descriptor(), "attachment", canon(atype))
	if attFd == nil {
		return fmt.Errorf("unknown attachment type %q", atype)
	}
	payload := m.NewField(attFd)
	// hook_non_blocking_error / hook_blocking_error wrap a shared "fields" set.
	// Any OTHER typed field the outer attachment declares (e.g.
	// HookBlockingErrorAttachment.blocking_error) is routed to the outer message;
	// the remaining keys populate the wrapped HookSuccessAttachment.
	if wrapFd := payload.Message().Descriptor().Fields().ByName("fields"); wrapFd != nil &&
		wrapFd.Kind() == protoreflect.MessageKind {
		pm := payload.Message()
		outerIdx := fieldIndex(pm.Descriptor())
		delete(outerIdx, "fields")
		inner := pm.NewField(wrapFd)
		wrapped := make(map[string]any, len(att))
		for k, v := range att {
			ck := canon(k)
			if ck == "type" {
				continue
			}
			if fd, ok := outerIdx[ck]; ok {
				c.setField(pm, fd, v, joinPath(string(attFd.Name()), k), cap)
				continue
			}
			wrapped[k] = v
		}
		c.assign(inner.Message(), wrapped, string(attFd.Name())+".fields", cap, map[string]bool{"type": true})
		pm.Set(wrapFd, inner)
	} else {
		c.fillMessage(payload.Message(), att, string(attFd.Name()), cap, map[string]bool{"type": true})
	}
	m.Set(attFd, payload)
	// Route the remaining top-level keys (the envelope).
	for k, v := range obj {
		ck := canon(k)
		if ck == "type" || ck == "attachment" {
			continue
		}
		c.routeEnvelope(env, k, v, cap)
	}
	return nil
}

// ---------------------------------------------------------------------------
// API messages & content blocks
// ---------------------------------------------------------------------------

func (c *Converter) apiUserMessage(m protoreflect.Message, obj map[string]any, cap *capture) {
	c.contentUnion(m, obj["content"], "message.content", cap)
	for k, v := range obj {
		ck := canon(k)
		if ck == "content" || msgIgnore[ck] {
			continue
		}
		cap.add(joinPath("message", k), v)
	}
}

func (c *Converter) apiAssistantMessage(m protoreflect.Message, obj map[string]any, cap *capture) {
	idx := fieldIndex(m.Descriptor())
	for k, v := range obj {
		ck := canon(k)
		if msgIgnore[ck] {
			continue
		}
		if ck == "content" {
			c.assistantContent(m, v, cap)
			continue
		}
		if fd, ok := idx[ck]; ok {
			c.setField(m, fd, v, joinPath("message", k), cap)
			continue
		}
		cap.add(joinPath("message", k), v)
	}
}

// assistantContent fills ApiAssistantMessage.content (repeated ContentBlock).
func (c *Converter) assistantContent(m protoreflect.Message, v any, cap *capture) {
	fd := m.Descriptor().Fields().ByName("content")
	arr, ok := v.([]any)
	if !ok {
		if v != nil {
			cap.add("message.content", v)
		}
		return
	}
	lv := m.Mutable(fd).List()
	for i, el := range arr {
		mo, ok := el.(map[string]any)
		if !ok {
			cap.add(fmt.Sprintf("message.content[%d]", i), el)
			continue
		}
		block := newConcrete(fd.Message())
		if c.contentBlock(block, mo, fmt.Sprintf("message.content[%d]", i), cap) {
			lv.Append(protoreflect.ValueOfMessage(block))
		}
	}
}

// contentUnion fills an ApiUserMessage / ToolResultBlock content oneof from a
// string (content_string) or an array of blocks (content_blocks).
func (c *Converter) contentUnion(m protoreflect.Message, v any, path string, cap *capture) {
	c.blockUnion(m, v, path, cap, "content_string", "content_blocks")
}

// blockUnion routes ONE string-or-blocks disk value onto a two-arm proto oneof,
// given the arm field names. The disk spells both arms with the SAME key, so the
// runtime JSON type is the only discriminator; the reflective assign cannot do
// this itself because fieldIndex deliberately excludes oneof members.
//
// blocksArm's message must carry a `repeated ContentBlock blocks` field
// (ApiContentBlocks / ToolResultBlockList).
func (c *Converter) blockUnion(m protoreflect.Message, v any, path string, cap *capture, stringArm, blocksArm string) {
	fs := m.Descriptor().Fields()
	switch t := v.(type) {
	case string:
		m.Set(fs.ByName(protoreflect.Name(stringArm)), protoreflect.ValueOfString(t))
	case []any:
		blocksFd := fs.ByName(protoreflect.Name(blocksArm))
		bl := m.NewField(blocksFd)
		listFd := bl.Message().Descriptor().Fields().ByName("blocks")
		lv := bl.Message().Mutable(listFd).List()
		for i, el := range t {
			mo, ok := el.(map[string]any)
			if !ok {
				cap.add(fmt.Sprintf("%s[%d]", path, i), el)
				continue
			}
			block := newConcrete(listFd.Message())
			if c.contentBlock(block, mo, fmt.Sprintf("%s[%d]", path, i), cap) {
				lv.Append(protoreflect.ValueOfMessage(block))
			}
		}
		m.Set(blocksFd, bl)
	case nil:
		// absent content
	default:
		cap.add(path, v)
	}
}

// contentBlock dispatches one content block by its "type" tag. Returns false only
// when the block has no usable/known type (captured, not appended).
func (c *Converter) contentBlock(block protoreflect.Message, obj map[string]any, path string, cap *capture) bool {
	typ, _ := obj["type"].(string)
	if typ == "" {
		cap.add(path, obj)
		return false
	}
	fd := oneofMemberByCanon(block.Descriptor(), "block", canon(typ))
	if fd == nil {
		// Unmodeled block type: capture the whole block verbatim (never fold it
		// into a known arm).
		cap.add(joinPath(path, typ), obj)
		return false
	}
	payload := block.NewField(fd)
	switch string(fd.Name()) {
	case "tool_result":
		c.toolResultBlock(payload.Message(), obj, path, cap)
	default:
		c.fillMessage(payload.Message(), obj, path, cap, map[string]bool{"type": true})
	}
	block.Set(fd, payload)
	return true
}

func (c *Converter) toolResultBlock(m protoreflect.Message, obj map[string]any, path string, cap *capture) {
	idx := fieldIndex(m.Descriptor())
	for k, v := range obj {
		ck := canon(k)
		if ck == "type" {
			continue
		}
		if ck == "content" {
			c.contentUnion(m, v, joinPath(path, "content"), cap)
			continue
		}
		if fd, ok := idx[ck]; ok {
			c.setField(m, fd, v, joinPath(path, k), cap)
			continue
		}
		cap.add(joinPath(path, k), v)
	}
}

// ---------------------------------------------------------------------------
// toolUseResult (string-or-object, classified per tool)
// ---------------------------------------------------------------------------

// ContentBlock converts a single bare content block object (e.g. a tool_use
// block from an assistant line). An absent/unknown "type" is a hard error.
func (c *Converter) ContentBlock(obj map[string]any) (*datav1.ContentBlock, *structpb.Struct, error) {
	typ, _ := obj["type"].(string)
	c.log.With(logging.Context{Operation: "convert-content-block"}).LogVerbose("converting content block type=%q keys=%d", typ, len(obj))
	if typ == "" {
		err := fmt.Errorf("content block missing required %q field", "type")
		return nil, nil, err
	}
	block := &datav1.ContentBlock{}
	bm := block.ProtoReflect()
	if oneofMemberByCanon(bm.Descriptor(), "block", canon(typ)) == nil {
		err := fmt.Errorf("unknown content block type %q", typ)
		return nil, nil, err
	}
	cap := c.newCapture()
	c.contentBlock(bm, obj, "block", cap)
	c.log.With(logging.Context{Operation: "convert-content-block"}).LogVerbose("converted content block type=%q extras=%t", typ, len(cap.fields) > 0)
	return block, cap.toStruct(), nil
}

// ToolUseResult exposes the toolUseResult union conversion for handler reuse.
func (c *Converter) ToolUseResult(v any) (*datav1.ToolUseResult, *structpb.Struct) {
	c.log.With(logging.Context{Operation: "convert-tool-use-result"}).LogVerbose("converting tool-use result value_type=%T", v)
	cap := c.newCapture()
	tur := &datav1.ToolUseResult{}
	c.toolUseResult(tur.ProtoReflect(), v, cap)
	c.log.With(logging.Context{Operation: "convert-tool-use-result"}).LogVerbose("converted tool-use result extras=%t", len(cap.fields) > 0)
	return tur, cap.toStruct()
}

func (c *Converter) toolUseResult(m protoreflect.Message, v any, cap *capture) {
	fs := m.Descriptor().Fields()
	// Universal caveat: a string result is an error/rejection → raw_string.
	if s, ok := v.(string); ok {
		m.Set(fs.ByName("raw_string"), protoreflect.ValueOfString(s))
		return
	}
	obj, ok := v.(map[string]any)
	if !ok {
		if v != nil {
			cap.add("toolUseResult", v)
		}
		return
	}
	arm := classifyToolResult(obj)
	if arm == "" {
		// Unclassifiable object: preserve verbatim into the unclassified Struct
		// arm and loud-log (capture, never silent classification).
		c.log.With(logging.Context{Operation: "classify-tool-use-result"}).Log("unclassified toolUseResult object keys=%v", sortedKeys(obj))
		s, err := structpb.NewStruct(obj)
		if err != nil {
			cap.add("toolUseResult", obj)
			return
		}
		m.Set(fs.ByName("unclassified"), protoreflect.ValueOfMessage(s.ProtoReflect()))
		return
	}
	fd := fs.ByName(protoreflect.Name(arm))
	payload := m.NewField(fd)
	path := "toolUseResult." + arm
	if arm == "task_output" {
		c.taskOutputResult(payload.Message(), obj, path, cap)
	} else {
		c.assign(payload.Message(), obj, path, cap, nil)
	}
	m.Set(fd, payload)
}

// taskOutputResult fills a TaskOutputResult. `retrieval_status` assigns
// generically, but `task` is a ONEOF whose arm is named by the NESTED object's
// `task_type` tag — a discriminator the generic assign cannot see, so it
// captured the entire task object into extras instead. Same dispatch shape as
// contentBlock, one level down.
func (c *Converter) taskOutputResult(m protoreflect.Message, obj map[string]any, path string, cap *capture) {
	idx := fieldIndex(m.Descriptor())
	for k, v := range obj {
		ck := canon(k)
		if ck == "task" {
			c.taskOutputTask(m, v, joinPath(path, k), cap)
			continue
		}
		if fd, ok := idx[ck]; ok {
			c.setField(m, fd, v, joinPath(path, k), cap)
			continue
		}
		cap.add(joinPath(path, k), v)
	}
}

// taskOutputTask sets the `task` oneof from the task object's task_type.
// A non-object, or a type naming no oneof member, is captured verbatim rather
// than folded into whichever arm happens to be nearest.
func (c *Converter) taskOutputTask(m protoreflect.Message, v any, path string, cap *capture) {
	task, ok := v.(map[string]any)
	if !ok {
		cap.add(path, v)
		return
	}
	typ, _ := task["task_type"].(string)
	fd := oneofMemberByCanon(m.Descriptor(), "task", canon(typ))
	if fd == nil {
		cap.add(joinPath(path, typ), task)
		return
	}
	payload := m.NewField(fd)
	c.fillMessage(payload.Message(), task, path, cap, nil)
	m.Set(fd, payload)
}

// classifyToolResult identifies the ToolUseResult arm for an object by its
// signature keys (§5.3 tools table). Ordered most-specific first; an object
// matching nothing returns "" (→ unclassified, never guessed).
func classifyToolResult(o map[string]any) string {
	has := func(k string) bool { return hasKey(o, k) }
	switch {
	case has("isAsync"):
		return "agent_async_launch"
	case has("runId"):
		return "workflow_launch"
	case has("retrievalStatus"):
		return "task_output"
	case has("stdout"):
		return "bash"
	case has("timeoutMs"):
		return "monitor"
	case has("file") && has("type"):
		return "read"
	case has("oldString"):
		return "edit"
	case has("commandName"):
		return "skill"
	case has("matches") && has("query"):
		return "tool_search"
	case has("clampedDelaySeconds") || has("scheduledFor"):
		return "schedule_wakeup"
	case has("questions") && has("answers"):
		return "ask_user_question"
	case has("searchCount") || has("durationSeconds"):
		return "web_search"
	case has("codeText") || (has("code") && has("url") && has("bytes")):
		return "web_fetch"
	case has("statusChange") || has("updatedFields"):
		return "task_update"
	case has("tasks"):
		return "task_list"
	case has("task"):
		return "task_create"
	case has("structuredPatch") && has("content") && has("filePath"):
		return "write"
	case has("pin"):
		return "send_message"
	case has("command") && (has("taskType") || has("task_type")):
		return "task_stop"
	case has("agentType") || has("toolStats") || has("totalDurationMs"):
		return "agent"
	default:
		return ""
	}
}

func hasKey(o map[string]any, k string) bool {
	ck := canon(k)
	for kk := range o {
		if canon(kk) == ck {
			return true
		}
	}
	return false
}

func sortedKeys(o map[string]any) []string {
	ks := make([]string, 0, len(o))
	for k := range o {
		ks = append(ks, k)
	}
	for i := 1; i < len(ks); i++ {
		for j := i; j > 0 && ks[j-1] > ks[j]; j-- {
			ks[j-1], ks[j] = ks[j], ks[j-1]
		}
	}
	return ks
}

// ---------------------------------------------------------------------------
// journal & meta
// ---------------------------------------------------------------------------

// JournalRecord converts one workflow-journal object (started|result).
func (c *Converter) JournalRecord(obj map[string]any) (*datav1.JournalRecord, *structpb.Struct, error) {
	typ, _ := obj["type"].(string)
	c.log.With(logging.Context{Operation: "convert-journal-record"}).LogVerbose("converting journal record type=%q keys=%d", typ, len(obj))
	if typ == "" {
		err := fmt.Errorf("journal record missing required %q field", "type")
		return nil, nil, err
	}
	rec := &datav1.JournalRecord{}
	rm := rec.ProtoReflect()
	fd := oneofMemberByCanon(rm.Descriptor(), "record", canon(typ))
	if fd == nil {
		err := fmt.Errorf("unknown journal record type %q", typ)
		return nil, nil, err
	}
	cap := c.newCapture()
	payload := rm.NewField(fd)
	pm := payload.Message()
	pidx := fieldIndex(pm.Descriptor())
	for k, v := range obj {
		ck := canon(k)
		if ck == "type" {
			continue
		}
		if ck == "result" {
			// JournalResult.result is a Struct-or-string oneof.
			fs := pm.Descriptor().Fields()
			switch t := v.(type) {
			case string:
				pm.Set(fs.ByName("result_string"), protoreflect.ValueOfString(t))
			case map[string]any:
				s, err := structpb.NewStruct(t)
				if err != nil {
					cap.add("result", v)
				} else {
					pm.Set(fs.ByName("result_object"), protoreflect.ValueOfMessage(s.ProtoReflect()))
				}
			default:
				cap.add("result", v)
			}
			continue
		}
		if pfd, ok := pidx[ck]; ok {
			c.setField(pm, pfd, v, joinPath(string(fd.Name()), k), cap)
			continue
		}
		cap.add(joinPath(string(fd.Name()), k), v)
	}
	rm.Set(fd, payload)
	c.log.With(logging.Context{Operation: "convert-journal-record"}).LogVerbose("converted journal record type=%q extras=%t", typ, len(cap.fields) > 0)
	return rec, cap.toStruct(), nil
}

// AgentMeta converts a sidechain agent-<id>.meta.json object.
func (c *Converter) AgentMeta(obj map[string]any) (*datav1.AgentMetaJson, *structpb.Struct) {
	c.log.With(logging.Context{Operation: "convert-agent-meta"}).LogVerbose("converting agent metadata keys=%d", len(obj))
	cap := c.newCapture()
	meta := &datav1.AgentMetaJson{}
	c.assign(meta.ProtoReflect(), obj, "meta", cap, nil)
	c.log.With(logging.Context{Operation: "convert-agent-meta"}).LogVerbose("converted agent metadata extras=%t", len(cap.fields) > 0)
	return meta, cap.toStruct()
}

// ---------------------------------------------------------------------------
// oneof helpers
// ---------------------------------------------------------------------------

// fillMessage populates m from obj, with two structural specializations over the
// generic assign:
//
//   - a ContentBlock is dispatched by its "type" tag (so repeated ContentBlock
//     fields like AgentResult.content classify each element instead of treating
//     the block's oneof members as unknown);
//   - a message whose ONLY field is a google.protobuf.Struct catchall ("payload"
//     / "reference"-style wrappers) absorbs the whole surrounding object when the
//     disk data does not itself carry that wrapper key (the payload-wrapper
//     attachments and ToolReferenceBlock).
func (c *Converter) fillMessage(m protoreflect.Message, obj map[string]any, path string, cap *capture, ignore map[string]bool) {
	if m.Descriptor().FullName() == "agentshim.data.v1.ContentBlock" {
		c.contentBlock(m, obj, path, cap)
		return
	}
	if m.Descriptor().FullName() == "agentshim.data.v1.QueuedCommandAttachment" {
		c.queuedCommandAttachment(m, obj, path, cap, ignore)
		return
	}
	if fd := loneStructField(m.Descriptor()); fd != nil && !hasKeyIgnoring(obj, string(fd.Name()), ignore) {
		if s, err := structpb.NewStruct(stripIgnore(obj, ignore)); err == nil {
			m.Set(fd, protoreflect.ValueOfMessage(s.ProtoReflect()))
			return
		}
	}
	c.assign(m, obj, path, cap, ignore)
}

// queuedCommandAttachment fills a QueuedCommandAttachment, routing its
// string-or-blocks `prompt` union (census 702 string / 29 blocks) onto the
// prompt_value oneof and leaving every other key to the reflective assign.
func (c *Converter) queuedCommandAttachment(m protoreflect.Message, obj map[string]any, path string, cap *capture, ignore map[string]bool) {
	rest := make(map[string]any, len(obj))
	for k, v := range obj {
		if canon(k) == "prompt" && !ignore[canon(k)] {
			c.blockUnion(m, v, joinPath(path, k), cap, "prompt", "prompt_blocks")
			continue
		}
		rest[k] = v
	}
	c.assign(m, rest, path, cap, ignore)
}

// loneStructField returns md's field when md has exactly one non-repeated
// google.protobuf.Struct field, else nil.
func loneStructField(md protoreflect.MessageDescriptor) protoreflect.FieldDescriptor {
	fs := md.Fields()
	if fs.Len() != 1 {
		return nil
	}
	fd := fs.Get(0)
	if isStruct(fd) && !fd.IsList() && fd.ContainingOneof() == nil {
		return fd
	}
	return nil
}

func hasKeyIgnoring(obj map[string]any, name string, ignore map[string]bool) bool {
	want := canon(name)
	for k := range obj {
		ck := canon(k)
		if ignore[ck] {
			continue
		}
		if ck == want {
			return true
		}
	}
	return false
}

func stripIgnore(obj map[string]any, ignore map[string]bool) map[string]any {
	if len(ignore) == 0 {
		return obj
	}
	out := make(map[string]any, len(obj))
	for k, v := range obj {
		if ignore[canon(k)] {
			continue
		}
		out[k] = v
	}
	return out
}

// oneofMemberByCanon returns the member of the named oneof whose field name
// canonically equals want, or nil.
func oneofMemberByCanon(md protoreflect.MessageDescriptor, oneof, want string) protoreflect.FieldDescriptor {
	od := md.Oneofs().ByName(protoreflect.Name(oneof))
	if od == nil {
		return nil
	}
	fs := od.Fields()
	for i := 0; i < fs.Len(); i++ {
		fd := fs.Get(i)
		if canon(string(fd.Name())) == want {
			return fd
		}
	}
	return nil
}

// newConcrete builds a fresh mutable message for md via the global type registry
// so the result is the concrete generated type (not a dynamicpb placeholder),
// keeping Any-marshaling and equality against generated messages exact.
func newConcrete(md protoreflect.MessageDescriptor) protoreflect.Message {
	return newFromRegistry(md)
}
