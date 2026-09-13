package convert

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"regexp"
	"sort"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/types/known/structpb"
)

// shape.go — THE SHAPE CATALOG OF EVERY LINE THIS SIDECAR DOES NOT STORE
// (owner ruling 2026-09-13, docs/REALTEST-JUDGEMENT-CALLS.md "the
// unmodelled-line shape catalog").
//
// Residue is classified and withheld (neverpersist.go), which takes the bytes
// out of the store and, with them, the only evidence the vendor emits that line
// at all. This file answers the second half of the ruling: the STRUCTURE is
// kept, one row per distinct recursive key structure, so the vendor's API stays
// discoverable at a cost bounded by the number of shapes rather than by traffic.
//
// THE SHAPE IS THE KEYS AND THE TYPES, NEVER THE VALUES. A value is a
// conversation; a key name and a JSON type are an API. Catalogue the second and
// the catalog cannot become a second copy of the first.

// ---------------------------------------------------------------------------
// the canonical rendering
// ---------------------------------------------------------------------------

// The scalar type names. A scalar contributes its TYPE and nothing else, so two
// lines that differ only in what a string says are one shape.
const (
	shapeString = "string"
	shapeNumber = "number"
	shapeBool   = "bool"
	shapeNull   = "null"
)

// WildcardKey replaces an object key that looks like a GENERATED ID.
//
// WITHOUT IT THE CATALOG EXPLODES. A map keyed by tool-call id or message id
// mints a brand-new key structure for every single line, which is the one
// failure mode that turns a bounded catalog back into a copy of the corpus. The
// rule is deliberately narrow (see generatedID) so a real vocabulary key is
// never mistaken for an id.
const WildcardKey = "*"

// UnparsableStructure is the rendering for bytes that are not JSON at all. They
// have no key structure to catalogue, so they share one row whose example is
// the first such line seen — which is exactly what a reader needs to see why.
const UnparsableStructure = "<unparsable>"

// generatedID reports whether an object key is a generated identifier rather
// than a name from the vendor's vocabulary.
//
// THE RULE, AND EVERY CLAUSE IS DELIBERATE:
//
//   - a uuid, in the canonical 8-4-4-4-12 hex form;
//   - a `toolu_`-prefixed id and a `msg_`-prefixed id, the vendor's own two
//     spellings;
//   - a hex run of 16 characters or more, which is what every other opaque
//     digest and id in this corpus looks like — SHORTER runs are excluded
//     because "deadbeef" and "abc" are also ordinary words;
//   - a key of nothing but digits, which is an array-shaped map's index.
//
// Everything else is a vocabulary key and is kept verbatim, because a shape
// whose key names have been guessed away says nothing about the vendor's API.
func generatedID(key string) bool {
	switch {
	case uuidKey.MatchString(key):
		return true
	case strings.HasPrefix(key, "toolu_") && len(key) > len("toolu_"):
		return true
	case strings.HasPrefix(key, "msg_") && len(key) > len("msg_"):
		return true
	case longHexKey.MatchString(key):
		return true
	case digitsKey.MatchString(key):
		return true
	default:
		return false
	}
}

var (
	uuidKey    = regexp.MustCompile(`^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$`)
	longHexKey = regexp.MustCompile(`^[0-9a-fA-F]{16,}$`)
	digitsKey  = regexp.MustCompile(`^[0-9]+$`)
)

// shape is one node of a key structure: the set of forms a value took at this
// position. It is a SET rather than a single form because an array merges its
// elements into one shape, and two elements need not agree.
type shape struct {
	// scalars is the scalar type names seen here.
	scalars map[string]bool
	// object is the union of keys seen here, each with its own merged shape.
	// Nil when this position was never an object.
	object map[string]*shape
	// array records that this position was an array, and elem is the MERGE of
	// its elements. elem is nil for an array that was always empty.
	array bool
	elem  *shape
}

// shapeOf builds the shape of one decoded JSON value.
func shapeOf(value any) *shape {
	s := &shape{}
	s.absorb(value)
	return s
}

// absorb folds one value into this shape.
func (s *shape) absorb(value any) {
	switch v := value.(type) {
	case map[string]any:
		if s.object == nil {
			s.object = map[string]*shape{}
		}
		for key, child := range v {
			name := key
			if generatedID(key) {
				name = WildcardKey
			}
			// A WILDCARD MERGES, it does not overwrite: every id-keyed entry of
			// the same map is the same position, and their shapes are unioned
			// exactly as an array's elements are.
			existing, ok := s.object[name]
			if !ok {
				existing = &shape{}
				s.object[name] = existing
			}
			existing.absorb(child)
		}
	case []any:
		s.array = true
		for _, item := range v {
			if s.elem == nil {
				s.elem = &shape{}
			}
			s.elem.absorb(item)
		}
	case string:
		s.scalar(shapeString)
	case float64, int, int64:
		s.scalar(shapeNumber)
	case bool:
		s.scalar(shapeBool)
	case nil:
		s.scalar(shapeNull)
	default:
		// A DECODER THAT PRODUCED SOMETHING ELSE IS STILL CATALOGUED rather than
		// dropped: encoding/json produces only the cases above, so this arm
		// means the input came from somewhere else and the honest rendering says
		// so instead of pretending the value was absent.
		s.scalar("unknown")
	}
}

func (s *shape) scalar(name string) {
	if s.scalars == nil {
		s.scalars = map[string]bool{}
	}
	s.scalars[name] = true
}

// render writes the canonical form: `{key:shape,...}` for an object with its
// keys SORTED, `[shape]` for an array, the type name for a scalar, and the
// forms joined by `|` when one position took several. Sorting is what makes the
// rendering independent of the order the vendor happened to write its keys in.
func (s *shape) render(out *strings.Builder) {
	var parts []string
	if s.object != nil {
		var b strings.Builder
		b.WriteByte('{')
		keys := make([]string, 0, len(s.object))
		for key := range s.object {
			keys = append(keys, key)
		}
		sort.Strings(keys)
		for i, key := range keys {
			if i > 0 {
				b.WriteByte(',')
			}
			b.WriteString(key)
			b.WriteByte(':')
			s.object[key].render(&b)
		}
		b.WriteByte('}')
		parts = append(parts, b.String())
	}
	if s.array {
		var b strings.Builder
		b.WriteByte('[')
		if s.elem != nil {
			s.elem.render(&b)
		}
		b.WriteByte(']')
		parts = append(parts, b.String())
	}
	names := make([]string, 0, len(s.scalars))
	for name := range s.scalars {
		names = append(names, name)
	}
	sort.Strings(names)
	parts = append(parts, names...)
	// A value that was never anything at all renders as the empty object rather
	// than as nothing, so a rendering is never an empty string and never hashes
	// to the same thing as a missing one.
	if len(parts) == 0 {
		parts = []string{"{}"}
	}
	sort.Strings(parts)
	out.WriteString(strings.Join(parts, "|"))
}

// KeyStructure renders one decoded JSON value's canonical key structure.
func KeyStructure(value any) string {
	var b strings.Builder
	shapeOf(value).render(&b)
	return b.String()
}

// ShapeHash is SHA-256 over the canonical rendering, lowercase hex. It is the
// catalog's primary key, so two renderings are the same row exactly when their
// bytes agree.
func ShapeHash(structure string) string {
	sum := sha256.Sum256([]byte(structure))
	return hex.EncodeToString(sum[:])
}

// ---------------------------------------------------------------------------
// the observation a withheld entry contributes
// ---------------------------------------------------------------------------

// ResidueShape builds the catalog observation for one withheld residue entry,
// or reports false for an entry that carries no residue arm.
//
// THE KIND IS THE WITHHELD TALLY'S OWN LABEL (ResidueLabel), so the catalog and
// the per-file summary name the same thing and a `--kind` filter matches what
// the log said.
//
// THE EXAMPLE IS THE RESIDUE ARM'S OWN PAYLOAD. For `unparsed` that IS the
// vendor's bytes, which is what the arm stores. For `vendor_specific` and
// `unknown` it is the decoded payload re-serialized as compact JSON: this door
// sits above the converter and the frame's original bytes are no longer in
// hand, so the example agrees with the vendor's line in every key and value and
// differs only in key order and whitespace. Nothing is added and nothing is
// dropped.
func ResidueShape(e *storev1.StoreEntry, seenMs int64) (*storev1.ShapeObservation, bool) {
	arm := e.GetAgentUpdate().GetUnservedItem().GetUnservedItem()
	var (
		decoded any
		example []byte
		ok      bool
	)
	switch a := arm.(type) {
	case *storev1.StoreUnservedItem_VendorSpecific:
		decoded, example, ok = a.VendorSpecific.GetRaw().AsMap(), rawJSON(a.VendorSpecific.GetRaw()), true
	case *storev1.StoreUnservedItem_Unknown:
		decoded, example, ok = a.Unknown.GetRaw().AsMap(), rawJSON(a.Unknown.GetRaw()), true
	case *storev1.StoreUnservedItem_Unparsed:
		raw := a.Unparsed.GetRaw()
		example = []byte(raw)
		// BYTES THAT ARE NOT JSON HAVE NO KEY STRUCTURE. They share one row
		// whose example is the first such line, which is what a reader needs to
		// see why the parse failed.
		if err := json.Unmarshal([]byte(raw), &decoded); err != nil {
			return &storev1.ShapeObservation{
				ShapeHash:    ShapeHash(UnparsableStructure),
				Kind:         ResidueLabel(e),
				KeyStructure: UnparsableStructure,
				FirstExample: example,
				SeenMs:       seenMs,
			}, true
		}
		ok = true
	}
	if !ok {
		return nil, false
	}
	structure := KeyStructure(decoded)
	return &storev1.ShapeObservation{
		ShapeHash:    ShapeHash(structure),
		Kind:         ResidueLabel(e),
		KeyStructure: structure,
		FirstExample: example,
		SeenMs:       seenMs,
	}, true
}

// rawJSON renders a residue arm's payload as compact JSON.
//
// IT IS COMPACTED ON PURPOSE: protojson deliberately varies its whitespace
// between calls, and an example that changed byte for byte between two runs of
// the same line would make the catalog's one piece of evidence unreproducible.
//
// A MARSHAL FAILURE YIELDS NO EXAMPLE RATHER THAN A WRONG ONE. The shape is
// still catalogued from the decoded map, so the row survives without the one
// field that could not be rendered.
func rawJSON(raw *structpb.Struct) []byte {
	if raw == nil {
		return nil
	}
	encoded, err := protojson.Marshal(raw)
	if err != nil {
		return nil
	}
	var buf bytes.Buffer
	if err := json.Compact(&buf, encoded); err != nil {
		return encoded
	}
	return buf.Bytes()
}
