package reuse

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strings"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/descriptorpb"
)

// THE GENERATED PROTOBUF CODE IS READ BY ITS SYMBOLS, NEVER BY ITS BYTES
// (owner ruling, 2026-11-13). A .proto edit regenerates whole files, and
// hashing them re-ran every unit whose build graph reached a binding though
// most of them name nothing that changed (a comment-only edit re-ran all 333
// suite units of the 2026-11-13 run). A unit's key instead holds the
// fingerprint of every message, enum and service its own code names, closed
// over what each of them contains, read from the descriptors with no source
// info -- so a comment changes no fingerprint, and a symbol's change re-runs
// exactly the units that name it, or name something holding it.
//
// THE NAMING IS OVER-APPROXIMATED, NEVER UNDER. A symbol is named when any
// identifier of a unit's non-generated input is one of its generated names,
// or begins with one followed by "_" (a oneof arm, a nested type, an enum
// value), so a mention in a comment counts. Code that names a whole
// generated file (its File_ or file_ descriptor) names every symbol of the
// file, and code that walks the global registry names every symbol the unit
// links -- that of every generated file it reads, which is all a registry
// holds.

// Bindings is the index of the generated protobuf code's symbols: where the
// generated code lives, and each symbol's names, fingerprint and contents.
type Bindings struct {
	// Roots are the repository-relative directories of generated code, each
	// ending in "/".
	Roots []string
	// symbols is every symbol, by index.
	symbols []bindingSymbol
	// byToken maps each generated name to the symbols it names.
	byToken map[string][]int
	// byFull maps each symbol's full name to its index.
	byFull map[string]int
	// byProto maps each .proto file to the symbols it declares, which a
	// registry walk over a generated file of it reaches.
	byProto map[string][]int
}

// bindingSymbol is one message, enum or service.
type bindingSymbol struct {
	full        string
	fingerprint string
	// refs are the full names of the symbols it contains: a message's field
	// types, a service's request and response types.
	refs []string
}

// globalTokens are the identifiers of code that walks every registered
// descriptor.
var globalTokens = []string{"protoregistry", "protodesc", "GlobalFiles", "GlobalTypes"}

// NewBindings indexes SET's files, whose generated code lives under ROOTS.
func NewBindings(roots []string, set *descriptorpb.FileDescriptorSet) (*Bindings, error) {
	b := &Bindings{Roots: roots, byToken: map[string][]int{}, byFull: map[string]int{}, byProto: map[string][]int{}}
	for _, f := range set.GetFile() {
		pkg := f.GetPackage()
		first := len(b.symbols)
		for _, m := range f.GetMessageType() {
			if err := b.addMessage(pkg, nil, m); err != nil {
				return nil, err
			}
		}
		for _, e := range f.GetEnumType() {
			if err := b.addEnum(pkg, nil, e); err != nil {
				return nil, err
			}
		}
		for _, s := range f.GetService() {
			if err := b.addService(pkg, s); err != nil {
				return nil, err
			}
		}
		// THE FILE'S OWN DESCRIPTOR names every symbol it declares.
		base := strings.TrimSuffix(f.GetName(), ".proto")
		var inFile []int
		for i := first; i < len(b.symbols); i++ {
			inFile = append(inFile, i)
		}
		for _, token := range []string{"File_" + goSanitized(base) + "_proto", "file_" + strings.NewReplacer("/", "_", "-", "_", ".", "_").Replace(base)} {
			b.byToken[token] = append(b.byToken[token], inFile...)
		}
		b.byProto[f.GetName()] = inFile
	}
	return b, nil
}

// add records one symbol under its full name and TOKENS.
func (b *Bindings) add(full string, desc proto.Message, refs []string, tokens ...string) error {
	raw, err := proto.MarshalOptions{Deterministic: true}.Marshal(desc)
	if err != nil {
		return fmt.Errorf("reuse: fingerprint %s: %w", full, err)
	}
	sum := sha256.Sum256(raw)
	i := len(b.symbols)
	b.symbols = append(b.symbols, bindingSymbol{full: full, fingerprint: hex.EncodeToString(sum[:]), refs: refs})
	b.byFull[full] = i
	seen := map[string]bool{}
	for _, t := range tokens {
		if t != "" && !seen[t] {
			seen[t] = true
			b.byToken[t] = append(b.byToken[t], i)
		}
	}
	return nil
}

// names is a nested symbol's generated names: the Go spelling and the
// TypeScript one (its proto names joined by "_"), and the latter's Schema.
func names(path []string) []string {
	goParts := make([]string, len(path))
	for i, p := range path {
		goParts[i] = goCamelCase(p)
	}
	goName, tsName := strings.Join(goParts, "_"), strings.Join(path, "_")
	return []string{goName, tsName, tsName + "Schema", goName + "Schema"}
}

func (b *Bindings) addMessage(pkg string, outer []string, m *descriptorpb.DescriptorProto) error {
	path := append(append([]string{}, outer...), m.GetName())
	full := pkg + "." + strings.Join(path, ".")
	var refs []string
	for _, f := range m.GetField() {
		if t := f.GetTypeName(); t != "" {
			refs = append(refs, strings.TrimPrefix(t, "."))
		}
	}
	for _, n := range m.GetNestedType() {
		refs = append(refs, full+"."+n.GetName())
	}
	for _, e := range m.GetEnumType() {
		refs = append(refs, full+"."+e.GetName())
	}
	if err := b.add(full, m, refs, names(path)...); err != nil {
		return err
	}
	for _, n := range m.GetNestedType() {
		if err := b.addMessage(pkg, path, n); err != nil {
			return err
		}
	}
	for _, e := range m.GetEnumType() {
		if err := b.addEnum(pkg, path, e); err != nil {
			return err
		}
	}
	return nil
}

func (b *Bindings) addEnum(pkg string, outer []string, e *descriptorpb.EnumDescriptorProto) error {
	path := append(append([]string{}, outer...), e.GetName())
	return b.add(pkg+"."+strings.Join(path, "."), e, nil, names(path)...)
}

func (b *Bindings) addService(pkg string, s *descriptorpb.ServiceDescriptorProto) error {
	name := s.GetName()
	tokens := []string{name, name + "Client", name + "Handler", "New" + name + "Client", "New" + name + "Handler",
		name + "Name", "Unimplemented" + name + "Handler"}
	var refs []string
	for _, m := range s.GetMethod() {
		tokens = append(tokens, name+m.GetName()+"Procedure")
		refs = append(refs, strings.TrimPrefix(m.GetInputType(), "."), strings.TrimPrefix(m.GetOutputType(), "."))
	}
	return b.add(pkg+"."+name, s, refs, tokens...)
}

// Bound reports whether the repository-relative path rel is generated code.
func (b *Bindings) Bound(rel string) bool {
	for _, root := range b.Roots {
		if strings.HasPrefix(rel, root) {
			return true
		}
	}
	return false
}

// identifier is one identifier of source text.
var identifier = regexp.MustCompile(`[A-Za-z_][A-Za-z0-9_]*`)

// Named is the symbols TEXT names, as indices: each identifier that is a
// generated name, and each that begins with one followed by "_"; and whether
// it walks the global registry, which names every symbol its unit links
// (Linked).
func (b *Bindings) Named(text []byte) ([]int, bool) {
	hit := map[int]bool{}
	global := false
	for _, raw := range identifier.FindAll(text, -1) {
		token := string(raw)
		if slices.Contains(globalTokens, token) {
			global = true
		}
		for _, i := range b.byToken[token] {
			hit[i] = true
		}
		for at := strings.IndexByte(token, '_'); at > 0; {
			for _, i := range b.byToken[token[:at]] {
				hit[i] = true
			}
			next := strings.IndexByte(token[at+1:], '_')
			if next < 0 {
				break
			}
			at += next + 1
		}
	}
	out := make([]int, 0, len(hit))
	for i := range hit {
		out = append(out, i)
	}
	sort.Ints(out)
	return out, global
}

// Linked is the symbols of the .proto file the generated file at the
// repository-relative path GEN was generated from (its `.pb.go`,
// `.connect.go` or `_pb.ts`), none for a file no .proto generated.
func (b *Bindings) Linked(gen string) []int {
	for _, root := range b.Roots {
		rest, ok := strings.CutPrefix(gen, root)
		if !ok {
			continue
		}
		_, rest, _ = strings.Cut(rest, "/")
		dir, name := path.Split(rest)
		if strings.HasSuffix(name, ".connect.go") {
			dir = path.Dir(strings.TrimSuffix(dir, "/")) + "/"
		}
		for _, suffix := range []string{".pb.go", ".connect.go", "_pb.ts"} {
			if stem, ok := strings.CutSuffix(name, suffix); ok {
				return b.byProto[strings.TrimPrefix(dir+stem+".proto", "./")]
			}
		}
	}
	return nil
}

// Fingerprints is every symbol NAMED reaches, closed over what each
// contains, by full name.
func (b *Bindings) Fingerprints(named []int) map[string]string {
	out := map[string]string{}
	queue := append([]int{}, named...)
	for len(queue) > 0 {
		i := queue[0]
		queue = queue[1:]
		s := b.symbols[i]
		if _, done := out[s.full]; done {
			continue
		}
		out[s.full] = s.fingerprint
		for _, ref := range s.refs {
			if j, ok := b.byFull[ref]; ok {
				queue = append(queue, j)
			}
		}
	}
	return out
}

// goCamelCase is protoc-gen-go's GoCamelCase: the Go spelling of a proto
// name.
func goCamelCase(s string) string {
	var out []byte
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c == '.' && i+1 < len(s) && isLower(s[i+1]):
		case c == '.':
			out = append(out, '_')
		case c == '_' && (i == 0 || s[i-1] == '.'):
			out = append(out, 'X')
		case c == '_' && i+1 < len(s) && isLower(s[i+1]):
		case c >= '0' && c <= '9':
			out = append(out, c)
		default:
			if isLower(c) {
				c -= 'a' - 'A'
			}
			out = append(out, c)
			for ; i+1 < len(s) && isLower(s[i+1]); i++ {
				out = append(out, s[i+1])
			}
		}
	}
	return string(out)
}

func isLower(c byte) bool { return c >= 'a' && c <= 'z' }

// goSanitized is protoc-gen-go's spelling of a file path in its File_ var.
func goSanitized(path string) string {
	return strings.Map(func(r rune) rune {
		if r == '_' || r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || r >= '0' && r <= '9' {
			return r
		}
		return '_'
	}, path)
}

// ExecBindings indexes the module's protobuf sources (MODULE/proto/src) by
// asking protoc for their descriptors with no source info, the generated
// code living under MODULE/proto/gen/ (both repository-relative through
// REPO).
func ExecBindings(repo, module string) (*Bindings, error) {
	src := filepath.Join(module, "proto", "src")
	var files []string
	err := filepath.WalkDir(src, func(p string, e os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !e.IsDir() && strings.HasSuffix(p, ".proto") {
			rel, err := filepath.Rel(src, p)
			if err != nil {
				return err
			}
			files = append(files, filepath.ToSlash(rel))
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("reuse: list the protobuf sources under %s: %w", src, err)
	}
	sort.Strings(files)
	out, err := os.CreateTemp("", "testrun-descriptors-*.pb")
	if err != nil {
		return nil, fmt.Errorf("reuse: make the descriptor set's file: %w", err)
	}
	out.Close()
	defer os.Remove(out.Name())
	args := append([]string{"-I", src, "--descriptor_set_out=" + out.Name()}, files...)
	if msg, err := exec.Command("protoc", args...).CombinedOutput(); err != nil {
		return nil, fmt.Errorf("reuse: protoc %s: %w: %s", src, err, strings.TrimSpace(string(msg)))
	}
	raw, err := os.ReadFile(out.Name())
	if err != nil {
		return nil, fmt.Errorf("reuse: read the descriptor set: %w", err)
	}
	set := &descriptorpb.FileDescriptorSet{}
	if err := proto.Unmarshal(raw, set); err != nil {
		return nil, fmt.Errorf("reuse: parse the descriptor set: %w", err)
	}
	gen, err := filepath.Rel(repo, filepath.Join(module, "proto", "gen"))
	if err != nil {
		return nil, err
	}
	return NewBindings([]string{filepath.ToSlash(gen) + "/"}, set)
}

// bindingUse is what one key's inputs did with the generated code: the
// generated files they read, the symbols their other files name, and
// whether one of them walks the global registry.
type bindingUse struct {
	bound  []string
	named  map[int]bool
	global bool
}

// noteNamed records the symbols file P names, through the cache.
func (h hasher) noteNamed(p string, info os.FileInfo) error {
	if h.bindings == nil {
		return nil
	}
	named, err := cachedRead(h, p, info, func(c *cachedDigest) (namedSymbols, bool) { return c.named, c.namedRead },
		func(c *cachedDigest, v namedSymbols) { c.named, c.namedRead = v, true },
		func(data []byte) namedSymbols {
			ids, global := h.bindings.Named(data)
			return namedSymbols{ids: ids, global: global}
		})
	if err != nil {
		return err
	}
	for _, i := range named.ids {
		h.use.named[i] = true
	}
	h.use.global = h.use.global || named.global
	return nil
}

// generatorLine is a generated file's statement of what generated it.
var generatorLine = regexp.MustCompile(`(?m)^//.*\b(protoc-gen-[a-z-]+|protoc)\s+v?[0-9][0-9A-Za-z.\-]*.*$`)

// addBindings adds, for a key that read generated code, the fingerprint of
// every symbol its other inputs name (closed over their contents), and the
// generators the generated code states: a new generator re-runs every
// unit that reads its output.
func (h hasher) addBindings() error {
	if h.bindings == nil || len(h.use.bound) == 0 {
		return nil
	}
	// A REGISTRY WALK reaches every symbol the unit links: those of every
	// generated file it reads.
	if h.use.global {
		for _, p := range h.use.bound {
			for _, i := range h.bindings.Linked(p) {
				h.use.named[i] = true
			}
		}
	}
	named := make([]int, 0, len(h.use.named))
	for i := range h.use.named {
		named = append(named, i)
	}
	for full, fp := range h.bindings.Fingerprints(named) {
		h.m["@proto "+full] = fp
	}
	seen := map[string]bool{}
	for _, p := range h.use.bound {
		info, err := os.Stat(h.abs(p))
		if err != nil {
			return fmt.Errorf("reuse: stat %s: %w", p, err)
		}
		lines, err := cachedRead(h, p, info, func(c *cachedDigest) ([]string, bool) { return c.generators, c.generatorsRead },
			func(c *cachedDigest, v []string) { c.generators, c.generatorsRead = v, true },
			func(data []byte) []string {
				head := data
				if len(head) > 4096 {
					head = head[:4096]
				}
				var out []string
				for _, l := range generatorLine.FindAll(head, -1) {
					out = append(out, strings.TrimSpace(string(l)))
				}
				return out
			})
		if err != nil {
			return err
		}
		for _, l := range lines {
			seen[l] = true
		}
	}
	generators := make([]string, 0, len(seen))
	for l := range seen {
		generators = append(generators, l)
	}
	sort.Strings(generators)
	h.m["@proto-generators"] = strings.Join(generators, "\n")
	return nil
}

// cachedRead answers what READ makes of file P's content, through the cache
// under the file's identity when there is one.
func cachedRead[T any](h hasher, p string, info os.FileInfo, get func(*cachedDigest) (T, bool), set func(*cachedDigest, T), read func([]byte) T) (T, error) {
	id := fileIdentity(info)
	if h.cache != nil {
		h.cache.mu.Lock()
		c, ok := h.cache.m[p]
		if ok && c.id == id {
			if v, done := get(&c); done {
				h.cache.mu.Unlock()
				return v, nil
			}
		}
		h.cache.mu.Unlock()
	}
	data, err := os.ReadFile(h.abs(p))
	if err != nil {
		var zero T
		return zero, fmt.Errorf("reuse: read %s: %w", p, err)
	}
	v := read(data)
	if h.cache != nil {
		h.cache.mu.Lock()
		c, ok := h.cache.m[p]
		if !ok || c.id != id {
			c = cachedDigest{id: id}
		}
		set(&c, v)
		h.cache.m[p] = c
		h.cache.mu.Unlock()
	}
	return v, nil
}

// namedSymbols is what one file names: symbols, and a registry walk.
type namedSymbols struct {
	ids    []int
	global bool
}
