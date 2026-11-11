package reuse

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"syscall"
)

// KeyVersion is part of every key. Changing what a key covers, or how it is
// spelled, bumps it, so no record made under the old meaning is ever reused.
const KeyVersion = "reuse-key-v2"

// Facts are the non-file inputs of a suite: everything besides the checkout
// that decides what its tests observe.
type Facts struct {
	// Suite describes the suite's roster entry: how it is built and run.
	Suite string
	// Toolchain names each tool the suite is built and run with and its
	// version (go, node, emacs, the OS and architecture).
	Toolchain map[string]string
	// Env is every environment variable the suite's units are given, with its
	// value (Confine gives them nothing else). Run-scoped variables, whose
	// values name this run's own directories, are absent: their value is not
	// something a test can depend on, only where it puts things.
	Env map[string]string
}

// Manifest is a key's itemized content: one digest per hashed entry
// (compacted, see compact), so two keys can be compared entry by entry to say
// WHAT changed.
type Manifest map[string]string

// Key is a suite's input key and the manifest it was computed from.
type Key struct {
	Digest   string
	Manifest Manifest
}

// Short is the key's first twelve hex digits, for a log line.
func (k Key) Short() string {
	if len(k.Digest) < 12 {
		return k.Digest
	}
	return k.Digest[:12]
}

// ComputeKey hashes facts and every entry of set under repo. A placed path
// that does not exist is an error: a declared input that is not there is a
// roster that has drifted from the tree, never something to hash around.
func ComputeKey(repo string, set *InputSet, facts Facts) (Key, error) {
	return ComputeKeyCached(repo, set, facts, nil)
}

// ComputeKeyCached is ComputeKey reading each file's digest through cache
// (nil reads every file): the units of one run share most of their inputs,
// and hashing each shared file once per unit cost a run seconds.
func ComputeKeyCached(repo string, set *InputSet, facts Facts, cache *HashCache) (Key, error) {
	if err := set.Overlap(); err != nil {
		return Key{}, err
	}
	m := Manifest{}
	m["@suite"] = facts.Suite
	for name, v := range facts.Toolchain {
		m["@tool "+name] = v
	}
	for name, v := range facts.Env {
		m["@env "+name] = v
	}
	h := hasher{repo: repo, generated: set.Generated(), m: m, cache: cache}
	for _, p := range set.Files() {
		if err := h.file(p); err != nil {
			return Key{}, err
		}
	}
	for _, p := range set.Dirs() {
		if err := h.listing(p); err != nil {
			return Key{}, err
		}
	}
	for _, p := range set.FlatDirs() {
		if err := h.flat(p); err != nil {
			return Key{}, err
		}
	}
	for _, p := range set.Trees() {
		if err := h.tree(p); err != nil {
			return Key{}, err
		}
	}
	return Key{Digest: m.digest(), Manifest: m.compact()}, nil
}

// compactValueLen is how many hex digits of an entry's value a stored
// manifest keeps.
const compactValueLen = 16

// compact is m with each value replaced by the first compactValueLen hex
// digits of its SHA-256: enough to tell an entry changed, a fraction of a
// directory listing's or a file line's length. The key's digest is over the
// full values; a manifest is only ever compared with another compacted one.
func (m Manifest) compact() Manifest {
	out := make(Manifest, len(m))
	for name, v := range m {
		sum := sha256.Sum256([]byte(v))
		out[name] = hex.EncodeToString(sum[:])[:compactValueLen]
	}
	return out
}

// digest is the key: a hash over the manifest's sorted lines.
func (m Manifest) digest() string {
	names := make([]string, 0, len(m))
	for name := range m {
		names = append(names, name)
	}
	sort.Strings(names)
	sum := sha256.New()
	fmt.Fprintf(sum, "%s\n", KeyVersion)
	for _, name := range names {
		// Lengths first, so no choice of names and values can collide.
		fmt.Fprintf(sum, "%d:%s=%d:%s\n", len(name), name, len(m[name]), m[name])
	}
	return hex.EncodeToString(sum.Sum(nil))
}

// Changed lists the entries that differ between an earlier manifest and m,
// sorted: added, removed and altered alike.
func (m Manifest) Changed(earlier Manifest) []string {
	var out []string
	for name, v := range m {
		if old, ok := earlier[name]; !ok || old != v {
			out = append(out, name)
		}
	}
	for name := range earlier {
		if _, ok := m[name]; !ok {
			out = append(out, name)
		}
	}
	sort.Strings(out)
	return out
}

type hasher struct {
	repo      string
	generated []string
	m         Manifest
	cache     *HashCache
}

// HashCache holds the digests of the files one phase of a run has read,
// each under the file's identity when it was read (its size, modification
// time, mode and inode), so a file that changed is read again. One cache
// lives for one phase: the decision's keys, or the re-keys after the run,
// never both, so a re-key reads every input afresh.
type HashCache struct {
	mu sync.Mutex
	m  map[string]cachedDigest
}

type cachedDigest struct {
	id     string
	digest string
}

// NewHashCache is an empty cache.
func NewHashCache() *HashCache { return &HashCache{m: map[string]cachedDigest{}} }

// fileIdentity is what a cached digest is held under.
func fileIdentity(info fs.FileInfo) string {
	id := fmt.Sprintf("%d %d %o", info.Size(), info.ModTime().UnixNano(), info.Mode())
	if st, ok := info.Sys().(*syscall.Stat_t); ok {
		id += fmt.Sprintf(" %d %d", st.Dev, st.Ino)
	}
	return id
}

// digest is the file p's content digest, through the cache when there is one.
func (h hasher) digest(p string, info fs.FileInfo) (string, error) {
	id := fileIdentity(info)
	if h.cache != nil {
		h.cache.mu.Lock()
		c, ok := h.cache.m[p]
		h.cache.mu.Unlock()
		if ok && c.id == id {
			return c.digest, nil
		}
	}
	data, err := os.ReadFile(h.abs(p))
	if err != nil {
		return "", fmt.Errorf("reuse: read %s: %w", p, err)
	}
	sum := sha256.Sum256(data)
	d := hex.EncodeToString(sum[:])
	if h.cache != nil {
		h.cache.mu.Lock()
		h.cache.m[p] = cachedDigest{id: id, digest: d}
		h.cache.mu.Unlock()
	}
	return d, nil
}

func (h hasher) abs(p string) string { return filepath.Join(h.repo, filepath.FromSlash(p)) }

func (h hasher) file(p string) error {
	info, err := os.Lstat(h.abs(p))
	if err != nil {
		return fmt.Errorf("reuse: input %s: %w", p, err)
	}
	return h.entry(p, info)
}

// entry hashes one placed path by its kind.
func (h hasher) entry(p string, info fs.FileInfo) error {
	switch {
	case info.Mode()&fs.ModeSymlink != 0:
		target, err := os.Readlink(h.abs(p))
		if err != nil {
			return fmt.Errorf("reuse: read the link %s: %w", p, err)
		}
		h.m[p] = "link " + target
	case info.Mode().IsRegular():
		d, err := h.digest(p, info)
		if err != nil {
			return err
		}
		// The executable bit is part of what a test can observe.
		h.m[p] = fmt.Sprintf("file %s %o", d, info.Mode().Perm()&0o111)
	default:
		return fmt.Errorf("reuse: input %s is neither a regular file nor a link (%s)", p, info.Mode())
	}
	return nil
}

// listing hashes the names in directory p, each with its kind.
//
// A GENERATED ENTRY IS NOT LISTED: the run itself creates and deletes those
// trees (an install, a build, a tool's cache), so whether one exists says
// nothing about the suite's inputs, and listing it would move the key the
// moment a run first creates it.
func (h hasher) listing(p string) error {
	entries, err := os.ReadDir(h.abs(p))
	if err != nil {
		return fmt.Errorf("reuse: list %s: %w", p, err)
	}
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		if h.isGenerated(path.Join(p, e.Name())) {
			continue
		}
		kind := "f"
		switch {
		case e.Type()&fs.ModeSymlink != 0:
			kind = "l"
		case e.IsDir():
			kind = "d"
		}
		names = append(names, kind+":"+e.Name())
	}
	sort.Strings(names)
	h.m[p+"/"] = "dir " + strings.Join(names, " ")
	return nil
}

// isGenerated reports whether the repository-relative path rel lies in one of
// the set's generated subtrees.
func (h hasher) isGenerated(rel string) bool {
	for _, g := range h.generated {
		if under(rel, g) {
			return true
		}
	}
	return false
}

// flat hashes a directory's listing and every non-directory entry in it.
func (h hasher) flat(p string) error {
	if err := h.listing(p); err != nil {
		return err
	}
	entries, err := os.ReadDir(h.abs(p))
	if err != nil {
		return fmt.Errorf("reuse: list %s: %w", p, err)
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		info, err := e.Info()
		if err != nil {
			return fmt.Errorf("reuse: stat %s/%s: %w", p, e.Name(), err)
		}
		if err := h.entry(p+"/"+e.Name(), info); err != nil {
			return err
		}
	}
	return nil
}

// tree hashes every entry beneath p, skipping the generated subtrees in it.
func (h hasher) tree(p string) error {
	info, err := os.Lstat(h.abs(p))
	if err != nil {
		return fmt.Errorf("reuse: input tree %s: %w", p, err)
	}
	if !info.IsDir() {
		return fmt.Errorf("reuse: input tree %s is not a directory", p)
	}
	return filepath.WalkDir(h.abs(p), func(abs string, d fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("reuse: walk %s: %w", abs, err)
		}
		rel, err := filepath.Rel(h.repo, abs)
		if err != nil {
			return fmt.Errorf("reuse: place %s under %s: %w", abs, h.repo, err)
		}
		rel = filepath.ToSlash(rel)
		if h.isGenerated(rel) {
			if d.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}
		if d.IsDir() {
			return h.listing(rel)
		}
		info, err := d.Info()
		if err != nil {
			return fmt.Errorf("reuse: stat %s: %w", rel, err)
		}
		return h.entry(path.Clean(rel), info)
	})
}
