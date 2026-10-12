package buildid

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// FileCached answers a file's content hash, as File does, remembering it in a
// sidecar `<path>.buildid` keyed by the file's identity so an unchanged file
// is not hashed again.
//
// WHY. The daemon hashes its own ~49MB binary at every boot (the build a
// deploy compares against): profiled, that was 31% of a booting daemon's CPU,
// paid by every test fixture's daemon and every real restart alike.
//
// WHY IT IS HONEST. The cached hash is used only while the file's identity --
// device, inode, size, modification time and CHANGE time, to the nanosecond --
// is exactly the one the hash was taken under. Rewriting the file in place
// moves its change time, which no unprivileged process can set back, and
// replacing it by rename gives it a new inode; either way the identity no
// longer matches and the file is hashed afresh. The answer is therefore
// always the content hash of the bytes the path names now, which is what
// File answers.
//
// The sidecar is a cache, never a source of truth: one that is missing,
// unreadable or for another identity is ignored and rewritten, and one that
// cannot be written (a read-only directory) leaves the freshly taken hash
// standing. Only a failure to hash the file itself is an error.
func FileCached(path string) (string, error) {
	id, err := identityOf(path)
	if err != nil {
		return "", fmt.Errorf("buildid: identify %s: %w", path, err)
	}
	sidecar := path + ".buildid"
	if hash, ok := readSidecar(sidecar, id); ok {
		return hash, nil
	}
	hash, err := File(path)
	if err != nil {
		return "", err
	}
	// The hash names the bytes read under THIS identity only if the file did
	// not change while it was read; a file that moved underneath is answered
	// as hashed, but not remembered.
	if after, err := identityOf(path); err == nil && after == id {
		writeSidecar(sidecar, id, hash)
	}
	return hash, nil
}

// readSidecar answers the cached hash when the sidecar names exactly id.
func readSidecar(sidecar, id string) (string, bool) {
	raw, err := os.ReadFile(sidecar)
	if err != nil {
		return "", false
	}
	line := strings.TrimSuffix(string(raw), "\n")
	recorded, hash, ok := strings.Cut(line, " ")
	if !ok || recorded != id || len(hash) != 64 || strings.Trim(hash, "0123456789abcdef") != "" {
		return "", false
	}
	return hash, true
}

// writeSidecar records hash under id, atomically (a reader sees the old
// sidecar or the new one, never half of one). A sidecar that cannot be
// written is not an error: the cache is an optimization over File.
func writeSidecar(sidecar, id, hash string) {
	tmp, err := os.CreateTemp(filepath.Dir(sidecar), filepath.Base(sidecar)+".tmp-*")
	if err != nil {
		return
	}
	name := tmp.Name()
	_, werr := tmp.WriteString(id + " " + hash + "\n")
	cerr := tmp.Close()
	if werr != nil || cerr != nil || os.Rename(name, sidecar) != nil {
		_ = os.Remove(name)
	}
}

// errNotRegular refuses to cache the hash of anything but a regular file.
var errNotRegular = errors.New("not a regular file")

// identityOf answers the file's identity as one string: device, inode, size,
// modification and change time, nanoseconds.
func identityOf(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "", err
	}
	if !info.Mode().IsRegular() {
		return "", errNotRegular
	}
	return statIdentity(info)
}

// statIdentityFrom formats the fields statIdentity reads off the platform's
// stat record.
func statIdentityFrom(dev, ino uint64, size int64, mtimeSec, mtimeNsec, ctimeSec, ctimeNsec int64) string {
	return fmt.Sprintf("%d:%d:%d:%d.%09d:%d.%09d", dev, ino, size, mtimeSec, mtimeNsec, ctimeSec, ctimeNsec)
}

// errNoStat refuses a FileInfo that carries no platform stat record.
var errNoStat = fs.ErrInvalid
