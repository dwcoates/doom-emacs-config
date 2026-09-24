package livelock

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// fileKey is a file's identity as /proc/locks prints it: the device's major and
// minor numbers and the inode.
type fileKey struct {
	major, minor, inode uint64
}

// parseFlocks reads a /proc/locks table and answers every file some process
// holds a flock(2) on.
//
// It is platform-neutral so its suite runs everywhere, although only the linux
// probe calls it. Each line reads
//
//	<n>: [->] <type> <mode> <access> <pid> <major>:<minor>:<inode> <start> <end>
//
// with major and minor in hex and the inode in decimal. A `->` line is a
// process WAITING for the lock, not holding it, and the holder has a line of
// its own, so waiters are skipped. Only FLOCK entries count: the shim's claim
// is a flock, and a POSIX or OFD lock on the same file is not what the shim
// takes.
//
// A LINE THAT DOES NOT PARSE FAILS THE WHOLE TABLE. A table this reader cannot
// read is a table it cannot answer from, and the caller reads that as "could
// not tell", never as "nobody holds anything".
func parseFlocks(r io.Reader) (map[fileKey]bool, error) {
	held := map[fileKey]bool{}
	scanner := bufio.NewScanner(r)
	for line := 1; scanner.Scan(); line++ {
		fields := strings.Fields(scanner.Text())
		if len(fields) == 0 {
			continue
		}
		if len(fields) > 1 && fields[1] == "->" {
			continue
		}
		if len(fields) < 6 {
			return nil, fmt.Errorf("line %d has %d field(s), want at least 6: %q", line, len(fields), scanner.Text())
		}
		if fields[1] != "FLOCK" {
			continue
		}
		key, err := parseFileKey(fields[5])
		if err != nil {
			return nil, fmt.Errorf("line %d: %w", line, err)
		}
		held[key] = true
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return held, nil
}

// parseFileKey reads `<major hex>:<minor hex>:<inode decimal>`.
func parseFileKey(raw string) (fileKey, error) {
	parts := strings.Split(raw, ":")
	if len(parts) != 3 {
		return fileKey{}, fmt.Errorf("file id %q is not major:minor:inode", raw)
	}
	major, err := strconv.ParseUint(parts[0], 16, 64)
	if err != nil {
		return fileKey{}, fmt.Errorf("file id %q has a bad major: %w", raw, err)
	}
	minor, err := strconv.ParseUint(parts[1], 16, 64)
	if err != nil {
		return fileKey{}, fmt.Errorf("file id %q has a bad minor: %w", raw, err)
	}
	inode, err := strconv.ParseUint(parts[2], 10, 64)
	if err != nil {
		return fileKey{}, fmt.Errorf("file id %q has a bad inode: %w", raw, err)
	}
	return fileKey{major: major, minor: minor, inode: inode}, nil
}
