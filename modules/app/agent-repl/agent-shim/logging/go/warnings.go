package logging

// warnings.go -- WARN AND ERROR RECORDS LIVE IN THEIR OWN FILE.
//
// Every runtime writes its warn and error records only to a warnings file
// beside its log, and its debug and info records only to the log, so a reader
// that wants warnings and errors opens the small warnings files alone. Reading
// them out of every record ever written made the deploy intake's read of a
// 20-minute window outlive its bound over 10 GB of logs (2026-11-17).
// proto/vocab/log-warnings-sink.json is the contract every language asserts.

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
)

// WarningsPath names the warnings file of the log at PATH: '.warn' before a
// final '.log', or '.warn.log' appended to a path with no such extension.
func WarningsPath(path string) string {
	if base, ok := strings.CutSuffix(path, ".log"); ok {
		return base + ".warn.log"
	}
	return path + ".warn.log"
}

// WarningsSink names the warnings twin of the per-workspace sink NAME, whose
// canonical link is NAME.warn.log beside NAME.log.
func WarningsSink(name string) string {
	return name + ".warn"
}

// IsWarningsLevel reports whether a record at LEVEL belongs in the warnings
// file. An unknown level is a programming error, as it is to Level.Allows.
func IsWarningsLevel(level string) bool {
	switch level {
	case "warn", "error":
		return true
	case "debug", "info":
		return false
	default:
		panic(fmt.Sprintf("logging: invalid level %q", level))
	}
}

// RecordLevel answers the level of one JSONL record. It reads the object's
// top-level keys only as far as "level", which every runtime writes among its
// first few, so a record's context is never decoded. A line that is not one
// JSON object with a string level is refused: every writer this serves writes
// whole records, so such a line is a defect, never a guess.
func RecordLevel(line []byte) (string, error) {
	dec := json.NewDecoder(bytes.NewReader(line))
	if tok, err := dec.Token(); err != nil || tok != json.Delim('{') {
		return "", fmt.Errorf("logging: a record is not a JSON object: %.80q", line)
	}
	for dec.More() {
		key, err := dec.Token()
		if err != nil {
			return "", fmt.Errorf("logging: a record's key cannot be read: %w", err)
		}
		if key == "level" {
			var level string
			if err := dec.Decode(&level); err != nil {
				return "", fmt.Errorf("logging: a record's level is not a string: %w", err)
			}
			return level, nil
		}
		var skip json.RawMessage
		if err := dec.Decode(&skip); err != nil {
			return "", fmt.Errorf("logging: a record's %v cannot be read: %w", key, err)
		}
	}
	return "", fmt.Errorf("logging: a record names no level: %.80q", line)
}

// SplitSink is an io.Writer over a log and its warnings file: each Write is
// one record, routed by its own level. A write that is not one record is
// refused.
type SplitSink struct {
	Log      io.Writer
	Warnings io.Writer
}

// Write routes the record P to the warnings file or the log.
func (s SplitSink) Write(p []byte) (int, error) {
	level, err := RecordLevel(p)
	if err != nil {
		return 0, err
	}
	if !validLevel(level) {
		return 0, fmt.Errorf("logging: a record names the unknown level %q", level)
	}
	if IsWarningsLevel(level) {
		return s.Warnings.Write(p)
	}
	return s.Log.Write(p)
}

// validLevel reports whether LEVEL is one of the four record levels.
func validLevel(level string) bool {
	switch level {
	case "debug", "info", "warn", "error":
		return true
	}
	return false
}

// RotatingPair is a rotating log and its rotating warnings file, opened
// together and written as one SplitSink.
type RotatingPair struct {
	Log      *RotatingFile
	Warnings *RotatingFile
}

// OpenRotatingPair opens the log at PATH and its warnings file
// (WarningsPath), each rotated as OpenRotating rotates it.
func OpenRotatingPair(path string, capBytes int64, backups int) (*RotatingPair, error) {
	log, err := OpenRotating(path, capBytes, backups)
	if err != nil {
		return nil, err
	}
	warnings, err := OpenRotating(WarningsPath(path), capBytes, backups)
	if err != nil {
		return nil, errors.Join(err, log.Close())
	}
	return &RotatingPair{Log: log, Warnings: warnings}, nil
}

// Write routes one record (SplitSink).
func (p *RotatingPair) Write(record []byte) (int, error) {
	return SplitSink{Log: p.Log, Warnings: p.Warnings}.Write(record)
}

// Close closes both files.
func (p *RotatingPair) Close() error {
	return errors.Join(p.Log.Close(), p.Warnings.Close())
}
