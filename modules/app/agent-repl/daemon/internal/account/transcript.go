package account

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	sharedlogging "agentrepl/logging"

	"claude-repld/internal/dlog"
	"claude-repld/internal/remint"
)

// projectsDir is the vendor CLI's own subdirectory of a config root under
// which every conversation is filed.
const projectsDir = "projects"

// transcriptExt is the vendor's transcript file extension.
const transcriptExt = ".jsonl"

// EncodeCWD returns the vendor CLI's `projects/<name>` encoding of an absolute
// cwd. The rule has ONE spelling, agentrepl/logging's VendorProjectSlug, which
// the sidecar attributes transcripts with; see it for the rule and its
// verification.
func EncodeCWD(cwd string) string {
	return sharedlogging.VendorProjectSlug(cwd)
}

// ProjectDir returns the directory the vendor files cwd's transcripts under,
// inside one config root.
func ProjectDir(configDir, cwd string) string {
	return filepath.Join(configDir, projectsDir, EncodeCWD(cwd))
}

// TranscriptPath returns the transcript path for one vendor session id rooted
// at cwd, inside one config root.
func TranscriptPath(configDir, cwd, vendorSessionID string) string {
	return filepath.Join(ProjectDir(configDir, cwd), vendorSessionID+transcriptExt)
}

// FindTranscript implements Resolver.
//
// PROBE ORDER IS THE ROUTED ROOT FIRST. The routed root is the account the
// workspace's path determines, so a transcript found there is the ordinary
// case and needs no porting. Only when it is absent there is the other root
// probed, and a hit under the other root is exactly the account-switch signal
// the caller acts on with MoveTranscript. The same vendor uuid under BOTH
// roots is therefore disambiguated by routing rather than by picking one.
func (r *resolver) FindTranscript(ctx context.Context, workspaceDir, vendorSessionID string) (Transcript, error) {
	if err := ctx.Err(); err != nil {
		return Transcript{}, err
	}
	if vendorSessionID == "" {
		err := errors.New("account: FindTranscript requires a vendor session id")
		r.log.Error("daemon.account.find_transcript", "transcript lookup rejected", dlog.Context{
			"workspace_dir": workspaceDir,
			"branch":        "empty-session-id",
			"error":         err.Error(),
		})
		return Transcript{}, err
	}
	if workspaceDir == "" {
		err := errors.New("account: FindTranscript requires a workspace dir")
		r.log.Error("daemon.account.find_transcript", "transcript lookup rejected", dlog.Context{
			"vendor_session_id": vendorSessionID,
			"branch":            "empty-workspace-dir",
			"error":             err.Error(),
		})
		return Transcript{}, err
	}

	var probed []string
	for _, configDir := range r.probeOrder(workspaceDir) {
		path := TranscriptPath(configDir, workspaceDir, vendorSessionID)
		probed = append(probed, path)

		info, err := os.Stat(path)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				r.log.Debug("daemon.account.find_transcript", "transcript absent under a probed root", dlog.Context{
					"vendor_session_id": vendorSessionID,
					"workspace_dir":     workspaceDir,
					"config_dir":        configDir,
					"path":              path,
					"branch":            "miss",
				})
				continue
			}
			wrapped := fmt.Errorf("account: stat %s: %w", path, err)
			r.log.Error("daemon.account.find_transcript", "transcript stat failed", dlog.Context{
				"vendor_session_id": vendorSessionID,
				"workspace_dir":     workspaceDir,
				"config_dir":        configDir,
				"path":              path,
				"branch":            "stat-error",
				"error":             wrapped.Error(),
			})
			return Transcript{}, wrapped
		}
		if info.IsDir() {
			wrapped := fmt.Errorf("account: %s is a directory, not a transcript", path)
			r.log.Error("daemon.account.find_transcript", "transcript path is a directory", dlog.Context{
				"vendor_session_id": vendorSessionID,
				"path":              path,
				"branch":            "not-a-file",
				"error":             wrapped.Error(),
			})
			return Transcript{}, wrapped
		}

		found := Transcript{Path: path, ConfigDir: configDir}
		sidecar := sidecarDir(path)
		if sidecarInfo, err := os.Stat(sidecar); err == nil && sidecarInfo.IsDir() {
			found.SidecarDir = sidecar
		} else if err != nil && !errors.Is(err, fs.ErrNotExist) {
			wrapped := fmt.Errorf("account: stat sidecar %s: %w", sidecar, err)
			r.log.Error("daemon.account.find_transcript", "transcript sidecar stat failed", dlog.Context{
				"vendor_session_id": vendorSessionID,
				"sidecar_dir":       sidecar,
				"branch":            "sidecar-stat-error",
				"error":             wrapped.Error(),
			})
			return Transcript{}, wrapped
		}

		r.log.Debug("daemon.account.find_transcript", "transcript found", dlog.Context{
			"vendor_session_id": vendorSessionID,
			"workspace_dir":     workspaceDir,
			"config_dir":        configDir,
			"path":              path,
			"has_sidecar":       found.SidecarDir != "",
			"routed":            configDir == r.ConfigDirFor(workspaceDir),
			"branch":            "found",
		})
		return found, nil
	}

	// A PROBE THAT COMES UP EMPTY IS AN ANSWER, NOT A WARNING. This package
	// cannot know whether the id it was handed ever wrote a transcript: an id
	// minted at spawn and bounced before its first turn has none and never
	// should have, while a conversation whose file vanished is a real loss.
	// Only the caller knows which, and each of them already records the miss
	// at the level its own case warrants — Fleet.classifySource at INFO or
	// WARN by whether the workspace was ever engaged, Fleet.portAcrossAccounts
	// at WARN, verbs.forkTranscript at ERROR. So the miss is stated here at
	// DEBUG, the same level as the per-root probe misses above it, and the
	// NotFoundError carrying every probed path is what the caller classifies.
	miss := &NotFoundError{VendorSessionID: vendorSessionID, WorkspaceDir: workspaceDir, Probed: probed}
	r.log.Debug("daemon.account.find_transcript", "no transcript under either account root", dlog.Context{
		"vendor_session_id": vendorSessionID,
		"workspace_dir":     workspaceDir,
		"probed":            probed,
		"branch":            "not-found",
	})
	return Transcript{}, miss
}

// probeOrder is the routed root first, then the other one. A single configured
// root is probed once.
func (r *resolver) probeOrder(workspaceDir string) []string {
	routed := r.ConfigDirFor(workspaceDir)
	other := r.roots.Default
	if routed == r.roots.Default {
		other = r.roots.MultiRepo
	}
	if other == routed {
		return []string{routed}
	}
	return []string{routed, other}
}

// sidecarDir is the `<vendor uuid>/` directory the vendor writes beside a
// transcript. Verified against the live install: a conversation's per-session
// directory sits in the same project dir, named by the uuid with no extension.
func sidecarDir(transcriptPath string) string {
	return strings.TrimSuffix(transcriptPath, transcriptExt)
}

// NewestTranscript implements Resolver.
//
// THE ROUTED ROOT IS THE ONLY ROOT PROBED. FindTranscript falls back to the
// other account's root because it resumes a KNOWN id and a hit there is the
// account-switch signal; adoption is different — there is no recorded id, so a
// transcript under the other account's root belongs to a DIFFERENT account and
// adopting it would run the conversation as the wrong one. So this probes the
// routed project dir alone.
func (r *resolver) NewestTranscript(ctx context.Context, workspaceDir string) (AdoptableTranscript, error) {
	if err := ctx.Err(); err != nil {
		return AdoptableTranscript{}, err
	}
	if workspaceDir == "" {
		err := errors.New("account: NewestTranscript requires a workspace dir")
		r.log.Error("daemon.account.newest_transcript", "adoption probe rejected", dlog.Context{
			"branch": "empty-workspace-dir",
			"error":  err.Error(),
		})
		return AdoptableTranscript{}, err
	}

	routed := r.ConfigDirFor(workspaceDir)
	projectDir := ProjectDir(routed, workspaceDir)
	entries, err := os.ReadDir(projectDir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			r.log.Debug("daemon.account.newest_transcript", "no vendor project dir for the workspace; nothing to adopt", dlog.Context{
				"workspace_dir": workspaceDir,
				"config_dir":    routed,
				"project_dir":   projectDir,
				"branch":        "no-project-dir",
			})
			return AdoptableTranscript{}, ErrNoTranscripts
		}
		wrapped := fmt.Errorf("account: reading %s: %w", projectDir, err)
		r.log.Error("daemon.account.newest_transcript", "could not read the vendor project dir", dlog.Context{
			"workspace_dir": workspaceDir,
			"config_dir":    routed,
			"project_dir":   projectDir,
			"branch":        "readdir-error",
			"error":         wrapped.Error(),
		})
		return AdoptableTranscript{}, wrapped
	}

	var best AdoptableTranscript
	var bestKey time.Time
	found := false
	for _, entry := range entries {
		if err := ctx.Err(); err != nil {
			return AdoptableTranscript{}, err
		}
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), transcriptExt) {
			continue
		}
		path := filepath.Join(projectDir, entry.Name())
		info, err := os.Stat(path)
		if err != nil {
			// A CANDIDATE THAT CANNOT BE STAT'D IS SKIPPED, NOT FATAL. One
			// unreadable file among many must not deny adoption of the rest, and
			// a probe that comes up empty falls the caller back to a fresh start
			// — never a crash. The skip is recorded so it is never silent.
			r.log.Warn("daemon.account.newest_transcript", "skipping a transcript that could not be stat'd", dlog.Context{
				"workspace_dir": workspaceDir,
				"path":          path,
				"branch":        "candidate-stat-error",
				"error":         err.Error(),
			})
			continue
		}
		if info.IsDir() {
			continue
		}
		lastRecordAt := r.lastRecordTimestamp(path)
		// THE SELECTION KEY IS THE LAST-RECORD TIMESTAMP, and file mtime is the
		// fallback only when no record carried a parseable one — an empty or
		// truncated transcript still gets an ordering rather than being dropped.
		key := lastRecordAt
		if key.IsZero() {
			key = info.ModTime()
		}
		if !found || key.After(bestKey) {
			found = true
			bestKey = key
			best = AdoptableTranscript{
				Transcript: Transcript{
					Path:      path,
					ConfigDir: routed,
				},
				VendorSessionID: strings.TrimSuffix(entry.Name(), transcriptExt),
				ModTime:         info.ModTime(),
				LastRecordAt:    lastRecordAt,
			}
		}
	}

	if !found {
		r.log.Debug("daemon.account.newest_transcript", "the vendor project dir holds no transcript; nothing to adopt", dlog.Context{
			"workspace_dir": workspaceDir,
			"config_dir":    routed,
			"project_dir":   projectDir,
			"branch":        "no-transcripts",
		})
		return AdoptableTranscript{}, ErrNoTranscripts
	}

	if sidecar := sidecarDir(best.Path); sidecarExists(sidecar) {
		best.SidecarDir = sidecar
	}
	r.log.Debug("daemon.account.newest_transcript", "selected the newest transcript to adopt", dlog.Context{
		"workspace_dir":     workspaceDir,
		"config_dir":        routed,
		"vendor_session_id": best.VendorSessionID,
		"path":              best.Path,
		"mod_time":          best.ModTime.Format(time.RFC3339Nano),
		"last_record_at":    best.LastRecordAt.Format(time.RFC3339Nano),
		"branch":            "selected",
	})
	return best, nil
}

// sidecarExists reports whether a transcript's sidecar directory is present.
func sidecarExists(dir string) bool {
	info, err := os.Stat(dir)
	return err == nil && info.IsDir()
}

// lastRecordTimestamp parses the timestamp of a transcript's LAST parseable
// record. The vendor writes one JSON object per line and stamps each with an
// RFC3339 `timestamp`; the newest transcript is the one whose conversation
// most recently spoke, which is that last line's stamp — not the file's mtime,
// which a mere metadata touch can move.
//
// It returns the zero time when the file cannot be read or no line carries a
// parseable timestamp, and the caller then falls back to mtime. A parse miss is
// never fatal: adoption is a best effort and a fresh start is always the safe
// fallback.
func (r *resolver) lastRecordTimestamp(path string) time.Time {
	raw, err := os.ReadFile(path) //nolint:gosec // daemon-derived path
	if err != nil {
		r.log.Warn("daemon.account.newest_transcript", "could not read a transcript for its last-record timestamp; falling back to mtime", dlog.Context{
			"path":   path,
			"branch": "read-error",
			"error":  err.Error(),
		})
		return time.Time{}
	}
	lines := bytes.Split(raw, []byte("\n"))
	for i := len(lines) - 1; i >= 0; i-- {
		line := bytes.TrimSpace(lines[i])
		if len(line) == 0 {
			continue
		}
		var rec struct {
			Timestamp string `json:"timestamp"`
		}
		if err := json.Unmarshal(line, &rec); err != nil || rec.Timestamp == "" {
			continue
		}
		ts, err := time.Parse(time.RFC3339, rec.Timestamp)
		if err != nil {
			continue
		}
		return ts
	}
	return time.Time{}
}

// PortTranscript implements Resolver: a COPY into the child's root, for a fork.
// The parent keeps its own conversation, which is the whole point of a fork.
func (r *resolver) PortTranscript(ctx context.Context, transcriptPath, childConfigDir, childWorkspaceDir, childVendorSessionID string) (RemintedID, error) {
	// EVERY IDENTITY IN THE PORTED HISTORY IS RE-MINTED, under one mapping
	// shared by the transcript and its sidecar directory. A byte copy would
	// hand the file plane the parent's own record uuids, message ids and
	// tool_use ids under the CHILD's book, and the store refuses that outright
	// ("would move the row from book A to book B") and parks the file. The
	// parent's vendor session id is seeded to the child's, so every reference
	// to the conversation the child resumes is the child's own.
	parentVendorSessionID := strings.TrimSuffix(filepath.Base(transcriptPath), transcriptExt)
	mapper := remint.New(parentVendorSessionID, childVendorSessionID, nil)
	if err := r.transfer(ctx, transferSpec{
		operation:  "daemon.account.port_transcript",
		source:     transcriptPath,
		destRoot:   childConfigDir,
		destCWD:    childWorkspaceDir,
		destID:     childVendorSessionID,
		removeSrc:  false,
		verbMoving: "copying",
		remint:     mapper,
	}); err != nil {
		return nil, err
	}
	return mapper.ID, nil
}

// MoveTranscript implements Resolver: a MOVE into the other root, for an
// account switch. The source must not survive — two roots holding the same
// conversation is exactly the state that makes a resume ambiguous.
func (r *resolver) MoveTranscript(ctx context.Context, transcriptPath, toConfigDir, workspaceDir string) error {
	return r.transfer(ctx, transferSpec{
		operation:  "daemon.account.move_transcript",
		source:     transcriptPath,
		destRoot:   toConfigDir,
		destCWD:    workspaceDir,
		removeSrc:  true,
		verbMoving: "moving",
	})
}

// transferSpec is one transcript transfer's inputs.
type transferSpec struct {
	operation string
	source    string
	destRoot  string
	destCWD   string
	// destID renames the transcript on the way: the destination is
	// `<destID>.jsonl` (and its sidecar `<destID>/`). Empty keeps the
	// source's own name, which every transfer but a fork wants.
	destID    string
	removeSrc bool
	// remint, when set, RE-MINTS every identity the transfer carries rather
	// than copying the bytes. Only a fork wants it: an account switch moves one
	// conversation between roots and must keep every id it had.
	remint     *remint.Mapper
	verbMoving string
}

// transfer carries a transcript, and its sidecar directory when one exists,
// into destRoot's project dir for destCWD.
//
// REFUSES WHEN THE DESTINATION EXISTS. A transcript already filed there is a
// different conversation with the same uuid, or the same one already ported;
// either way, overwriting it destroys a conversation, and abandonment is the
// one irreversible outcome.
func (r *resolver) transfer(ctx context.Context, spec transferSpec) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	if spec.source == "" || spec.destRoot == "" || spec.destCWD == "" {
		err := fmt.Errorf("account: transcript transfer requires a source, a destination root and a workspace dir (got %q, %q, %q)",
			spec.source, spec.destRoot, spec.destCWD)
		r.log.Error(spec.operation, "transcript transfer rejected", dlog.Context{
			"branch": "missing-input",
			"error":  err.Error(),
		})
		return err
	}

	destDir := ProjectDir(spec.destRoot, spec.destCWD)
	destName := filepath.Base(spec.source)
	if spec.destID != "" {
		destName = spec.destID + transcriptExt
	}
	dest := filepath.Join(destDir, destName)
	logCtx := dlog.Context{
		"source":      spec.source,
		"destination": dest,
		"dest_root":   spec.destRoot,
		"workspace":   spec.destCWD,
		"remove_src":  spec.removeSrc,
	}

	if _, err := os.Stat(spec.source); err != nil {
		wrapped := fmt.Errorf("account: source transcript %s: %w", spec.source, err)
		r.log.Error(spec.operation, "source transcript unusable", withBranch(logCtx, "source-stat-error", wrapped))
		return wrapped
	}
	if _, err := os.Stat(dest); err == nil {
		wrapped := fmt.Errorf("account: refusing to overwrite the transcript already at %s", dest)
		r.log.Error(spec.operation, "transcript transfer refused: destination exists", withBranch(logCtx, "destination-exists", wrapped))
		return wrapped
	} else if !errors.Is(err, fs.ErrNotExist) {
		wrapped := fmt.Errorf("account: stat destination %s: %w", dest, err)
		r.log.Error(spec.operation, "destination stat failed", withBranch(logCtx, "destination-stat-error", wrapped))
		return wrapped
	}

	srcSidecar := sidecarDir(spec.source)
	destSidecar := sidecarDir(dest)
	haveSidecar := false
	if info, err := os.Stat(srcSidecar); err == nil && info.IsDir() {
		haveSidecar = true
		if _, err := os.Stat(destSidecar); err == nil {
			wrapped := fmt.Errorf("account: refusing to overwrite the transcript sidecar already at %s", destSidecar)
			r.log.Error(spec.operation, "transcript transfer refused: sidecar destination exists", withBranch(logCtx, "sidecar-destination-exists", wrapped))
			return wrapped
		} else if !errors.Is(err, fs.ErrNotExist) {
			wrapped := fmt.Errorf("account: stat sidecar destination %s: %w", destSidecar, err)
			r.log.Error(spec.operation, "sidecar destination stat failed", withBranch(logCtx, "sidecar-destination-stat-error", wrapped))
			return wrapped
		}
	} else if err != nil && !errors.Is(err, fs.ErrNotExist) {
		wrapped := fmt.Errorf("account: stat sidecar %s: %w", srcSidecar, err)
		r.log.Error(spec.operation, "sidecar stat failed", withBranch(logCtx, "sidecar-stat-error", wrapped))
		return wrapped
	}

	if err := os.MkdirAll(destDir, 0o700); err != nil {
		wrapped := fmt.Errorf("account: creating project dir %s: %w", destDir, err)
		r.log.Error(spec.operation, "destination project dir could not be created", withBranch(logCtx, "mkdir-error", wrapped))
		return wrapped
	}

	if err := spec.carryTranscript(dest); err != nil {
		r.log.Error(spec.operation, "transcript "+spec.verbMoving+" failed", withBranch(logCtx, "transfer-error", err))
		return err
	}
	if haveSidecar {
		if err := spec.carrySidecar(srcSidecar, destSidecar); err != nil {
			r.log.Error(spec.operation, "transcript sidecar "+spec.verbMoving+" failed", withBranch(logCtx, "sidecar-transfer-error", err))
			return err
		}
	}

	logCtx["has_sidecar"] = haveSidecar
	logCtx["reminted"] = spec.remint != nil
	r.log.Debug(spec.operation, "transcript transferred", withBranch(logCtx, "transferred", nil))
	return nil
}

// carryTranscript carries the transcript itself: re-minted for a fork, byte for
// byte otherwise.
func (spec transferSpec) carryTranscript(dest string) error {
	if spec.remint != nil {
		return remintFile(spec.source, dest, spec.remint)
	}
	return carryFile(spec.source, dest, spec.removeSrc)
}

// carrySidecar carries the transcript's sidecar directory under the SAME
// mapping the transcript was carried under, which is what keeps a subagent's
// `agent-<id>` file name and the `agentId` its parent's records state pointing
// at each other.
func (spec transferSpec) carrySidecar(source, dest string) error {
	if spec.remint != nil {
		return remintTree(source, dest, spec.remint)
	}
	return carryTree(source, dest, spec.removeSrc)
}

// remintFile writes a re-minted copy of one transcript to dest.
func remintFile(source, dest string, mapper *remint.Mapper) error {
	raw, err := os.ReadFile(source) //nolint:gosec // daemon-derived path
	if err != nil {
		return fmt.Errorf("account: reading %s: %w", source, err)
	}
	info, err := os.Stat(source)
	if err != nil {
		return fmt.Errorf("account: stat %s: %w", source, err)
	}
	converted, err := mapper.Lines(raw)
	if err != nil {
		return fmt.Errorf("account: re-minting %s: %w", source, err)
	}
	return writeFileAtomic(dest, converted, info.Mode().Perm())
}

// remintTree copies a sidecar tree, re-minting every record it carries and
// every identity its path names.
//
// A `.jsonl` file is a record stream and a `.json` file is one document (the
// subagent's `agent-<id>.meta.json`, whose `toolUseId` IS that agent's
// identity); anything else is carried unchanged. A malformed one is an ERROR:
// porting it as it stands would file the parent's identity in the child's book,
// which is the exact failure this pass exists to prevent.
func remintTree(source, dest string, mapper *remint.Mapper) error {
	return filepath.WalkDir(source, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("account: walking %s: %w", path, err)
		}
		rel, err := filepath.Rel(source, path)
		if err != nil {
			return fmt.Errorf("account: relativizing %s against %s: %w", path, source, err)
		}
		target := filepath.Join(dest, filepath.FromSlash(mapper.PathRel(filepath.ToSlash(rel))))
		switch {
		case d.IsDir():
			if err := os.MkdirAll(target, 0o700); err != nil {
				return fmt.Errorf("account: creating %s: %w", target, err)
			}
			return nil
		case d.Type().IsRegular():
			return remintSidecarFile(path, target, mapper)
		default:
			return fmt.Errorf("account: %s is neither a regular file nor a directory (%s)", path, d.Type())
		}
	})
}

// remintSidecarFile carries one file out of a sidecar tree.
func remintSidecarFile(source, dest string, mapper *remint.Mapper) error {
	switch {
	case strings.HasSuffix(source, ".jsonl"):
		return remintFile(source, dest, mapper)
	case strings.HasSuffix(source, ".json"):
		raw, err := os.ReadFile(source) //nolint:gosec // daemon-derived path
		if err != nil {
			return fmt.Errorf("account: reading %s: %w", source, err)
		}
		info, err := os.Stat(source)
		if err != nil {
			return fmt.Errorf("account: stat %s: %w", source, err)
		}
		converted, err := mapper.Document(raw)
		if err != nil {
			return fmt.Errorf("account: re-minting %s: %w", source, err)
		}
		return writeFileAtomic(dest, converted, info.Mode().Perm())
	default:
		return copyFileAtomic(source, dest)
	}
}

// writeFileAtomic writes data through a temporary in dest's directory, fsyncs
// it, and renames it into place — the same durability the copy path has.
func writeFileAtomic(dest string, data []byte, mode fs.FileMode) error {
	if err := os.MkdirAll(filepath.Dir(dest), 0o700); err != nil {
		return fmt.Errorf("account: creating %s: %w", filepath.Dir(dest), err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(dest), ".port-"+filepath.Base(dest)+"-*")
	if err != nil {
		return fmt.Errorf("account: creating a temporary beside %s: %w", dest, err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName) //nolint:errcheck // best-effort cleanup of a named temporary

	if _, err := tmp.Write(data); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("account: writing %s: %w", tmpName, err)
	}
	if err := tmp.Chmod(mode); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("account: setting the mode of %s: %w", tmpName, err)
	}
	if err := tmp.Sync(); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("account: fsyncing %s: %w", tmpName, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("account: closing %s: %w", tmpName, err)
	}
	if err := os.Rename(tmpName, dest); err != nil {
		return fmt.Errorf("account: renaming %s to %s: %w", tmpName, dest, err)
	}
	return nil
}

// withBranch stamps the branch (and the cause, when there is one) onto a copy
// of a context so one record's fields are never mutated by the next.
func withBranch(base dlog.Context, branch string, cause error) dlog.Context {
	out := make(dlog.Context, len(base)+2)
	for k, v := range base {
		out[k] = v
	}
	out["branch"] = branch
	if cause != nil {
		out["error"] = cause.Error()
	}
	return out
}

// carryFile moves or copies one file.
//
// A rename is atomic and is tried FIRST — within one filesystem it is the only
// transfer that cannot leave a half-written transcript behind. Across
// filesystems rename fails with EXDEV, and the fallback rebuilds atomicity out
// of copy + fsync + rename: the copy lands on a temporary name in the
// DESTINATION directory (so the final rename is same-filesystem), is fsync'd
// before it is named, and the source is removed only after the destination is
// durable.
func carryFile(source, dest string, removeSrc bool) error {
	if removeSrc {
		if err := os.Rename(source, dest); err == nil {
			return nil
		} else if !isCrossDevice(err) {
			return fmt.Errorf("account: renaming %s to %s: %w", source, dest, err)
		}
	}
	if err := copyFileAtomic(source, dest); err != nil {
		return err
	}
	if removeSrc {
		if err := os.Remove(source); err != nil {
			return fmt.Errorf("account: removing the ported source %s: %w", source, err)
		}
	}
	return nil
}

// copyFileAtomic copies source to dest through a temporary in dest's
// directory, fsyncs it, and renames it into place.
func copyFileAtomic(source, dest string) error {
	in, err := os.Open(source) //nolint:gosec // daemon-derived path
	if err != nil {
		return fmt.Errorf("account: opening %s: %w", source, err)
	}
	defer in.Close() //nolint:errcheck // read-only handle

	info, err := in.Stat()
	if err != nil {
		return fmt.Errorf("account: stat %s: %w", source, err)
	}

	tmp, err := os.CreateTemp(filepath.Dir(dest), ".port-"+filepath.Base(dest)+"-*")
	if err != nil {
		return fmt.Errorf("account: creating a temporary beside %s: %w", dest, err)
	}
	tmpName := tmp.Name()
	// Every failure below removes the temporary; a successful rename makes the
	// removal a no-op on a name that no longer exists.
	defer os.Remove(tmpName) //nolint:errcheck // best-effort cleanup of a named temporary

	if _, err := io.Copy(tmp, in); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("account: copying %s to %s: %w", source, tmpName, err)
	}
	if err := tmp.Chmod(info.Mode().Perm()); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("account: setting the mode of %s: %w", tmpName, err)
	}
	if err := tmp.Sync(); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("account: fsyncing %s: %w", tmpName, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("account: closing %s: %w", tmpName, err)
	}
	if err := os.Rename(tmpName, dest); err != nil {
		return fmt.Errorf("account: renaming %s to %s: %w", tmpName, dest, err)
	}
	return nil
}

// carryTree moves or copies a directory tree (the transcript's sidecar).
func carryTree(source, dest string, removeSrc bool) error {
	if removeSrc {
		if err := os.Rename(source, dest); err == nil {
			return nil
		} else if !isCrossDevice(err) {
			return fmt.Errorf("account: renaming %s to %s: %w", source, dest, err)
		}
	}
	if err := copyTree(source, dest); err != nil {
		return err
	}
	if removeSrc {
		if err := os.RemoveAll(source); err != nil {
			return fmt.Errorf("account: removing the ported source tree %s: %w", source, err)
		}
	}
	return nil
}

// copyTree copies a directory tree file by file. Anything that is neither a
// regular file nor a directory is an error: a symlink or a socket inside a
// vendor sidecar is not something to reproduce silently.
func copyTree(source, dest string) error {
	return filepath.WalkDir(source, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("account: walking %s: %w", path, err)
		}
		rel, err := filepath.Rel(source, path)
		if err != nil {
			return fmt.Errorf("account: relativizing %s against %s: %w", path, source, err)
		}
		target := filepath.Join(dest, rel)
		switch {
		case d.IsDir():
			if err := os.MkdirAll(target, 0o700); err != nil {
				return fmt.Errorf("account: creating %s: %w", target, err)
			}
			return nil
		case d.Type().IsRegular():
			return copyFileAtomic(path, target)
		default:
			return fmt.Errorf("account: %s is neither a regular file nor a directory (%s)", path, d.Type())
		}
	})
}

// isCrossDevice reports whether err is the kernel's "not the same filesystem"
// refusal of a rename, which is the one rename failure with a fallback.
func isCrossDevice(err error) bool {
	var linkErr *os.LinkError
	if errors.As(err, &linkErr) {
		return errors.Is(linkErr.Err, crossDeviceErrno)
	}
	return errors.Is(err, crossDeviceErrno)
}
