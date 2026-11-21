// Package holderensure brings a socket holder up on a machine that predates
// it. A holder is a requirement (bin/requirements.sh, kind socket_holder), and
// a daemon or a store restart that needs one first proves a holder serves its
// control socket with a probe, one connect and one byte. Only when no holder
// serves it at all is the holder's own installer run (`requirements.sh
// ensure-holder`), the same renderer `install` uses, so no machine is left
// without the holder agent-repl's sockets depend on
// (docs/investigations/2026-11-18-stable-sockets-design.md, owner ruling
// 2026-11-18).
package holderensure

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"time"

	"agentrepl/logging/buildreport"
	"agentrepl/logging/sockethold"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/dlog"
	"agent-repld/internal/reqmanifest"
)

// op is the operation every record of the ensure is filed under.
const op = "daemon.holder.ensure"

// DaemonHolder is the id of the holder the daemon takes its shared listener
// from (daemon/requirements.json).
const DaemonHolder = "socket-holder-daemon"

// InstallBound bounds one holder install: the sockethold build (0.92s with a
// cold cache, 0.19s warm), the manifests' load and the render (0.13s), and
// the service manager's load, measured 2026-11-18 at load average 5.8-7.1.
// The load itself was not measured; every install's time is recorded at
// INFO, so the bound is re-sized from real installs.
const InstallBound = 3 * time.Second

// InstallMeasured is the healthy worst install InstallBound was sized from:
// the cold build and the render, without the unmeasured load.
const InstallMeasured = 1050 * time.Millisecond

// Holder is one socket holder requirement: its id, its control socket, and
// the unix sockets it holds, each with a leading ~ expanded.
type Holder struct {
	ID      string
	Control string
	Sockets []string
	// Label is its service manager label.
	Label string
	// Service is the name its build report carries
	// (buildreport.HolderService of its first held socket).
	Service string
}

// manifest is the slice of a requirements manifest the holders are read from.
type manifest struct {
	Requirements []struct {
		ID        string   `json:"id"`
		Kind      string   `json:"kind"`
		Platforms []string `json:"platforms"`
		Control   string   `json:"control"`
		Label     string   `json:"label"`
		Sockets   []struct {
			Name string `json:"name"`
			Unix string `json:"unix"`
		} `json:"sockets"`
	} `json:"requirements"`
}

// Declared reads every socket holder the requirements manifests at the module
// ROOT declare for this platform: the root manifest and each one it includes,
// as bin/requirements.sh loads them. HOME expands a leading ~.
func Declared(root, home string) ([]Holder, error) {
	rootPath := filepath.Join(root, "requirements.json")
	rootManifest, err := read(rootPath)
	if err != nil {
		return nil, err
	}
	includes, err := reqmanifest.Includes(rootPath)
	if err != nil {
		return nil, err
	}
	files := []manifest{rootManifest}
	for _, rel := range includes {
		m, err := read(filepath.Join(root, rel))
		if err != nil {
			return nil, err
		}
		files = append(files, m)
	}
	var out []Holder
	for _, m := range files {
		for _, e := range m.Requirements {
			if e.Kind != "socket_holder" || (len(e.Platforms) > 0 && !slices.Contains(e.Platforms, runtime.GOOS)) {
				continue
			}
			control, err := dirpath.Absolute(e.Control, home)
			if err != nil {
				return nil, fmt.Errorf("holderensure: the control socket of %s: %w", e.ID, err)
			}
			if len(e.Sockets) == 0 || e.Sockets[0].Name == "" {
				return nil, fmt.Errorf("holderensure: the socket holder %s names no socket it holds", e.ID)
			}
			h := Holder{ID: e.ID, Control: control, Label: e.Label, Service: buildreport.HolderService(e.Sockets[0].Name)}
			for _, sock := range e.Sockets {
				if sock.Unix == "" {
					continue
				}
				path, err := dirpath.Absolute(sock.Unix, home)
				if err != nil {
					return nil, fmt.Errorf("holderensure: a socket %s holds: %w", e.ID, err)
				}
				h.Sockets = append(h.Sockets, path)
			}
			out = append(out, h)
		}
	}
	return out, nil
}

func read(path string) (manifest, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return manifest{}, fmt.Errorf("holderensure: read the requirements manifest: %w", err)
	}
	var m manifest
	if err := json.Unmarshal(data, &m); err != nil {
		return manifest{}, fmt.Errorf("holderensure: parse the requirements manifest %s: %w", path, err)
	}
	return m, nil
}

// ControlOf answers the control socket of the holder ID among HOLDERS.
func ControlOf(holders []Holder, id string) (string, error) {
	for _, h := range holders {
		if h.ID == id {
			return h.Control, nil
		}
	}
	return "", fmt.Errorf("holderensure: no requirement on %s declares the socket holder %s", runtime.GOOS, id)
}

// HolderOfSocket answers the id of the holder among HOLDERS that holds the
// unix socket SOCKET, false when none does: a server listening there takes it
// from that holder, and one at a socket no holder declares binds its own.
func HolderOfSocket(holders []Holder, socket string) (string, bool) {
	for _, h := range holders {
		if slices.Contains(h.Sockets, socket) {
			return h.ID, true
		}
	}
	return "", false
}

// Ensurer proves a holder serves a control socket, and installs a declared
// holder a probe finds missing.
type Ensurer struct {
	// Holders are the declared holders (Declared): only these are installed.
	Holders []Holder
	// Probe is sockethold.Probe; a seam for the tests.
	Probe sockethold.ProbeFunc
	// Install runs the installer for the holder ID (`requirements.sh
	// ensure-holder ID`, deploy.ScriptHolderInstaller), within its context.
	Install func(ctx context.Context, id string) error
	Log     dlog.Logger
}

// Ensure proves a holder serves CONTROL. The probe is the whole cost when
// one does. A control socket no holder serves is installed when a declared
// holder owns it, and an error otherwise: agent-repl installs only the
// holders its requirements declare. Any other probe failure is the holder's,
// and is answered as it is.
func (e *Ensurer) Ensure(ctx context.Context, control string) error {
	fields := dlog.Context{"control": control}
	took, err := sockethold.ProbeTimed(e.Probe, control)
	fields["probe_ms"] = took.Milliseconds()
	if err == nil {
		e.Log.Debug(op, "a holder serves the control socket", fields)
		return nil
	}
	fields["error"] = err.Error()
	if !errors.Is(err, sockethold.ErrNoHolder) {
		fields["bound_ms"], fields["measured_ms"] = sockethold.TakeBound.Milliseconds(), sockethold.TakeMeasured.Milliseconds()
		e.Log.Error(op, "the holder's control socket did not answer its probe; it is the holder's failure, and nothing was installed", fields)
		return fmt.Errorf("holderensure: probe %s: %w", control, err)
	}
	var id string
	for _, h := range e.Holders {
		if h.Control == control {
			id = h.ID
		}
	}
	if id == "" {
		e.Log.Error(op, "no holder serves the control socket, and no requirement declares a holder there, so none is installed", fields)
		return fmt.Errorf("holderensure: no holder serves %s, and no requirement declares one there: %w", control, err)
	}
	fields["holder"] = id
	e.Log.Info(op, "no holder serves the control socket; installing the holder its requirement declares", fields)
	installCtx, cancel := context.WithTimeout(ctx, InstallBound)
	defer cancel()
	began := time.Now()
	err = e.Install(installCtx, id)
	fields["install_ms"] = time.Since(began).Milliseconds()
	if err != nil {
		fields["error"] = err.Error()
		fields["bound_ms"], fields["measured_ms"] = InstallBound.Milliseconds(), InstallMeasured.Milliseconds()
		e.Log.Error(op, "the holder could not be installed", fields)
		return fmt.Errorf("holderensure: install %s: %w", id, err)
	}
	delete(fields, "error")
	e.Log.Info(op, "installed the holder", fields)
	return nil
}

// EnsureID is Ensure for the declared holder ID's control socket. An id no
// requirement declares is an error.
func (e *Ensurer) EnsureID(ctx context.Context, id string) error {
	control, err := ControlOf(e.Holders, id)
	if err != nil {
		e.Log.Error(op, "the holder to ensure is one no requirement declares", dlog.Context{"holder": id, "error": err.Error()})
		return err
	}
	return e.Ensure(ctx, control)
}

// ServingID reports whether a holder serves the declared holder ID's control
// socket, by its probe alone: nothing is installed. A control socket nothing
// serves is false; any other probe failure is the holder's, recorded and
// answered.
func (e *Ensurer) ServingID(ctx context.Context, id string) (bool, error) {
	control, err := ControlOf(e.Holders, id)
	if err != nil {
		e.Log.Error(op, "the holder to probe is one no requirement declares", dlog.Context{"holder": id, "error": err.Error()})
		return false, err
	}
	fields := dlog.Context{"holder": id, "control": control}
	took, err := sockethold.ProbeTimed(e.Probe, control)
	fields["probe_ms"] = took.Milliseconds()
	switch {
	case err == nil:
		e.Log.Debug(op, "a holder serves the control socket", fields)
		return true, nil
	case errors.Is(err, sockethold.ErrNoHolder):
		e.Log.Info(op, "no holder serves the control socket", fields)
		return false, nil
	default:
		fields["error"] = err.Error()
		e.Log.Error(op, "the holder's control socket did not answer its probe; it is the holder's failure", fields)
		return false, fmt.Errorf("holderensure: probe %s: %w", control, err)
	}
}
