// Command sockethold holds listening sockets for the servers that accept on
// them, so no restart of a server ever closes its socket.
//
// launchd (systemd on Linux) creates every socket from the holder's service
// definition, which never changes, and keeps it for as long as that service is
// loaded. This process takes them (activate), never accepts on a held socket
// itself, and serves the control socket a server asks for its listener on
// (sockethold.Take). An install, a deploy or a crash of the server leaves the
// socket standing, and a connect made while no server accepts queues in the
// kernel's backlog.
//
//	argv: --control NAME --hold NAME [--hold NAME]... [--log PATH]
//	      NAME is a socket's key in the service definition.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"net"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"agentrepl/logging/buildreport"
	"agentrepl/logging/logsite"
	"agentrepl/logging/recordlog"
	"agentrepl/logging/sockethold"
	"agentrepl/logging/sockethold/activate"
)

// runtimeName is the runtime every holder record carries.
const runtimeName = "sockethold"

// names is a repeatable flag.
type names []string

func (n *names) String() string     { return strings.Join(*n, ",") }
func (n *names) Set(v string) error { *n = append(*n, v); return nil }

func main() {
	var hold names
	control := flag.String("control", "", "the control socket's key in the service definition")
	flag.Var(&hold, "hold", "a held socket's key in the service definition (repeatable)")
	logPath := flag.String("log", defaultLog(), "log file path")
	flag.Parse()
	log, closeLog, err := recordlog.Open(runtimeName, *logPath, os.Stderr, os.Getenv, time.Now)
	if err != nil {
		fmt.Fprintf(os.Stderr, "sockethold: opening the log failed: %v\n", err)
		os.Exit(1)
	}
	defer func() { _ = closeLog() }()
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGTERM, syscall.SIGINT)
	defer stop()
	if err := run(ctx, *control, hold, activate.Sockets, log, os.Getenv); err != nil {
		os.Exit(1)
	}
}

// run takes the control socket CONTROL and every socket of HOLD from ACTIVATE
// and serves them until CTX ends. Every failure is recorded before it returns.
func run(ctx context.Context, control string, hold []string, activateSockets func([]string) ([]activate.Socket, error), log *recordlog.Logger, getenv func(string) string) error {
	fail := func(message logsite.Message, err error) error {
		log.Log("error", "sockethold.run", message, recordlog.Context{"control": control, "hold": strings.Join(hold, ","), "cause": err.Error()})
		return err
	}
	if control == "" || len(hold) == 0 {
		return fail(logsite.Msg("the holder was given no control socket or no socket to hold"), errors.New("--control and at least one --hold are required"))
	}
	sockets, err := activateSockets(append([]string{control}, hold...))
	if err != nil {
		return fail(logsite.Msg("the service manager handed the holder no sockets"), err)
	}
	byName := map[string]*os.File{}
	for _, s := range sockets {
		byName[s.Name] = s.File
	}
	controlFile, ok := byName[control]
	if !ok {
		return fail(logsite.Msg("the service manager handed no control socket"), fmt.Errorf("no socket named %q", control))
	}
	delete(byName, control)
	for _, name := range hold {
		if _, ok := byName[name]; !ok {
			return fail(logsite.Msg("the service manager handed no socket to hold"), fmt.Errorf("no socket named %q", name))
		}
	}
	ln, err := net.FileListener(controlFile)
	if err != nil {
		return fail(logsite.Msg("the control socket is no listener"), err)
	}
	unix, ok := ln.(*net.UnixListener)
	if !ok {
		return fail(logsite.Msg("the control socket is not a unix socket"), fmt.Errorf("listener %T", ln))
	}
	log.Info("sockethold.run", "holding the sockets and serving their control socket", recordlog.Context{"control": control, "hold": strings.Join(hold, ",")})
	// THE HOLDER STATES ITS BUILD, as every service a deploy restarts does: a
	// deploy restarts a holder whose report is not the fresh build before it
	// rolls out a daemon that speaks the holder's newer requests.
	buildreport.Announce(buildreport.HolderService(hold[0]), "sockethold.buildreport", log, getenv)
	err = sockethold.Serve(ctx, unix, byName, func(g sockethold.Grant) {
		switch {
		case g.Probe && g.Err != nil:
			log.Error("sockethold.probe", "a probe could not be answered", recordlog.Context{"cause": g.Err.Error()})
		case g.Probe:
			log.Info("sockethold.probe", "answered a probe: the holder is serving", nil)
		case g.Verb == sockethold.VerbReturn && g.Err != nil:
			log.Error("sockethold.return", "a connection handed back was not kept", recordlog.Context{"socket": g.Name, "cause": g.Err.Error()})
		case g.Verb == sockethold.VerbReturn:
			log.Info("sockethold.return", "kept a connection a departing server handed back, for the next to accept", recordlog.Context{"socket": g.Name})
		case g.Verb == sockethold.VerbAccept && g.Err != nil:
			log.Error("sockethold.accept", "an accept stream of handed-back connections was refused, or failed before its end", recordlog.Context{"socket": g.Name, "cause": g.Err.Error()})
		case g.Verb == sockethold.VerbAccept:
			log.Info("sockethold.accept", "an accept stream of handed-back connections ended", recordlog.Context{"socket": g.Name})
		case g.Err != nil:
			log.Error("sockethold.grant", "a request for a held socket was not granted", recordlog.Context{"socket": g.Name, "cause": g.Err.Error()})
		default:
			log.Info("sockethold.grant", "handed a server its held socket", recordlog.Context{"socket": g.Name})
		}
	})
	if err != nil {
		return fail(logsite.Msg("serving the control socket failed"), err)
	}
	log.Info("sockethold.run", "the holder is exiting on its service manager's signal", nil)
	return nil
}

func defaultLog() string {
	home, _ := os.UserHomeDir()
	if d := os.Getenv("XDG_CACHE_HOME"); d != "" {
		return filepath.Join(d, "agent-repl", "log", "sockethold.log")
	}
	return filepath.Join(home, ".cache", "agent-repl", "log", "sockethold.log")
}
