package server

import (
	"context"
	"fmt"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"

	"connectrpc.com/connect"
)

// WatchBashRun follows one detached shell run's rows: every row already stored,
// in the run's own first-insert order, then rows as they are written, ENDING
// after the terminal.
//
// IT IS THE ONE STREAM IN THIS SERVICE WITH A NATURAL END. A book never ends —
// an agent can always say more — but a shell run concludes, and its conclusion
// is a row the store can recognize. So the caller needs no cancellation
// protocol and no timeout to know it has the whole run: the stream closes.
//
// THERE IS NO TOKEN AND NO FAILURE ARM. A run is addressed by the identity the
// spawning stream already announced, so there is nothing to mint; and a run the
// store holds no row for is a REFUSED OPEN at the transport (CodeNotFound), the
// same convention WatchAgentSession uses for a token it does not know.
func (s *Server) WatchBashRun(ctx context.Context, req *connect.Request[storev1.WatchBashRunRequest], stream *connect.ServerStream[storev1.WatchBashRunResponse]) error {
	log := s.rpcLogger(storev1connect.ShimStoreWatchBashRunProcedure, req.Header())
	if ref := validateWatchBashRunRequest(req.Msg); ref != nil {
		s.logRefusal(log, "store.rpc.watch-bash-run", ref, logging.Fields{})
		// A malformed ADDRESS is not an unknown run: the caller must fix the
		// request rather than conclude the run does not exist.
		return connect.NewError(connect.CodeInvalidArgument, ref)
	}
	runID := req.Msg.GetRun().GetValue()
	log = log.With(logging.Fields{TaskID: runID})

	// SUBSCRIBE BEFORE THE REPLAY QUERY, so a row committed from this instant
	// on reaches the channel: the replay can only overlap the live stream,
	// never leave a hole in it, and the overlap is removed below by ordinal.
	sub := s.bashFan.subscribe(runID, "")
	defer s.bashFan.unsubscribe(sub)

	replay, err := s.store.BashRun(correlated(ctx, req.Header()), runID)
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.watch-bash-run", err, logging.Fields{})
		return connect.NewError(connect.CodeInternal, ref)
	}
	if len(replay.Rows) == 0 {
		ref := refuseClass(classUnknownRun, SiteUnknownBashRun, "run",
			fmt.Sprintf("watch: this store holds no row for run %q, so there is no run to follow", runID))
		s.logRefusal(log, "store.rpc.watch-bash-run", ref, logging.Fields{})
		return connect.NewError(connect.CodeNotFound, ref)
	}

	replayed := make(map[uint64]struct{}, len(replay.Rows))
	// THE REPLAY SERVES EVERY STORED ROW, TERMINAL OR NOT.
	//
	// Returning at the terminal row dropped anything first inserted after it —
	// a late delta the sidecar reached only once the spool was already closed,
	// which is ordinary rather than exceptional. The consumer would then have
	// concatenated a run that produced less output than it did, with no way to
	// tell. So the natural end fires AFTER the last stored row, once a terminal
	// has been sent.
	terminated := false
	var terminalSeq uint64
	for _, row := range replay.Rows {
		if err := s.sendBashRow(log, stream, row); err != nil {
			return err
		}
		replayed[row.WriteSeq] = struct{}{}
		if db.BashRowIsTerminal(row.Row) {
			terminated = true
			terminalSeq = row.WriteSeq
		}
	}
	if terminated {
		// The run is already over: it can never speak again, so the stream ends
		// rather than standing open on it.
		log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run", WriteSeq: terminalSeq},
			"bash run watch ended: the terminal row was already stored replayed=%d", len(replayed))
		return nil
	}
	// The headers go out before the tail blocks, for the reason openStream
	// documents: until they do, the producer that would write the next row is
	// itself still waiting on this stream.
	if err := s.openStream(ctx, log, "store.rpc.watch-bash-run"); err != nil {
		return err
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run", WriteSeq: replay.PinSeq},
		"bash run watch live after replay replayed=%d", len(replay.Rows))

	for {
		// Overflow is checked FIRST and on its own, so a subscriber already
		// dropped ends deterministically instead of racing the rows still
		// sitting in its buffer.
		select {
		case <-sub.overflow:
			return s.endBashOverflowed(log, sub)
		default:
		}
		select {
		case <-sub.overflow:
			return s.endBashOverflowed(log, sub)
		case <-s.done:
			log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run"}, "bash run watch ended: the store is shutting down")
			return nil
		case <-ctx.Done():
			log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run"}, "bash run watch ended: the caller went away")
			return nil
		case row := <-sub.items:
			if row.WriteSeq <= replay.PinSeq {
				log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run", WriteSeq: row.WriteSeq}, "dropping a row at or below the pin")
				continue
			}
			if _, duplicate := replayed[row.WriteSeq]; duplicate {
				delete(replayed, row.WriteSeq)
				log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run", WriteSeq: row.WriteSeq}, "dropping a row the replay already delivered")
				continue
			}
			if err := s.sendBashRow(log, stream, row); err != nil {
				return err
			}
			if db.BashRowIsTerminal(row.Row) {
				log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run", WriteSeq: row.WriteSeq},
					"bash run watch ended: the run reached its terminal")
				return nil
			}
		}
	}
}

func (s *Server) sendBashRow(log *logging.Logger, stream *connect.ServerStream[storev1.WatchBashRunResponse], row BashRowWritten) error {
	if err := stream.Send(&storev1.WatchBashRunResponse{Row: row.Row}); err != nil {
		log.Log(logging.Fields{Operation: "store.rpc.watch-bash-run", Level: "warn", WriteSeq: row.WriteSeq},
			"sending a row to the bash watcher failed: %v", err)
		return err
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-bash-run", WriteSeq: row.WriteSeq}, "bash row delivered")
	return nil
}

func (s *Server) endBashOverflowed(log *logging.Logger, sub *sink[BashRowWritten]) error {
	log.Log(logging.Fields{Operation: "store.rpc.watch-bash-run", Level: "warn"},
		"bash run watch ended: this subscriber overflowed its buffer and must re-open dropped=%d", sub.dropped)
	return connect.NewError(connect.CodeResourceExhausted, refuse(SiteWatchBufferOverflow, "run",
		"watch: the subscriber fell too far behind its buffer; re-open to replay the run"))
}

// publishBashRows fans the committed run rows out and warns for every watcher
// that could not keep up. It runs AFTER the commit: nothing is ever published
// that is not already durable.
func (s *Server) publishBashRows(log *logging.Logger, producer string, rows []BashRowWritten) {
	if len(rows) == 0 {
		return
	}
	overflowed := s.bashFan.publish(rows)
	log.LogVerbose(logging.Fields{Operation: "store.fanout.publish-bash", Producer: producer},
		"published rows=%d bash_watchers=%d", len(rows), s.bashFan.subscribers())
	for _, sub := range overflowed {
		log.Log(logging.Fields{Operation: "store.fanout.overflow", Level: "warn", TaskID: sub.key},
			"bash watch buffer overflowed; ending this subscriber's stream buffer=%d dropped=%d", s.bashFan.buffer, sub.dropped)
	}
}
