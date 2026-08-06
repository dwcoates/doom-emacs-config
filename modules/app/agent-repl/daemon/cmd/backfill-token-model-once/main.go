package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/statedb"
	"google.golang.org/protobuf/proto"
)

type row struct {
	sessionID string
	messageID string
	raw       []byte
	record    frontendv1.TokenUtilization
}

func main() {
	dbPath := flag.String("db", "", "state database")
	model := flag.String("model", "", "replacement model")
	flag.Parse()
	if *dbPath == "" || strings.TrimSpace(*model) == "" {
		fmt.Fprintln(os.Stderr, "-db and nonblank -model are required")
		os.Exit(2)
	}
	db, err := statedb.OpenExisting(*dbPath)
	if err != nil {
		panic(err)
	}
	defer db.Close()
	tx, err := db.Begin()
	if err != nil {
		panic(err)
	}
	defer tx.Rollback()
	result, err := tx.Query(`SELECT agent_repl_session_id, api_message_id, record FROM token_utilization ORDER BY agent_repl_session_id, api_message_id`)
	if err != nil {
		panic(err)
	}
	var targets []row
	for result.Next() {
		var candidate row
		if err := result.Scan(&candidate.sessionID, &candidate.messageID, &candidate.raw); err != nil {
			panic(err)
		}
		if err := proto.Unmarshal(candidate.raw, &candidate.record); err != nil {
			panic(err)
		}
		if strings.TrimSpace(candidate.record.GetModel()) == "" {
			targets = append(targets, candidate)
		}
	}
	if err := result.Err(); err != nil {
		panic(err)
	}
	if err := result.Close(); err != nil {
		panic(err)
	}
	for _, target := range targets {
		target.record.Model = *model
		updated, err := proto.Marshal(&target.record)
		if err != nil {
			panic(err)
		}
		write, err := tx.Exec(`UPDATE token_utilization SET record=? WHERE agent_repl_session_id=? AND api_message_id=? AND record=?`, updated, target.sessionID, target.messageID, target.raw)
		if err != nil {
			panic(err)
		}
		count, err := write.RowsAffected()
		if err != nil || count != 1 {
			panic(fmt.Sprintf("update %s/%s affected %d rows: %v", target.sessionID, target.messageID, count, err))
		}
		var check []byte
		if err := tx.QueryRow(`SELECT record FROM token_utilization WHERE agent_repl_session_id=? AND api_message_id=?`, target.sessionID, target.messageID).Scan(&check); err != nil {
			panic(err)
		}
		if !bytes.Equal(check, updated) {
			panic(fmt.Sprintf("verification mismatch for %s/%s", target.sessionID, target.messageID))
		}
	}
	if err := tx.Commit(); err != nil {
		panic(err)
	}
	fmt.Printf("backfilled=%d model=%q\n", len(targets), *model)
}
