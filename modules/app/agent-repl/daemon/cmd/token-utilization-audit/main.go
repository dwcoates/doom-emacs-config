// Command token-utilization-audit enumerates or explicitly migrates durable
// response-accounting rows whose model identity is blank.  Audit mode is the
// command's normal mode and opens SQLite read-only.
package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"claude-repld/internal/dlog"
	"claude-repld/internal/statedb"
)

func main() {
	logger := dlog.New(os.Stderr, io.Discard, false)
	if err := run(os.Args[1:], os.Stdout, logger); err != nil {
		_ = logger.EmitNormal(dlog.GlobalScope(), dlog.Event{Runtime: dlog.RuntimeDaemon, Level: dlog.LevelError, Operation: "token-utilization-model-audit", Message: "token utilization model audit failed", Context: map[string]any{"outcome": "error", "error": err.Error()}})
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func run(args []string, output io.Writer, logger *dlog.Logger) error {
	flags := flag.NewFlagSet("token-utilization-audit", flag.ContinueOnError)
	flags.SetOutput(io.Discard)
	stateDB := flags.String("db", "", "path to an existing claude-repld SQLite state database")
	action := flags.String("action", "audit", "operator action: audit, quarantine, or delete")
	apply := flags.Bool("apply", false, "execute the selected quarantine or delete action")
	if err := flags.Parse(args); err != nil {
		return fmt.Errorf("token-utilization-audit: parse flags: %w", err)
	}
	if *stateDB == "" {
		return fmt.Errorf("token-utilization-audit: -db is required")
	}
	if flags.NArg() != 0 {
		return fmt.Errorf("token-utilization-audit: unexpected argument %q", flags.Arg(0))
	}
	switch *action {
	case "audit":
		if *apply {
			return fmt.Errorf("token-utilization-audit: -apply is only valid with -action=quarantine or -action=delete")
		}
		db, err := statedb.OpenReadOnly(*stateDB)
		if err != nil {
			return err
		}
		defer db.Close()
		invalid, err := statedb.AuditBlankModelTokenUtilizations(db)
		if err != nil {
			return err
		}
		return reportAndLog(output, logger, statedb.TokenUtilizationModelAuditReport{Invalid: invalid}, "read-only-audit")
	case string(statedb.TokenUtilizationAuditQuarantine), string(statedb.TokenUtilizationAuditDelete):
		if !*apply {
			return fmt.Errorf("token-utilization-audit: -action=%s requires explicit -apply", *action)
		}
		db, err := statedb.OpenExisting(*stateDB)
		if err != nil {
			return err
		}
		defer db.Close()
		report, err := statedb.MigrateBlankModelTokenUtilizations(db, statedb.TokenUtilizationAuditAction(*action))
		if err != nil {
			return err
		}
		return reportAndLog(output, logger, report, "migration-committed")
	default:
		return fmt.Errorf("token-utilization-audit: -action must be audit, quarantine, or delete; got %q", *action)
	}
}

func reportAndLog(output io.Writer, logger *dlog.Logger, report statedb.TokenUtilizationModelAuditReport, outcome string) error {
	if logger != nil {
		for _, row := range report.Invalid {
			if err := logger.EmitNormal(dlog.GlobalScope(), dlog.Event{Runtime: dlog.RuntimeDaemon, Level: dlog.LevelWarn, Operation: "token-utilization-model-audit", Message: "invalid durable token utilization model identity", AgentReplSessionID: row.AgentReplSessionID, ClaudeSessionID: row.ClaudeSessionID, RequestID: row.APIMessageID, Context: map[string]any{"action": reportAction(report), "field_path": "TokenUtilization.model", "source_plane": "durable-store", "api_message_id": row.APIMessageID, "root_turn_id": row.RootTurnID, "raw_model": row.Model, "outcome": outcome}}); err != nil {
				return fmt.Errorf("token-utilization-audit: log invalid row: %w", err)
			}
		}
		if err := logger.EmitNormal(dlog.GlobalScope(), dlog.Event{Runtime: dlog.RuntimeDaemon, Level: dlog.LevelInfo, Operation: "token-utilization-model-audit", Message: "token utilization model audit completed", Context: map[string]any{"action": reportAction(report), "field_path": "TokenUtilization.model", "source_plane": "durable-store", "candidate_count": len(report.Invalid), "mutated_count": report.Mutated, "outcome": outcome}}); err != nil {
			return fmt.Errorf("token-utilization-audit: log outcome: %w", err)
		}
	}
	return writeReport(output, report, outcome)
}

func reportAction(report statedb.TokenUtilizationModelAuditReport) string {
	if report.Action == "" {
		return "audit"
	}
	return string(report.Action)
}

func writeReport(output io.Writer, report statedb.TokenUtilizationModelAuditReport, outcome string) error {
	if output == nil {
		return fmt.Errorf("token-utilization-audit: report output is required")
	}
	if _, err := fmt.Fprintf(output, "TOKEN UTILIZATION MODEL AUDIT outcome=%q action=%q field_path=%q source_plane=%q candidate_count=%d mutated_count=%d\n", outcome, reportAction(report), "TokenUtilization.model", "durable-store", len(report.Invalid), report.Mutated); err != nil {
		return fmt.Errorf("token-utilization-audit: write report header: %w", err)
	}
	for _, row := range report.Invalid {
		if _, err := fmt.Fprintf(output, "invalid model identity: agent_repl_session_id=%q api_message_id=%q claude_session_id=%q root_turn_id=%q raw_model=%q field_path=%q source_plane=%q\n", row.AgentReplSessionID, row.APIMessageID, row.ClaudeSessionID, row.RootTurnID, row.Model, "TokenUtilization.model", "durable-store"); err != nil {
			return fmt.Errorf("token-utilization-audit: write report row: %w", err)
		}
	}
	return nil
}
