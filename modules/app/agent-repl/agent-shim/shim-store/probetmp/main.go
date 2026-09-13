package main

import (
	"context"
	"database/sql"
	"fmt"
	"net/url"
	"os"
	"time"

	_ "modernc.org/sqlite"
)

// Grows the WAL under a pinned reader, then reports each batch's commit time.
func main() {
	path := os.Args[1]
	autockpt := os.Args[2] // "default" or "off"
	pin := os.Args[3] == "pin"

	dsn := "file:" + path + "?" + url.Values{
		"_pragma": {"journal_mode(WAL)", "busy_timeout(5000)", "synchronous(NORMAL)", "foreign_keys(ON)"},
		"_txlock": {"immediate"},
	}.Encode()
	db, err := sql.Open("sqlite", dsn)
	must(err)
	defer db.Close()
	must(db.Ping())
	ctx := context.Background()
	if autockpt == "off" {
		_, err := db.ExecContext(ctx, `PRAGMA wal_autocheckpoint=0`)
		must(err)
	}
	var reader *sql.Conn
	if pin {
		reader, err = db.Conn(ctx)
		must(err)
		_, err = reader.ExecContext(ctx, `BEGIN`)
		must(err)
		var n int64
		must(reader.QueryRowContext(ctx, `SELECT COUNT(*) FROM agent`).Scan(&n))
		defer reader.Close()
	}

	var maxSeqStart sql.NullInt64
	must(db.QueryRow(`SELECT MAX(write_seq) FROM entry`).Scan(&maxSeqStart))
	blob := make([]byte, 32*1024)
	worst := time.Duration(0)
	for round := 0; round < 400; round++ {
		t0 := time.Now()
		tx, err := db.BeginTx(ctx, nil)
		must(err)
		tBegin := time.Since(t0)
		t3 := time.Now()
		for i := 0; i < 7; i++ {
			seq := maxSeqStart.Int64 + int64(round*100+i+1)
			key := fmt.Sprintf("probetmp:%d:%d:%d", time.Now().UnixNano(), round, i)
			_, err := tx.ExecContext(ctx, `INSERT INTO entry (upsert_key, write_id, write_seq, plane, kind, book_agent_id, frame, first_inserted_at_ms, last_written_at_ms) VALUES (?,?,?,?,?,?,?,?,?)`,
				key, key, seq, 1, "page_line", "probe-book", blob, time.Now().UnixMilli(), time.Now().UnixMilli())
			must(err)
			_, err = tx.ExecContext(ctx, `INSERT INTO write_ledger (write_id, upsert_key, write_seq, applied_at_ms) VALUES (?,?,?,?)`,
				key, key, seq, time.Now().UnixMilli())
			must(err)
		}
		tIns := time.Since(t3)
		t4 := time.Now()
		must(tx.Commit())
		tCommit := time.Since(t4)
		total := time.Since(t0)
		if total > worst {
			worst = total
		}
		fi, _ := os.Stat(path + "-wal")
		walMB := 0.0
		if fi != nil {
			walMB = float64(fi.Size()) / (1 << 20)
		}
		if total > 100*time.Millisecond || round%50 == 0 {
			fmt.Printf("autockpt=%s pin=%v round %3d: total=%-8v begin=%-8v insert=%-8v COMMIT=%-8v wal=%.1fMB\n",
				autockpt, pin, round, total.Round(time.Millisecond), tBegin.Round(time.Millisecond),
				tIns.Round(time.Millisecond), tCommit.Round(time.Millisecond), walMB)
		}
	}
	fmt.Printf("autockpt=%s pin=%v WORST=%v\n", autockpt, pin, worst.Round(time.Millisecond))
}

func must(err error) {
	if err != nil {
		panic(err)
	}
}
