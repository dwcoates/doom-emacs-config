#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# lib-realtest-backup.sh — copy the owner's live state aside before a realtest.
#
# A realtest drives the OWNER'S ACTUAL EDITOR against the OWNER'S ACTUAL state:
# ~/.claude-emacs/wsm.db holds every workspace, its branch, its selection and
# its held prompts, and the store's events.db holds every conversation the
# sidecar has ever picked up. Nothing in a realtest is supposed to write to
# either — but "supposed to" is not a guarantee, and the failure mode is the
# owner losing their workspaces, so the copy is taken first and unconditionally.
#
# THE HELPER REFUSES TO OVERWRITE AN EXISTING BACKUP. That is the one rule here
# and it is not a convenience check: a second run reusing a stamp would replace
# the copy of the state as it was BEFORE the first run with a copy of the state
# as the first run left it, which is precisely the artifact somebody would reach
# for after the first run went wrong. A refusal costs a re-run; a silent
# overwrite costs the thing the backup was for.
#
# The `-wal` and `-shm` siblings travel WITH the database, because a WAL
# database is not one file: restoring a `.db` without the `-wal` standing beside
# it discards every transaction the daemon had committed to the log and not yet
# checkpointed. They are copied when present and their absence is not an error —
# SQLite creates them on demand and a cleanly closed database has neither.
#
# Sourced by bin/realtest.sh, and tested by bin/test-realtest.sh.

# realtest_backup_stamp — one timestamp for a whole run's backups, so every
# copy a run takes carries the same suffix and they are recognizable as a set.
realtest_backup_stamp() {
    date +%Y%m%d-%H%M%S
}

# realtest_backup_suffix STAMP — the sibling suffix a backup takes.
realtest_backup_suffix() {
    printf '.realtest-bak-%s' "$1"
}

# realtest_backup_file SRC STAMP — clone SRC to its `.realtest-bak-STAMP`
# sibling and print the destination.
#
# Exit 0 with the path printed on success; exit 0 and print nothing when SRC
# does not exist (a `-wal` that is not there is not a failure); exit 1 when the
# destination already exists or the copy fails.
realtest_backup_file() {
    local src="$1" stamp="$2" dest
    if [ -z "$src" ] || [ -z "$stamp" ]; then
        echo "realtest_backup_file: a source and a stamp are required" >&2
        return 1
    fi
    if [ ! -e "$src" ]; then
        return 0
    fi
    dest="${src}$(realtest_backup_suffix "$stamp")"
    if [ -e "$dest" ]; then
        echo "realtest_backup_file: refusing to overwrite the existing backup $dest" >&2
        echo "realtest_backup_file: it holds the state as it was BEFORE an earlier run; overwriting it would replace that with the state that run left behind" >&2
        return 1
    fi
    # `cp -c` asks for an APFS clone (clonefile(2)): the backup shares the
    # source's blocks and costs no real disk until one side is later written
    # to, instead of duplicating a database that runs into double digits of
    # gigabytes on EVERY SINGLE RUN. `-p` carries the mtime, which is how a
    # reader tells which backup belongs to which run once the stamp is no
    # longer in front of them.
    if cp -c -p "$src" "$dest" 2>/dev/null; then
        printf '%s\n' "$dest"
        return 0
    fi
    # Not every volume is APFS, and not every `cp` understands `-c` at all.
    # The clone is for disk economy; the backup's correctness only needs a
    # real copy, so a clone failure falls back to a plain copy instead of
    # failing the whole backup over a feature that was never the point.
    echo "realtest_backup_file: cp -c (clone) failed for $src, falling back to a plain copy" >&2
    rm -f "$dest"
    if ! cp -p "$src" "$dest"; then
        echo "realtest_backup_file: could not copy $src to $dest" >&2
        return 1
    fi
    printf '%s\n' "$dest"
}

# realtest_backup_database DB STAMP — back up a SQLite database with its `-wal`
# and `-shm` siblings, printing every destination.
#
# Fails if ANY of the three that exists cannot be copied. A partial backup is
# worse than none: it looks like a backup.
realtest_backup_database() {
    local db="$1" stamp="$2" suffix out status=0
    for suffix in "" "-wal" "-shm"; do
        if ! out="$(realtest_backup_file "${db}${suffix}" "$stamp")"; then
            status=1
            continue
        fi
        [ -n "$out" ] && printf '%s\n' "$out"
    done
    return "$status"
}

# realtest_prune_backups DB KEEP — delete every `.realtest-bak-*` set for DB
# (the db plus its `-wal`/`-shm` siblings) except the KEEP most recent,
# printing one line per file removed.
#
# A set's age is read from its stamp, not the filesystem mtime, because the
# stamp is what ties the three files of one set together in the first place.
# The primary `.db.realtest-bak-STAMP` file is what a set is discovered from;
# a `-wal`/`-shm` sibling with no db of its own is not a set this counts.
realtest_prune_backups() {
    local db="$1" keep="$2" pattern stamps stamp index=0 suffix f
    if [ -z "$db" ] || [ -z "$keep" ]; then
        echo "realtest_prune_backups: a database and a keep count are required" >&2
        return 1
    fi
    pattern="${db}.realtest-bak-"
    # Newest stamp first: the stamp sorts lexicographically the same as it
    # sorts in time (realtest_backup_stamp is YYYYmmdd-HHMMSS), so a reverse
    # sort orders sets newest-to-oldest without parsing dates.
    stamps="$(
        for f in "${pattern}"*; do
            [ -e "$f" ] || continue
            printf '%s\n' "${f#"$pattern"}"
        done | sort -r
    )"
    [ -n "$stamps" ] || return 0
    while IFS= read -r stamp; do
        [ -n "$stamp" ] || continue
        index=$((index + 1))
        [ "$index" -gt "$keep" ] || continue
        for suffix in "" "-wal" "-shm"; do
            f="${db}${suffix}.realtest-bak-${stamp}"
            if [ -e "$f" ]; then
                rm -f "$f"
                printf '%s\n' "$f"
            fi
        done
    done <<EOF
$stamps
EOF
}

# realtest_free_kib DIR — free space in KiB on the filesystem holding DIR, or
# nothing if it cannot be determined (a caller that gets nothing back should
# not treat that as "no space").
realtest_free_kib() {
    df -Pk "$1" 2>/dev/null | awk 'NR==2 {print $4}'
}
