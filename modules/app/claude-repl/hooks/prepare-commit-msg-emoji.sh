#!/bin/bash
# prepare-commit-msg hook: inject a random emoji into commits whose
# conventional-commit scope matches the current git branch.
#
# Install into .git/hooks/prepare-commit-msg or use
# M-x claude-repl-install-commit-emoji-hook from Emacs.
#
# Convention enforced: <type>(<branch>): <emoji> <description>
#
# - Only acts on messages whose scope is the current branch name.
# - Skips messages whose description already starts with a non-ASCII
#   char (likely emoji from a prior run / hand-written).
# - Skips merge / squash auto-templates.

set -euo pipefail

MSG_FILE="$1"
# $2 is the source of the commit message (message, template, merge, squash, commit)
# $3 is the commit SHA (only for amend)
MSG_SOURCE="${2:-}"

# Only act on user-authored messages (not merge/squash templates)
if [[ "$MSG_SOURCE" == "merge" ]] || [[ "$MSG_SOURCE" == "squash" ]]; then
    exit 0
fi

BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")
if [[ -z "$BRANCH" ]] || [[ "$BRANCH" == "HEAD" ]]; then
    exit 0
fi

MSG=$(cat "$MSG_FILE")
FIRST_LINE=$(printf '%s' "$MSG" | head -n1)
REST_LINES=$(printf '%s' "$MSG" | tail -n +2)

# Escape branch for safe inclusion in a bash regex.
ESCAPED_BRANCH=$(printf '%s' "$BRANCH" | sed 's/[][\.*^$()+?{|/]/\\&/g')

# Match: <type>(<branch>): <description>
PREFIX_REGEX="^([a-z]+)\(${ESCAPED_BRANCH}\): (.*)$"
if [[ ! "$FIRST_LINE" =~ $PREFIX_REGEX ]]; then
    exit 0
fi

TYPE="${BASH_REMATCH[1]}"
DESCRIPTION="${BASH_REMATCH[2]}"

# Already has a non-ASCII description prefix (likely emoji)? Skip.
FIRST_BYTE=$(printf '%s' "$DESCRIPTION" | head -c 1 | od -An -tx1 | tr -d ' ')
if [[ -n "${FIRST_BYTE}" ]] && [[ "${FIRST_BYTE}" > "7f" ]]; then
    exit 0
fi

# --- Emoji pools by conventional-commit type ---

FEAT_EMOJIS=("✨" "🚀" "🎉" "🌟" "💡" "🎨" "🌈" "🔮" "🎯" "⚡" "🏗" "🧩" "🪅" "🌻" "🍀")
FIX_EMOJIS=("🔧" "🩹" "🐛" "🔨" "🛠" "🪛" "🏥" "💊" "🩺" "🪚" "🔩" "⛏" "🪠" "🧰" "🦷")
REFACTOR_EMOJIS=("♻" "🧹" "🪄" "🧬" "🏛" "🪆" "🎭" "🗿" "🧊" "💎" "🪨" "⚗" "🔬" "🧪" "📐")
TEST_EMOJIS=("🧪" "🔍" "🕵" "🎯" "📋" "✅" "🧫" "🔎" "📊" "🎓" "🧮" "📏" "⚖" "🏁" "🔬")
DOCS_EMOJIS=("📝" "📖" "📚" "🗒" "📄" "✏" "🖊" "📑" "📓" "🔖" "📰" "🏷" "🗞" "📃" "🗂")
STYLE_EMOJIS=("💅" "🎨" "🖌" "🎭" "👗" "💄" "🪞" "🎀" "🌸" "🦋" "🧶" "🪡" "🎏" "🏮" "🪭")
PERF_EMOJIS=("⚡" "🏎" "💨" "🚄" "🏃" "⏱" "🔥" "💪" "🦅" "🎿" "🏊" "🏋" "🧲" "⛷" "🏇")
CHORE_EMOJIS=("🔖" "📦" "🏷" "🔗" "📌" "🗃" "🧹" "📎" "🗄" "🛒" "🧺" "📍" "🪝" "🗑" "📮")
CI_EMOJIS=("🤖" "⚙" "🔄" "🏗" "🔀" "🛞" "🧩" "🪤" "⛓" "🎰" "🕹" "📡" "🛸" "🧭" "🏭")
WILDCARD_EMOJIS=(
    "🦄" "🐉" "🌵" "🍄" "🎸" "🪩" "🫧" "🧊" "🌋" "🦑" "🪸" "🎪" "🛸" "🪐" "🦕"
    "🐙" "🦥" "🦔" "🐝" "🦊" "🐸" "🐧" "🦉" "🐺" "🦁" "🐨" "🦋" "🐬" "🦈" "🐢"
    "🌮" "🍕" "🥨" "🧁" "🍩" "🫐" "🍉" "🥝" "🍇" "🧀" "🌶" "🥑" "🍑" "🫠" "🍣"
)

# Select pool based on type
case "$TYPE" in
    feat)     POOL=("${FEAT_EMOJIS[@]}") ;;
    fix)      POOL=("${FIX_EMOJIS[@]}") ;;
    refactor) POOL=("${REFACTOR_EMOJIS[@]}") ;;
    test)     POOL=("${TEST_EMOJIS[@]}") ;;
    docs)     POOL=("${DOCS_EMOJIS[@]}") ;;
    style)    POOL=("${STYLE_EMOJIS[@]}") ;;
    perf)     POOL=("${PERF_EMOJIS[@]}") ;;
    chore)    POOL=("${CHORE_EMOJIS[@]}") ;;
    ci)       POOL=("${CI_EMOJIS[@]}") ;;
    *)        POOL=("${WILDCARD_EMOJIS[@]}") ;;
esac

# 30% chance of wildcard injection for variety
if (( RANDOM % 100 < 30 )); then
    POOL=("${WILDCARD_EMOJIS[@]}")
fi

# --- Lookback exclusion: build list of emojis used by the last
# LOOKBACK conventional commits and remove them from POOL,
# deterministically guaranteeing variety from git history.
#
# Recognizes both formats:
#   Legacy:  <emoji> type(scope): description    -> first token
#   Current: type(scope): <emoji> description    -> first token after ': '
# The grep pattern accepts any scope so the post-branch-scope refactor
# doesn't lose history visibility.

LOOKBACK=50
RECENTS=()
extract_emoji() {
    local line="$1" tok byte
    # New format first: type(scope): EMOJI rest
    if [[ "$line" =~ ^[a-z]+\([^\)]+\):\ ([^[:space:]]+) ]]; then
        tok="${BASH_REMATCH[1]}"
    else
        tok="${line%% *}"
    fi
    [[ -z "$tok" ]] && return 0
    byte=$(printf '%s' "$tok" | head -c 1 | od -An -tx1 | tr -d ' \n')
    if [[ -n "$byte" ]] && [[ "${byte}" > "7f" ]]; then
        printf '%s' "$tok"
    fi
}

while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    emoji=$(extract_emoji "$line")
    [[ -n "$emoji" ]] && RECENTS+=("$emoji")
done < <(git log -n "$LOOKBACK" -E --grep='^[a-z]+\(.+\):' --format=%s 2>/dev/null || true)

filter_pool() {
    local result=() item recent skip
    for item in "${POOL[@]}"; do
        skip=0
        for recent in "${RECENTS[@]}"; do
            if [[ "$item" == "$recent" ]]; then skip=1; break; fi
        done
        [[ $skip -eq 0 ]] && result+=("$item")
    done
    POOL=("${result[@]}")
}

filter_pool

# Typed pool exhausted? Fall back to wildcard pool minus recents.
if [[ ${#POOL[@]} -eq 0 ]]; then
    POOL=("${WILDCARD_EMOJIS[@]}")
    filter_pool
fi

# Final fallback if even wildcard is exhausted.
if [[ ${#POOL[@]} -eq 0 ]]; then
    POOL=("${WILDCARD_EMOJIS[@]}")
fi

# Pick a random emoji from the pool
EMOJI="${POOL[$((RANDOM % ${#POOL[@]}))]}"

# Reassemble: <type>(<branch>): <emoji> <description>\n<rest>
NEW_FIRST_LINE="${TYPE}(${BRANCH}): ${EMOJI} ${DESCRIPTION}"
if [[ -n "$REST_LINES" ]]; then
    printf '%s\n%s' "$NEW_FIRST_LINE" "$REST_LINES" > "$MSG_FILE"
else
    printf '%s\n' "$NEW_FIRST_LINE" > "$MSG_FILE"
fi
