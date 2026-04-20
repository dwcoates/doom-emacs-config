#!/bin/bash
# prepare-commit-msg hook: prefix claude-repl commits with a random emoji.
#
# Install into .git/hooks/prepare-commit-msg or use
# M-x claude-repl-install-commit-emoji-hook from Emacs.
#
# Only modifies commit messages containing "(claude-repl)".
# Skips messages that already start with a non-ASCII character (emoji).

set -euo pipefail

MSG_FILE="$1"
# $2 is the source of the commit message (message, template, merge, squash, commit)
# $3 is the commit SHA (only for amend)
MSG_SOURCE="${2:-}"

# Only act on user-authored messages (not merge/squash templates)
if [[ "$MSG_SOURCE" == "merge" ]] || [[ "$MSG_SOURCE" == "squash" ]]; then
    exit 0
fi

MSG=$(cat "$MSG_FILE")

# Only prefix commits with (claude-repl) scope
if [[ "$MSG" != *"(claude-repl)"* ]]; then
    exit 0
fi

# Already has a non-ASCII prefix (likely emoji)? Skip.
FIRST_BYTE=$(echo -n "$MSG" | head -c 1 | od -An -tx1 | tr -d ' ')
if [[ "${FIRST_BYTE}" > "7f" ]]; then
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

# Extract commit type from conventional commit format
TYPE=$(echo "$MSG" | grep -oP '^\K[a-z]+(?=\()' || echo "")

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

# Pick a random emoji from the pool
EMOJI="${POOL[$((RANDOM % ${#POOL[@]}))]}"

# Prepend emoji to the commit message
echo "$EMOJI $MSG" > "$MSG_FILE"
