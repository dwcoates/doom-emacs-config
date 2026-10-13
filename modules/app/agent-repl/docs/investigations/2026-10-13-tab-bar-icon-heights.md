# Tab-bar icon heights (2026-10-13)

Owner goal: every icon the Emacs tab bar can draw before a workspace name has
the SAME VISIBLE HEIGHT, as large as possible, and none of them ever makes the
tab bar's rows taller than a plain text row (a taller row makes two rows of
tabs clip in the space allotted to the tab bar).

## How Emacs decides a row's height

- A row is as tall as the largest space any character on it RESERVES (its
  font's ascent above the baseline plus descent below), not as tall as what the
  character visibly draws.
- The owner's tab font is Menlo 13px: it reserves 12px + 3px, so a text row is
  15px.
- Apple Color Emoji reserves far more than it draws: at 13px it reserves
  16 + 5 = 21px while its picture is about 16px.
- Emacs has no way to make a font reserve less (`:height` scales both; the
  `line-height` property only adds; `(raise N)` measured no help, see below).

## The icons, measured on the owner's frame

Row height in px of a Menlo 13 row holding the icon at each requested size
(from `measure.el` below; `buffer-text-pixel-size` in the live Emacs agrees):

| requested px | 6  | 7  | 8  | 9  | 10 | 11 | 12 | 13 |
|--------------|----|----|----|----|----|----|----|----|
| any emoji    | 15 | 15 | 15 | 16 | 17 | 18 | 20 | 21 |
| `●`, `≡`     | 15 | 15 | 15 | 15 | 15 | 15 | 15 | 15 |

A raise of 0.05 to 0.15 at 9px still measured 16: raising does not buy 9px.

| Icon | Meaning            | Font  | Max px | Visible height (points) |
|------|--------------------|-------|--------|-------------------------|
| ❄️   | cold gate          | emoji | 8      | ~9.9 (was 16 at 13px)   |
| 🎯   | one-shot           | emoji | 8      | 8.5                     |
| ❗   | permission gate    | emoji | 8      | ~8.0                    |
| ❓   | question gate      | emoji | 8      | ~8.0                    |
| ●    | compaction / attention | Menlo | 13 | ~7.9                    |
| ≡    | failed merge       | Menlo | 13     | ~5.7                    |

Visible heights of ❄️ (at 13px) and 🎯 (at 8px) were measured from screen
pixels; the rest are derived as requested px x 1.24 (Apple Color Emoji draws
its picture about 1.24x the requested size) x the share of its square the
picture fills.

## Why equal visible heights are not reachable with fonts

The share of its square each picture fills, measured at the emoji font's
native 160px: ❄️ 99%, 🎯 84%, ❗ and ❓ 81% (and narrow), Menlo `●` 61%,
`≡` 44%. A font cannot crop that margin, and the 8px emoji cap leaves 🎯
at 8.5 points while the text's `f` ascender is about 9.9.

## The snowflake fix (landed with this document)

`agent-repl-status-glyph-height` (lisp/status.el) asked `font-at` which font
draws a glyph. For `❄️` (U+2744 U+FE0F) it answered Menlo, while Emacs composes
the sequence with Apple Color Emoji, so the snowflake stayed at 13px and grew
every row to 18px. Each glyph is now sized by measuring the row it lays out
(`buffer-text-pixel-size` on the tab bar's face) and taking the largest size
whose row is no taller than the text's, returned as an absolute `:height`.

## The route to equal visible heights (not built)

Draw every icon as an IMAGE: an image reserves exactly its pixel height, so an
image up to the text row's height (15px) can never grow the row. Crop each
icon to its picture and scale every one to one shared visible height.
Sources: render Apple's emoji on the machine at install/build time (not
committed: redistribution is a licensing question), or bundle our own SVG
icons (the sidebar would then need the same icons so both surfaces match).

## Re-measuring

`2026-10-13-tab-bar-icon-heights/measure.el` starts its own windowed
`Emacs -Q`, measures every icon at every size and raise, writes a results file
and exits (60s safety exit):

```sh
OUT=/tmp/tabicons.txt; open -gjnW -a /Applications/Emacs.app --env TABICONS_OUT=$OUT \
  --args -Q -l modules/app/agent-repl/docs/investigations/2026-10-13-tab-bar-icon-heights/measure.el
cat $OUT
```

`tab-bar-height` reads a constant in that hidden frame, so the buffer-line
column is the one to read.
