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

## The route to equal visible heights (built, 2026-10-13)

Owner rulings: draw every icon as an IMAGE from our own bundled SVGs
(`images/glyphs/<name>.svg`, which the webapp sidebar draws too), at the
tallest height that provably leaves the tab bar's rows as tall as a text row.

How tall an image can be, measured by `measure.el`'s image section (a square
SVG at each height, `:ascent` as listed, beside Menlo 13px text whose row is
15px):

| `:ascent` | 6  | 8  | 10 | 12 | 13 | 14 | 15 | 16 | 17 |
|-----------|----|----|----|----|----|----|----|----|----|
| 50 (default) | 15 | 16 | 17 | 18 | 19 | 19 | 20 | 20 | 21 |
| `center`  | 15 | 15 | 15 | 15 | 15 | 15 | 15 | 16 | 17 |
| 80        | 15 | 15 | 15 | 15 | 15 | 15 | 15 | 16 | 17 |
| 100       | 15 | 15 | 15 | 15 | 16 | 17 | 18 | 19 | 20 |

- `:ascent center` puts (HEIGHT + ASCENT - DESCENT) / 2 above the baseline
  and the rest below, so at HEIGHT = ASCENT + DESCENT (the text row) it fills
  exactly the text's ascent and descent: the row keeps its height, and one
  pixel more grows it.  Each of the six real pictures measured the same: 15px
  keeps a 15px row, 16px makes it 16.
- So every picture is drawn at the MEASURED text row's height, centered
  (`agent-repl-status-glyph-image-height` in `lisp/status.el`), and that
  function measures the row holding the image beside the text's and fails
  hard if it grew.
- Each SVG's viewBox is 16 units tall and its picture touches the top and
  bottom, so every glyph is exactly the row's height on screen.
- Rasterized at the requested height alone, an image is blown up 2x on a
  Retina screen and draws soft; the pictures are rasterized at
  `frame-scale-factor` times the height and drawn back down by `:scale`.
- The developer documentation is AGENTS.md, "Status glyphs are one set of
  pictures, drawn as tall as a text row".

## Re-measuring

`2026-10-13-tab-bar-icon-heights/measure.el` starts its own windowed
`Emacs -Q`, measures every icon at every size and raise, writes a results file
and exits (60s safety exit):

```sh
OUT=/tmp/tabicons.txt; open -gjnW -a /Applications/Emacs.app --env TABICONS_OUT=$OUT \
  --args -Q -l $PWD/modules/app/agent-repl/docs/investigations/2026-10-13-tab-bar-icon-heights/measure.el
cat $OUT
```

`tab-bar-height` reads a constant in that hidden frame, so the buffer-line
column is the one to read.

It then measures images the same way: every height from 6 to 18px at each
`:ascent`, then every picture in `images/glyphs/` at the text row's height and
one pixel more.
