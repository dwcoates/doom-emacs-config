# What keeping workspace pages live costs, by component (2026-10-13)

Question (owner, 2026-10-13): is the sidebar most of the work of keeping every
workspace page live in the webhost, and is it most of the memory?

Short answer: the sidebar is the largest share of each page's DOM (91%), the
second-largest share of the main-thread work the streams do (about 27%, behind
the feed), and that work is duplicated in every page. But the work the streams
do is itself only about 7% of the webhost's CPU; most of the CPU is spent
somewhere the meter cannot see (rendering, style and layout, animations,
garbage collection), so the sidebar's real share of CPU is still open.

## How it was measured

- `page.stream-cost` (webapp/src/stream-cost.ts, landed in ffb8c4407): every
  workspace page records, once a minute, how many frames each of its streams
  handled and how long `watchStream` spent on them (the strict check plus the
  component's draw), and how many elements each component's region holds.
  Decoding happens in the transport before `watchStream` and is not included.
  A held roster drawn when a page becomes seen is drawn outside `watchStream`
  too; those draws are added from `sidebar.unseen-resumed` below.
- The webhost's WebKit page process (`com.apple.WebKit.WebContent`, pid 27327)
  sampled every 10s with `ps` for 10 minutes: resident memory and CPU time.
- One live roster push (58,912 bytes, from the running daemon) decoded 200
  times in Node with the webapp's own generated code.
- Window: 16:59:32 to 17:07:32, 7 workspace pages in the webhost, 51 window
  records. The owner was switching workspaces throughout (37 shows and hides
  of one page in 15 minutes), and several workspaces were working, so this is
  a busy sample, not an idle one.

## Main-thread work done by the streams

| Stream | Component | Work | Share | Frames | Per frame |
|---|---|---|---|---|---|
| `WatchFeed` | feed | 9,704ms | 66% | 6,418 | 1.5ms |
| `WatchWorkspaceRoster` | sidebar | 3,500ms + 409ms held draws | 27% | 512 | 6.8ms |
| `WatchPage` | the page's one connection | 471ms | 3% | 8,716 | 0.05ms |
| `WatchFooter` | footer | 444ms | 3% | 1,135 | 0.4ms |
| `WatchTopbar` | topbar | 163ms | 1% | 606 | 0.3ms |

- A sidebar update drawn costs 8ms typical and 24ms at worst per page (from
  31 windows); a sidebar update held by a page nobody sees costs 0.7ms.
- Decoding one 59KB sidebar update costs 1.1ms typical, 2.2ms slow, plus
  0.5ms for the strict check (Node; WebKit's engine differs but not by an
  order of magnitude). Every live page pays this on every update.
- The feed's cost is NOT duplicated: each workspace's feed is drawn only in
  its own page, and feeds already stay live on cold pages. The sidebar's cost
  IS duplicated: every page draws the same sidebar.
- Drawn once in the webhost instead of in 7 pages, the sidebar's 3.9s would
  have been about 0.6s, cutting the streams' total by about 23% in this
  sample. The saving grows with the number of open workspaces.

## Memory

| Region | Elements per page (median) |
|---|---|
| sidebar | 12,413 |
| feed | 1,075 |
| footer | 31 |
| topbar | 29 |
| whole page | 13,577 |

- The sidebar is 91% of every page's elements. The feed is small because it
  pages its history.
- The webhost process held 455MB to 1,587MB (median 1,280MB) for 7 pages,
  about 180MB per page at the median. It swings by a gigabyte, which looks
  like garbage collection.
- How much of that is the sidebar is NOT measured: WebKit gives a page no
  heap reading, and element counts are only a proxy. Each page also loads its
  own copy of the webapp's code and keeps its own decoded rosters, feed rows
  and caches, so the owner's guess that the sidebar is not most of the memory
  may well be right even though it is most of the elements.

## Where the CPU actually goes

- The webhost process used 208s of CPU in about 9.5 minutes, about 37% of one
  core on average. The streams' measured work was about 15s of that, about 7%.
- The other 93% is unattributed. Candidates, none measured yet:
  - style, layout and paint of 7 copies of a 12,000-element sidebar;
  - the sidebar's looping animations (attention blinks, status glyphs), which
    keep running in pages that are hidden with `visibility: hidden` (host.css
    says hidden pages keep their timers and animation frames, measured
    2026-10-12);
  - decoding every frame of every stream (not in the meter);
  - garbage collection, given the gigabyte swing in memory.
- So "most of the work is the sidebar" is not confirmed by the stream meter,
  and the larger question is what the other 93% is.

## What this means for the sidebar move

- The move is justified by the flicker (one copy cannot be stale), by the
  duplicated stream work (27% of the measured work, times the page count), and
  by the element count (91% of each page).
- After the move, the same meter shows directly what the sidebar cost, and
  the process CPU before and after shows whether the unattributed 93% was
  mostly the sidebar's rendering and animations.

## Next measurements

- Profile the webhost's CPU in WebKit's own inspector (Safari Develop menu,
  attached to the Emacs web view) during a busy minute, to attribute the 93%.
- Repeat this measurement after the sidebar moves into the webhost: same
  meter, same process sampling, same kind of activity.
