/**
 * Ambient declaration for markdown-it-task-lists, which ships no types.
 * The plugin renders GFM `- [ ]` / `- [x]` list items as disabled
 * checkboxes; it is applied via `md.use(taskLists)`.
 */
declare module "markdown-it-task-lists" {
  import type MarkdownIt from "markdown-it";

  interface TaskListsOptions {
    /** Render enabled (interactive) checkboxes. Default false (read-only). */
    enabled?: boolean;
    /** Wrap the item text in a <label>. Default false. */
    label?: boolean;
    /** Place the <label> after the checkbox. Default false. */
    labelAfter?: boolean;
  }

  const taskLists: (md: MarkdownIt, options?: TaskListsOptions) => void;
  export default taskLists;
}
