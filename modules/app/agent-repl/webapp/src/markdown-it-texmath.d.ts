/**
 * Ambient declaration for markdown-it-texmath, which ships no types. The
 * plugin parses TeX between the DELIMITERS families it is given into math
 * tokens and renders each with ENGINE (KaTeX here); it is applied via
 * `md.use(texmath, options)`.
 */
declare module "markdown-it-texmath" {
  import type MarkdownIt from "markdown-it";
  import type { KatexOptions } from "katex";

  interface TexmathOptions {
    /** The renderer: an object with KaTeX's `renderToString`. */
    engine: { renderToString(tex: string, options?: KatexOptions): string };
    /** The delimiter families, merged when several are named. */
    delimiters?: string | string[];
    /** Options handed to the engine on every render. */
    katexOptions?: KatexOptions;
  }

  const texmath: ((md: MarkdownIt, options?: TexmathOptions) => void) & {
    render(tex: string, displayMode: boolean, options: KatexOptions): string;
  };
  export default texmath;
}
