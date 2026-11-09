// Renders Zettel.app's icon at one pixel size: a New York serif "Z" in the
// canonical purple (AGENTS.md "The canonical purple", its fixed dark value
// #7d4bcf) on a rounded square of the macOS app-icon grid filled with the
// composer's background (the webapp's `--sidebar-ground` in the dark theme:
// `--sidebar-base` #1a1a1a mixed 88% with black, #171717). The icon is
// rendered from this source at build time, never committed as an image.
//
// Usage: ZettelIcon <pixels> <out.png>
import AppKit

guard CommandLine.arguments.count == 3, let pixels = Int(CommandLine.arguments[1]), pixels > 0 else {
    FileHandle.standardError.write(Data("usage: ZettelIcon <pixels> <out.png>\n".utf8))
    exit(2)
}
let size = CGFloat(pixels)
let rep = NSBitmapImageRep(
    bitmapDataPlanes: nil, pixelsWide: pixels, pixelsHigh: pixels, bitsPerSample: 8, samplesPerPixel: 4,
    hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
NSGraphicsContext.saveGraphicsState()
NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
// The app-icon grid: the body is about 80% of the canvas, its corners a
// continuous curve approximated at 22.5% of its width.
let inset = size * 0.0977
let body = NSRect(x: inset, y: inset, width: size - 2 * inset, height: size - 2 * inset)
let composerBackground = NSColor(srgbRed: 0x17 / 255, green: 0x17 / 255, blue: 0x17 / 255, alpha: 1)
composerBackground.setFill()
NSBezierPath(roundedRect: body, xRadius: body.width * 0.225, yRadius: body.width * 0.225).fill()
let descriptor = NSFont.systemFont(ofSize: body.height * 0.78, weight: .semibold).fontDescriptor.withDesign(.serif)!
let font = NSFont(descriptor: descriptor, size: body.height * 0.78)!
let canonicalPurple = NSColor(srgbRed: 0x7d / 255, green: 0x4b / 255, blue: 0xcf / 255, alpha: 1)
let line = CTLineCreateWithAttributedString(NSAttributedString(string: "Z", attributes: [.font: font, .foregroundColor: canonicalPurple]))
let bounds = CTLineGetBoundsWithOptions(line, .useGlyphPathBounds)
let context = NSGraphicsContext.current!.cgContext
context.textPosition = CGPoint(x: body.midX - bounds.midX, y: body.midY - bounds.midY)
CTLineDraw(line, context)
NSGraphicsContext.restoreGraphicsState()
do {
    try rep.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: CommandLine.arguments[2]))
} catch {
    FileHandle.standardError.write(Data("ZettelIcon: write \(CommandLine.arguments[2]): \(error)\n".utf8))
    exit(1)
}
