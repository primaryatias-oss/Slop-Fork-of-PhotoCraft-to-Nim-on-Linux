//! Oracle commands for the Nim port of the Affinity reader (`crates/affinity`, see nim/PORTING.md).
//!
//! `af.read FILE...` reads each document and prints a canonical dump of the model (floats as bit
//! patterns, strings escaped, pixel data hashed) or the error; `af.preview FILE...` prints the
//! embedded preview's size and position. `nim/tools/afcheck.nim` prints the same dumps.

use photocraft_affinity::model::{Affine, Document, Image, Kind, Node, Path, Pixels, Point, Rect};
use photocraft_affinity::paint::{Color, Paint};
use photocraft_affinity::{Limits, preview, read};

fn fnv(b: &[u8]) -> u64 {
    let mut h = 0xcbf2_9ce4_8422_2325u64;
    for &x in b {
        h ^= u64::from(x);
        h = h.wrapping_mul(0x100_0000_01b3);
    }
    h
}
fn f(v: f64) -> String {
    v.to_bits().to_string()
}
fn s(v: &str) -> String {
    let mut o = String::new();
    for &b in v.as_bytes() {
        if (0x21..=0x7e).contains(&b) && b != b'\\' {
            o.push(b as char);
        } else {
            o.push_str(&format!("\\x{b:02x}"));
        }
    }
    o
}
fn rect(r: &Rect) -> String {
    format!("{},{},{},{}", f(r.x0), f(r.y0), f(r.x1), f(r.y1))
}
fn pt(p: &Point) -> String {
    format!("{},{}", f(p.x), f(p.y))
}
fn aff(a: &Affine) -> String {
    a.0.iter().map(|v| f(*v)).collect::<Vec<_>>().join(",")
}
fn path(p: &Path) -> String {
    let mut o = format!("n={}", p.subpaths.len());
    for sp in &p.subpaths {
        o.push_str(&format!(" sp c={} s={} segs=", u8::from(sp.closed), pt(&sp.start)));
        o.push_str(&sp.segments.iter().map(|g| g.iter().map(pt).collect::<Vec<_>>().join(";")).collect::<Vec<_>>().join("|"));
    }
    o
}
fn color(c: &Color) -> String {
    match c {
        Color::Rgb { r, g, b, a } => format!("rgb({},{},{},{})", f(*r), f(*g), f(*b), f(*a)),
        Color::Cmyk { c, m, y, k, a } => format!("cmyk({},{},{},{},{})", f(*c), f(*m), f(*y), f(*k), f(*a)),
        Color::Gray { v, a } => format!("gray({},{})", f(*v), f(*a)),
        Color::Lab { l, a, b, alpha } => format!("lab({},{},{},{})", f(*l), f(*a), f(*b), f(*alpha)),
    }
}
fn paint(p: &Paint) -> String {
    match p {
        Paint::None => "none".into(),
        Paint::Solid(c) => format!("solid {}", color(c)),
        Paint::Gradient(g) => {
            let stops = g.stops.iter().map(|st| format!("{}/{}/{}", f(st.offset), color(&st.color), f(st.bias))).collect::<Vec<_>>().join(" ");
            format!("grad k={} xf={} stops={}", g.kind as u8, aff(&g.transform), stops)
        }
    }
}
fn image(i: &Image) -> String {
    let (kind, data) = match &i.pixels {
        Pixels::Rgba8(v) => ("rgba", v),
        Pixels::Encoded(v) => ("enc", v),
    };
    format!("w={} h={} px={} len={} fnv={:016x} xf={}", i.width, i.height, kind, data.len(), fnv(data), aff(&i.transform))
}
fn node(n: &Node, depth: usize, o: &mut Vec<String>) {
    o.push(format!(
        "node d={} class={} name={} vis={} lock={} op={} blend={}",
        depth,
        s(&n.class.to_string()),
        s(&n.name),
        u8::from(n.visible),
        u8::from(n.locked),
        f(n.opacity),
        n.blend as u8
    ));
    match &n.kind {
        Kind::Layer => o.push(" kind layer".into()),
        Kind::Group => o.push(" kind group".into()),
        Kind::Unsupported => o.push(" kind unsupported".into()),
        Kind::Artboard { rect: r, background } => {
            o.push(format!(" kind artboard {}", rect(r)));
            for b in background {
                o.push(format!("  bg {}", paint(b)));
            }
        }
        Kind::Shape { path: p, fills, strokes, even_odd } => {
            o.push(format!(" kind shape eo={}", u8::from(*even_odd)));
            o.push(format!("  path {}", path(p)));
            for fl in fills {
                o.push(format!("  fill {}", paint(fl)));
            }
            for st in strokes {
                let dash = st.dash.as_ref().map_or("-".to_string(), |(d, ph)| format!("{} phase={}", d.iter().map(|v| f(*v)).collect::<Vec<_>>().join(","), f(*ph)));
                o.push(format!(
                    "  stroke w={} cap={} join={} miter={} align={} behind={} dash={}",
                    f(st.width),
                    st.cap as u8,
                    st.join as u8,
                    f(st.miter_limit),
                    st.align as u8,
                    u8::from(st.behind),
                    dash
                ));
                o.push(format!("   spaint {}", paint(&st.paint)));
            }
        }
        Kind::Text(t) => {
            o.push(format!(
                " kind text align={} anchor={} frame={} xf={}",
                t.align as u8,
                pt(&t.anchor),
                t.frame.as_ref().map_or("-".to_string(), rect),
                aff(&t.transform)
            ));
            for r in &t.runs {
                o.push(format!(
                    "  run text={} ps={} fam={} w={} it={} size={} tr={} lead={} fill={}",
                    s(&r.text),
                    s(&r.postscript),
                    s(&r.family),
                    r.weight,
                    u8::from(r.italic),
                    f(r.size),
                    f(r.tracking),
                    r.leading.map_or("-".to_string(), f),
                    paint(&r.fill)
                ));
            }
        }
        Kind::Image(i) => o.push(format!(" kind image {}", image(i))),
    }
    if let Some(m) = &n.mask {
        o.push(format!(" mask {}", path(m)));
    }
    if let Some(m) = &n.pixel_mask {
        o.push(format!(" pmask {}", image(m)));
    }
    for c in &n.children {
        node(c, depth + 1, o);
    }
}

pub fn dump(d: &Document) -> Vec<String> {
    let mut o = vec![format!("ok dpi={} saved={}", f(d.dpi), d.saved_by.as_deref().map_or("-".to_string(), s))];
    for sp in &d.spreads {
        o.push(format!("spread {} transparent={} pages={}", rect(&sp.bounds), u8::from(sp.transparent), sp.pages.len()));
        for p in &sp.pages {
            o.push(format!(" page {}", rect(p)));
        }
        for n in &sp.nodes {
            node(n, 0, &mut o);
        }
    }
    for w in &d.warnings {
        o.push(format!("warn {}", s(w)));
    }
    o
}

pub fn run(args: &[String]) -> bool {
    match args[1].as_str() {
        "af.read" => {
            // A panic in the reader (it has some on hostile input) is reported, not fatal.
            std::panic::set_hook(Box::new(|_| {}));
            for p in &args[2..] {
                println!("== {p}");
                let b = std::fs::read(p).unwrap();
                match std::panic::catch_unwind(|| read(&b, Limits::default())) {
                    Ok(Ok(d)) => {
                        for l in dump(&d) {
                            println!("{l}");
                        }
                    }
                    Ok(Err(e)) => println!("err {e}"),
                    Err(_) => println!("panic"),
                }
            }
        }
        "af.preview" => {
            for p in &args[2..] {
                println!("== {p}");
                let b = std::fs::read(p).unwrap();
                match preview(&b) {
                    Ok(pv) => println!("ok {} {} {} {}", pv.width, pv.height, pv.png.as_ptr() as usize - b.as_ptr() as usize, pv.png.len()),
                    Err(e) => println!("err {e}"),
                }
            }
        }
        _ => return false,
    }
    true
}

/// Debug: `af.zstd FILE OFFSET LEN SIZE` streams one zstd entry as the reader does, printing errors.
pub fn run_zstd(args: &[String]) -> bool {
    if args[1] != "af.zstd" {
        return false;
    }
    use std::io::Read;
    let b = std::fs::read(&args[2]).unwrap();
    let (off, len, size): (usize, usize, usize) = (args[3].parse().unwrap(), args[4].parse().unwrap(), args[5].parse().unwrap());
    let mut src = &b[off..off + len];
    let d = ruzstd::decoding::StreamingDecoder::new_with_max_window_size(&mut src, 64 << 20);
    match d {
        Err(e) => println!("init error: {e}"),
        Ok(d) => {
            let mut out = Vec::new();
            let r = d.take(size as u64 + 1).read_to_end(&mut out);
            println!("read {:?} -> {} bytes", r.map_err(|e| e.to_string()), out.len());
        }
    }
    true
}

/// Debug: `skr.d FONT SIZE` prints skrifa's 'd' bounds and cap height (text first baselines).
pub fn run_skr(args: &[String]) -> bool {
    if args[1] != "skr.d" {
        return false;
    }
    use skrifa::MetadataProvider;
    let data = std::fs::read(&args[2]).unwrap();
    let font = skrifa::FontRef::from_index(&data, 0).unwrap();
    let size = skrifa::instance::Size::new(args[3].parse().unwrap());
    let loc = skrifa::instance::LocationRef::default();
    let g = font.charmap().map('d');
    let b = g.and_then(|g| font.glyph_metrics(size, loc).bounds(g));
    let m = font.metrics(size, loc);
    println!("d={:?} bounds={:?} cap={:?} ascent={}", g, b, m.cap_height, m.ascent);
    true
}
