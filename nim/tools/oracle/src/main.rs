//! Rust reference for the Nim port: renders, imports, exports and filters documents with the
//! Rust crates so the Nim output can be compared (see nim/PORTING.md).
use photocraft_color::{BlendMode, Color, ColorMode, SampleType};
use photocraft_doc::*;
use photocraft_geom::*;
fn write_f32(path: &str, px: &[[f32; 4]]) {
    let mut out = Vec::with_capacity(px.len() * 16);
    for p in px { for v in p { out.extend_from_slice(&v.to_le_bytes()); } }
    std::fs::write(path, out).unwrap();
}
fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args[1].as_str() {
        "write" => {
            let mut d = Document::with_background("Rust", Size::new(300, 200), ColorMode::Rgb, SampleType::U16, Color::WHITE);
            let mut l = Layer::raster("Paint", d.pixel_format());
            l.surface_mut().unwrap().fill_rect(Rect::new(10, 10, 280, 150), &[0.25, 0.5, 0.75, 1.0]);
            l.blend = BlendMode::Multiply;
            l.opacity = 0.5;
            l.effects.items.push(Effect::default_drop_shadow());
            let mut m = LayerMask::hide_all();
            m.surface.fill_rect(Rect::new(0, 0, 100, 100), &[1.0]);
            l.mask = Some(m);
            d.layers.push(l);
            d.layers.push(Layer::new("Inv", LayerContent::Adjustment(Adjustment::Invert)));
            d.layers.push(Layer::group("G", vec![Layer::new("F", LayerContent::Fill(Fill::Solid(Color::rgb(1.0, 0.0, 0.0))))]));
            let b = photocraft_format::save_to_bytes(&d, &Default::default()).unwrap();
            std::fs::write(&args[2], b).unwrap();
        }
        "read" => {
            let b = std::fs::read(&args[2]).unwrap();
            match photocraft_format::load_from_bytes(&b) {
                Ok(d) => println!("OK {} layers, {}x{}, {:?} {:?}", d.layers.len(), d.size.width, d.size.height, d.mode, d.depth),
                Err(e) => println!("ERR {e}"),
            }
        }
        "render" => {
            let d = photocraft_format::load_from_bytes(&std::fs::read(&args[2]).unwrap()).unwrap();
            write_f32(&args[3], &photocraft_compose::flatten(&d).px);
        }
        "import" => {
            let b = std::fs::read(&args[2]).unwrap();
            match photocraft_io::import(&args[2], &b) {
                Ok(r) => {
                    let out = photocraft_format::save_to_bytes(&r.document, &Default::default()).unwrap();
                    std::fs::write(&args[3], out).unwrap();
                    for w in r.warnings { println!("warning: {w}"); }
                }
                Err(e) => println!("ERR {e}"),
            }
        }
        "export" => {
            let d = photocraft_format::load_from_bytes(&std::fs::read(&args[2]).unwrap()).unwrap();
            match photocraft_io::export(&d, &args[3], &Default::default()) {
                Ok(r) => { std::fs::write(&args[4], r.bytes).unwrap(); for w in r.warnings { println!("warning: {w}"); } }
                Err(e) => println!("ERR {e}"),
            }
        }
        "conv" => {
            let b = std::fs::read(&args[2]).unwrap();
            let opts = photocraft_io::ExportOptions { tiff_layers: std::env::var("TIFF_LAYERS").is_ok(), ..Default::default() };
            match photocraft_io::import(&args[2], &b) {
                Ok(r) => match photocraft_io::export(&r.document, &args[3], &opts) {
                    Ok(e) => { std::fs::write(&args[4], e.bytes).unwrap(); for w in r.warnings { println!("warning: {w}"); } for w in e.warnings { println!("warning: {w}"); } }
                    Err(e) => println!("ERR {e}"),
                },
                Err(e) => println!("ERR {e}"),
            }
        }
        "font-info" => {
            use skrifa::MetadataProvider;
            let data = std::fs::read(&args[2]).unwrap();
            let fr = skrifa::FontRef::from_index(&data, args.get(3).map_or(0, |s| s.parse().unwrap())).unwrap();
            let m = fr.metrics(skrifa::instance::Size::unscaled(), skrifa::instance::LocationRef::default());
            let name = |id| fr.localized_strings(id).english_or_first().map(|s| s.to_string()).unwrap_or_default();
            println!("family={} style={} ps={} upem={} ascent={} descent={} leading={} glyphs={} xh={:?} cap={:?} ul={:?} st={:?}",
                name(skrifa::string::StringId::FAMILY_NAME), name(skrifa::string::StringId::SUBFAMILY_NAME), name(skrifa::string::StringId::POSTSCRIPT_NAME),
                m.units_per_em, m.ascent, m.descent, m.leading, m.glyph_count, m.x_height, m.cap_height, m.underline.map(|d| (d.offset, d.thickness)), m.strikeout.map(|d| (d.offset, d.thickness)));
        }
        "outline" => {
            // outline <font> <gids> [ppem]
            use skrifa::MetadataProvider;
            let data = std::fs::read(&args[2]).unwrap();
            let fr = skrifa::FontRef::new(&data).unwrap();
            let size = args.get(4).map_or(skrifa::instance::Size::unscaled(), |s| skrifa::instance::Size::new(s.parse().unwrap()));
            struct P(String);
            impl skrifa::outline::OutlinePen for P {
                fn move_to(&mut self, x: f32, y: f32) { self.0 += &format!("M {x} {y}\n"); }
                fn line_to(&mut self, x: f32, y: f32) { self.0 += &format!("L {x} {y}\n"); }
                fn quad_to(&mut self, a: f32, b: f32, x: f32, y: f32) { self.0 += &format!("Q {a} {b} {x} {y}\n"); }
                fn curve_to(&mut self, a: f32, b: f32, c: f32, d: f32, x: f32, y: f32) { self.0 += &format!("C {a} {b} {c} {d} {x} {y}\n"); }
                fn close(&mut self) { self.0 += "Z\n"; }
            }
            let gm = fr.glyph_metrics(skrifa::instance::Size::unscaled(), skrifa::instance::LocationRef::default());
            for g in args[3].split(',') {
                let gid: u32 = g.parse().unwrap();
                let mut p = P(String::new());
                if let Some(o) = fr.outline_glyphs().get(skrifa::GlyphId::new(gid)) {
                    let _ = o.draw(skrifa::outline::DrawSettings::unhinted(size, skrifa::instance::LocationRef::default()), &mut p);
                }
                println!("G {gid} adv={:?}", gm.advance_width(skrifa::GlyphId::new(gid)));
                print!("{}", p.0);
            }
        }
        "cmap" => {
            use skrifa::MetadataProvider;
            let data = std::fs::read(&args[2]).unwrap();
            let fr = skrifa::FontRef::new(&data).unwrap();
            let n = fr.charmap().mappings().count();
            let sum: u64 = fr.charmap().mappings().map(|(c, g)| c as u64 * 31 + g.to_u32() as u64).sum();
            println!("mappings={n} sum={sum}");
        }
        "shape" => {
            // shape <font> <text> [features "kern=0,liga=0"] [script Latn] [lang en] [rtl]
            let data = std::fs::read(&args[2]).unwrap();
            let font = harfrust::FontRef::from_index(&data, 0).unwrap();
            let sd = harfrust::ShaperData::new(&font);
            let inst = harfrust::ShaperInstance::from_variations(&font, std::iter::empty::<harfrust::Variation>());
            let shaper = sd.shaper(&font).instance(Some(&inst)).build();
            let text = &args[3];
            let feats: Vec<harfrust::Feature> = args.get(4).map(|s| s.split(',').filter(|x| !x.is_empty()).map(|f| {
                let (t, v) = f.split_once('=').unwrap();
                harfrust::Feature::new(harfrust::Tag::new_checked(t.as_bytes()).unwrap(), v.parse().unwrap(), ..)
            }).collect()).unwrap_or_default();
            let script = harfrust::Script::from_iso15924_tag(harfrust::Tag::new_checked(args.get(5).map_or("Latn", |s| s.as_str()).as_bytes()).unwrap()).unwrap();
            let lang: Option<harfrust::Language> = args.get(6).filter(|s| !s.is_empty() && s.as_str() != "-").and_then(|s| s.parse().ok());
            let dir = if args.get(7).map_or(false, |s| s == "rtl") { harfrust::Direction::RightToLeft } else { harfrust::Direction::LeftToRight };
            let plan = harfrust::ShapePlan::new(&shaper, dir, Some(script), lang.as_ref(), &feats);
            let mut buf = harfrust::UnicodeBuffer::new();
            for (i, ch) in text.chars().enumerate() { buf.add(ch, i as u32); }
            buf.set_direction(dir);
            buf.set_script(script);
            if let Some(l) = lang { buf.set_language(l); }
            let out = shaper.shape(buf, harfrust::ShapeOptions::new().plan(Some(&plan)).features(&feats));
            for (i, p) in out.glyph_infos().iter().zip(out.glyph_positions()) {
                println!("{} {} {} {} {} {}", i.glyph_id, i.cluster, p.x_advance, p.y_advance, p.x_offset, p.y_offset);
            }
        }
        "layout" => {
            // layout <layer.json> [dpi]: {text, font_family, size_pt, runs, paragraphs, shape, orientation}
            let v: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();
            let mut layer = photocraft_doc::TextLayer::default();
            layer.text = v["text"].as_str().unwrap_or("").to_string();
            if let Some(f) = v.get("font_family").and_then(|x| x.as_str()) { layer.font_family = f.to_string(); }
            if let Some(s) = v.get("size_pt").and_then(|x| x.as_f64()) { layer.size_pt = s as f32; }
            if let Some(r) = v.get("runs") { layer.runs = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("paragraphs") { layer.paragraphs = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("shape") { layer.shape = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("orientation") { layer.orientation = serde_json::from_value(r.clone()).unwrap(); }
            let dpi: f32 = args.get(3).map_or(72.0, |s| s.parse().unwrap());
            let mut eng = if std::env::var_os("SYSTEM_FONTS").is_some() { photocraft_text::TextEngine::with_system_fonts() } else { photocraft_text::TextEngine::new() };
            let l = eng.layout(&layer, dpi);
            for f in &l.faces { println!("F {} {} {} {}", f.size_px, f.embolden, f.skew_deg, f.coords.len()); }
            for g in &l.glyphs { println!("G {} {} {} {} {} {:?}", g.face, g.id, g.x, g.y, g.style, g.orient); }
            for ln in &l.lines { println!("L {} {} {} {} {} {} {} {}", ln.range.start, ln.range.end, ln.baseline, ln.x0, ln.x1, ln.ascent, ln.descent, ln.paragraph); }
            for c in &l.clusters { println!("C {} {} {} {} {} {}", c.range.start, c.range.end, c.x, c.advance, c.line, c.rtl); }
            for d in &l.decorations { println!("D {} {} {} {} {}", d.x0, d.y0, d.x1, d.y1, d.style); }
        }
        "text-render" => {
            // text-render <layer.json> <out.f32> [dpi]: rasterizes like TextEngine::render (RGBA f32)
            let v: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();
            let mut layer = photocraft_doc::TextLayer::default();
            layer.text = v["text"].as_str().unwrap_or("").to_string();
            if let Some(f) = v.get("font_family").and_then(|x| x.as_str()) { layer.font_family = f.to_string(); }
            if let Some(s) = v.get("size_pt").and_then(|x| x.as_f64()) { layer.size_pt = s as f32; }
            if let Some(r) = v.get("runs") { layer.runs = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("paragraphs") { layer.paragraphs = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("shape") { layer.shape = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("orientation") { layer.orientation = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("antialias") { layer.antialias = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(r) = v.get("warp") { layer.warp = serde_json::from_value(r.clone()).unwrap(); }
            if let Some(m) = v.get("transform").and_then(|x| x.as_array()) {
                let m: Vec<f64> = m.iter().map(|x| x.as_f64().unwrap()).collect();
                layer.transform = photocraft_geom::Affine { m: [m[0], m[1], m[2], m[3], m[4], m[5]] };
            }
            let dpi: f32 = args.get(4).map_or(72.0, |s| s.parse().unwrap());
            let mut eng = if std::env::var_os("SYSTEM_FONTS").is_some() { photocraft_text::TextEngine::with_system_fonts() } else { photocraft_text::TextEngine::new() };
            let fmt = photocraft_color::PixelFormat { mode: photocraft_color::ColorMode::Rgb, sample: photocraft_color::SampleType::F32, alpha: true };
            let (_, r) = eng.render(&layer, dpi, fmt);
            println!("{} {} {} {}", r.rect.x0, r.rect.y0, r.rect.x1, r.rect.y1);
            let data = r.surface.read_region(r.rect);
            let bytes: Vec<u8> = data.iter().flat_map(|f| f.to_le_bytes()).collect();
            std::fs::write(&args[3], bytes).unwrap();
        }
        "tysh" => {
            // TySh/Txt2 ⇄ TextLayer: model JSON per type layer plus rebuilt blocks in `out`.
            use photocraft_text::psd as tp;
            let b = std::fs::read(&args[2]).unwrap();
            let dpi: f32 = args[3].parse().unwrap();
            let out = &args[4];
            let f = photocraft_psd::PsdFile::from_bytes(&b).unwrap();
            let txt2 = f.global_blocks.iter().find(|b| &b.key == b"Txt2").map(|b| b.data.clone());
            let txt2v = txt2.as_deref().and_then(tp::parse_txt2);
            let mut objects: Vec<(i32, TextLayer)> = Vec::new();
            for (i, l) in f.layer_info.iter().flat_map(|li| li.layers.iter()).enumerate() {
                for blk in &l.blocks {
                    if &blk.key != b"TySh" { continue; }
                    let d = &blk.data;
                    println!("LAYER {i} blank={} index={:?}", tp::engine_text_is_blank(d), tp::text_index(d));
                    let Some(mut layer) = tp::text_layer_from_tysh(d, dpi) else { println!("NONE"); continue };
                    if let Some(t) = txt2v.as_ref() { tp::apply_txt2(&mut layer, d, t); }
                    println!("{}", serde_json::json!({"text": layer.text, "font_family": layer.font_family, "size_pt": layer.size_pt,
                        "color": layer.color, "transform": layer.transform.m, "runs": layer.runs, "paragraphs": layer.paragraphs,
                        "shape": layer.shape, "orientation": layer.orientation, "antialias": layer.antialias, "warp": layer.warp}));
                    if let Some(t) = tp::parse_tysh(d) {
                        if let Some(e) = tp::engine_data(&t.text) { std::fs::write(format!("{out}/{i}.ed"), photocraft_text::engine_data::write(&e)).unwrap(); }
                    }
                    std::fs::write(format!("{out}/{i}.fresh"), tp::build_tysh(&layer, dpi, Some([1.5, -2.0, 30.25, 40.0]))).unwrap();
                    layer.psd_raw = Some(std::sync::Arc::new(d.clone()));
                    std::fs::write(format!("{out}/{i}.tysh"), tp::build_tysh(&layer, dpi, None)).unwrap();
                    std::fs::write(format!("{out}/{i}.moved"), tp::translate_tysh(d, 3.5, -7.0).unwrap_or_default()).unwrap();
                    std::fs::write(format!("{out}/{i}.index"), tp::set_text_index(d, 7).unwrap_or_default()).unwrap();
                    if let Some(s) = photocraft_text::psd_styles::read_style_sheets(d, dpi) { println!("SHEETS {}", serde_json::json!({
                        "normal_char": s.normal_char, "normal_para": s.normal_para, "char_sheets": s.char_sheets, "para_sheets": s.para_sheets,
                        "run_char": s.run_char, "run_char_lens": s.run_char_lens, "run_para": s.run_para})); }
                    println!("NAMED {}", photocraft_text::psd_styles::has_named_sheets(d));
                    objects.push((tp::text_index(d).unwrap_or(i as i32), layer));
                }
            }
            let objs: Vec<(i32, &TextLayer)> = objects.iter().map(|(i, l)| (*i, l)).collect();
            println!("NEEDS {}", tp::needs_txt2(&objs));
            std::fs::write(format!("{out}/txt2"), tp::build_txt2(&objs, txt2.as_deref())).unwrap();
            let mut edited: Vec<(i32, TextLayer)> = objects.clone();
            for (_, l) in edited.iter_mut() {
                for r in l.runs.iter_mut() { r.style.kerning = photocraft_doc::text::Kerning::Optical; }
            }
            let objs: Vec<(i32, &TextLayer)> = edited.iter().map(|(i, l)| (*i, l)).collect();
            std::fs::write(format!("{out}/txt2.optical"), tp::build_txt2(&objs, txt2.as_deref())).unwrap();
            std::fs::write(format!("{out}/txt2.fresh"), tp::build_txt2(&objs, None)).unwrap();
        }
        "filter" => {
            let mut d = photocraft_format::load_from_bytes(&std::fs::read(&args[2]).unwrap()).unwrap();
            let p: photocraft_algo::FilterParams = serde_json::from_str(&args[3]).unwrap();
            let idx: usize = args[5].parse().unwrap();
            let bounds = d.bounds();
            let s = d.layers[idx].surface().unwrap().clone();
            let area = photocraft_algo::output_area(&p, s.content_bounds(), bounds, None);
            let out = photocraft_algo::apply(&s, &p, area, bounds, None);
            *d.layers[idx].surface_mut().unwrap() = out;
            std::fs::write(&args[4], photocraft_format::save_to_bytes(&d, &Default::default()).unwrap()).unwrap();
        }
        _ => {}
    }
}
