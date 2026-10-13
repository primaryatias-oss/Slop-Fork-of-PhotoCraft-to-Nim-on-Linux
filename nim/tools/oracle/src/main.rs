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
