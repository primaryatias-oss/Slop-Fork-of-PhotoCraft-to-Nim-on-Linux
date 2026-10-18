//! Oracle commands for the Nim port of the brush engine (`crates/paint`, see nim/PORTING.md).

use crate::ax_retouch::{fmt, surface};
use crate::axio::{self, bl, f, i};
use photocraft_color::PixelFormat;
use photocraft_geom::{Point, Rect};
use photocraft_paint::bg_erase::{BgEraseSettings, apply_background_eraser};
use photocraft_paint::mixer::{MixerSettings, MixerState, apply_mixer_stroke};
use photocraft_paint::replace::{ReplaceSettings, apply_color_replacement};
use photocraft_paint::retouch::{self, Region, Smudge, apply_dab_stroke};
use photocraft_paint::symmetry::{PresetSymmetry, SymmetryMode};
use photocraft_paint::tile::{GrayTile, Mips};
use photocraft_paint::*;
use photocraft_raster::Surface;

fn b32(v: f32) -> String {
    v.to_bits().to_string()
}
fn b64(v: f64) -> String {
    v.to_bits().to_string()
}
fn rect_s(r: Rect) -> String {
    format!("{} {} {} {}", r.x0, r.y0, r.x1, r.y1)
}

fn brush(s: &str) -> BrushSettings {
    serde_json::from_str(s).unwrap()
}
fn points(s: &str) -> Vec<StrokePoint> {
    serde_json::from_str(s).unwrap()
}

fn target(f: &str, img: &str) -> Surface {
    if img == "-" { Surface::new(fmt(f)) } else { surface(f, &axio::read(img)) }
}

fn selection(path: &str, area: Rect) -> Option<Surface> {
    (path != "-").then(|| photocraft_algo::selection::mask_to_surface(&axio::read(path).f, area))
}

/// The area every surface result is dumped over (the inputs are at most 97×71).
const AREA: Rect = Rect::new(-12, -12, 112, 84);

fn dab_line(d: &Dab) -> String {
    format!(
        "{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
        b64(d.center.x),
        b64(d.center.y),
        b32(d.radius),
        b32(d.alpha),
        b32(d.angle),
        b32(d.roundness),
        d.flip_x as u8,
        d.flip_y as u8,
        b32(d.opacity),
        b32(d.color[0]),
        b32(d.color[1]),
        b32(d.color[2]),
        b32(d.color[3]),
        b32(d.depth),
        d.index,
        b32(d.proj_angle),
        b32(d.proj_scale),
        b32(d.wet),
        b32(d.mix)
    )
}

fn tile_of(kind: &str, n: u32, seed: u32, extra: u32) -> GrayTile {
    use photocraft_paint::procedural::*;
    match kind {
        "chalk" => chalk_tip(n, seed),
        "spatter" => spatter_tip(n, seed, extra),
        "bristle" => bristle_tip(n, seed),
        "charcoal" => charcoal_tip(n, seed),
        "leaf" => leaf_tip(n),
        "grass" => grass_tip(n, seed, extra),
        "sponge" => sponge_tip(n, seed),
        "star" => star_tip(n),
        _ => rake_tip(n, seed, extra),
    }
}

fn pattern_style(s: &str) -> PatternStyle {
    match s {
        "noise" => PatternStyle::Noise,
        "canvas" => PatternStyle::Canvas,
        "dots" => PatternStyle::Dots,
        _ => PatternStyle::Paper,
    }
}

fn mask_mode(k: usize) -> MaskMode {
    use MaskMode::*;
    [Multiply, Subtract, Darken, Overlay, ColorDodge, ColorBurn, LinearBurn, HardMix, LinearHeight, Height][k % 10]
}

fn sym_mode(s: &str) -> SymmetryMode {
    SymmetryMode::from_id(s).unwrap_or(SymmetryMode::Vertical)
}

pub fn run(a: &[String]) -> bool {
    let out = a.last().map(String::as_str).unwrap_or("");
    match a[1].as_str() {
        "pt.rng" => {
            let (seed, n) = (a[2].parse::<u64>().unwrap(), a[3].parse::<u64>().unwrap());
            for k in 0..n {
                println!(
                    "{} {} {} {}",
                    b32(rng::rand01(seed, k, k % 21)),
                    b32(rng::rand_signed(seed ^ 0x55, k * 3, 7)),
                    b32(rng::hash2(k as i32 * 7 - 50, (k as i32) * -13 + 90, seed)),
                    rng::mix64(seed.wrapping_add(k))
                );
            }
            println!("{}", rng::seed_from_bytes(a[2].as_bytes()));
        }
        "pt.misc" => {
            for k in 0..60 {
                let d = k as f32 * 0.37 - 2.0;
                let (r, h) = ((k % 7) as f32 * 1.3 + 0.2, (k % 5) as f32 * 0.25);
                let (gx, gy) = grid_center(d as f64 * 3.1, -(d as f64) * 1.7, r * 2.0);
                let sq = grid_square(d as f64 * 3.1, d as f64, r * 2.0 - 1.0);
                println!(
                    "{} {} {} {} {} {} {} {} {}",
                    b32(tip_falloff(d.abs(), r, h)),
                    b32(half_coverage_radius(r, h)),
                    b32(dab_coverage(d.abs(), r, h)),
                    b32(gx),
                    b32(gy),
                    b64(sq[0]),
                    b64(sq[1]),
                    b64(sq[2]),
                    b64(sq[3])
                );
                let (v, t, dd) = ((k % 11) as f32 * 0.1, (k % 13) as f32 / 12.0, (k % 4) as f32 / 3.0);
                let line: Vec<String> = (0..10).map(|m| b32(render::mask_combine(mask_mode(m), v, t, dd))).collect();
                println!("{}", line.join(" "));
            }
            println!("{}", b32(half_coverage_radius(10.0, f32::NAN)));
            let g = grid_center(1.2, 3.7, f32::NAN);
            println!("{} {}", b32(g.0), b32(g.1));
            println!("{}", grid_square(1.2, 3.7, f32::INFINITY).iter().map(|v| b64(*v)).collect::<Vec<_>>().join(" "));
            let pts = points(&a[2]);
            for p in smooth(&pts, f(&a[3])) {
                println!("{} {} {}", b64(p.x), b64(p.y), b32(p.pressure));
            }
        }
        "pt.pattern" => {
            let p = procedural::pattern(pattern_style(&a[2]), a[3].parse().unwrap(), a[4].parse().unwrap());
            axio::write_f32(out, p.width, p.height, 1, &p.data);
            println!("{} {}", p.width, p.height);
        }
        "pt.tip" => {
            let t = tile_of(&a[2], a[3].parse().unwrap(), a[4].parse().unwrap(), a[5].parse().unwrap());
            axio::write_f32(out, t.width as usize, t.height as usize, 1, &t.to_f32());
            let p = t.preview(a[6].parse().unwrap());
            let pv: Vec<String> = p.data.iter().take(40).map(|v| v.to_string()).collect();
            println!("{} {} preview {} {} {}", t.width, t.height, p.width, p.height, pv.join(","));
            let m = Mips::new(&t);
            let lv: Vec<String> = m.levels.iter().map(|(w, h, v)| format!("{w}x{h}:{}", b32(v[v.len() / 2]))).collect();
            println!("{}", lv.join(" "));
            for k in 0..20 {
                let px = k as f32 * 3.7 + 0.5;
                let lv = m.level_for(px);
                println!("{} {}", lv, b32(m.sample_clamped(lv, k as f32 * 0.061 - 0.1, 1.05 - k as f32 * 0.057)));
            }
        }
        "pt.brushjson" => match serde_json::from_str::<BrushSettings>(&a[2]) {
            Ok(b) => {
                println!("{}", serde_json::to_string(&b).unwrap());
                println!("{}", serde_json::to_string(&b.bounded_for_render()).unwrap());
            }
            Err(_) => println!("ERR"),
        },
        "pt.presets" => {
            println!("{}", serde_json::to_string(&presets::builtin()).unwrap());
        }
        "pt.dabs" => {
            let st = Stroke { brush: brush(&a[2]), points: points(&a[3]) };
            let chunk: usize = a[4].parse().unwrap();
            // One shot, then the generator fed in chunks (the dabs must agree).
            for d in dabs(&st) {
                println!("{}", dab_line(&d));
            }
            let mut g = dynamics::DabGenerator::new(&st.brush, f(&a[5]));
            let (mut ds, mut dual) = (Vec::new(), Vec::new());
            for c in st.points.chunks(chunk.max(1)) {
                g.push(c, &mut ds, &mut dual);
                println!("chunk {} {} {}", ds.len(), dual.len(), g.wants_time());
                if let Some((p, q)) = g.smoothing_string() {
                    println!("string {} {} {} {}", b64(p[0]), b64(p[1]), b64(q[0]), b64(q[1]));
                }
            }
            g.finish(&mut ds, &mut dual);
            println!("total {} {}", ds.len(), dual.len());
            for d in dual {
                println!("dual {}", dab_line(&d));
            }
        }
        "pt.render" => {
            // fmt img brush points sel lock zoom out
            let mut s = target(&a[2], &a[3]);
            let area = Rect::new(0, 0, 97, 71);
            let sel = selection(&a[6], area);
            let d = render_stroke(&mut s, &brush(&a[4]), &points(&a[5]), sel.as_ref(), bl(&a[7]), f(&a[8]));
            println!("{}", rect_s(d));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.chunked" => {
            // fmt img brush points chunk out: interactive compositing in chunks.
            let mut s = target(&a[2], &a[3]);
            let pre = s.clone();
            let pts = points(&a[5]);
            let mut r = StrokeRenderer::new(&brush(&a[4]), Some(s.format()), 1.0).record_dabs();
            for c in pts.chunks(a[6].parse::<usize>().unwrap().max(1)) {
                r.push(c);
                let d = r.composite(&pre, &mut s, None, false, false);
                println!("chunk {} {} {}", rect_s(d), rect_s(r.bounds()), r.dab_count());
                if let Some(mut t) = r.tail_preview() {
                    let mut s2 = s.clone();
                    let d2 = t.composite(&pre, &mut s2, None, false, false);
                    println!("tail {} {} {}", rect_s(d2), r.tail_dabs().len(), b32(s2.pixel(40, 30)[0]));
                }
            }
            r.mark_dirty(Rect::new(0, 0, 50, 50));
            println!("dirty {}", rect_s(r.take_dirty_rect()));
            r.finish();
            let d = r.composite(&pre, &mut s, None, false, false);
            println!("final {} {}", rect_s(d), r.dabs().len());
            let cov = r.coverage_in(Rect::new(10, 10, 40, 30));
            let cs: Vec<String> = cov.iter().step_by(7).map(|v| b32(*v)).collect();
            println!("{}", cs.join(" "));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.coverage" => {
            let st = Stroke { brush: brush(&a[2]), points: points(&a[3]) };
            let (r, cov) = retouch::stroke_coverage(&st);
            println!("{}", rect_s(r));
            axio::write_f32(out, r.width() as usize, r.height() as usize, 1, &cov);
        }
        "pt.mixer" => {
            // fmt img brush points mixer state sampleAll lock out
            let mut s = target(&a[2], &a[3]);
            let st = Stroke { brush: brush(&a[4]), points: points(&a[5]) };
            let m: MixerSettings = serde_json::from_str(&a[6]).unwrap();
            let mut state: MixerState = serde_json::from_str(&a[7]).unwrap();
            let sample = (a[8] != "-").then(|| surface("rgb32", &axio::read(&a[8])));
            let d = apply_mixer_stroke(&mut s, sample.as_ref(), &st, &m, &mut state, None, bl(&a[9]));
            let opt = |v: Option<[f32; 4]>| v.map_or("none".to_string(), |c| c.iter().map(|x| b32(*x)).collect::<Vec<_>>().join(","));
            println!("{} {} {} {}", rect_s(d), b32(state.amount), opt(state.reservoir), opt(state.pickup));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.replace" => {
            let mut s = target(&a[2], &a[3]);
            let st = Stroke { brush: brush(&a[4]), points: points(&a[5]) };
            let rs: ReplaceSettings = serde_json::from_str(&a[6]).unwrap();
            let d = apply_color_replacement(&mut s, &st, &rs, None, bl(&a[7]));
            println!("{}", rect_s(d));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.bgerase" => {
            let mut s = target(&a[2], &a[3]);
            let st = Stroke { brush: brush(&a[4]), points: points(&a[5]) };
            let bs: BgEraseSettings = serde_json::from_str(&a[6]).unwrap();
            let sel = selection(&a[7], Rect::new(0, 0, 97, 71));
            let d = apply_background_eraser(&mut s, &st, &bs, sel.as_ref());
            println!("{}", rect_s(d));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.smudge" => {
            // fmt img brush points strength finger alpha opacity out
            let mut s = target(&a[2], &a[3]);
            let mut st = Stroke { brush: brush(&a[4]), points: points(&a[5]) };
            st.brush.opacity = f(&a[9]);
            let finger = (a[7] != "-").then(|| axio::floats(&a[7]));
            let mut sm = Smudge::new(f(&a[6]), finger, st.brush.size);
            if bl(&a[8]) {
                sm = sm.with_alpha(retouch::alpha_index(&s.format()));
            }
            let d = apply_dab_stroke(&mut s, &st, None, false, i(&a[10]), |w, fp| sm.dab(w, fp));
            println!("{}", rect_s(d));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.retouch" => {
            // fmt img brush points opacity blend lock out: footprint/stroke coverage composite
            let mut s = target(&a[2], &a[3]);
            let st = Stroke { brush: brush(&a[4]), points: points(&a[5]) };
            let (r, cov) = retouch::stroke_coverage(&st);
            let fm: PixelFormat = s.format();
            let mut paint = Region::new(r, fm.channels());
            let n = fm.channels();
            for (k, v) in paint.data.iter_mut().enumerate() {
                *v = ((k * 37 % 101) as f32 / 100.0).min(1.0);
                if fm.alpha && k % n == n - 1 {
                    *v = 0.75;
                }
            }
            let blend = crate::ax_retouch::blend(&a[7]);
            let d = retouch::apply_coverage(&mut s, r, &cov, f(&a[6]), None, bl(&a[8]), &paint, blend);
            println!("{} {}", rect_s(d), rect_s(retouch::stroke_bounds(&dabs(&st))));
            for (k, dab) in dabs(&st).iter().take(3).enumerate() {
                let fp = retouch::footprint(dab, k, st.brush.hardness);
                let v: Vec<String> = fp.cov.iter().step_by(5).map(|x| b32(*x)).collect();
                println!("{} {}", rect_s(fp.rect), v.join(" "));
            }
            axio::dump_surface(out, &s, AREA);
        }
        "pt.mixpm" => {
            let v = axio::floats(&a[2]);
            let (mut d, src) = (v[..4].to_vec(), v[4..8].to_vec());
            retouch::mix_premultiplied(&mut d, &src, i(&a[3]) as usize, f(&a[4]));
            println!("{}", d.iter().map(|x| b32(*x)).collect::<Vec<_>>().join(" "));
            let mut e = v[..4].to_vec();
            retouch::over_native(&PixelFormat::RGBA32F, &mut e, &src, f(&a[4]), bl(&a[5]));
            println!("{}", e.iter().map(|x| b32(*x)).collect::<Vec<_>>().join(" "));
        }
        "pt.outline" => match outline::tip_outline(&brush(&a[2]), bl(&a[3])) {
            None => println!("none"),
            Some(rings) => {
                for r in rings {
                    println!("{}", r.iter().map(|p| format!("{},{}", b32(p[0]), b32(p[1]))).collect::<Vec<_>>().join(" "));
                }
            }
        },
        "pt.sym" => {
            // mode cx cy rot w h points
            let c = [a[3].parse::<f64>().unwrap(), a[4].parse::<f64>().unwrap()];
            match PresetSymmetry::new(sym_mode(&a[2]), c, a[5].parse().unwrap()) {
                None => println!("none"),
                Some(s) => {
                    println!("{} {}", s.mirror_count(), b64(s.rotation));
                    for d in s.directions() {
                        println!("dir {} {}", b64(d[0]), b64(d[1]));
                    }
                    for seg in s.segments([a[6].parse().unwrap(), a[7].parse().unwrap()]) {
                        println!("seg {} {} {} {}", b64(seg[0][0]), b64(seg[0][1]), b64(seg[1][0]), b64(seg[1][1]));
                    }
                    for pass in s.reflected_passes(&points(&a[8])) {
                        println!("{}", pass.iter().map(|p| format!("{},{}", b64(p.x), b64(p.y))).collect::<Vec<_>>().join(" "));
                    }
                }
            }
        }
        "pt.union" => {
            // fmt img brush points mode cx cy out: a symmetry stroke composited as one union.
            let mut s = target(&a[2], &a[3]);
            let pre = s.clone();
            let b = brush(&a[4]);
            let pts = points(&a[5]);
            let sym = PresetSymmetry::new(sym_mode(&a[6]), [f(&a[7]) as f64, f(&a[8]) as f64], 0.0).unwrap();
            let mut main = StrokeRenderer::new(&b, Some(s.format()), 1.0);
            main.push(&pts);
            main.finish();
            let others: Vec<StrokeRenderer> = sym
                .reflected_passes(&pts)
                .into_iter()
                .map(|p| {
                    let mut r = StrokeRenderer::new(&b, Some(s.format()), 1.0);
                    r.push(&p);
                    r.finish();
                    r
                })
                .collect();
            let d = main.composite_union_many(others.iter(), &pre, &mut s, None, false);
            println!("{}", rect_s(d));
            axio::dump_surface(out, &s, AREA);
        }
        "pt.ctx" => {
            // brush dab-spec: rasterize single dabs through the brush context (dual and primary)
            let b = brush(&a[2]);
            let ctx = BrushContext::new(&b);
            let v = axio::floats(&a[3]);
            let mut d = Dab::round(Point::new(v[0] as f64, v[1] as f64), v[2], v[3]);
            d.angle = v[4];
            d.roundness = v[5];
            d.flip_x = v[6] > 0.5;
            d.proj_angle = v[7];
            d.proj_scale = v[8];
            d.depth = v[9];
            for dual in [false, true] {
                let r = ctx.dab_rect(&d, dual);
                let mut cov = Vec::new();
                ctx.rasterize(&d, dual, r, &mut cov);
                let fp = ctx.footprint(&d, 3);
                println!("{} {} {}", rect_s(r), cov.len(), rect_s(fp.rect));
                println!("{}", cov.iter().map(|x| b32(*x)).collect::<Vec<_>>().join(" "));
            }
            for k in 0..30 {
                let (x, y) = (k * 7 - 40, k * 5 - 31);
                println!("{} {}", b32(ctx.texture_at(x, y)), b32(ctx.stroke_mask(0.6, Some(0.3), x, y)));
            }
        }
        _ => return false,
    }
    true
}
