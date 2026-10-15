//! Oracle commands for the Nim port of the retouch / paint group (see nim/PORTING.md).

use crate::axio::{self, bl, f, floats, i, rect};
use photocraft_algo::{erase, paint, retouch, selection as sel, stack, trap};
use photocraft_color::{BlendMode, ColorMode, PixelFormat, SampleType};
use photocraft_geom::Rect;
use photocraft_raster::Surface;

fn fmt(s: &str) -> PixelFormat {
    let (mode, rest) = if let Some(r) = s.strip_prefix("cmyk") {
        (ColorMode::Cmyk, r)
    } else if let Some(r) = s.strip_prefix("gray") {
        (ColorMode::Grayscale, r)
    } else if let Some(r) = s.strip_prefix("lab") {
        (ColorMode::Lab, r)
    } else {
        (ColorMode::Rgb, s.strip_prefix("rgb").unwrap_or(s))
    };
    let alpha = !rest.ends_with('x');
    let st = match rest.trim_end_matches('x') {
        "16" => SampleType::U16,
        "32" => SampleType::F32,
        _ => SampleType::U8,
    };
    PixelFormat::new(mode, st, alpha)
}

/// The RGBA float image as a surface in format `f`.
fn surface(f: &str, b: &axio::Buf) -> Surface {
    let mut s = Surface::new(PixelFormat::new(ColorMode::Rgb, SampleType::F32, true));
    s.write_region(Rect::new(0, 0, b.w as i32, b.h as i32), &b.f);
    s.convert(fmt(f))
}

fn mask(path: &str, area: Rect) -> Option<Surface> {
    (path != "-").then(|| sel::mask_to_surface(&axio::read(path).f, area))
}

fn blend(s: &str) -> BlendMode {
    match s {
        "multiply" => BlendMode::Multiply,
        "screen" => BlendMode::Screen,
        "overlay" => BlendMode::Overlay,
        "difference" => BlendMode::Difference,
        "color" => BlendMode::Color,
        "luminosity" => BlendMode::Luminosity,
        "dissolve" => BlendMode::Dissolve,
        "hardmix" => BlendMode::HardMix,
        "lighten" => BlendMode::Lighten,
        _ => BlendMode::Normal,
    }
}

fn shape(s: &str) -> paint::GradientShape {
    match s {
        "radial" => paint::GradientShape::Radial,
        "angle" => paint::GradientShape::Angle,
        "reflected" => paint::GradientShape::Reflected,
        "diamond" => paint::GradientShape::Diamond,
        _ => paint::GradientShape::Linear,
    }
}

fn stat(s: &str) -> stack::Stat {
    use stack::Stat::*;
    match s {
        "entropy" => Entropy,
        "kurtosis" => Kurtosis,
        "maximum" => Maximum,
        "mean" => Mean,
        "median" => Median,
        "minimum" => Minimum,
        "range" => Range,
        "skewness" => Skewness,
        "sd" => StandardDeviation,
        "sum" => Summation,
        _ => Variance,
    }
}

fn tone(s: &str) -> retouch::ToneRange {
    match s {
        "shadows" => retouch::ToneRange::Shadows,
        "highlights" => retouch::ToneRange::Highlights,
        _ => retouch::ToneRange::Midtones,
    }
}

fn opt_alpha(s: &str) -> Option<usize> {
    let v: i64 = s.parse().unwrap();
    (v >= 0).then_some(v as usize)
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "rt.erase" => {
            // fmt img x y tol contiguous aa amount selection lock
            let b = axio::read(a[3]);
            let area = Rect::new(0, 0, b.w as i32, b.h as i32);
            let mut s = surface(a[2], &b);
            let img = sel::rgba8_image(&s, area);
            let Some(region) = sel::wand_region(&img, area, (i(a[4]), i(a[5])), f(a[6]), bl(a[7]), bl(a[8])) else {
                println!("none");
                return true;
            };
            let lock = (a[11] != "-").then(|| floats(a[11]));
            let r = erase::magic_erase(&mut s, &region, f(a[9]), mask(a[10], area).as_ref(), lock.as_deref());
            println!("dmg {} {} {} {}", r.x0, r.y0, r.x1, r.y1);
            axio::dump_surface(out, &s, area);
        }
        "rt.stack" => {
            let layers: Vec<Vec<[f32; 4]>> = a[3..a.len() - 1].iter().map(|p| axio::rgba(&axio::read(p))).collect();
            let r = stack::combine(&layers, stat(a[2]));
            let b = axio::read(a[3]);
            axio::write_f32(out, b.w, b.h, 4, &axio::flat(&r));
        }
        "rt.trap" => {
            let b = axio::read(a[2]);
            let mut px = b.f.clone();
            trap::trap(&mut px, b.w, b.h, b.ch, a[3].parse().unwrap());
            axio::write_f32(out, b.w, b.h, b.ch, &px);
        }
        "rt.gradient" => {
            // fmt img shape fx,fy,tx,ty stops reverse opacity blend dither selection lock area
            let b = axio::read(a[3]);
            let mut s = surface(a[2], &b);
            let p = floats(a[5]);
            let stops: Vec<(f32, [f32; 4])> = a[6].split(';').map(|t| {
                let v = floats(t);
                (v[0], [v[1], v[2], v[3], v[4]])
            })
            .collect();
            let full = Rect::new(0, 0, b.w as i32, b.h as i32);
            paint::paint_gradient(&mut s, rect(a[13]), (p[0], p[1]), (p[2], p[3]), shape(a[4]), &stops, bl(a[7]), f(a[8]), blend(a[9]), bl(a[10]), mask(a[11], full).as_ref(), bl(a[12]));
            axio::dump_surface(out, &s, full);
        }
        "rt.bucket" => {
            // fmt img x y tol contiguous aa rgba opacity selection lock
            let b = axio::read(a[3]);
            let mut s = surface(a[2], &b);
            let full = Rect::new(0, 0, b.w as i32, b.h as i32);
            let c = floats(a[9]);
            let ok = paint::bucket_fill(&mut s, full, (i(a[4]), i(a[5])), f(a[6]), bl(a[7]), bl(a[8]), [c[0], c[1], c[2], c[3]], f(a[10]), mask(a[11], full).as_ref(), bl(a[12]));
            println!("{ok}");
            axio::dump_surface(out, &s, full);
        }
        "rt.kernels" => {
            // Every tone curve, dodge/burn, sponge and the stop sampler over a grid of inputs.
            let mut v = Vec::new();
            let vals = [-0.2f32, 0.0, 0.01, 0.1, 0.25, 0.5, 0.73, 0.9, 1.0, 1.3, f32::NAN];
            for r in ["shadows", "midtones", "highlights"] {
                for burn in [false, true] {
                    for e in [0.0f32, 0.1, 0.5, 1.0, 2.0] {
                        for x in vals {
                            v.push(retouch::tone_curve(x, e, tone(r), burn));
                        }
                        for c in [[0.2f32, 0.5, 0.8], [0.9, 0.95, 1.0], [0.0, 0.0, 0.0], [0.6, 0.1, 0.1], [1.2, -0.1, 0.4]] {
                            for protect in [false, true] {
                                v.extend(retouch::dodge_burn(c, e, tone(r), burn, protect));
                            }
                        }
                    }
                }
            }
            for c in [[0.2f32, 0.5, 0.8], [0.9, 0.95, 1.0], [0.5, 0.5, 0.5], [0.6, 0.1, 0.1]] {
                for amt in [0.0f32, 0.2, 0.7, 1.0] {
                    for (s, vb) in [(false, false), (true, false), (false, true), (true, true)] {
                        v.extend(retouch::sponge(c, amt, s, vb));
                    }
                }
                v.push(retouch::saturation(c));
                v.push(retouch::luma(c));
            }
            let stops = [(0.0f32, [1.0f32, 0.0, 0.0, 1.0]), (0.4, [0.0, 1.0, 0.0, 0.5]), (0.4, [0.0, 0.0, 1.0, 1.0]), (1.0, [1.0, 1.0, 1.0, 0.0])];
            for t in [-1.0f32, 0.0, 0.2, 0.4, 0.41, 0.7, 1.0, 2.0] {
                v.extend(paint::sample_stops(&stops, t));
                for sh in ["linear", "radial", "angle", "reflected", "diamond"] {
                    v.push(paint::tool_gradient_t(shape(sh), (3.0, 4.0), (40.0, -9.0), t * 30.0, 20.0 - t * 7.0));
                }
            }
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "rt.blur" => {
            let b = axio::read(a[2]);
            let w = rect(a[4]);
            let win = (w.x0 as usize, w.y0 as usize, w.x1 as usize, w.y1 as usize);
            let r = retouch::local_blur(&b.f, b.w, b.h, b.ch, opt_alpha(a[5]), win, f(a[3]));
            axio::write_f32(out, (w.x1 - w.x0) as usize, (w.y1 - w.y0) as usize, b.ch, &r);
        }
        "rt.sharpen" => {
            let b = axio::read(a[2]);
            let w = rect(a[4]);
            let win = (w.x0 as usize, w.y0 as usize, w.x1 as usize, w.y1 as usize);
            let r = retouch::local_sharpen(&b.f, b.w, b.h, b.ch, opt_alpha(a[5]), win, f(a[3]), bl(a[6]));
            axio::write_f32(out, (w.x1 - w.x0) as usize, (w.y1 - w.y0) as usize, b.ch, &r);
        }
        _ => return false,
    }
    true
}
