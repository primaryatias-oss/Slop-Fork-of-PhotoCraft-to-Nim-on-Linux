//! Oracle commands for the Nim port of the selection group (see nim/PORTING.md).

use crate::axio::{self, bl, f, floats, i, rect};
use photocraft_algo::selection::{self as sel, LabRange, RangeSample, SelectionMode};
use photocraft_geom::Rect;
use photocraft_raster::Surface;

fn mode(s: &str) -> SelectionMode {
    SelectionMode::parse(s)
}

fn surface_of(path: &str, area: Rect) -> Option<Surface> {
    if path == "-" {
        return None;
    }
    let b = axio::read(path);
    Some(sel::mask_to_surface(&b.f, area))
}

fn combo(op: &str) -> fn(f32, f32) -> f32 {
    match op {
        "add" => |a, b| a.max(b),
        "sub" => |a, b| (a - b).max(0.0),
        "int" => |a, b| a.min(b),
        _ => |a, b| (a - b).abs(),
    }
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "sel.wand" => {
            let b = axio::read(a[2]);
            let area = Rect::new(0, 0, b.w as i32, b.h as i32);
            let m = sel::magic_wand(&axio::rgba(&b), area, (i(a[3]), i(a[4])), f(a[5]), bl(a[6]), bl(a[7]));
            axio::write_f32(out, b.w, b.h, 1, &m);
        }
        "sel.wandregion" => {
            let b = axio::read(a[2]);
            let area = Rect::new(0, 0, b.w as i32, b.h as i32);
            match sel::wand_region(&axio::rgba8(&b), area, (i(a[3]), i(a[4])), f(a[5]), bl(a[6]), bl(a[7])) {
                Some(r) => {
                    println!("bbox {} {} {} {}", r.bbox.x0, r.bbox.y0, r.bbox.x1, r.bbox.y1);
                    axio::write_u8(out, r.bbox.width() as usize, r.bbox.height() as usize, 1, &r.mask);
                }
                None => println!("none"),
            }
        }
        "sel.colorrange" => {
            let b = axio::read(a[2]);
            let c = floats(a[3]);
            axio::write_f32(out, b.w, b.h, 1, &sel::color_range(&axio::rgba(&b), [c[0], c[1], c[2]], f(a[4])));
        }
        "sel.samples" => {
            let b = axio::read(a[2]);
            let samples: Vec<RangeSample> = a[3]
                .split(';')
                .filter(|s| !s.is_empty())
                .map(|s| {
                    let v = floats(s);
                    RangeSample { color: [v[0], v[1], v[2]], at: (v.len() >= 5).then(|| (v[3], v[4])) }
                })
                .collect();
            let loc = if a[5] == "-" { None } else { Some(f(a[5])) };
            axio::write_f32(out, b.w, b.h, 1, &sel::color_range_samples(&axio::rgba(&b), b.w, &samples, f(a[4]), loc));
        }
        "sel.lab" => {
            // lab image, lo, hi, fuzziness, grid8, localized ("-" or "radius;x,y;x,y")
            let b = axio::read(a[2]);
            let (lo, hi) = (floats(a[3]), floats(a[4]));
            let r = LabRange { lo: [lo[0], lo[1], lo[2]], hi: [hi[0], hi[1], hi[2]] };
            let pts: Vec<(f32, f32)>;
            let loc = if a[7] == "-" {
                None
            } else {
                let mut it = a[7].split(';');
                let radius = f(it.next().unwrap());
                pts = it.map(|p| {
                    let v = floats(p);
                    (v[0], v[1])
                })
                .collect();
                Some((radius, pts.as_slice()))
            };
            let m = sel::color_range_lab(&axio::rgba(&b), b.w, &r, f(a[5]), bl(a[6]), loc);
            axio::write_f32(out, b.w, b.h, 1, &m);
        }
        "sel.labpoint" => {
            // lab, lo, hi, fuzziness: prints both coverages and the 8-bit quantized Lab
            let (l, lo, hi) = (floats(a[2]), floats(a[3]), floats(a[4]));
            let r = LabRange { lo: [lo[0], lo[1], lo[2]], hi: [hi[0], hi[1], hi[2]] };
            let lab = [l[0], l[1], l[2]];
            let q = sel::quantize_lab8(lab);
            let g = r.grow(&lab);
            println!(
                "{:?} {:?} {:?} {:?} {:?}",
                sel::lab_range_coverage(lab, &r, f(a[5])).to_bits(),
                sel::lab_range_coverage_8bit(lab, &r, f(a[5])).to_bits(),
                q.map(f32::to_bits),
                g.lo.map(f32::to_bits),
                g.hi.map(f32::to_bits)
            );
            let _ = out;
        }
        "sel.subtract" => {
            let (lo, hi, m) = (floats(a[2]), floats(a[3]), floats(a[4]));
            let r = LabRange { lo: [lo[0], lo[1], lo[2]], hi: [hi[0], hi[1], hi[2]] }.subtract(&[m[0], m[1], m[2]], f(a[5]));
            println!("{:?} {:?}", r.lo.map(f32::to_bits), r.hi.map(f32::to_bits));
        }
        "sel.hue" => {
            let b = axio::read(a[2]);
            axio::write_f32(out, b.w, b.h, 1, &sel::hue_range(&axio::rgba(&b), f(a[3])));
        }
        "sel.tone" => {
            let b = axio::read(a[2]);
            axio::write_f32(out, b.w, b.h, 1, &sel::tone_range(&axio::rgba(&b), f(a[3]), f(a[4]), f(a[5])));
        }
        "sel.modify" => {
            let b = axio::read(a[3]);
            let r = f(a[4]);
            let m = match a[2] {
                "expand" => sel::expand(&b.f, b.w, b.h, r),
                "contract" => sel::contract(&b.f, b.w, b.h, r),
                "border" => sel::border(&b.f, b.w, b.h, r),
                "smooth" => sel::smooth(&b.f, b.w, b.h, r),
                _ => sel::feather(&b.f, b.w, b.h, r),
            };
            axio::write_f32(out, b.w, b.h, 1, &m);
        }
        "sel.polygon" => {
            let p = floats(a[2]);
            let pts: Vec<(f32, f32)> = p.chunks_exact(2).map(|c| (c[0], c[1])).collect();
            let area = rect(a[3]);
            let m = sel::polygon(&pts, area, bl(a[4]));
            axio::write_f32(out, area.width() as usize, area.height() as usize, 1, &m);
        }
        "sel.ellipse" => {
            let s = sel::ellipse_surface(rect(a[2]), rect(a[3]), bl(a[4]));
            axio::dump_surface(out, &s, rect(a[5]));
        }
        "sel.combine" => {
            // mode, old mask (over area), new mask (over area), area, dump area
            let area = rect(a[5]);
            let old = surface_of(a[3], area);
            let new = axio::read(a[4]);
            match sel::combine(old.as_ref(), &new.f, area, mode(a[2])) {
                Some(s) => axio::dump_surface(out, &s, rect(a[6])),
                None => println!("none"),
            }
        }
        "sel.combineregion" => {
            // mode, old mask|- (over the image), rgba8 image, x, y, tol, contiguous, aa
            let b = axio::read(a[4]);
            let area = Rect::new(0, 0, b.w as i32, b.h as i32);
            let old = surface_of(a[3], area);
            let r = sel::wand_region(&axio::rgba8(&b), area, (i(a[5]), i(a[6])), f(a[7]), bl(a[8]), bl(a[9]));
            match sel::combine_region(old.as_ref(), r.as_ref(), mode(a[2])) {
                Some(s) => axio::dump_surface(out, &s, area),
                None => println!("none"),
            }
        }
        "sel.combinesurf" => {
            // op, mask a, area a, mask b, area b, area, dump area
            let sa = surface_of(a[3], rect(a[4])).unwrap();
            let sb = surface_of(a[5], rect(a[6])).unwrap();
            let s = sel::combine_surfaces(&sa, &sb, rect(a[7]), combo(a[2]));
            axio::dump_surface(out, &s, rect(a[8]));
        }
        "sel.edt" => {
            let b = axio::read(a[2]);
            let inside: Vec<bool> = b.f.iter().map(|v| *v >= 0.5).collect();
            axio::write_f32(out, b.w, b.h, 1, &sel::edt(&inside, b.w, b.h));
        }
        _ => return false,
    }
    true
}
