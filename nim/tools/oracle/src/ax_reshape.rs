//! Oracle commands for the Nim port of the reshaping tools: Content-Aware Scale (seam carving)
//! and, as they land, Liquify, Puppet Warp, Adaptive Wide Angle and Lens Correction (see
//! nim/PORTING.md).

use crate::axio;
use photocraft_algo::{magnetic, seam};
use photocraft_geom::Rect;

fn opt_mask(path: &str) -> Option<Vec<f32>> {
    if path == "-" { None } else { Some(axio::read(path).f) }
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    let u = |k: usize| -> usize { a[k].parse().unwrap() };
    match a[1] {
        "cs.energy" => {
            let b = axio::read(a[2]);
            let e = seam::energy(b.w, b.h, b.ch, &b.f);
            println!("{:?}", seam::find_seam(b.w, b.h, &e));
            axio::write_f32(out, b.w, b.h, 1, &e);
        }
        "cs.width" => {
            let b = axio::read(a[2]);
            let p = opt_mask(a[3]);
            let nw = u(4).max(1);
            let r = seam::carve_width(b.w, b.h, b.ch, &b.f, p.as_deref(), nw);
            println!("{}", r.len());
            axio::write_f32(out, nw, b.h, b.ch, &r);
        }
        "cs.carve" => {
            let b = axio::read(a[2]);
            let p = opt_mask(a[3]);
            let (nw, nh) = (u(4).max(1), u(5).max(1));
            let r = seam::carve(b.w, b.h, b.ch, &b.f, p.as_deref(), nw, nh);
            println!("{}", r.len());
            axio::write_f32(out, nw, nh, b.ch, &r);
        }
        "cs.resize" => {
            let b = axio::read(a[2]);
            let (nw, nh) = (u(3), u(4));
            axio::write_f32(out, nw, nh, b.ch, &seam::resize_bilinear(b.w, b.h, b.ch, &b.f, nw, nh));
        }
        "cs.skin" => {
            let b = axio::read(a[2]);
            axio::write_f32(out, b.w, b.h, 1, &seam::skin_mask(b.w, b.h, b.ch, &b.f));
        }
        "ml.run" => {
            // img8 ox oy bad ops…  (ops: "s x y width contrast" | "t fx fy tx ty width contrast gx gy …"
            // | "simplify tol x y …" | "len x y …")
            let im = axio::read(a[2]);
            let px = axio::rgba8(&im);
            let (ox, oy): (i32, i32) = (a[3].parse().unwrap(), a[4].parse().unwrap());
            let bad = a[5] == "1";
            let (w, h) = (im.w as i32, im.h as i32);
            let mut fetch = |r: Rect| -> Vec<[u8; 4]> {
                if bad {
                    return vec![[255u8; 4]; 3];
                }
                (r.y0..r.y1).flat_map(|y| (r.x0..r.x1).map(move |x| (x, y))).map(|(x, y)| px[((y - oy) * w + (x - ox)) as usize]).collect()
            };
            let mut t = magnetic::Tracer::new(Rect::new(ox, oy, ox + w, oy + h));
            let b = |v: f64| v.to_bits().to_string();
            let line = |v: &[[f64; 2]]| v.iter().map(|p| format!("{} {}", b(p[0]), b(p[1]))).collect::<Vec<_>>().join(" ");
            for op in &a[6..a.len() - 1] {
                let t_: Vec<&str> = op.split_whitespace().collect();
                let n = |k: usize| -> f64 { t_[k].parse().unwrap() };
                let pts = |from: usize| -> Vec<[f64; 2]> { t_[from..].chunks_exact(2).map(|c| [c[0].parse().unwrap(), c[1].parse().unwrap()]).collect() };
                match t_[0] {
                    "s" => {
                        let s = magnetic::Settings { width: n(3), contrast: n(4) as f32 };
                        match t.snap(&mut fetch, [n(1), n(2)], s) {
                            Some(p) => println!("snap {} {}", b(p[0]), b(p[1])),
                            None => println!("snap none"),
                        }
                    }
                    "t" => {
                        let s = magnetic::Settings { width: n(5), contrast: n(6) as f32 };
                        let r = t.trace(&mut fetch, [n(1), n(2)], [n(3), n(4)], &pts(7), s);
                        println!("trace {} {}", r.len(), line(&r));
                    }
                    "simplify" => println!("simplify {}", line(&magnetic::simplify(&pts(2), n(1)))),
                    "len" => println!("len {}", b(magnetic::length(&pts(1)))),
                    _ => panic!("bad op"),
                }
            }
            println!("cached {}", t.cached_pixels());
            std::fs::write(out, b"").unwrap();
        }
        _ => return false,
    }
    true
}
