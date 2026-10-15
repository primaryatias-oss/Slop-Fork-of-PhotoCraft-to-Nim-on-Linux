//! Oracle commands for the Nim port of the reshaping tools: Content-Aware Scale (seam carving)
//! and, as they land, Liquify, Puppet Warp, Adaptive Wide Angle and Lens Correction (see
//! nim/PORTING.md).

use crate::axio;
use photocraft_algo::{liquify as lq, magnetic, render2, seam};
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
        "lq.field" | "lq.apply" | "lq.proxy" | "lq.full" => {
            // lq.field bounds cell strokes | lq.apply img fmt bounds cell strokes
            // | lq.proxy img fmt bounds cell strokes max_side | lq.full img fmt bounds cell strokes region
            let k = if a[1] == "lq.field" { 2 } else { 4 };
            let bounds = axio::rect(a[k]);
            let cell: f64 = a[k + 1].parse().unwrap();
            let strokes: Vec<lq::LiquifyStroke> = serde_json::from_str(a[k + 2]).unwrap();
            let mut field = lq::LiquifyField::new(bounds, cell);
            for st in &strokes {
                let r = field.apply_stroke(st);
                println!("dirty {} {} {} {} {:?}", r.x0, r.y0, r.x1, r.y1, st.validate().is_ok());
            }
            let b = |v: f64| v.to_bits().to_string();
            println!("field {} {} {} {} {}", field.w, field.h, b(field.cell), field.max_displacement().to_bits(), field.is_identity());
            if a[1] == "lq.field" {
                let mut line = Vec::new();
                for j in 0..=12 {
                    for i in 0..=12 {
                        let (x, y) = (f64::from(bounds.x0) - 10.0 + f64::from(bounds.width() + 20) * f64::from(i) / 12.0, f64::from(bounds.y0) - 10.0 + f64::from(bounds.height() + 20) * f64::from(j) / 12.0);
                        let d = field.sample(x, y);
                        line.push(format!("{} {} {}", b(d[0]), b(d[1]), field.freeze_at(x, y).to_bits()));
                    }
                }
                println!("{}", line.join(" "));
                let fo: Vec<String> = [-0.5, 0.0, 0.1, 0.5, 0.9, 1.0, 2.0, f64::NAN].iter().flat_map(|t| [0.0, 0.3, 1.0, f64::NAN, 7.0].map(|d| b(lq::falloff(*t, d)))).collect();
                println!("falloff {}", fo.join(" "));
                let mut v = Vec::new();
                for (d, fr) in field.d.iter().zip(&field.freeze) {
                    v.extend_from_slice(&[d[0], d[1], *fr]);
                }
                axio::write_f32(out, field.w, field.h, 3, &v);
            } else {
                let im = axio::read(a[2]);
                let src = crate::ax_retouch::surface(a[3], &im);
                let full = Rect::new(0, 0, im.w as i32, im.h as i32);
                match a[1] {
                    "lq.apply" => axio::dump_surface(out, &lq::apply_liquify(&src, &field), full),
                    "lq.proxy" => {
                        let p = lq::ProxyImage::new(&src, bounds, a[k + 3].parse().unwrap());
                        println!("proxy {} {} {} {}", p.w, p.h, b(p.scale), p.bounds.x0);
                        let mut o = vec![[0u8; 4]; p.w * p.h];
                        p.render(&field, [0, 0, p.w, p.h], &mut o);
                        let mut o2 = vec![[7u8; 4]; p.w * p.h];
                        p.render(&field, p.proxy_rect(Rect::new(bounds.x0 + 5, bounds.y0 + 3, bounds.x0 + 30, bounds.y0 + 20)), &mut o2);
                        println!("rect {:?}", p.proxy_rect(Rect::new(bounds.x0 + 5, bounds.y0 + 3, bounds.x0 + 30, bounds.y0 + 20)));
                        let mut v: Vec<u8> = o.iter().flatten().copied().collect();
                        v.extend(o2.iter().flatten().copied());
                        v.extend(p.px.iter().flatten().flat_map(|x| x.to_le_bytes()));
                        axio::write_u8(out, v.len(), 1, 1, &v);
                    }
                    _ => {
                        let region = axio::rect(a[k + 3]);
                        let ob = bounds.intersect(&field.bounds);
                        let mut o = vec![[3u8; 4]; ob.width() as usize * ob.height() as usize];
                        let mut scratch = Vec::new();
                        println!("{}", lq::ProxyImage::render_full_resolution(&src, &field, bounds, region, &mut o, &mut scratch));
                        let v: Vec<u8> = o.iter().flatten().copied().collect();
                        axio::write_u8(out, v.len(), 1, 1, &v);
                    }
                }
            }
        }
        "r2.run" => {
            // kind spec canvas img fmt mask|- paths
            let canvas = axio::rect(a[4]);
            let prims: Vec<render2::Prim> = match a[2] {
                "flame" => {
                    let spec: render2::FlameSpec = serde_json::from_str(a[3]).unwrap();
                    let paths: Vec<Vec<(f32, f32)>> = serde_json::from_str(a[8]).unwrap();
                    let paths = if paths.is_empty() { vec![render2::default_flame_path(canvas)] } else { paths };
                    render2::flame(&spec, &paths)
                }
                "tree" => render2::tree(&serde_json::from_str(a[3]).unwrap(), canvas),
                _ => render2::picture_frame(&serde_json::from_str(a[3]).unwrap(), canvas),
            };
            let mut v: Vec<f32> = Vec::new();
            for p in &prims {
                match p.shape {
                    render2::Shape::Capsule { a, b, ra, rb } => v.extend_from_slice(&[0.0, a.0, a.1, b.0, b.1, ra, rb]),
                    render2::Shape::Ellipse { c, rx, ry, angle } => v.extend_from_slice(&[1.0, c.0, c.1, rx, ry, angle, 0.0]),
                }
                v.extend_from_slice(&p.c0);
                v.extend_from_slice(&p.c1);
                v.push(p.soft);
                v.push(match p.blend {
                    render2::Blend::Normal => 0.0,
                    render2::Blend::Add => 1.0,
                    render2::Blend::Lighten => 2.0,
                });
                let r = p.bounds();
                v.extend_from_slice(&[r.x0 as f32, r.y0 as f32, r.x1 as f32, r.y1 as f32]);
            }
            let im = axio::read(a[5]);
            let mut surf = crate::ax_retouch::surface(a[6], &im);
            let full = Rect::new(0, 0, im.w as i32, im.h as i32);
            let sel = (a[7] != "-").then(|| photocraft_algo::selection::mask_to_surface(&axio::read(a[7]).f, full));
            let used = render2::composite(&mut surf, &prims, canvas.intersect(&full), sel.as_ref());
            println!("prims {} used {} {} {} {}", prims.len(), used.x0, used.y0, used.x1, used.y1);
            v.extend(surf.read_region(full));
            axio::write_f32(out, v.len(), 1, 1, &v);
            let b = surf.content_bounds();
            println!("tiles {} bounds {} {} {} {}", surf.tile_count(), b.x0, b.y0, b.x1, b.y1);
        }
        _ => return false,
    }
    true
}
