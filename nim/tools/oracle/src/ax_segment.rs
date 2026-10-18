//! Oracle commands for the Nim port of smart selection (`segment`), Select and Mask (`matting`)
//! and Content-Aware Fill (`content_aware`).

use crate::axio;
use photocraft_algo::content_aware as ca;
use photocraft_algo::matting::{self as mt, EdgeBrush, EdgeStroke, RefineParams};
use photocraft_algo::segment::gmm::{self, Gmm};
use photocraft_algo::segment::maxflow::Graph;
use photocraft_algo::segment::{self as sg, ImageSampler, Region, RgbImage, focus, grabcut, quick, sky, slic, subject};
use photocraft_geom::Rect;

fn rgb(b: &axio::Buf) -> RgbImage {
    RgbImage { w: b.w, h: b.h, px: b.f.chunks_exact(b.ch).map(|p| if b.ch >= 3 { [p[0], p[1], p[2]] } else { [p[0], p[0], p[0]] }).collect() }
}

fn bits(v: &[f32]) -> String {
    v.iter().map(|x| x.to_bits().to_string()).collect::<Vec<_>>().join(" ")
}

fn region(out: &str, r: Option<Region>) {
    match r {
        None => println!("none"),
        Some(r) => {
            println!("region {} {} {} {}", r.bbox.x0, r.bbox.y0, r.bbox.x1, r.bbox.y1);
            axio::write_u8(out, r.bbox.width() as usize, r.bbox.height() as usize, 1, &r.mask);
        }
    }
}

fn gmm_line(g: &Gmm) -> String {
    g.comps.iter().map(|c| format!("{} {} {}", c.weight.to_bits(), bits(&c.mean), bits(&c.cov.concat()))).collect::<Vec<_>>().join(" | ")
}

fn strokes(s: &str) -> Vec<EdgeStroke> {
    // size:add:x,y,x,y|…
    if s == "-" {
        return Vec::new();
    }
    s.split('|')
        .map(|t| {
            let p: Vec<&str> = t.split(':').collect();
            let v = axio::floats(p[2]);
            EdgeStroke { size: p[0].parse().unwrap(), add: p[1] == "1", points: v.chunks_exact(2).map(|c| (c[0], c[1])).collect() }
        })
        .collect()
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "sg.cut" => {
            // img roles gamma
            let im = axio::read(a[2]);
            let img = rgb(&im);
            let roles = axio::read(a[3]);
            let fixed: Vec<u8> = roles.f.iter().map(|v| *v as u8).collect();
            let cf: Vec<f32> = img.px.iter().map(|p| (1.0 - p[0]) * 2.0).collect();
            let cb: Vec<f32> = img.px.iter().map(|p| p[0] * 2.0 + 0.1 * p[2]).collect();
            let beta = sg::contrast_beta(&img);
            println!("beta {}", beta.to_bits());
            let g: f32 = a[4].parse().unwrap();
            let c1 = sg::grid_cut(&img, &cf, &cb, &fixed, g, beta);
            let c2 = sg::grid_cut_with(&img, &cf, &vec![0.0; cf.len()], &fixed, |d2| 0.02 + 1.0 / (1.0 + (d2.sqrt() / 0.05).powf(2.0)));
            let v: Vec<f32> = c1.iter().chain(&c2).map(|b| *b as u8 as f32).collect();
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "sg.graph" => {
            // nodes seed: a random graph, both solvers
            let n: usize = a[2].parse().unwrap();
            let mut rng = sg::Rng::new(a[3].parse().unwrap());
            let mut g = Graph::with_capacity(n, n * 3);
            for i in 0..n {
                let s = rng.f32() * 2.0 - 0.6;
                let t = rng.f32() * 2.0 - 0.6;
                g.add_tweights(i, s.max(0.0), t.max(0.0));
            }
            for i in 0..n {
                for _ in 0..3 {
                    let j = (rng.next_u64() % n as u64) as usize;
                    if j != i {
                        g.add_edge(i, j, rng.f32(), rng.f32());
                    }
                }
            }
            let f = g.maxflow();
            println!("flow {} residual {}", f.to_bits(), g.residual_out_of_source_side().to_bits());
            println!("{}", (0..n).map(|i| if g.in_source(i) { '1' } else { '0' }).collect::<String>());
        }
        "sg.gmm" => {
            // img k reg em
            let im = axio::read(a[2]);
            let img = rgb(&im);
            let k: usize = a[3].parse().unwrap();
            println!("kmeans {}", img.px.is_empty().then(String::new).unwrap_or_else(|| bits(&gmm::kmeans(&img.px, k, 10).concat())));
            match Gmm::fit_em(&img.px, k, a[4].parse().unwrap(), a[5].parse().unwrap()) {
                None => println!("none"),
                Some(g) => {
                    println!("gmm {}", gmm_line(&g));
                    let probe: Vec<f32> = img.px.iter().step_by(37).map(|p| g.neg_log(*p)).collect();
                    println!("neglog {}", bits(&probe));
                    println!("comp {}", img.px.iter().step_by(37).map(|p| g.component_of(*p).to_string()).collect::<Vec<_>>().join(" "));
                    match g.refit(&img.px) {
                        Some(r) => println!("refit {}", gmm_line(&r)),
                        None => println!("refit none"),
                    }
                }
            }
        }
        "sg.misc" => {
            // mask frac holeFrac
            let m = axio::read(a[2]);
            let (w, h) = (m.w, m.h);
            let mask: Vec<bool> = m.f.iter().map(|v| *v > 0.5).collect();
            let (lab, sizes) = sg::components(&mask, w, h);
            println!("sizes {:?}", sizes);
            let seeds: Vec<bool> = (0..w * h).map(|i| i % 97 == 3).collect();
            let kept = sg::keep_seeded(&mask, &seeds, w, h);
            let clean = sg::clean_mask(&mask, w, h, a[3].parse().unwrap(), a[4].parse().unwrap());
            let mut u8m: Vec<u8> = clean.iter().map(|b| if *b { 255 } else { 0 }).collect();
            sg::antialias_u8(&mut u8m, w, h);
            match sg::trim_region(Region { bbox: Rect::new(3, 4, 3 + w as i32, 4 + h as i32), mask: u8m.clone() }) {
                Some(r) => println!("trim {:?} {}", r.bbox, r.mask.len()),
                None => println!("trim none"),
            }
            let v: Vec<f32> = lab.iter().map(|l| *l as f32).chain(kept.iter().map(|b| *b as u8 as f32)).chain(u8m.iter().map(|b| *b as f32)).collect();
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "sg.slic" => {
            // img target m iters
            let img = rgb(&axio::read(a[2]));
            let sp = slic::slic(&img, a[3].parse().unwrap(), a[4].parse().unwrap(), a[5].parse().unwrap());
            println!("count {} step {}", sp.count, sp.step.to_bits());
            let v: Vec<f32> = sp.labels.iter().map(|l| *l as f32).collect();
            axio::write_f32(out, v.len(), 1, 1, &v);
            println!("lab {}", bits(&img.px.iter().step_by(13).flat_map(|p| slic::rgb_to_lab(*p)).collect::<Vec<_>>()));
        }
        "sg.quick" => {
            // img points size
            let img = rgb(&axio::read(a[2]));
            let v = axio::floats(a[3]);
            let pts: Vec<(f32, f32)> = v.chunks_exact(2).map(|c| (c[0], c[1])).collect();
            let s = ImageSampler { img: &img, origin: (0, 0) };
            region(out, quick::quick_select(&s, Rect::new(0, 0, img.w as i32, img.h as i32), &pts, a[4].parse().unwrap()));
        }
        "sg.object" => {
            let img = rgb(&axio::read(a[2]));
            let s = ImageSampler { img: &img, origin: (0, 0) };
            region(out, grabcut::object_select(&s, Rect::new(0, 0, img.w as i32, img.h as i32), axio::rect(a[3]), a[4].parse().unwrap()));
        }
        "sg.sky" => {
            let img = rgb(&axio::read(a[2]));
            let s = ImageSampler { img: &img, origin: (0, 0) };
            let p = sky::SkyParams { threshold: a[3].parse().unwrap(), softness: a[4].parse().unwrap() };
            println!("like {}", bits(&sky::likelihood(&img).iter().step_by(11).copied().collect::<Vec<_>>()));
            region(out, sky::select_sky(&s, Rect::new(0, 0, img.w as i32, img.h as i32), p));
        }
        "sg.subject" => {
            let img = rgb(&axio::read(a[2]));
            let s = ImageSampler { img: &img, origin: (0, 0) };
            println!("sal {}", bits(&subject::saliency(&img).iter().step_by(7).copied().collect::<Vec<_>>()));
            region(out, subject::select_subject(&s, Rect::new(0, 0, img.w as i32, img.h as i32)));
        }
        "sg.focus" => {
            let img = rgb(&axio::read(a[2]));
            let s = ImageSampler { img: &img, origin: (0, 0) };
            println!("sharp {}", bits(&focus::sharpness(&img, a[4].parse().unwrap()).iter().step_by(7).copied().collect::<Vec<_>>()));
            region(out, focus::focus_area(&s, Rect::new(0, 0, img.w as i32, img.h as i32), a[3].parse().unwrap(), a[4].parse().unwrap()));
        }
        "mt.filters" => {
            // img r eps sigma morphR
            let im = axio::read(a[2]);
            let img = rgb(&im);
            let (w, h) = (img.w, img.h);
            let r: usize = a[3].parse().unwrap();
            let eps: f32 = a[4].parse().unwrap();
            let luma: Vec<f32> = img.px.iter().map(|p| 0.3 * p[0] + 0.6 * p[1] + 0.1 * p[2]).collect();
            let p: Vec<f32> = img.px.iter().map(|q| if q[1] > 0.5 { 1.0 } else { 0.0 }).collect();
            let mut v = mt::box_mean(&luma, w, h, r);
            v.extend(mt::guided_filter_gray(&luma, &p, w, h, r, eps));
            v.extend(mt::guided_filter_color(&img, &p, r, eps));
            v.extend(mt::gaussian_blur(&luma, w, h, a[5].parse().unwrap()));
            let mr: usize = a[6].parse().unwrap();
            v.extend(mt::morph(&luma, w, h, mr, true));
            v.extend(mt::morph(&luma, w, h, mr, false));
            v.extend(mt::edge_width(&img, r));
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "mt.refine" => {
            // img mask params-json strokes
            let img = rgb(&axio::read(a[2]));
            let m = axio::read(a[3]);
            let canvas = Rect::new(0, 0, img.w as i32, img.h as i32);
            let reg = Region { bbox: canvas, mask: m.f.iter().map(|v| (v.clamp(0.0, 1.0) * 255.0).round() as u8).collect() };
            let p: RefineParams = serde_json::from_str(a[4]).unwrap();
            println!("halo {} spread {}", p.halo(), p.spread());
            let st = strokes(a[5]);
            let brush = EdgeBrush::new(&st, canvas);
            if let Some(b) = &brush {
                println!("brush {:?} {:?} {} {}", b.bounds(), b.added_bounds(), b.window(), bits(&b.read(canvas).iter().step_by(5).copied().collect::<Vec<_>>()));
            }
            println!("swept {}", mt::swept_area(&st).to_bits());
            let s = ImageSampler { img: &img, origin: (0, 0) };
            let reader = mt::region_reader(&reg);
            region(out, mt::refine_mask_brushed(&s, &reader, Rect::new(5, 5, img.w as i32 - 5, img.h as i32 - 5), canvas, &p, brush.as_ref()));
        }
        "mt.decon" => {
            // img fmt alphaMask radius amount
            let im = axio::read(a[2]);
            let src = crate::ax_retouch::surface(a[3], &im);
            let m = axio::read(a[4]);
            let reg = Region { bbox: Rect::new(0, 0, m.w as i32, m.h as i32), mask: m.f.iter().map(|v| (v.clamp(0.0, 1.0) * 255.0).round() as u8).collect() };
            let d = mt::decontaminate(&src, &reg, a[5].parse().unwrap(), a[6].parse().unwrap());
            let mc = mt::masked_copy(&src, &reg);
            let rs = mt::region_surface(&reg);
            println!("masked {:?} {} region {}", mc.format(), mc.tile_count(), rs.tile_count());
            let full = Rect::new(0, 0, im.w as i32, im.h as i32);
            let mut v = d.read_region(full);
            v.extend(mc.read_region(full));
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "ca.fill" => {
            // img hole source|- color rotations scale mirror seed
            let im = axio::read(a[2]);
            let hole: Vec<bool> = axio::read(a[3]).f.iter().map(|v| *v > 0.5).collect();
            let source: Vec<bool> = if a[4] == "-" { hole.iter().map(|h| !h).collect() } else { axio::read(a[4]).f.iter().map(|v| *v > 0.5).collect() };
            let opts = ca::FillOptions {
                color_adaptation: ca::color_level(a[5]),
                rotations: ca::rotation_level(a[6]),
                scale: axio::bl(a[7]),
                mirror: axio::bl(a[8]),
                seed: a[9].parse().unwrap(),
            };
            let v = ca::fill(im.w, im.h, im.ch, &im.f, &hole, &source, &opts);
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "ca.auto" => {
            let im = axio::read(a[2]);
            let hole: Vec<bool> = axio::read(a[3]).f.iter().map(|v| *v > 0.5).collect();
            let v: Vec<f32> = ca::auto_sampling(im.w, im.h, im.ch, &im.f, &hole).iter().map(|b| *b as u8 as f32).collect();
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "ca.misc" => {
            for (hole, canvas) in [
                (Rect::new(10, 10, 40, 30), Rect::new(0, 0, 1000, 800)),
                (Rect::new(0, 0, 5, 5), Rect::new(0, 0, 1000, 800)),
                (Rect::new(990, 700, 1000, 800), Rect::new(0, 0, 1000, 800)),
                (Rect::new(100, 100, 900, 120), Rect::new(0, 0, 1000, 800)),
                (Rect::new(-50, -20, 30, 41), Rect::new(-100, -100, 100, 100)),
                (Rect::new(0, 0, 0, 0), Rect::new(0, 0, 10, 10)),
                (Rect::new(3, 3, 7, 9), Rect::new(0, 0, 50, 50)),
            ] {
                println!("{:?}", ca::sampling_window(hole, canvas));
            }
            for c in 0..=12u8 {
                let ad = ca::color_amount(c);
                println!("amount {} {} {}", ad.gain.0.to_bits(), ad.gain.1.to_bits(), ad.bias.to_bits());
            }
            let from: Vec<f32> = (0..60).map(|i| (i % 7) as f32 / 9.0).collect();
            let to: Vec<f32> = (0..45).map(|i| 0.3 + (i % 5) as f32 / 11.0).collect();
            for lvl in ["none", "default", "high", "veryHigh"] {
                let t = ca::level_transform(3, &from, &to, ca::color_level(lvl));
                println!("level {}", t.iter().map(|(g, b)| format!("{} {}", g.to_bits(), b.to_bits())).collect::<Vec<_>>().join(" "));
            }
            let t = ca::level_transform(3, &[], &to, ca::color_level("high"));
            println!("empty {:?}", t);
        }
        _ => return false,
    }
    true
}
