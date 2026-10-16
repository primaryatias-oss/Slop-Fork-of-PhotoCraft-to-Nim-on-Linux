//! Oracle commands for the Nim port of Photomerge and Auto-Align: Harris/BRIEF features and
//! model fitting (`features`), pair matching, alignment, photometric correction, seams and
//! multi-band blending (`panorama`) and direct registration (`direct`).

use crate::axio;
use photocraft_algo::direct::{self as dr, Plane, Pose};
use photocraft_algo::features::{self as ft, Model};
use photocraft_algo::panorama::{self as pm, AlignOptions, Layout, Motion, Placement, Projection, RoiImage};
use photocraft_algo::transform::Homography;

fn luma(b: &axio::Buf) -> Vec<f32> {
    b.f.chunks_exact(b.ch).map(|p| if b.ch >= 3 { 0.2126 * p[0] + 0.7152 * p[1] + 0.0722 * p[2] } else { p[0] }).collect()
}

fn mask(a: &str) -> Option<Vec<bool>> {
    (a != "-").then(|| axio::read(a).f.iter().map(|v| *v > 0.5).collect())
}

fn b(v: f64) -> String {
    v.to_bits().to_string()
}

fn hs(h: &Homography) -> String {
    h.0.iter().map(|v| b(*v)).collect::<Vec<_>>().join(" ")
}

fn nums(a: &str) -> Vec<f64> {
    if a.is_empty() { Vec::new() } else { a.split(',').map(|x| x.parse().unwrap()).collect() }
}

fn pts(a: &str) -> (Vec<[f64; 2]>, Vec<[f64; 2]>) {
    let v = nums(a);
    let p: Vec<[f64; 2]> = v.chunks_exact(2).map(|c| [c[0], c[1]]).collect();
    let n = p.len() / 2;
    (p[..n].to_vec(), p[n..2 * n].to_vec())
}

fn model(a: &str) -> Model {
    match a {
        "translation" => Model::Translation,
        "similarity" => Model::Similarity,
        _ => Model::Homography,
    }
}

fn motion(a: &str) -> Motion {
    match a {
        "translation" => Motion::Translation,
        "euclidean" => Motion::Euclidean,
        "similarity" => Motion::Similarity,
        _ => Motion::Homography,
    }
}

fn opt_h(r: Option<(Homography, Vec<usize>)>) -> String {
    match r {
        Some((h, inl)) => format!("{} | {}", hs(&h), inl.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(" ")),
        None => "None".into(),
    }
}

fn placement(p: &Placement) -> String {
    let pr = match p.projection {
        Projection::Plane => "plane",
        Projection::Cylinder => "cylinder",
        Projection::Sphere => "sphere",
    };
    format!("{} {} {} {} {} {}", pr, b(p.focal), b(p.center[0]), b(p.center[1]), b(p.k1), hs(&p.h))
}

/// A placement's maps sampled on a grid over a `w × h` image.
fn probe(p: &Placement, w: f64, h: f64) {
    let mut line = Vec::new();
    for j in 0..=4 {
        for i in 0..=4 {
            let (x, y) = (-10.0 + (w + 20.0) * i as f64 / 4.0, -10.0 + (h + 20.0) * j as f64 / 4.0);
            let q = p.project(x, y);
            let (u, v) = p.forward(x, y);
            line.push(format!("{} {} {} {}", b(q[0]), b(q[1]), b(u), b(v)));
            match p.unproject(q[0], q[1]) {
                Some((a, c)) => line.push(format!("{} {}", b(a), b(c))),
                None => line.push("none".into()),
            }
            match p.inverse(u, v) {
                Some((a, c)) => line.push(format!("{} {}", b(a), b(c))),
                None => line.push("none".into()),
            }
        }
    }
    println!("probe {}", line.join(" "));
    println!("bounds {}", p.bounds(w, h).iter().map(|v| b(*v)).collect::<Vec<_>>().join(" "));
    println!("scaled {}", placement(&p.scaled(2.5)));
    println!("translated {}", placement(&p.translated(-7.25, 3.5)));
}

/// `img x0 y0` triples from `a[k..]` (all but the output path) as panorama ROIs (alpha = coverage).
fn rois(a: &[&str], k: usize) -> Vec<RoiImage> {
    a[k..a.len() - 1]
        .chunks_exact(3)
        .map(|t| {
            let im = axio::read(t[0]);
            let ch = if im.ch == 4 || im.ch == 2 { im.ch - 1 } else { im.ch };
            let alpha_ch = im.ch != ch;
            let mut px = Vec::new();
            let mut alpha = Vec::new();
            for p in im.f.chunks_exact(im.ch) {
                px.extend_from_slice(&p[..ch]);
                alpha.push(if alpha_ch { p[ch] } else { 1.0 });
            }
            RoiImage { x0: t[1].parse().unwrap(), y0: t[2].parse().unwrap(), w: im.w, h: im.h, ch, px, alpha }
        })
        .collect()
}

/// Canvas-sized luminance and coverage of each ROI.
fn canvas(w: usize, h: usize, rs: &[RoiImage]) -> (Vec<Vec<f32>>, Vec<Vec<bool>>) {
    let mut ls = Vec::new();
    let mut cs = Vec::new();
    for r in rs {
        let mut l = vec![0.0f32; w * h];
        let mut c = vec![false; w * h];
        for y in 0..r.h {
            for x in 0..r.w {
                let (cx, cy) = (x as i64 + r.x0 as i64, y as i64 + r.y0 as i64);
                if cx < 0 || cy < 0 || cx >= w as i64 || cy >= h as i64 {
                    continue;
                }
                let k = cy as usize * w + cx as usize;
                let p = &r.px[(y * r.w + x) * r.ch..(y * r.w + x + 1) * r.ch];
                l[k] = if r.ch >= 3 { 0.2126 * p[0] + 0.7152 * p[1] + 0.0722 * p[2] } else { p[0] };
                c[k] = r.alpha[y * r.w + x] > 0.5;
            }
        }
        ls.push(l);
        cs.push(c);
    }
    (ls, cs)
}

fn pose_s(p: &Pose) -> String {
    p.to_vec().iter().map(|v| b(*v)).collect::<Vec<_>>().join(" ")
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "ft.harris" => {
            // img max min_dist mask|-
            let im = axio::read(a[2]);
            let l = luma(&im);
            let valid = mask(a[5]);
            let c = ft::harris(im.w, im.h, &l, a[3].parse().unwrap(), a[4].parse().unwrap(), valid.as_deref());
            println!("n {}", c.len());
            println!("{}", c.iter().map(|k| format!("{} {} {}", k.x.to_bits(), k.y.to_bits(), k.score.to_bits())).collect::<Vec<_>>().join(" "));
            for upright in [true, false] {
                let d = ft::describe(im.w, im.h, &l, &c, upright);
                println!("{}", d.iter().map(|w| format!("{} {} {} {}", w[0], w[1], w[2], w[3])).collect::<Vec<_>>().join(" "));
            }
        }
        "ft.match" => {
            // imgA imgB max ratio upright
            let (ia, ib) = (axio::read(a[2]), axio::read(a[3]));
            let (la, lb) = (luma(&ia), luma(&ib));
            let max: usize = a[4].parse().unwrap();
            let up = axio::bl(a[6]);
            let ca = ft::harris(ia.w, ia.h, &la, max, 5.0, None);
            let cb = ft::harris(ib.w, ib.h, &lb, max, 5.0, None);
            let (da, db) = (ft::describe(ia.w, ia.h, &la, &ca, up), ft::describe(ib.w, ib.h, &lb, &cb, up));
            let m = ft::match_descriptors(&da, &db, a[5].parse().unwrap());
            println!("{} {} {}", ca.len(), cb.len(), m.iter().map(|(i, j)| format!("{i}:{j}")).collect::<Vec<_>>().join(" "));
            if let (Some(x), Some(y)) = (da.first(), db.last()) {
                println!("hamming {}", ft::hamming(x, y));
            }
        }
        "ft.fit" | "pm.fit" => {
            // model pts iters threshold seed
            let (src, dst) = pts(a[3]);
            let (iters, thr, seed): (usize, f64, u64) = (a[4].parse().unwrap(), a[5].parse().unwrap(), a[6].parse().unwrap());
            if a[1] == "ft.fit" {
                let m = model(a[2]);
                println!("fit {}", ft::fit(m, &src, &dst).map(|h| hs(&h)).unwrap_or("None".into()));
                println!("ransac {}", opt_h(ft::ransac(m, &src, &dst, iters, thr, seed)));
            } else {
                let m = motion(a[2]);
                println!("fit {}", pm::fit_motion(m, &src, &dst).map(|h| hs(&h)).unwrap_or("None".into()));
                println!("ransac {}", opt_h(pm::ransac_motion(m, &src, &dst, iters, thr, seed)));
            }
        }
        "ft.reg" => {
            // ref mov model maskRef|- maskMov|-
            let (ir, im) = (axio::read(a[2]), axio::read(a[3]));
            let (vr, vm) = (mask(a[5]), mask(a[6]));
            let r = ft::register(ir.w, ir.h, &luma(&ir), &luma(&im), vr.as_deref(), vm.as_deref(), model(a[4]));
            match r {
                Some((h, n)) => println!("{} | {}", hs(&h), n),
                None => println!("None"),
            }
        }
        "pm.focal" => {
            let v = nums(a[2]);
            let h = Homography([v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]]);
            println!("{:?}", pm::focal_from_homography(&h, [v[9], v[10]], [v[11], v[12]]).map(b));
        }
        "pm.place" => {
            // projection focal cx cy k1 h(9) w h
            let v = nums(a[3]);
            let projection = match a[2] {
                "cylinder" => Projection::Cylinder,
                "sphere" => Projection::Sphere,
                _ => Projection::Plane,
            };
            let p = Placement {
                projection,
                focal: v[0],
                center: [v[1], v[2]],
                k1: v[3],
                h: Homography([v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12]]),
            };
            probe(&p, v[13], v[14]);
        }
        "pm.align" => {
            // layout focal|- reference|- geometric max img|img:mask …
            let imgs: Vec<pm::PreparedImage> = a[7..a.len() - 1]
                .iter()
                .map(|s| {
                    let (p, m) = s.split_once(':').unwrap_or((s, "-"));
                    let im = axio::read(p);
                    (im.w, im.h, luma(&im), mask(m))
                })
                .collect();
            let feats = pm::detect_many(&imgs, a[6].parse().unwrap());
            for f in &feats {
                println!(
                    "feat {} {} {} {}",
                    f.w,
                    f.h,
                    f.points.iter().map(|p| format!("{} {}", b(p[0]), b(p[1]))).collect::<Vec<_>>().join(" "),
                    f.desc.iter().map(|w| format!("{} {} {} {}", w[0], w[1], w[2], w[3])).collect::<Vec<_>>().join(" ")
                );
            }
            let matches = pm::match_pairs(&feats);
            for m in &matches {
                let (fa, fb) = (&feats[m.a], &feats[m.b]);
                let ca = [fa.w as f64 / 2.0, fa.h as f64 / 2.0];
                let cb = [fb.w as f64 / 2.0, fb.h as f64 / 2.0];
                println!(
                    "pair {} {} {} {} | {} | {:?}",
                    m.a,
                    m.b,
                    m.pts_a.iter().zip(&m.pts_b).map(|(p, q)| format!("{} {} {} {}", b(p[0]), b(p[1]), b(q[0]), b(q[1]))).collect::<Vec<_>>().join(" "),
                    m.pts_a.len(),
                    hs(&m.homography),
                    pm::focal_from_homography(&m.homography, ca, cb).map(b)
                );
            }
            let opts = AlignOptions {
                layout: Layout::parse(a[2]).unwrap_or(Layout::Auto),
                focal: (a[3] != "-").then(|| a[3].parse().unwrap()),
                reference: (a[4] != "-").then(|| a[4].parse().unwrap()),
                geometric: axio::bl(a[5]),
            };
            match pm::align(&feats, &matches, &opts) {
                None => println!("align None"),
                Some(al) => {
                    println!(
                        "align {} {} {} {} {} {}",
                        al.layout.name(),
                        al.reference,
                        b(al.focal),
                        b(al.k1),
                        b(al.rms),
                        al.pairs.iter().map(|p| format!("{}-{}-{}", p.0, p.1, p.2)).collect::<Vec<_>>().join(" ")
                    );
                    for (i, p) in al.placements.iter().enumerate() {
                        match p {
                            None => println!("placement {i} None"),
                            Some(p) => {
                                println!("placement {i} {}", placement(p));
                                probe(p, feats[i].w as f64, feats[i].h as f64);
                            }
                        }
                    }
                }
            }
        }
        "pm.photo" => {
            // gain vignette geometry(0/1) focal img x0 y0 …
            let rs = rois(&a, 6);
            let focal: f64 = a[5].parse().unwrap();
            let pls: Vec<Placement> = rs
                .iter()
                .map(|r| Placement {
                    projection: Projection::Plane,
                    focal,
                    center: [r.w as f64 / 2.0, r.h as f64 / 2.0],
                    k1: 0.0,
                    h: Homography([1.0, 0.0, r.x0 as f64 + r.w as f64 / 2.0, 0.0, 1.0, r.y0 as f64 + r.h as f64 / 2.0, 0.0, 0.0, 1.0]),
                })
                .collect();
            let sizes: Vec<(usize, usize)> = rs.iter().map(|r| (r.w, r.h)).collect();
            let geom = axio::bl(a[4]).then_some((pls.as_slice(), sizes.as_slice()));
            let ph = pm::photometric(&rs, geom, axio::bl(a[2]), axio::bl(a[3]));
            println!(
                "gains {} vignette {} {}",
                ph.gains.iter().map(|g| g.iter().map(|v| v.to_bits().to_string()).collect::<Vec<_>>().join(",")).collect::<Vec<_>>().join(" "),
                b(ph.vignette[0]),
                b(ph.vignette[1])
            );
            let mut f = Vec::new();
            for i in 0..rs.len() {
                for c in 0..rs[i].ch {
                    for r2 in [0.0, 0.25, 0.8, 1.0] {
                        f.push(ph.factor(i, c, r2).to_bits().to_string());
                    }
                }
            }
            println!("factor {}", f.join(" "));
        }
        "pm.seam" | "pm.blend" => {
            // W H order|levels img x0 y0 …
            let (w, h): (usize, usize) = (a[2].parse().unwrap(), a[3].parse().unwrap());
            let rs = rois(&a, 5);
            let (ls, cs) = canvas(w, h, &rs);
            let order: Vec<usize> = if a[1] == "pm.seam" { a[4].split(',').map(|x| x.parse().unwrap()).collect() } else { (0..rs.len()).collect() };
            let labels = pm::seam_labels(w, h, &ls, &cs, &order);
            if a[1] == "pm.seam" {
                let v: Vec<f32> = labels.iter().map(|l| *l as f32).collect();
                axio::write_f32(out, w, h, 1, &v);
            } else {
                let lv: usize = if a[4] == "auto" { pm::blend_levels(w, h) } else { a[4].parse().unwrap() };
                println!("levels {} {}", pm::blend_levels(w, h), lv);
                let wts: Vec<Vec<f32>> = rs
                    .iter()
                    .enumerate()
                    .map(|(k, r)| {
                        (0..r.w * r.h)
                            .map(|i| {
                                let (cx, cy) = ((i % r.w) as i64 + r.x0 as i64, (i / r.w) as i64 + r.y0 as i64);
                                let inside = cx >= 0 && cy >= 0 && cx < w as i64 && cy < h as i64;
                                if inside && labels[cy as usize * w + cx as usize] == k as i32 { 1.0 } else { 0.0 }
                            })
                            .collect()
                    })
                    .collect();
                let (px, cov) = pm::multiband(w, h, &rs, &wts, lv);
                let mut v = px;
                v.extend(cov);
                axio::write_f32(out, v.len(), 1, 1, &v);
            }
        }
        "dr.reg" => {
            // motion coarsen init(8 csv)|- ref tgt maskRef|- maskTgt|-
            let (ir, it) = (axio::read(a[5]), axio::read(a[6]));
            let (lr, lt) = (luma(&ir), luma(&it));
            let (vr, vt) = (mask(a[7]), mask(a[8]));
            let init = if a[4] == "-" {
                Pose::IDENTITY
            } else {
                let v = nums(a[4]);
                Pose::from_vec(&[v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]])
            };
            let pr = Plane { w: ir.w, h: ir.h, luma: &lr, valid: vr.as_deref() };
            let pt = Plane { w: it.w, h: it.h, luma: &lt, valid: vt.as_deref() };
            let r = dr::register(&pr, &pt, motion(a[2]), init, a[3].parse().unwrap());
            println!("pose {} h {} rms {} overlap {} corr {}", pose_s(&r.pose), hs(&r.h), b(r.rms), r.overlap, b(r.correlation));
            let (o, v) = dr::warp(&pt, &r.pose);
            let mut f = o;
            f.extend(v.iter().map(|x| *x as u8 as f32));
            axio::write_f32(out, f.len(), 1, 1, &f);
        }
        "dr.warp" => {
            // pose(8 csv) img mask|-
            let im = axio::read(a[3]);
            let l = luma(&im);
            let vm = mask(a[4]);
            let v = nums(a[2]);
            let pose = Pose::from_vec(&[v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]]);
            println!("matrix {}", hs(&pose.matrix(im.w, im.h)));
            let (o, c) = dr::warp(&Plane { w: im.w, h: im.h, luma: &l, valid: vm.as_deref() }, &pose);
            let mut f = o;
            f.extend(c.iter().map(|x| *x as u8 as f32));
            axio::write_f32(out, f.len(), 1, 1, &f);
        }
        _ => return false,
    }
    true
}
