//! Oracle commands for the Nim port of Perspective Warp, Crop and Straighten Photos and Red Eye
//! (see nim/PORTING.md). Floats are printed as their bit patterns.

use crate::axio::{self, f};
use photocraft_algo::{perspective as pw, redeye, scancrop};

fn nums(s: &str) -> Vec<f64> {
    s.split(',').map(|t| t.trim().parse().unwrap()).collect()
}

fn planes(s: &str) -> Vec<pw::Plane> {
    let q = |c: &[f64]| [[c[0], c[1]], [c[2], c[3]], [c[4], c[5]], [c[6], c[7]]];
    nums(s).chunks_exact(16).map(|c| pw::Plane { src: q(&c[..8]), dst: q(&c[8..]) }).collect()
}

fn b(v: f64) -> String {
    v.to_bits().to_string()
}

fn quad_line(q: &[[f64; 2]; 4]) -> String {
    q.iter().map(|p| format!("{} {}", b(p[0]), b(p[1]))).collect::<Vec<_>>().join(" ")
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "pw.map" => {
            // planes x0 y0 x1 y1 n
            let ps = planes(a[2]);
            let (x0, y0, x1, y1): (f64, f64, f64, f64) = (a[3].parse().unwrap(), a[4].parse().unwrap(), a[5].parse().unwrap(), a[6].parse().unwrap());
            let n: usize = a[7].parse().unwrap();
            match pw::PerspectiveMap::new(&ps) {
                None => println!("none"),
                Some(m) => {
                    let mut line = Vec::new();
                    for j in 0..=n {
                        for i in 0..=n {
                            let (x, y) = (x0 + (x1 - x0) * i as f64 / n as f64, y0 + (y1 - y0) * j as f64 / n as f64);
                            let (u, v) = m.map(x, y);
                            line.push(b(u));
                            line.push(b(v));
                        }
                    }
                    println!("{}", line.join(" "));
                }
            }
            println!("identity {}", pw::PerspectiveMap::is_identity(&ps));
            for p in &ps {
                for (u, v) in [(0.0, 0.0), (0.3, 0.7), (1.0, 0.5), (1.5, -0.2)] {
                    for d in [false, true] {
                        let q = pw::plane_point(p, u, v, d);
                        println!("pp {} {}", b(q[0]), b(q[1]));
                    }
                }
            }
            println!("unit {}", quad_line(&pw::unit_corners()));
            std::fs::write(out, b"").unwrap();
        }
        "pw.straighten" => {
            let mut ps = planes(a[2]);
            for g in pw::linked_corners(&ps) {
                println!("group {:?}", g);
            }
            match pw::Straighten::parse(a[3]) {
                None => println!("bad mode"),
                Some(m) => pw::straighten(&mut ps, m),
            }
            for p in &ps {
                println!("src {}", quad_line(&p.src));
                println!("dst {}", quad_line(&p.dst));
            }
            std::fs::write(out, b"").unwrap();
        }
        "sc.find" => {
            let im = axio::read(a[2]);
            let found = scancrop::find_photos(im.w, im.h, &axio::rgba(&im));
            println!("{}", found.len());
            for p in &found {
                println!("{} {} {} {} {} {}", b(p.center[0]), b(p.center[1]), b(p.width), b(p.height), b(p.angle), b(p.area));
                println!("corners {}", quad_line(&p.corners()));
            }
            std::fs::write(out, b"").unwrap();
        }
        "re.apply" => {
            // img click_x click_y origin_x origin_y pupil darken
            let im = axio::read(a[2]);
            let mut px = axio::rgba(&im);
            let p = |k: usize| -> i32 { a[k].parse().unwrap() };
            let n = redeye::apply(&mut px, im.w, im.h, (p(3), p(4)), (p(5), p(6)), f(a[7]), f(a[8]));
            println!("{n}");
            axio::write_f32(out, im.w, im.h, 4, &axio::flat(&px));
        }
        "re.consts" => {
            println!("halo {}", redeye::halo_radius());
            for v in [f32::NAN, f32::INFINITY, -5.0, 0.0, 1.0, 37.5, 50.0, 100.0, 101.0, 400.0, 401.0, 1e30] {
                println!("{} {:?}", redeye::search_radius(v).to_bits(), redeye::unclamped_window_side(v));
            }
            std::fs::write(out, b"").unwrap();
        }
        _ => return false,
    }
    true
}
