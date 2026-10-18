//! Oracle commands for the Nim port of hole filling (`inpaint`): content-aware completion,
//! proximity match and texture synthesis.

use crate::axio;
use photocraft_algo::inpaint::{self as ip, Adapt, CompleteParams, Px};

fn params(a: &[&str]) -> CompleteParams {
    // radius em minEm pm refineExtent gainLo gainHi bias seed
    CompleteParams {
        patch_radius: a[0].parse().unwrap(),
        em_iters: a[1].parse().unwrap(),
        min_em_iters: a[2].parse().unwrap(),
        pm_iters: a[3].parse().unwrap(),
        refine_extent: a[4].parse().unwrap(),
        adapt: Adapt { gain: (a[5].parse().unwrap(), a[6].parse().unwrap()), bias: a[7].parse().unwrap() },
        seed: a[8].parse().unwrap(),
    }
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "ip.complete" | "ip.masked" => {
            // img mask|roles params(9)
            let im = axio::read(a[2]);
            let m = axio::read(a[3]);
            let p = params(&a[4..13]);
            let r = if a[1] == "ip.complete" {
                let hole: Vec<bool> = m.f.iter().map(|v| *v > 0.5).collect();
                ip::complete(im.w, im.h, im.ch, &im.f, &hole, &p)
            } else {
                let px: Vec<Px> = m
                    .f
                    .iter()
                    .map(|v| match *v as i32 {
                        0 => Px::Void,
                        1 => Px::Known,
                        2 => Px::Source,
                        _ => Px::Hole,
                    })
                    .collect();
                ip::complete_masked(im.w, im.h, im.ch, &im.f, &px, &p, &photocraft_raster::Interrupt::NONE).unwrap()
            };
            match r {
                Some(v) => {
                    println!("some {}", v.len());
                    axio::write_f32(out, v.len(), 1, 1, &v);
                }
                None => println!("none"),
            }
        }
        "ip.offset" => {
            // img mask ring maxRadius
            let im = axio::read(a[2]);
            let hole: Vec<bool> = axio::read(a[3]).f.iter().map(|v| *v > 0.5).collect();
            println!("{:?}", ip::best_offset(im.w, im.h, im.ch, &im.f, &hole, a[4].parse().unwrap(), a[5].parse().unwrap()));
        }
        "ip.synth" => {
            // img mask block seed
            let im = axio::read(a[2]);
            let hole: Vec<bool> = axio::read(a[3]).f.iter().map(|v| *v > 0.5).collect();
            let v = ip::synthesize(im.w, im.h, im.ch, &im.f, &hole, a[4].parse().unwrap(), a[5].parse().unwrap());
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        _ => return false,
    }
    true
}
