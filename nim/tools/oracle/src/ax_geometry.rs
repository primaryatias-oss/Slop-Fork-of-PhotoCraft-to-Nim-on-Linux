//! Oracle commands for the Nim port of the colour-reduction / geometry group (see nim/PORTING.md).

use crate::axio::{self, f};
use photocraft_algo::quantize::{self as q, BitmapMethod, Dither, Forced, HalftoneShape, PaletteKind};

fn kind(s: &str) -> PaletteKind {
    PaletteKind::from_id(s).unwrap()
}
fn forced(s: &str) -> Forced {
    match s {
        "bw" => Forced::BlackWhite,
        "primaries" => Forced::Primaries,
        "web" => Forced::Web,
        _ => Forced::None,
    }
}
fn dither(s: &str) -> Dither {
    match s {
        "diffusion" => Dither::Diffusion,
        "pattern" => Dither::Pattern,
        "noise" => Dither::Noise,
        _ => Dither::None,
    }
}
fn pal_line(p: &[[u8; 3]]) -> String {
    p.iter().map(|c| format!("{:02x}{:02x}{:02x}", c[0], c[1], c[2])).collect::<Vec<_>>().join(" ")
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "q.fixed" => {
            println!("{}", pal_line(&q::web_palette()));
            println!("{}", pal_line(&q::mac_palette()));
            println!("{}", pal_line(&q::windows_palette()));
            for n in [0, 2, 8, 9, 26, 27, 64, 100, 216, 256, 1000] {
                println!("{n} {}", pal_line(&q::uniform_palette(n)));
            }
        }
        "q.palette" => {
            let b = axio::read(a[2]);
            match q::build_palette(&axio::rgba(&b), kind(a[3]), a[4].parse().unwrap(), forced(a[5])) {
                Ok(p) => println!("{}", pal_line(&p)),
                Err(e) => println!("ERR {e}"),
            }
        }
        "q.quantize" => {
            // img kind colors forced dither amount transparent(-1 = none)
            let b = axio::read(a[2]);
            let mut px = axio::rgba(&b);
            let pal = q::build_palette(&px, kind(a[3]), a[4].parse().unwrap(), forced(a[5])).unwrap();
            let t: i64 = a[8].parse().unwrap();
            let idx = q::quantize(&mut px, b.w, &pal, dither(a[6]), f(a[7]), (t >= 0).then_some(t as usize));
            println!("{}", idx.iter().map(|v| format!("{v:02x}")).collect::<String>());
            axio::write_f32(out, b.w, b.h, 4, &axio::flat(&px));
        }
        "q.bitmap" => {
            // gray image (1 channel), method, cell, angle, shape
            let b = axio::read(a[2]);
            let mut g = b.f.clone();
            let m = match a[3] {
                "threshold" => BitmapMethod::Threshold,
                "pattern" => BitmapMethod::Pattern,
                "diffusion" => BitmapMethod::Diffusion,
                _ => BitmapMethod::Halftone { cell: f(a[4]), angle: f(a[5]), shape: HalftoneShape::from_id(a[6]).unwrap() },
            };
            q::to_bitmap(&mut g, b.w, m);
            axio::write_f32(out, b.w, b.h, 1, &g);
        }
        _ => return false,
    }
    true
}
