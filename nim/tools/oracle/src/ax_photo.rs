//! Oracle commands for the Nim port of the photo-merge group: Auto-Blend pyramids and, as they
//! land, features, Photomerge, lens and camera models (see nim/PORTING.md).

use crate::axio;
use photocraft_algo::pyramid as py;

fn luma(b: &axio::Buf) -> Vec<f32> {
    b.f.chunks_exact(b.ch).map(|p| if b.ch >= 3 { 0.2126 * p[0] + 0.7152 * p[1] + 0.0722 * p[2] } else { p[0] }).collect()
}

fn alpha(b: &axio::Buf) -> Vec<f32> {
    b.f.chunks_exact(b.ch).map(|p| if b.ch == 4 || b.ch == 2 { p[b.ch - 1] } else { 1.0 }).collect()
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "py.lap" => {
            let b = axio::read(a[2]);
            let pyr = py::laplacian(b.w, b.h, b.ch, &b.f, a[3].parse().unwrap());
            let g = py::gaussian(b.w, b.h, b.ch, &b.f, a[3].parse().unwrap());
            let mut v = Vec::new();
            for l in pyr.iter().chain(&g) {
                println!("level {} {}", l.w, l.h);
                v.extend_from_slice(&l.px);
            }
            v.extend(py::collapse(&pyr, b.ch));
            println!("auto {} stack {}", py::auto_levels(b.w, b.h), py::stack_levels(b.w, b.h));
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "py.blend" | "py.fuse" | "py.stackw" | "py.panow" => {
            // py.blend levels img mask img mask … | py.fuse levels|- radius halo guide img… | py.stackw img… | py.panow img…
            let (k, imgs): (usize, Vec<axio::Buf>) = match a[1] {
                "py.blend" => (3, a[3..a.len() - 1].iter().step_by(2).map(|p| axio::read(p)).collect()),
                "py.fuse" => (6, a[6..a.len() - 1].iter().map(|p| axio::read(p)).collect()),
                _ => (2, a[2..a.len() - 1].iter().map(|p| axio::read(p)).collect()),
            };
            let _ = k;
            let (w, h, ch) = (imgs[0].w, imgs[0].h, imgs[0].ch);
            let px: Vec<&[f32]> = imgs.iter().map(|b| b.f.as_slice()).collect();
            let r = match a[1] {
                "py.blend" => {
                    let wts: Vec<Vec<f32>> = a[4..a.len() - 1].iter().step_by(2).map(|p| axio::read(p).f).collect();
                    py::blend(w, h, ch, &px, &wts, a[2].parse().unwrap())
                }
                "py.fuse" => {
                    let p = py::StackFusion {
                        energy_radius: a[3].parse().unwrap(),
                        halo: a[4].parse().unwrap(),
                        guide: a[5].parse().unwrap(),
                        levels: if a[2] == "-" { None } else { Some(a[2].parse().unwrap()) },
                    };
                    let l: Vec<Vec<f32>> = imgs.iter().map(luma).collect();
                    let al: Vec<Vec<f32>> = imgs.iter().map(alpha).collect();
                    py::fuse_stack(w, h, ch, &px, &l, &al, p)
                }
                "py.stackw" => {
                    let l: Vec<Vec<f32>> = imgs.iter().map(luma).collect();
                    let al: Vec<Vec<f32>> = imgs.iter().map(alpha).collect();
                    py::stack_weights(w, h, &l, &al).concat()
                }
                _ => {
                    let al: Vec<Vec<f32>> = imgs.iter().map(alpha).collect();
                    py::panorama_weights(w, h, &al).concat()
                }
            };
            axio::write_f32(out, r.len(), 1, 1, &r);
        }
        _ => return false,
    }
    true
}
