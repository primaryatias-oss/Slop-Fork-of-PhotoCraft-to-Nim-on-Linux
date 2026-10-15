//! Oracle commands for the Nim port of the photo-merge group: Auto-Blend pyramids and, as they
//! land, features, Photomerge, lens and camera models (see nim/PORTING.md).

use crate::axio;
use photocraft_algo::{lens, pyramid as py};
use photocraft_geom::Rect;

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
        "lc.run" => {
            // img fmt frame spec
            let im = axio::read(a[2]);
            let src = crate::ax_retouch::surface(a[3], &im);
            let frame = axio::rect(a[4]);
            let lc: lens::LensCorrection = serde_json::from_str(a[5]).unwrap();
            let b = |v: f64| v.to_bits().to_string();
            let m = lens::LensMap::new(&lc, frame);
            let mut line = Vec::new();
            for j in 0..=8 {
                for i in 0..=8 {
                    let (x, y) = (f64::from(frame.x0) - 5.0 + f64::from(frame.width() + 10) * f64::from(i) / 8.0, f64::from(frame.y0) - 5.0 + f64::from(frame.height() + 10) * f64::from(j) / 8.0);
                    match m.sample(x, y) {
                        Some(s) => line.push(format!("{} {} {} {} {} {} {}", b(s.p[0]), b(s.p[1]), b(s.pr[0]), b(s.pr[1]), b(s.pb[0]), b(s.pb[1]), s.gain.to_bits())),
                        None => line.push("none".into()),
                    }
                }
            }
            println!("{}", line.join(" "));
            println!("auto {}", b(lens::auto_scale(&lc, frame)));
            let full = Rect::new(0, 0, im.w as i32, im.h as i32);
            let out_s = lens::correct(&src, frame, &lc);
            println!("format {:?}", out_s.format());
            axio::dump_surface(out, &out_s, full);
        }
        "lc.remapall" => {
            use photocraft_color::{ColorMode, PixelFormat, SampleType};
            use photocraft_raster::Surface;
            let bounds = Rect::new(-13, -9, 39, 26);
            let mut v: Vec<f32> = Vec::new();
            for mode in [ColorMode::Rgb, ColorMode::Grayscale, ColorMode::Cmyk, ColorMode::Lab, ColorMode::Bitmap, ColorMode::Duotone, ColorMode::Indexed, ColorMode::Multichannel] {
                for sample in [SampleType::U8, SampleType::U16, SampleType::F32] {
                    for alpha in [false, true] {
                        let fmt = PixelFormat::new(mode, sample, alpha);
                        let n = fmt.channels();
                        let mut s = Surface::new(fmt);
                        let data: Vec<f32> = (0..bounds.width() as usize * bounds.height() as usize * n)
                            .map(|i| if i % 17 == 0 { 0.0 } else { ((i * 103 + i / n * 71) % 65536) as f32 / 65535.0 })
                            .collect();
                        s.write_region(bounds, &data);
                        for edge in [lens::EdgeMode::Transparency, lens::EdgeMode::Extension, lens::EdgeMode::Color([0.2, 0.5, 0.7, 0.4])] {
                            for ca in [false, true] {
                                let map = |x: f64, y: f64| {
                                    if (x as i32 + y as i32) % 13 == 0 {
                                        return None;
                                    }
                                    let p = [x * 0.83 - 3.7, y * 1.14 + 1.3];
                                    Some(lens::Sample { p, pr: if ca { [p[0] - 0.9, p[1] + 0.3] } else { p }, pb: if ca { [p[0] + 0.7, p[1] - 0.4] } else { p }, gain: 1.17 })
                                };
                                let o = bounds.inflate(3);
                                let r = lens::remap(&s, bounds, o, edge, &map);
                                println!("{:?} {} {}", r.format(), r.tile_count(), r.channels());
                                v.extend(r.read_region(o));
                            }
                        }
                    }
                }
            }
            axio::write_f32(out, v.len(), 1, 1, &v);
        }
        "lc.misc" => {
            let b = |v: f64| v.to_bits().to_string();
            for f in [-5.0, 0.0, 12.0, 14.0, 17.5, 18.5, 24.0, 35.0, 49.9, 85.0, 120.0, 200.0, 999.0, f64::NAN, f64::INFINITY] {
                let p = lens::generic_profile(f);
                println!("{} {} {} {} {} {} {} {} {} {}", p.name, b(p.k1), b(p.k2), b(p.p1), b(p.p2), b(p.vignette[0]), b(p.vignette[1]), b(p.vignette[2]), b(p.ca_red), b(p.ca_blue));
            }
            for (ax, ay, bx, by) in [(0.0, 0.0, 10.0, 1.0), (0.0, 0.0, 1.0, 10.0), (5.0, 5.0, -3.0, 4.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, -10.0, -10.0), (1.0, 2.0, 1.0, f64::NAN)] {
                println!("straighten {}", b(lens::straighten_angle([ax, ay], [bx, by])));
            }
            std::fs::write(out, b"").unwrap();
        }
        _ => return false,
    }
    true
}
