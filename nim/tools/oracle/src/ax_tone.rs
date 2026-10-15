//! Oracle commands for the Nim port of the tone group (see nim/PORTING.md).

use crate::axio::{self, bl, f, floats};
use photocraft_algo::histogram::{RgbHistogram, clipping};
use photocraft_algo::tone::{self, HdrToning, MatchColor, ShadowsHighlights};

fn bits(v: &[f32]) -> Vec<u32> {
    v.iter().map(|x| x.to_bits()).collect()
}

fn curve(a: &str) -> Vec<(f32, f32)> {
    if a == "-" {
        return Vec::new();
    }
    a.split(';').map(|p| {
        let v = floats(p);
        (v[0], v[1])
    })
    .collect()
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "tone.hist" => {
            let b = axio::read(a[2]);
            let h = RgbHistogram::from_rgba(&axio::rgba(&b));
            println!("{} {} {} {:?} {:?} {:?} {:?}", h.samples, h.transparent, h.invalid, h.underflow, h.overflow, h.shadows, h.highlights);
            let flat: Vec<f32> = h.channels.iter().flatten().map(|v| *v as f32).collect();
            axio::write_f32(out, 256, 3, 1, &flat);
        }
        "tone.clip" => {
            let v = floats(a[2]);
            println!("{:?}", clipping([v[0], v[1], v[2], v[3]]));
        }
        "tone.sh" => {
            let mut b = axio::read(a[2]);
            let v = floats(a[3]);
            let p = ShadowsHighlights {
                shadow_amount: v[0],
                shadow_tone: v[1],
                shadow_radius: v[2],
                highlight_amount: v[3],
                highlight_tone: v[4],
                highlight_radius: v[5],
                color: v[6],
                midtone: v[7],
                black_clip: v[8],
                white_clip: v[9],
            };
            let mut px = axio::rgba(&b);
            tone::shadows_highlights(&mut px, b.w, b.h, &p);
            b.f = axio::flat(&px);
            axio::write_f32(out, b.w, b.h, 4, &b.f);
        }
        "tone.replace" => {
            let b = axio::read(a[2]);
            let c = floats(a[3]);
            let mut px = axio::rgba(&b);
            let m = tone::replace_color(&mut px, [c[0], c[1], c[2]], f(a[4]), f(a[5]), f(a[6]), f(a[7]));
            let mut v: Vec<f32> = px.iter().zip(&m).flat_map(|(p, k)| [p[0], p[1], p[2], p[3], *k]).collect();
            v.extend_from_slice(&m[px.len()..]);
            axio::write_f32(out, b.w, b.h, 5, &v);
        }
        "tone.labstats" => {
            let b = axio::read(a[2]);
            let m = (a[3] != "-").then(|| axio::read(a[3]).f);
            match tone::lab_stats(&axio::rgba(&b), m.as_deref()) {
                Some(s) => println!("{:?} {:?}", bits(&s.mean), bits(&s.std)),
                None => println!("none"),
            }
        }
        "tone.match" => {
            let b = axio::read(a[2]);
            let mut px = axio::rgba(&b);
            let t = tone::lab_stats(&px, None).unwrap();
            let s = (a[3] != "-").then(|| tone::lab_stats(&axio::rgba(&axio::read(a[3])), None).unwrap());
            let o = MatchColor { luminance: f(a[4]), intensity: f(a[5]), fade: f(a[6]), neutralize: bl(a[7]) };
            tone::match_color(&mut px, &t, s.as_ref(), &o);
            axio::write_f32(out, b.w, b.h, 4, &axio::flat(&px));
        }
        "tone.hdr" => {
            let b = axio::read(a[2]);
            let v = floats(a[3]);
            let p = HdrToning {
                radius: v[0],
                strength: v[1],
                gamma: v[2],
                exposure: v[3],
                detail: v[4],
                shadow: v[5],
                highlight: v[6],
                vibrance: v[7],
                saturation: v[8],
                curve: curve(a[4]),
            };
            let mut px = axio::rgba(&b);
            tone::hdr_toning(&mut px, b.w, b.h, &p);
            axio::write_f32(out, b.w, b.h, 4, &axio::flat(&px));
        }
        _ => return false,
    }
    true
}

pub fn run_hdr(args: &[String]) -> bool {
    use photocraft_algo::hdr::{self, MergeOptions, ToneMethod};
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    let grey = |b: &axio::Buf| -> Vec<f32> { b.f.chunks_exact(4).map(|p| 0.2126 * p[0] + 0.7152 * p[1] + 0.0722 * p[2]).collect() };
    match a[1] {
        "hdr.estimate" => {
            let imgs: Vec<Vec<[f32; 4]>> = a[2..a.len() - 1].iter().map(|p| axio::rgba(&axio::read(p))).collect();
            let refs: Vec<&[[f32; 4]]> = imgs.iter().map(Vec::as_slice).collect();
            println!("{:?}", hdr::estimate_exposures(&refs).iter().map(|v| v.to_bits()).collect::<Vec<_>>());
        }
        "hdr.mtb" => {
            // img, dx, dy, scale, levels: aligns a shifted, darker copy of the image's grey
            let b = axio::read(a[2]);
            let (w, h) = (b.w, b.h);
            let g = grey(&b);
            let (dx, dy, k) = (axio::i(a[3]), axio::i(a[4]), f(a[5]));
            let shifted: Vec<f32> = (0..w * h)
                .map(|i| {
                    let (x, y) = ((i % w) as i32 - dx, (i / w) as i32 - dy);
                    g[(y.clamp(0, h as i32 - 1) as usize) * w + x.clamp(0, w as i32 - 1) as usize] * k
                })
                .collect();
            println!("{:?}", hdr::mtb_offset(w, h, &shifted, &g, a[6].parse().unwrap()));
        }
        "hdr.samples" => {
            let b = axio::read(a[2]);
            println!("{:?}", hdr::sample_pixels(b.w, b.h, &axio::rgba(&b), a[3].parse().unwrap()));
        }
        "hdr.merge" => {
            // exposures, ghosts, base (-1 = none), images…
            let ex: Vec<f64> = floats(a[2]).iter().map(|v| *v as f64).collect();
            let imgs: Vec<axio::Buf> = a[5..a.len() - 1].iter().map(|p| axio::read(p)).collect();
            let px: Vec<Vec<[f32; 4]>> = imgs.iter().map(axio::rgba).collect();
            let refs: Vec<&[[f32; 4]]> = px.iter().map(Vec::as_slice).collect();
            let base = a[4].parse::<i64>().unwrap();
            let opts = MergeOptions { exposures: ex, remove_ghosts: bl(a[3]), ghost_base: (base >= 0).then_some(base as usize), response: None };
            let m = hdr::merge(imgs[0].w, imgs[0].h, &refs, &opts);
            println!("{} {} {}", m.ghost_base, m.ghost_fraction.to_bits(), m.stops.to_bits());
            for r in &m.response {
                println!("{:?}", r.iter().map(|v| v.to_bits()).collect::<Vec<_>>());
            }
            axio::write_f32(out, imgs[0].w, imgs[0].h, 4, &axio::flat(&m.px));
        }
        "hdr.tonemap" => {
            let b = axio::read(a[2]);
            let v = floats(a[4]);
            let method = match a[3] {
                "local" => ToneMethod::LocalAdaptation(HdrToning {
                    radius: v[0],
                    strength: v[1],
                    gamma: v[2],
                    exposure: v[3],
                    detail: v[4],
                    shadow: v[5],
                    highlight: v[6],
                    vibrance: v[7],
                    saturation: v[8],
                    curve: Vec::new(),
                }),
                "expgamma" => ToneMethod::ExposureGamma { exposure: v[0], gamma: v[1] },
                "highlight" => ToneMethod::HighlightCompression,
                _ => ToneMethod::EqualizeHistogram,
            };
            let mut px = axio::rgba(&b);
            hdr::tone_map(&mut px, b.w, b.h, &method);
            axio::write_f32(out, b.w, b.h, 4, &axio::flat(&px));
        }
        _ => return false,
    }
    true
}

fn exif_line(i: &photocraft_algo::exif::CameraInfo) -> String {
    let s = |v: &Option<String>| v.clone().unwrap_or_else(|| "-".into());
    let n = |v: Option<f64>| v.map_or("-".to_string(), |x| x.to_bits().to_string());
    format!(
        "{}|{}|{}|{}|{}|{}|{}|{}|{}|{}",
        s(&i.make),
        s(&i.model),
        s(&i.lens),
        n(i.exposure_time),
        n(i.f_number),
        n(i.iso),
        n(i.focal_length),
        n(i.focal_length_35mm),
        s(&i.date_taken),
        n(i.exposure_factor())
    )
}

pub fn run_exif(args: &[String]) -> bool {
    use photocraft_algo::exif::{self, CameraInfo};
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    let sample = || CameraInfo {
        make: Some("Generic".into()),
        model: Some("Camera 1".into()),
        lens: Some("24-70mm \u{fffd}".into()),
        exposure_time: Some(0.004),
        f_number: Some(8.0),
        iso: Some(200.0),
        focal_length: Some(35.0),
        focal_length_35mm: Some(52.0),
        date_taken: Some("2024:02:17 12:40:19".into()),
    };
    match a[1] {
        "exif.build" => {
            let b = exif::build(&sample());
            std::fs::write(out, &b).unwrap();
            println!("{}", exif_line(&exif::read(&b)));
        }
        "exif.trunc" => {
            let b = exif::build(&sample());
            for cut in 0..=b.len() {
                println!("{cut} {}", exif_line(&exif::read(&b[..cut])));
            }
        }
        "exif.fuzz" => {
            let mut x: u32 = a[2].parse().unwrap();
            let mut good = exif::build(&sample());
            for k in 0..a[3].parse::<usize>().unwrap() {
                let mut n = if k % 2 == 0 { b"II*\0\x08\0\0\0".to_vec() } else { b"MM\0*\0\0\0\x08".to_vec() };
                for _ in 0..96 {
                    x ^= x << 13;
                    x ^= x >> 17;
                    x ^= x << 5;
                    n.push(x as u8);
                }
                println!("{k} {}", exif_line(&exif::read(&n)));
                // Flip one byte of a valid payload.
                let at = (x as usize) % good.len();
                good[at] ^= (x >> 8) as u8;
                println!("{k}f {}", exif_line(&exif::read(&good)));
            }
        }
        _ => return false,
    }
    true
}
