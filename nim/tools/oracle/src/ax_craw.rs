//! Oracle commands for the Nim port of Filter › Camera Raw Filter (`camera_raw`).

use crate::axio;
use photocraft_algo::camera_raw::{self as cr, CameraRaw};

fn bits(v: &[f32]) -> String {
    v.iter().map(|x| x.to_bits().to_string()).collect::<Vec<_>>().join(" ")
}

fn dump(p: &CameraRaw) {
    let f = [
        p.temperature, p.tint, p.exposure, p.contrast, p.highlights, p.shadows, p.whites, p.blacks, p.texture, p.clarity, p.dehaze,
        p.vibrance, p.saturation, p.curve_highlights, p.curve_lights, p.curve_darks, p.curve_shadows, p.grade_blending, p.grade_balance,
        p.sharpen_amount, p.sharpen_radius, p.sharpen_detail, p.sharpen_masking, p.noise_luminance, p.noise_luminance_detail,
        p.noise_color, p.noise_color_detail, p.grain_amount, p.grain_size, p.grain_roughness, p.vignette_amount, p.vignette_midpoint,
        p.vignette_roundness, p.vignette_feather, p.vignette_highlights, p.pixel_scale,
    ];
    println!("f {}", bits(&f));
    println!("splits {} hsl {} {} {}", bits(&p.curve_splits), bits(&p.hsl_hue), bits(&p.hsl_sat), bits(&p.hsl_lum));
    for c in [&p.point_curve, &p.point_curve_red, &p.point_curve_green, &p.point_curve_blue] {
        println!("curve {}", bits(&c.iter().flatten().copied().collect::<Vec<_>>()));
    }
    for w in [p.grade_shadows, p.grade_midtones, p.grade_highlights, p.grade_global] {
        println!("wheel {}", bits(&[w.hue, w.sat, w.lum]));
    }
    println!("style {} seed {} identity {}", p.vignette_style, p.seed, p.is_identity());
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "cr.json" => match serde_json::from_str::<CameraRaw>(a[2]) {
            Err(_) => println!("Err"),
            Ok(p) => {
                dump(&p);
                println!("validate {}", p.validate().is_ok());
                println!("changes {}", p.validate_changes(&CameraRaw::default()).is_ok());
                let back: CameraRaw = serde_json::from_str(&serde_json::to_string(&p).unwrap()).unwrap();
                println!("roundtrip {}", back == p);
            }
        },
        "cr.dev" => {
            // img json float
            let im = axio::read(a[2]);
            let mut px = axio::rgba(&im);
            let p: CameraRaw = serde_json::from_str(a[3]).unwrap();
            cr::develop(&mut px, im.w, im.h, &p, axio::bl(a[4]));
            axio::write_f32(out, im.w, im.h, 4, &axio::flat(&px));
        }
        "cr.misc" => {
            // points(csv) n
            let v = axio::floats(a[2]);
            let pts: Vec<[f32; 2]> = v.chunks_exact(2).map(|c| [c[0], c[1]]).collect();
            println!("validate {}", cr::validate_curve(&pts).is_ok());
            println!("lut {}", bits(&cr::curve_lut(&pts, a[3].parse().unwrap())));
            let mut bw = Vec::new();
            for k in -20..40 {
                bw.extend(cr::band_weights(k as f32 * 11.3 - 30.0));
            }
            bw.extend(cr::band_weights(f32::NAN));
            bw.extend(cr::band_weights(1e20));
            println!("bands {}", bits(&bw));
            let mut g = Vec::new();
            for (t, n) in [(0.0, 0.0), (50.0, -20.0), (-100.0, 100.0), (1e30, -1e30)] {
                g.extend(cr::white_balance_gains(t, n));
            }
            println!("wb {}", bits(&g));
        }
        _ => return false,
    }
    true
}
