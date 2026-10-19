//! Oracle commands for the Nim port of the camera raw decoder (`crates/raw`, see nim/PORTING.md).
//!
//! `raw.gen DIR -` writes synthetic raw files (the crate's testgen plus a Nikon Huffman encoder
//! re-stated from the crate's tests); the other commands decode, develop, fit or list a file and
//! print a canonical summary (floats as bit patterns), writing bulk data to the output path.

use photocraft_raw::testgen::*;
use photocraft_raw::*;

fn fnv(b: &[u8]) -> u64 {
    let mut h = 0xcbf2_9ce4_8422_2325u64;
    for &x in b {
        h ^= u64::from(x);
        h = h.wrapping_mul(0x100_0000_01b3);
    }
    h
}
fn le16(v: &[u16]) -> Vec<u8> {
    v.iter().flat_map(|x| x.to_le_bytes()).collect()
}
fn h16(v: &[u16]) -> String {
    format!("{} {:016x}", v.len(), fnv(&le16(v)))
}
fn b32(v: &[f32]) -> String {
    v.iter().map(|x| x.to_bits().to_string()).collect::<Vec<_>>().join(",")
}
fn b64(v: &[f64]) -> String {
    v.iter().map(|x| x.to_bits().to_string()).collect::<Vec<_>>().join(",")
}
fn mat(m: &Mat3) -> String {
    b64(&m.concat())
}
fn opt(o: &Option<String>) -> String {
    o.clone().unwrap_or_else(|| "-".into())
}
fn rect(r: &Rect) -> String {
    format!("{} {} {} {}", r.x, r.y, r.width, r.height)
}

fn sensor_summary(s: &Sensor) -> String {
    let mut o = Vec::new();
    o.push(format!("format {}", s.format.name()));
    o.push(format!("make {}", opt(&s.make)));
    o.push(format!("model {}", opt(&s.model)));
    o.push(format!("size {} {} {}", s.width, s.height, s.samples));
    o.push(format!("data {}", h16(&s.data)));
    o.push(match &s.cfa {
        Some(c) => format!("cfa {} {} {} {} {}", c.width, c.height, c.colors.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","), c.origin_x, c.origin_y),
        None => "cfa -".into(),
    });
    o.push(format!("lin {}", s.linearization.as_ref().map_or("-".into(), |l| h16(l))));
    o.push(format!("black {} {} {} | {} | {}", s.black.rows, s.black.cols, b32(&s.black.values), b32(&s.black.delta_h), b32(&s.black.delta_v)));
    o.push(format!("white {}", b32(&s.white)));
    o.push(format!("active {}", rect(&s.active)));
    o.push(format!("crop {}", rect(&s.crop)));
    for c in &s.color.calibrations {
        o.push(format!(
            "cal {} {} {} {}",
            c.temperature.to_bits(),
            mat(&c.color_matrix),
            c.forward_matrix.as_ref().map_or("-".into(), mat),
            mat(&c.camera_calibration)
        ));
    }
    o.push(format!("analog {}", s.color.analog_balance.map_or("-".into(), |v| b64(&v))));
    o.push(format!("neutral {}", s.color.as_shot_neutral.map_or("-".into(), |v| b64(&v))));
    o.push(format!("xy {}", s.color.as_shot_white_xy.map_or("-".into(), |v| b64(&v))));
    o.push(format!("camwb {}", s.camera_wb.map_or("-".into(), |v| b64(&v))));
    o.push(format!("orientation {}", s.orientation));
    o.push(format!("baseline {}", s.baseline_exposure.to_bits()));
    for g in &s.gain_maps {
        o.push(format!(
            "gain {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
            g.top,
            g.left,
            g.bottom,
            g.right,
            g.plane,
            g.planes,
            g.row_pitch,
            g.col_pitch,
            g.points_v,
            g.points_h,
            g.spacing_v.to_bits(),
            g.spacing_h.to_bits(),
            g.origin_v.to_bits(),
            g.origin_h.to_bits(),
            g.map_planes,
            b32(&g.gains)
        ));
    }
    o.push(format!("tone {}", b32(&s.tone_curve.concat())));
    for w in &s.warnings {
        o.push(format!("warn {w}"));
    }
    o.join("\n")
}

/// Applies a mutation spec: `-`, `trunc:N` (first N bytes), `flip:SEED:K` (K seeded bit flips).
fn mutate(mut b: Vec<u8>, spec: &str) -> Vec<u8> {
    let p: Vec<&str> = spec.split(':').collect();
    match p[0] {
        "trunc" => {
            let n: usize = p[1].parse().unwrap();
            b.truncate(n);
        }
        "flip" => {
            let mut s: u32 = p[1].parse().unwrap();
            let k: usize = p[2].parse().unwrap();
            for _ in 0..k {
                s = s.wrapping_mul(1_103_515_245).wrapping_add(12345);
                if b.is_empty() {
                    break;
                }
                let at = (s >> 8) as usize % b.len();
                b[at] ^= 1 << (s % 8);
            }
        }
        _ => {}
    }
    b
}

fn wb(spec: &str) -> WhiteBalance {
    match spec {
        "asshot" => WhiteBalance::AsShot,
        "auto" => WhiteBalance::Auto,
        m => {
            let v: Vec<f64> = m.trim_start_matches("m:").split(',').map(|x| x.parse().unwrap()).collect();
            WhiteBalance::Multipliers([v[0], v[1], v[2]])
        }
    }
}

fn look_summary(l: &CameraLook) -> String {
    format!(
        "look {} | {} | {} {}",
        mat(&l.to_xyz),
        l.tone.as_ref().map_or("-".into(), |t| b64(&t.knots().concat())),
        l.delta_e.to_bits(),
        l.delta_e_fallback.to_bits()
    )
}

pub fn run(args: &[String]) -> bool {
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = *a.last().unwrap();
    match a[1] {
        "raw.gen" => gen_files(a[2]),
        "raw.decode" => {
            // file mutation
            let b = mutate(std::fs::read(a[2]).unwrap(), a[3]);
            println!("identify {}", identify(&b).map_or("-", |f| f.name()));
            match decode(&b, &Limits::default()) {
                Ok(s) => {
                    println!("{}", sensor_summary(&s));
                    std::fs::write(out, le16(&s.data)).unwrap();
                }
                Err(e) => println!("ERR {e}"),
            }
        }
        "raw.develop" => {
            // file mutation demosaic wb exposure orient look(-|ref:w:h:orientation)
            let b = mutate(std::fs::read(a[2]).unwrap(), a[3]);
            let mut opts = DevelopOptions {
                demosaic: Demosaic::from_id(a[4]).unwrap(),
                white_balance: wb(a[5]),
                exposure: a[6].parse().unwrap(),
                orient: a[7] == "1",
                ..Default::default()
            };
            let s = match decode(&b, &opts.limits) {
                Ok(s) => s,
                Err(e) => {
                    println!("ERR {e}");
                    return true;
                }
            };
            if a[8] != "-" {
                let p: Vec<&str> = a[8].split(':').collect();
                let rgb = std::fs::read(p[0]).unwrap();
                let r = Reference { width: p[1].parse().unwrap(), height: p[2].parse().unwrap(), rgb: &rgb };
                match fit_look(&s, &opts, &r) {
                    Ok(l) => {
                        println!("{}", look_summary(&l));
                        opts.camera_look = Some(l);
                    }
                    Err(e) => println!("REJECT {e}"),
                }
            }
            match develop_sensor(&s, &opts) {
                Ok(d) => {
                    println!("dev {} {} {}", d.width, d.height, h16(&d.rgb));
                    let i = &d.info;
                    println!(
                        "info {} {} {} {} {} {} {} {} {}",
                        i.format.name(),
                        opt(&i.make),
                        opt(&i.model),
                        i.sensor_width,
                        i.sensor_height,
                        opt(&i.cfa),
                        b64(&i.wb_multipliers),
                        i.orientation,
                        i.baseline_exposure.to_bits()
                    );
                    for w in &d.warnings {
                        println!("warn {w}");
                    }
                    std::fs::write(out, le16(&d.rgb)).unwrap();
                }
                Err(e) => println!("ERR {e}"),
            }
        }
        "raw.look" => {
            // file ref width height wb orientation(- = the file's)
            let s0 = decode(&std::fs::read(a[2]).unwrap(), &Limits::default());
            let mut s = match s0 {
                Ok(s) => s,
                Err(e) => {
                    println!("ERR {e}");
                    return true;
                }
            };
            if a[7] != "-" {
                s.orientation = a[7].parse().unwrap();
            }
            let rgb = std::fs::read(a[3]).unwrap();
            let opts = DevelopOptions { white_balance: wb(a[6]), ..Default::default() };
            let r = Reference { width: a[4].parse().unwrap(), height: a[5].parse().unwrap(), rgb: &rgb };
            match fit_look(&s, &opts, &r) {
                Ok(l) => println!("{}", look_summary(&l)),
                Err(e) => println!("REJECT {e}"),
            }
        }
        "raw.preview" => {
            let b = mutate(std::fs::read(a[2]).unwrap(), a[3]);
            for p in embedded_previews(&b) {
                println!("preview {} {} {} {}", p.jpeg.as_ptr() as usize - b.as_ptr() as usize, p.jpeg.len(), p.width, p.height);
            }
            match embedded_preview(&b) {
                Some(p) => println!("best {} {} {} {}", p.jpeg.as_ptr() as usize - b.as_ptr() as usize, p.jpeg.len(), p.width, p.height),
                None => println!("best -"),
            }
        }
        "raw.lj" => {
            // file max-samples
            let b = std::fs::read(a[2]).unwrap();
            match decode_lossless_jpeg(&b, a[3].parse().unwrap()) {
                Ok((w, h, c, v)) => println!("lj {w} {h} {c} {}", h16(&v)),
                Err(e) => println!("ERR {e}"),
            }
        }
        "raw.dump" => {
            let b = mutate(std::fs::read(a[2]).unwrap(), a[3]);
            print!("{}", dump_structure(&b));
        }
        _ => return false,
    }
    true
}

// ---------------------------------------------------------------- Nikon Huffman encoder
// Re-stated from the crate's `nefc.rs` tests (the tables are the decoder's).

const UNSEEN: u8 = u8::MAX;
struct Table {
    counts: &'static [u8],
    symbols: &'static [u8],
}
const LOSSLESS_14: Table = Table { counts: &[0, 1, 4, 2, 2, 3, 1, 2], symbols: &[7, 6, 8, 5, 9, 4, 10, 3, 11, 12, 2, 0, 1, 13, 14] };
const LOSSY_12: Table = Table { counts: &[0, 1, 5, 1, 1, 1, 1, 1, 1], symbols: &[5, 4, 3, 6, 2, 7, 1, 0, 8, 9, UNSEEN, 10] };

#[derive(Default)]
struct Writer {
    out: Vec<u8>,
    acc: u64,
    n: u32,
}
impl Writer {
    fn put(&mut self, v: u32, len: u32) {
        for i in (0..len).rev() {
            self.acc = (self.acc << 1) | u64::from((v >> i) & 1);
            self.n += 1;
            if self.n == 8 {
                self.out.push(self.acc as u8);
                self.acc = 0;
                self.n = 0;
            }
        }
    }
    fn finish(mut self) -> Vec<u8> {
        if self.n > 0 {
            self.out.push((self.acc << (8 - self.n)) as u8);
        }
        self.out.extend_from_slice(&[0; 4]);
        self.out
    }
}
fn codes(t: &Table) -> Vec<(u32, u32)> {
    let mut out = vec![(0, 0); 17];
    let mut code = 0u32;
    let mut symbols = t.symbols.iter();
    for (i, &n) in t.counts.iter().enumerate() {
        for _ in 0..n {
            let s = *symbols.next().unwrap();
            if s != UNSEEN {
                out[s as usize] = (code, i as u32 + 1);
            }
            code += 1;
        }
        code <<= 1;
    }
    out
}
fn nef_encode(values: &[i32], w: usize, t: &Table, seeds: [[i32; 2]; 2]) -> Vec<u8> {
    let codes = codes(t);
    let mut wr = Writer::default();
    let mut vpred = seeds;
    for (y, row) in values.chunks(w).enumerate() {
        for (x, &v) in row.iter().enumerate() {
            let pred = if x < 2 { vpred[y & 1][x] } else { row[x - 2] };
            if x < 2 {
                vpred[y & 1][x] = v;
            }
            let d = v - pred;
            let k = 32 - d.unsigned_abs().leading_zeros();
            let (code, len) = codes[k as usize];
            wr.put(code, len);
            wr.put(if d < 0 { (d + (1 << k) - 1) as u32 } else { d as u32 }, k);
        }
    }
    wr.finish()
}
fn nef_image(w: usize, h: usize, max: i32, seed: u32) -> Vec<i32> {
    let mut s = seed;
    (0..w * h)
        .map(|i| {
            let (x, y) = ((i % w) as i32, (i / w) as i32);
            s = s.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            let noise = (s >> 24) as i32 % 9 - 4;
            let base = (x * 37 + y * 11) % (max / 2) + if (x / 7 + y / 5) % 3 == 0 { max / 3 } else { 0 };
            (base + noise).clamp(0, max)
        })
        .collect()
}
fn lossy_table(bits: u32, le: bool) -> Vec<u8> {
    let mut t = vec![0x44, 0x20];
    let put = |t: &mut Vec<u8>, v: u16| t.extend_from_slice(&if le { v.to_le_bytes() } else { v.to_be_bytes() });
    for _ in 0..4 {
        put(&mut t, 300);
    }
    put(&mut t, 257);
    let max = f64::from((1u32 << bits) - 1);
    for i in 0..257u32 {
        put(&mut t, (max * (f64::from(i) / 64.0).min(1.0).sqrt()) as u16);
    }
    t.resize(624, 0);
    t
}
fn lossless_table(seed: u16, le: bool) -> Vec<u8> {
    let mut t = vec![0x46, 0x30];
    for _ in 0..4 {
        t.extend_from_slice(&if le { seed.to_le_bytes() } else { seed.to_be_bytes() });
    }
    t.resize(46, 0);
    t
}

// ---------------------------------------------------------------- the file set

const CM: [f64; 9] = [0.9, -0.3, -0.1, -0.4, 1.3, 0.1, -0.1, 0.2, 0.7];
const CM_A: [f64; 9] = [1.1, -0.4, 0.05, -0.5, 1.4, 0.15, -0.05, 0.3, 0.55];
const FM: [f64; 9] = [0.65, 0.12, 0.2, 0.3, 0.6, 0.1, 0.05, -0.2, 0.98];
const FM_A: [f64; 9] = [0.6, 0.15, 0.21, 0.28, 0.62, 0.1, 0.04, -0.15, 0.94];

fn gen_files(dir: &str) {
    std::fs::create_dir_all(dir).unwrap();
    let put = |n: &str, b: &[u8]| std::fs::write(format!("{dir}/{n}"), b).unwrap();
    let (w, h) = (64usize, 48usize);
    let sc = scene(w, h);
    for (name, cfa) in [("rggb", [0u8, 1, 1, 2]), ("bggr", [2, 1, 1, 0]), ("grbg", [1, 0, 2, 1]), ("gbrg", [1, 2, 0, 1])] {
        let mut s = DngSpec::cfa(w, h, mosaic(&sc, w, cfa, 0, 65535));
        s.cfa = cfa;
        s.color_matrix1 = Some((21, CM));
        s.as_shot_neutral = Some([0.5, 1.0, 0.7]);
        put(&format!("dng_{name}.dng"), &s.build());
    }
    // Odd size with an active area and a default crop.
    {
        let (w, h) = (37, 29);
        let mut s = DngSpec::cfa(w, h, mosaic(&scene(w, h), w, [0, 1, 1, 2], 512, 60000));
        s.black = vec![512];
        s.white = 60000;
        s.active_area = Some([1, 2, 28, 36]);
        s.default_crop = Some(([1, 1], [30, 22]));
        s.color_matrix1 = Some((17, CM_A));
        s.color_matrix2 = Some((21, CM));
        put("dng_odd.dng", &s.build());
    }
    // 12-bit packed strips, two calibrations with forward matrices, baseline exposure, rotated.
    {
        let mut s = DngSpec::cfa(w, h, mosaic(&sc, w, [0, 1, 1, 2], 256, 4095));
        s.bits = 12;
        s.storage = DngStorage::Strips { rows: 7 };
        s.black = vec![256];
        s.white = 4095;
        s.color_matrix1 = Some((17, CM_A));
        s.color_matrix2 = Some((21, CM));
        s.forward_matrix1 = Some(FM_A);
        s.forward_matrix2 = Some(FM);
        s.as_shot_neutral = Some([0.45, 1.0, 0.62]);
        s.baseline_exposure = Some(0.5);
        s.orientation = 6;
        put("dng_12bit.dng", &s.build());
    }
    // 10-bit big-endian.
    {
        let mut s = DngSpec::cfa(w, h, mosaic(&sc, w, [1, 0, 2, 1], 64, 1023));
        s.cfa = [1, 0, 2, 1];
        s.bits = 10;
        s.big_endian = true;
        s.black = vec![64];
        s.white = 1023;
        s.color_matrix1 = Some((21, CM));
        put("dng_10bit_be.dng", &s.build());
    }
    // 8-bit with a linearization table.
    {
        let mut s = DngSpec::cfa(w, h, mosaic(&sc, w, [0, 1, 1, 2], 0, 255));
        s.bits = 8;
        s.white = 255;
        s.linearization = Some((0..256u32).map(|i| ((i * i) as f64 / 65025.0 * 65535.0) as u16).collect());
        s.color_matrix1 = Some((21, CM));
        s.as_shot_neutral = Some([0.6, 1.0, 0.8]);
        put("dng_8bit_lin.dng", &s.build());
    }
    // Lossless JPEG tiles and strips, deflate strips.
    {
        let mut s = DngSpec::cfa(w, h, mosaic(&sc, w, [0, 1, 1, 2], 0, 16383));
        s.bits = 14;
        s.white = 16383;
        s.color_matrix1 = Some((21, CM));
        s.storage = DngStorage::Lj92Tiles { width: 16, height: 16 };
        put("dng_lj92tiles.dng", &s.build());
        s.storage = DngStorage::Lj92Tiles { width: 24, height: 20 };
        put("dng_lj92tiles_edge.dng", &s.build());
        s.storage = DngStorage::Lj92Strips { rows: 10 };
        put("dng_lj92strips.dng", &s.build());
        s.bits = 16;
        s.storage = DngStorage::DeflateStrips { rows: 16, predictor: 1 };
        put("dng_deflate1.dng", &s.build());
        s.storage = DngStorage::DeflateStrips { rows: 13, predictor: 2 };
        put("dng_deflate2.dng", &s.build());
        s.storage = DngStorage::LossyJpegTiles { width: 32, height: 32 };
        put("dng_lossy.dng", &s.build());
    }
    // LinearRaw RGB (uncompressed, deflate with predictor, lossy JPEG) and monochrome.
    {
        let rgb: Vec<u16> = sc.iter().flat_map(|p| p.map(|v| (v * 50000.0).round() as u16)).collect();
        let mut s = DngSpec::cfa(w, h, rgb);
        s.samples = 3;
        s.white = 50000;
        s.color_matrix1 = Some((21, CM));
        s.as_shot_neutral = Some([0.7, 1.0, 0.9]);
        put("dng_linear.dng", &s.build());
        s.storage = DngStorage::DeflateStrips { rows: 9, predictor: 2 };
        put("dng_linear_deflate.dng", &s.build());
        s.storage = DngStorage::LossyJpegTiles { width: 16, height: 16 };
        put("dng_linear_lossy.dng", &s.build());
        let mono: Vec<u16> = sc.iter().map(|p| (p[1] * 60000.0) as u16).collect();
        let mut m = DngSpec::monochrome(w, h, mono);
        m.white = 60000;
        put("dng_mono.dng", &m.build());
    }
    // Per-position black, gain map, crops, ProfileGainTableMap, no calibration, white xy, analog.
    {
        let base = || {
            let mut s = DngSpec::cfa(w, h, mosaic(&sc, w, [0, 1, 1, 2], 130, 60000));
            s.color_matrix1 = Some((21, CM));
            s.as_shot_neutral = Some([0.5, 1.0, 0.7]);
            s.white = 60000;
            s
        };
        let mut s = base();
        s.black_repeat = (2, 2);
        s.black = vec![100, 110, 120, 130];
        put("dng_black2x2.dng", &s.build());
        let mut s = base();
        s.black = vec![130];
        s.opcode_list2 = Some(gain_map_opcode_list([0, 0, h as u32, w as u32], 1, [[1.0, 1.5], [2.0, 1.2]]));
        put("dng_gainmap.dng", &s.build());
        let mut s = base();
        s.active_area = Some([2, 2, 46, 62]);
        s.opcode_list2 = Some(gain_map_opcode_list([4, 2, 40, 58], 2, [[0.8, 1.3], [1.1, 1.6]]));
        put("dng_gainmap_pitch.dng", &s.build());
        let mut s = base();
        s.default_crop = Some(([1, 1], [60, 40]));
        s.default_crop_origin_double = Some([2.5, 1.5]);
        put("dng_crop_double.dng", &s.build());
        let mut s = base();
        s.default_crop_size_only = Some([50, 30]);
        put("dng_crop_size.dng", &s.build());
        let mut s = base();
        s.default_crop = Some(([10, 10], [60, 60]));
        put("dng_crop_bad.dng", &s.build());
        let mut s = base();
        s.default_crop_origin_double = Some([-1.0, 2.0]);
        put("dng_crop_neg.dng", &s.build());
        let mut s = base();
        s.profile_gain_table_map = Some(vec![1, 2, 3, 4, 5, 6]);
        put("dng_pgtm.dng", &s.build());
        let mut s = base();
        s.color_matrix1 = None;
        s.as_shot_neutral = None;
        put("dng_nocal.dng", &s.build());
        let mut s = base();
        s.as_shot_neutral = None;
        s.ifd0_extra = vec![(50729, Val::Rational(vec![(3457, 10000), (3585, 10000)]))];
        put("dng_xy.dng", &s.build());
        let mut s = base();
        s.ifd0_extra = vec![
            (50727, Val::Rational(vec![(11000, 10000), (10000, 10000), (9000, 10000)])),
            (50723, Val::SRational(vec![(10200, 10000), (0, 10000), (0, 10000), (0, 10000), (10000, 10000), (0, 10000), (0, 10000), (0, 10000), (9800, 10000)])),
            (50718, Val::Rational(vec![(1, 1), (2, 1)])),
        ];
        put("dng_analog.dng", &s.build());
        let mut s = base();
        s.color_matrix1 = Some((21, [1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 1.0, 1.0, 1.0]));
        put("dng_singular.dng", &s.build());
    }
    // Every orientation on a small image.
    for o in 1..=8u16 {
        let (w, h) = (20, 14);
        let mut s = DngSpec::cfa(w, h, mosaic(&scene(w, h), w, [0, 1, 1, 2], 0, 65535));
        s.orientation = o;
        s.color_matrix1 = Some((21, CM));
        put(&format!("dng_orient{o}.dng"), &s.build());
    }
    // CR2: masked border, slices, WB, model ID.
    {
        let (w, h) = (160usize, 64usize);
        let sc = scene(w, h);
        let data: Vec<u16> = (0..w * h)
            .map(|i| {
                let (x, y) = (i % w, i / w);
                if x < 40 || y < 8 {
                    2048 + ((x * 7 + y * 3) % 5) as u16
                } else {
                    let c = [0usize, 1, 1, 2][(y & 1) * 2 + (x & 1)];
                    (2048.0 + sc[i][c].clamp(0.0, 1.0) * 12000.0).round() as u16
                }
            })
            .collect();
        let spec = Cr2Spec {
            width: w,
            height: h,
            data: data.clone(),
            precision: 14,
            components: 2,
            slices: vec![64, 64, 32],
            borders: Some([40, 8, 159, 63]),
            wb_rggb: Some([2000, 1024, 1024, 1500]),
            orientation: 1,
            model_id: Some(0x8000_0218),
        };
        put("cr2_basic.cr2", &spec.build());
        let spec2 = Cr2Spec { components: 4, slices: vec![], borders: None, wb_rggb: None, orientation: 8, model_id: None, ..spec.clone() };
        put("cr2_plain.cr2", &spec2.build());
        // Green-first data (a row shift).
        let shifted: Vec<u16> = (0..w * h).map(|i| data[(i + w) % (w * h)]).collect();
        let spec3 = Cr2Spec { data: shifted, ..spec };
        put("cr2_shift.cr2", &spec3.build());
    }
    // TIFF/EP raws.
    {
        let d12 = mosaic(&sc, w, [0, 1, 1, 2], 150, 4095);
        put("nef_tiffep.nef", &tiff_ep("NIKON CORPORATION", w, h, &d12, [0, 1, 1, 2], 12, vec![]));
        put("arw_tiffep.arw", &tiff_ep("SONY", w, h, &mosaic(&sc, w, [2, 1, 1, 0], 512, 16383), [2, 1, 1, 0], 14, vec![]));
        put("pef_tiffep.pef", &tiff_ep("PENTAX", w, h, &d12, [0, 1, 1, 2], 12, vec![(50714, Val::Long(vec![150, 150, 150, 150])), (50717, Val::Long(vec![4095]))]));
        put("tiffep_other.tif", &tiff_ep("Leaf", w, h, &d12, [1, 2, 0, 1], 12, vec![(50719, Val::Long(vec![2, 2])), (50720, Val::Long(vec![40, 30]))]));
    }
    // Nikon compressed NEF.
    {
        let (w, h) = (40usize, 6usize);
        let img = nef_image(w, h, 16383, 11);
        let src = nef_encode(&img, w, &LOSSLESS_14, [[2048; 2]; 2]);
        let note = vec![(0x0096, Val::Undefined(lossless_table(2048, false))), (0x003d, Val::Short(vec![600, 600, 600, 600]))];
        put("nefc_lossless.nef", &nef_compressed(w, h, 14, src.clone(), [0, 1, 1, 2], true, note));
        put("nefc_noblack.nef", &nef_compressed(w, h, 14, src, [0, 1, 1, 2], true, vec![(0x0096, Val::Undefined(lossless_table(2048, false)))]));
        let table = lossy_table(12, true);
        let img = nef_image(w, h, 1023, 5);
        let src = nef_encode(&img, w, &LOSSY_12, [[300; 2]; 2]);
        let note = vec![(0x0096, Val::Undefined(table.clone())), (0x003d, Val::Short(vec![600, 600, 600, 600]))];
        put("nefc_lossy.nef", &nef_compressed(w, h, 12, src.clone(), [2, 1, 1, 0], false, note));
        let mut split = table;
        split[562..564].copy_from_slice(&3u16.to_le_bytes());
        put("nefc_split.nef", &nef_compressed(w, h, 12, src.clone(), [2, 1, 1, 0], false, vec![(0x0096, Val::Undefined(split))]));
        put("nefc_notable.nef", &nef_compressed(w, h, 12, src, [2, 1, 1, 0], false, vec![(0x0001, Val::Short(vec![0]))]));
    }
    // Sony cRAW.
    {
        let curve = [8000, 10400, 12900, 14100];
        let (w, h) = (640, 8);
        put("arw_craw.arw", &sony_craw(w, h, &mosaic(&scene(w, h), w, [0, 1, 1, 2], 256, 1900), curve));
        let mut s = 12345u32;
        let codes: Vec<u16> = (0..64 * 8)
            .map(|_| {
                s = s.wrapping_mul(1103515245).wrapping_add(12345);
                ((s >> 16) % 2000) as u16
            })
            .collect();
        put("arw_craw_wide.arw", &sony_craw(64, 8, &codes, curve));
        put("arw_craw_badcurve.arw", &sony_craw(32, 4, &vec![300u16; 32 * 4], [9000, 8000, 12000, 13000]));
    }
    // Panasonic RW2, Olympus ORF.
    {
        let (w, h) = (60, 20);
        put("rw2_12.rw2", &rw2(w, h, &mosaic(&scene(w, h), w, [0, 1, 1, 2], 128, 4095), 12));
        let (w, h) = (63, 21);
        put("rw2_14.rw2", &rw2(w, h, &mosaic(&scene(w, h), w, [0, 1, 1, 2], 128, 16383), 14));
        let (w, h) = (40, 30);
        put("orf.orf", &orf(w, h, &mosaic(&scene(w, h), w, [1, 0, 2, 1], 64, 4095)));
    }
    // Fujifilm RAF.
    {
        let (w, h) = (66usize, 48usize);
        let sc = scene(w, h);
        let x = mosaic_cfa(&sc, w, &XTRANS, 6, 1024, 16383);
        let mut r = RafSpec::new(w, h, x.clone(), XTRANS.to_vec());
        r.crop = Some([2, 3, 40, 60]);
        r.exposure_bias = Some((-2, 3));
        put("raf_xtrans.raf", &r.build());
        let b = mosaic_cfa(&sc, w, &[0, 1, 1, 2], 2, 1024, 16383);
        let mut r = RafSpec::new(w, h, b.clone(), vec![0, 1, 1, 2]);
        r.packing = RafPacking::U16 { le: false };
        r.orientation = 6;
        put("raf_bayer_be.raf", &r.build());
        let b12: Vec<u16> = b.iter().map(|v| v >> 2).collect();
        let mut r = RafSpec::new(w, h, b12, vec![0, 1, 1, 2]);
        r.bits = 12;
        r.packing = RafPacking::Lsb12;
        r.black = vec![256, 256, 256, 256];
        r.wb_grb = None;
        put("raf_lsb12.raf", &r.build());
        let mut r = RafSpec::new(w, h, x.clone(), XTRANS.to_vec());
        r.packing = RafPacking::Words14;
        put("raf_words14.raf", &r.build());
        let mut r = RafSpec::new(w, h, x.clone(), XTRANS.to_vec());
        r.packing = RafPacking::Compressed;
        put("raf_compressed.raf", &r.build());
        let mut r = RafSpec::new(w, h, x, XTRANS.to_vec());
        r.cfa_tiff = false;
        put("raf_nocfa.raf", &r.build());
    }
    // A lossless JPEG stream on its own (restart markers, 3 components, predictor 6).
    {
        let v: Vec<u16> = (0..30 * 12 * 3).map(|i| ((i * 37) % 4096) as u16).collect();
        put("lj_restart.ljpg", &lj92_encode(&v, 30, 12, 3, 12, 6, Some(4)));
        for p in 1..=7u8 {
            put(&format!("lj_p{p}.ljpg"), &lj92_encode(&v, 30, 12, 3, 12, p, None));
        }
    }
    // A NEF-like raw and its camera rendering for the camera-look fit (the crate's look tests).
    {
        let (w, h) = (480usize, 320usize);
        let wbv = [1.5f64, 1.0, 1.25];
        let mut seed = 12345u32;
        let mut rnd = || {
            seed = seed.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            (seed >> 8) as f32 / (1u32 << 24) as f32
        };
        let (pw, ph) = (12, 8);
        let colours: Vec<[f32; 3]> = (0..pw * ph).map(|_| [0.15 + 0.45 * rnd(), 0.15 + 0.45 * rnd(), 0.15 + 0.45 * rnd()]).collect();
        let patches: Vec<[f32; 3]> = (0..w * h)
            .map(|i| {
                let (x, y) = (i % w, i / w);
                let (px, py) = (x * pw / w, y * ph / h);
                let ramp = 0.15 + 0.85 * ((x * pw) % w) as f32 / w as f32;
                colours[py * pw + px].map(|v| v * ramp)
            })
            .collect();
        let cam: Vec<[f32; 3]> = patches.iter().map(|p| [0, 1, 2].map(|k| p[k] / wbv[k] as f32)).collect();
        let data = mosaic(&cam, w, [0, 1, 1, 2], 0, 4095);
        put("look.nef", &tiff_ep("NIKON CORPORATION", w, h, &data, [0, 1, 1, 2], 12, vec![]));
        // The true balanced camera → ProPhoto matrix: rows of a saturating twist (sum 1), applied
        // through the fallback path (camera = sRGB primaries); see look.rs tests.
        let k = [[1.35, -0.25, -0.1], [-0.15, 1.25, -0.1], [0.0, -0.3, 1.3]];
        let m = mul3(&mul3(&XYZ_D50_TO_PRO, &SRGB_TO_XYZ_D50), &k);
        let pro_to_srgb = mul3(&inv3(&SRGB_TO_XYZ_D50), &PRO_TO_XYZ);
        let f = |x: f64| 1.6 * x / (1.0 + 0.6 * x);
        let enc = |v: f64| {
            let v = v.clamp(0.0, 1.0);
            let e = if v <= 0.0031308 { 12.92 * v } else { 1.055 * v.powf(1.0 / 2.4) - 0.055 };
            (e * 255.0).round() as u8
        };
        let rgb: Vec<u8> = cam
            .iter()
            .flat_map(|p| {
                let bal = [0, 1, 2].map(|k| f64::from(p[k]) * wbv[k]);
                let pro = apply3(&m, bal).map(|v| f(v.clamp(0.0, 1.0)));
                apply3(&pro_to_srgb, pro).map(enc)
            })
            .collect();
        put("look.ref", &rgb);
        // Upright reference for the file viewed with orientation 6.
        let mut up = vec![0u8; rgb.len()];
        for y2 in 0..w {
            for x2 in 0..h {
                let (x, y) = (y2, h - 1 - x2);
                let (o, i) = ((y2 * h + x2) * 3, (y * w + x) * 3);
                up[o..o + 3].copy_from_slice(&rgb[i..i + 3]);
            }
        }
        put("look_up.ref", &up);
        let mut seed = 7u32;
        let noise: Vec<u8> = (0..rgb.len())
            .map(|_| {
                seed = seed.wrapping_mul(1_103_515_245).wrapping_add(12345);
                (seed >> 16) as u8
            })
            .collect();
        put("look_noise.ref", &noise);
        put("look_grey.ref", &vec![118u8; rgb.len()]);
    }
}

// Colour constants for the look reference (the crate's color module is private): the same
// derivations as `color::xyz_d50_to_prophoto`, `srgb_to_xyz_d50` and `rgb_to_xyz(ROMM, D50)`
// are not needed bit-exactly here, since both ports read the reference from this file.
const SRGB_TO_XYZ_D50: [[f64; 3]; 3] = [[0.4360747, 0.3850649, 0.1430804], [0.2225045, 0.7168786, 0.0606169], [0.0139322, 0.0971045, 0.7141733]];
const PRO_TO_XYZ: [[f64; 3]; 3] = [[0.7976749, 0.1351917, 0.0313534], [0.2880402, 0.7118741, 0.0000857], [0.0, 0.0, 0.8252100]];
const XYZ_D50_TO_PRO: [[f64; 3]; 3] = [[1.3459433, -0.2556075, -0.0511118], [-0.5445989, 1.5081673, 0.0205351], [0.0, 0.0, 1.2118128]];

fn mul3(a: &[[f64; 3]; 3], b: &[[f64; 3]; 3]) -> [[f64; 3]; 3] {
    std::array::from_fn(|i| std::array::from_fn(|j| (0..3).map(|k| a[i][k] * b[k][j]).sum()))
}
fn apply3(m: &[[f64; 3]; 3], v: [f64; 3]) -> [f64; 3] {
    std::array::from_fn(|i| m[i][0] * v[0] + m[i][1] * v[1] + m[i][2] * v[2])
}
fn inv3(m: &[[f64; 3]; 3]) -> [[f64; 3]; 3] {
    let [[a, b, c], [d, e, f], [g, h, i]] = *m;
    let det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g);
    [
        [(e * i - f * h) / det, (c * h - b * i) / det, (b * f - c * e) / det],
        [(f * g - d * i) / det, (a * i - c * g) / det, (c * d - a * f) / det],
        [(d * h - e * g) / det, (b * g - a * h) / det, (a * e - b * d) / det],
    ]
}
