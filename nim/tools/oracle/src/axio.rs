//! Raw buffer files shared by the algo oracle commands and `nim/tools/algocheck.nim`:
//! `PCIM`, then u32 w, h, ch, kind (0 = f32, 1 = u8), then the samples, all little-endian.
#![allow(dead_code)]

use photocraft_geom::Rect;
use photocraft_raster::Surface;

pub struct Buf {
    pub w: usize,
    pub h: usize,
    pub ch: usize,
    pub f: Vec<f32>,
    pub b: Vec<u8>,
}

pub fn read(path: &str) -> Buf {
    let d = std::fs::read(path).unwrap();
    assert_eq!(&d[..4], b"PCIM");
    let u = |o: usize| u32::from_le_bytes(d[o..o + 4].try_into().unwrap()) as usize;
    let (w, h, ch, kind) = (u(4), u(8), u(12), u(16));
    let body = &d[20..];
    if kind == 0 {
        Buf { w, h, ch, f: body.chunks_exact(4).map(|c| f32::from_le_bytes(c.try_into().unwrap())).collect(), b: Vec::new() }
    } else {
        Buf { w, h, ch, f: Vec::new(), b: body.to_vec() }
    }
}

fn header(w: usize, h: usize, ch: usize, kind: u32) -> Vec<u8> {
    let mut o = b"PCIM".to_vec();
    for v in [w as u32, h as u32, ch as u32, kind] {
        o.extend_from_slice(&v.to_le_bytes());
    }
    o
}

pub fn write_f32(path: &str, w: usize, h: usize, ch: usize, v: &[f32]) {
    let mut o = header(w, h, ch, 0);
    for x in v {
        o.extend_from_slice(&x.to_le_bytes());
    }
    std::fs::write(path, o).unwrap();
}

pub fn write_u8(path: &str, w: usize, h: usize, ch: usize, v: &[u8]) {
    let mut o = header(w, h, ch, 1);
    o.extend_from_slice(v);
    std::fs::write(path, o).unwrap();
}

pub fn rgba(b: &Buf) -> Vec<[f32; 4]> {
    b.f.chunks_exact(4).map(|c| [c[0], c[1], c[2], c[3]]).collect()
}

pub fn rgba8(b: &Buf) -> Vec<[u8; 4]> {
    b.b.chunks_exact(4).map(|c| [c[0], c[1], c[2], c[3]]).collect()
}

pub fn flat(v: &[[f32; 4]]) -> Vec<f32> {
    v.iter().flatten().copied().collect()
}

/// Writes a surface's samples over `area` and prints its tile count and content bounds.
pub fn dump_surface(path: &str, s: &Surface, area: Rect) {
    let v = s.read_region(area);
    write_f32(path, area.width() as usize, area.height() as usize, s.channels(), &v);
    let b = s.content_bounds();
    println!("tiles {} bounds {} {} {} {}", s.tile_count(), b.x0, b.y0, b.x1, b.y1);
}

pub fn f(a: &str) -> f32 {
    a.parse().unwrap()
}
pub fn i(a: &str) -> i32 {
    a.parse().unwrap()
}
pub fn bl(a: &str) -> bool {
    a == "1" || a == "true"
}
pub fn rect(a: &str) -> Rect {
    let v: Vec<i32> = a.split(',').map(|x| x.parse().unwrap()).collect();
    Rect::new(v[0], v[1], v[2], v[3])
}
pub fn floats(a: &str) -> Vec<f32> {
    if a.is_empty() { Vec::new() } else { a.split(',').map(|x| x.parse().unwrap()).collect() }
}
