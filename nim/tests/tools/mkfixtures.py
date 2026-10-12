"""Generates codec test fixtures with Pillow (test-time oracle only) into the given directory.

Each fixture gets a .raw sidecar: the pixels Pillow decodes, as 8-bit interleaved samples with a
small header "w h channels\n".
"""
import io, os, sys
from PIL import Image

out = sys.argv[1]
os.makedirs(out, exist_ok=True)

def base(w=67, h=45):
    im = Image.new("RGB", (w, h))
    px = im.load()
    for y in range(h):
        for x in range(w):
            px[x, y] = ((x * 7 + y * 3) % 256, (x * 2 + y * 11) % 256, (x * y) % 256)
    return im

def save(name, im, fmt, **kw):
    p = os.path.join(out, name)
    im.save(p, fmt, **kw)
    ref = Image.open(p)
    ref.load()
    if ref.mode == "P":
        ref = ref.convert("RGBA" if "transparency" in ref.info else "RGB")
    if ref.mode == "CMYK":
        data = ref.tobytes()
        ch = 4
    else:
        mode = {"L": "L", "LA": "LA", "RGB": "RGB", "RGBA": "RGBA", "I;16": "L", "1": "L"}.get(ref.mode, "RGB")
        ref = ref.convert(mode)
        data = ref.tobytes()
        ch = len(mode)
    with open(p + ".raw", "wb") as f:
        f.write(f"{ref.width} {ref.height} {ch}\n".encode())
        f.write(data)

im = base()
save("rgb.png", im, "PNG")
save("rgba.png", im.convert("RGBA").putalpha(128) or im.convert("RGBA"), "PNG")
a = im.convert("RGBA"); a.putalpha(Image.linear_gradient("L").resize(im.size)); save("rgba_grad.png", a, "PNG")
save("gray.png", im.convert("L"), "PNG")
la = im.convert("LA"); save("la.png", la, "PNG")
save("pal.png", im.convert("P", palette=Image.ADAPTIVE, colors=37), "PNG")
p4 = im.convert("P", palette=Image.ADAPTIVE, colors=13); save("pal4.png", p4, "PNG", bits=4)
save("bw.png", im.convert("1"), "PNG")
save("interlaced.png", im, "PNG", interlace=1)
save("rgb.jpg", im, "JPEG", quality=92)
save("rgb444.jpg", im, "JPEG", quality=95, subsampling=0)
save("rgb422.jpg", im, "JPEG", quality=90, subsampling=1)
save("prog.jpg", im, "JPEG", quality=90, progressive=True)
save("gray.jpg", im.convert("L"), "JPEG", quality=90)
save("cmyk.jpg", im.convert("CMYK"), "JPEG", quality=95)
save("restart.jpg", im, "JPEG", quality=90, restart_marker_blocks=2)
big = base(300, 200)
save("big_prog.jpg", big, "JPEG", quality=80, progressive=True)
save("rgb.bmp", im, "BMP")
save("rgb.gif", im.convert("P", palette=Image.ADAPTIVE, colors=64), "GIF")
save("rgb.tga", im, "TGA")
save("rgba.tga", im.convert("RGBA"), "TGA", compression="tga_rle")
save("rgb.ppm", im, "PPM")
save("gray.pgm", im.convert("L"), "PPM")
save("rgb.qoi", im, "QOI") if "QOI" in Image.SAVE else None
save("rgb.tif", im, "TIFF")
save("rgb_lzw.tif", im, "TIFF", compression="tiff_lzw")
save("rgb_zip.tif", im, "TIFF", compression="tiff_adobe_deflate")
save("rgba_pack.tif", im.convert("RGBA"), "TIFF", compression="packbits")
save("gray.tif", im.convert("L"), "TIFF")
save("cmyk.tif", im.convert("CMYK"), "TIFF")
save("rgb.webp", im, "WEBP", lossless=True)
save("rgba_lossy.webp", im.convert("RGBA"), "WEBP", quality=90)
save("rgb.ico", im.resize((32, 32)).convert("RGBA"), "ICO")

try:
    import OpenEXR, numpy as np
except ImportError:
    OpenEXR = None
if OpenEXR is not None:
    # Float fixtures: values outside [0, 1], a smooth ramp plus noise so every compressor has work.
    rng = np.random.default_rng(7)
    h, w = 37, 53
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    rgba = np.stack([xx / w * 4 - 1, yy / h * 2, np.sin(xx * 0.3) * 3, np.ones_like(xx) * 0.5], -1)
    rgba[5:9, 5:40] += rng.normal(0, 1, (4, 35, 4)).astype(np.float32)
    rgba = rgba.astype(np.float32)
    comps = {"none": OpenEXR.NO_COMPRESSION, "rle": OpenEXR.RLE_COMPRESSION, "zips": OpenEXR.ZIPS_COMPRESSION,
             "zip": OpenEXR.ZIP_COMPRESSION, "piz": OpenEXR.PIZ_COMPRESSION, "pxr24": OpenEXR.PXR24_COMPRESSION}
    def exr(name, arr, comp, tiled=False):
        hdr = {"compression": comp, "type": OpenEXR.tiledimage if tiled else OpenEXR.scanlineimage}
        if tiled:
            td = OpenEXR.TileDescription()
            td.xSize, td.ySize = 16, 16
            td.mode, td.roundingMode = OpenEXR.ONE_LEVEL, OpenEXR.ROUND_DOWN
            hdr["tiles"] = td
        with OpenEXR.File(hdr, {"RGBA": arr}) as f:
            f.write(os.path.join(out, name))
        back = OpenEXR.File(os.path.join(out, name)).channels()["RGBA"].pixels.astype(np.float32)
        with open(os.path.join(out, name + ".f32"), "wb") as f:
            f.write(b"%d %d %d\n" % (back.shape[1], back.shape[0], back.shape[2]))
            f.write(back.tobytes())
    for k, c in comps.items():
        exr("half_%s.exr" % k, rgba.astype(np.float16), c)
        exr("float_%s.exr" % k, rgba, c)
    exr("tiled_zip.exr", rgba.astype(np.float16), OpenEXR.ZIP_COMPRESSION, tiled=True)
    exr("tiled_piz.exr", rgba, OpenEXR.PIZ_COMPRESSION, tiled=True)
    gray = rgba[..., 0].copy()
    with OpenEXR.File({"compression": OpenEXR.ZIP_COMPRESSION, "type": OpenEXR.scanlineimage}, {"Y": gray.astype(np.float16)}) as f:
        f.write(os.path.join(out, "gray_zip.exr"))
    with open(os.path.join(out, "gray_zip.exr.f32"), "wb") as f:
        f.write(b"%d %d 1\n" % (w, h))
        f.write(gray.astype(np.float16).astype(np.float32).tobytes())
    P0 = OpenEXR.Part({"compression": OpenEXR.ZIP_COMPRESSION, "type": OpenEXR.scanlineimage}, {"Z": gray}, name="depth")
    P1 = OpenEXR.Part({"compression": OpenEXR.PIZ_COMPRESSION, "type": OpenEXR.scanlineimage}, {"RGBA": rgba.astype(np.float16)}, name="beauty")
    with OpenEXR.File([P0, P1]) as f:
        f.write(os.path.join(out, "multipart.exr"))
print("ok")
