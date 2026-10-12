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
print("ok")
