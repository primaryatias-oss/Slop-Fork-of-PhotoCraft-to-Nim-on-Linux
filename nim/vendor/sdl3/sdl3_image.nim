# SDL3_image bindings for Nim.
# Based on SDL_image 3.4.6
# Requires SDL_image 3.4.0+ (libSDL3_image.so / SDL3_image.dll / SDL3_image.dylib)

import sdl3

when defined(windows):
  const ImageLibName* = "SDL3_image.dll"
elif defined(macosx):
  const ImageLibName* = "libSDL3_image.dylib"
else:
  const ImageLibName* = "libSDL3_image.so"

{.push callConv: cdecl, dynlib: ImageLibName.}


type
  Animation* = ptr object
    w*: cint
    h*: cint
    count*: cint
    frames*: ptr ptr Surface
    delays*: ptr cint
  AnimationEncoder* = ptr object
  AnimationDecoder* = ptr object
  AnimationDecoderStatus* {.size: sizeof(cint).} = enum
    imgDecoderStatusInvalid = -1,
    imgDecoderStatusOK,
    imgDecoderStatusFailed,
    imgDecoderStatusComplete


const PROP_ANIMATION_ENCODER_CREATE_FILENAME_STRING* = "SDL_image.animation_encoder.create.filename"
const PROP_ANIMATION_ENCODER_CREATE_IOSTREAM_POINTER* = "SDL_image.animation_encoder.create.iostream"
const PROP_ANIMATION_ENCODER_CREATE_IOSTREAM_AUTOCLOSE_BOOLEAN* = "SDL_image.animation_encoder.create.iostream.autoclose"
const PROP_ANIMATION_ENCODER_CREATE_TYPE_STRING* = "SDL_image.animation_encoder.create.type"
const PROP_ANIMATION_ENCODER_CREATE_QUALITY_NUMBER* = "SDL_image.animation_encoder.create.quality"
const PROP_ANIMATION_ENCODER_CREATE_TIMEBASE_NUMERATOR_NUMBER* = "SDL_image.animation_encoder.create.timebase.numerator"
const PROP_ANIMATION_ENCODER_CREATE_TIMEBASE_DENOMINATOR_NUMBER* = "SDL_image.animation_encoder.create.timebase.denominator"
const PROP_ANIMATION_ENCODER_CREATE_AVIF_MAX_THREADS_NUMBER* = "SDL_image.animation_encoder.create.avif.max_threads"
const PROP_ANIMATION_ENCODER_CREATE_AVIF_KEYFRAME_INTERVAL_NUMBER* = "SDL_image.animation_encoder.create.avif.keyframe_interval"
const PROP_ANIMATION_ENCODER_CREATE_GIF_USE_LUT_BOOLEAN* = "SDL_image.animation_encoder.create.gif.use_lut"

const PROP_ANIMATION_DECODER_CREATE_FILENAME_STRING* = "SDL_image.animation_decoder.create.filename"
const PROP_ANIMATION_DECODER_CREATE_IOSTREAM_POINTER* = "SDL_image.animation_decoder.create.iostream"
const PROP_ANIMATION_DECODER_CREATE_IOSTREAM_AUTOCLOSE_BOOLEAN* = "SDL_image.animation_decoder.create.iostream.autoclose"
const PROP_ANIMATION_DECODER_CREATE_TYPE_STRING* = "SDL_image.animation_decoder.create.type"
const PROP_ANIMATION_DECODER_CREATE_TIMEBASE_NUMERATOR_NUMBER* = "SDL_image.animation_decoder.create.timebase.numerator"
const PROP_ANIMATION_DECODER_CREATE_TIMEBASE_DENOMINATOR_NUMBER* = "SDL_image.animation_decoder.create.timebase.denominator"
const PROP_ANIMATION_DECODER_CREATE_AVIF_MAX_THREADS_NUMBER* = "SDL_image.animation_decoder.create.avif.max_threads"
const PROP_ANIMATION_DECODER_CREATE_AVIF_ALLOW_INCREMENTAL_BOOLEAN* = "SDL_image.animation_decoder.create.avif.allow_incremental"
const PROP_ANIMATION_DECODER_CREATE_AVIF_ALLOW_PROGRESSIVE_BOOLEAN* = "SDL_image.animation_decoder.create.avif.allow_progressive"
const PROP_ANIMATION_DECODER_CREATE_GIF_TRANSPARENT_COLOR_INDEX_NUMBER* = "SDL_image.animation_encoder.create.gif.transparent_color_index"
const PROP_ANIMATION_DECODER_CREATE_GIF_NUM_COLORS_NUMBER* = "SDL_image.animation_encoder.create.gif.num_colors"

const PROP_METADATA_IGNORE_PROPS_BOOLEAN* = "SDL_image.metadata.ignore_props"
const PROP_METADATA_DESCRIPTION_STRING* = "SDL_image.metadata.description"
const PROP_METADATA_COPYRIGHT_STRING* = "SDL_image.metadata.copyright"
const PROP_METADATA_TITLE_STRING* = "SDL_image.metadata.title"
const PROP_METADATA_AUTHOR_STRING* = "SDL_image.metadata.author"
const PROP_METADATA_CREATION_TIME_STRING* = "SDL_image.metadata.creation_time"
const PROP_METADATA_FRAME_COUNT_NUMBER* = "SDL_image.metadata.frame_count"
const PROP_METADATA_LOOP_COUNT_NUMBER* = "SDL_image.metadata.loop_count"


proc version*(): cint {.importc: "IMG_Version".}
proc load*(file: cstring): ptr Surface {.importc: "IMG_Load".}
proc loadIO*(src: IOStream, closeio: bool): ptr Surface {.importc: "IMG_Load_IO".}
proc loadTypedIO*(src: IOStream, closeio: bool, format: cstring): ptr Surface {.importc: "IMG_LoadTyped_IO".}

proc loadTexture*(renderer: Renderer, file: cstring): Texture {.importc: "IMG_LoadTexture".}
proc loadTextureIO*(renderer: Renderer, src: IOStream, closeio: bool): Texture {.importc: "IMG_LoadTexture_IO".}
proc loadTextureTypedIO*(renderer: Renderer, src: IOStream, closeio: bool, format: cstring): Texture {.importc: "IMG_LoadTextureTyped_IO".}

proc loadGPUTexture*(device: GPUDevice, copyPass: GPUCopyPass, file: cstring; width, height: ptr cint): GPUTexture {.importc: "IMG_LoadGPUTexture".}
proc loadGPUTextureIO*(device: GPUDevice, copyPass: GPUCopyPass, src: IOStream, closeio: bool; width, height: ptr cint): GPUTexture {.importc: "IMG_LoadGPUTexture_IO".}
proc loadGPUTextureTypedIO*(device: GPUDevice, copyPass: GPUCopyPass, src: IOStream, closeio: bool, format: cstring; width, height: ptr cint): GPUTexture {.importc: "IMG_LoadGPUTextureTyped_IO".}

proc getClipboardImage*(): ptr Surface {.importc: "IMG_GetClipboardImage".}

proc isANI*(src: IOStream): bool {.importc: "IMG_isANI".}
proc isAVIF*(src: IOStream): bool {.importc: "IMG_isAVIF".}
proc isCUR*(src: IOStream): bool {.importc: "IMG_isCUR".}
proc isBMP*(src: IOStream): bool {.importc: "IMG_isBMP".}
proc isGIF*(src: IOStream): bool {.importc: "IMG_isGIF".}
proc isICO*(src: IOStream): bool {.importc: "IMG_isICO".}
proc isJPG*(src: IOStream): bool {.importc: "IMG_isJPG".}
proc isJXL*(src: IOStream): bool {.importc: "IMG_isJXL".}
proc isLBM*(src: IOStream): bool {.importc: "IMG_isLBM".}
proc isPCX*(src: IOStream): bool {.importc: "IMG_isPCX".}
proc isPNG*(src: IOStream): bool {.importc: "IMG_isPNG".}
proc isPNM*(src: IOStream): bool {.importc: "IMG_isPNM".}
proc isQOI*(src: IOStream): bool {.importc: "IMG_isQOI".}
proc isSVG*(src: IOStream): bool {.importc: "IMG_isSVG".}
proc isTIF*(src: IOStream): bool {.importc: "IMG_isTIF".}
proc isWEBP*(src: IOStream): bool {.importc: "IMG_isWEBP".}
proc isXCF*(src: IOStream): bool {.importc: "IMG_isXCF".}
proc isXPM*(src: IOStream): bool {.importc: "IMG_isXPM".}
proc isXV*(src: IOStream): bool {.importc: "IMG_isXV".}

proc loadAVIF_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadAVIF_IO".}
proc loadBMP_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadBMP_IO".}
proc loadCUR_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadCUR_IO".}
proc loadGIF_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadGIF_IO".}
proc loadICO_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadICO_IO".}
proc loadJPG_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadJPG_IO".}
proc loadJXL_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadJXL_IO".}
proc loadLBM_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadLBM_IO".}
proc loadPCX_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadPCX_IO".}
proc loadPNG_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadPNG_IO".}
proc loadPNM_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadPNM_IO".}
proc loadSVG_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadSVG_IO".}
proc loadSizedSVG_IO*(src: IOStream; width, height: cint): ptr Surface {.importc: "IMG_LoadSizedSVG_IO".}
proc loadQOI_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadQOI_IO".}
proc loadTGA_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadTGA_IO".}
proc loadTIF_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadTIF_IO".}
proc loadWEBP_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadWEBP_IO".}
proc loadXCF_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadXCF_IO".}
proc loadXPM_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadXPM_IO".}
proc loadXV_IO*(src: IOStream): ptr Surface {.importc: "IMG_LoadXV_IO".}

proc readXPMFromArray*(xpm: ptr cstring): ptr Surface {.importc: "IMG_ReadXPMFromArray".}
proc readXPMFromArrayToRGB888*(xpm: ptr cstring): ptr Surface {.importc: "IMG_ReadXPMFromArrayToRGB888".}

proc save*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_Save".}
proc saveTypedIO*(surface: ptr Surface, dst: IOStream, closeio: bool, format: cstring): bool {.importc: "IMG_SaveTyped_IO".}
proc saveAVIF*(surface: ptr Surface, file: cstring, quality: cint): bool {.importc: "IMG_SaveAVIF".}
proc saveAVIF_IO*(surface: ptr Surface, dst: IOStream, closeio: bool, quality: cint): bool {.importc: "IMG_SaveAVIF_IO".}
proc saveBMP*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_SaveBMP".}
proc saveBMP_IO*(surface: ptr Surface, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveBMP_IO".}
proc saveCUR*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_SaveCUR".}
proc saveCUR_IO*(surface: ptr Surface, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveCUR_IO".}
proc saveGIF*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_SaveGIF".}
proc saveGIF_IO*(surface: ptr Surface, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveGIF_IO".}
proc saveICO*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_SaveICO".}
proc saveICO_IO*(surface: ptr Surface, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveICO_IO".}
proc saveJPG*(surface: ptr Surface, file: cstring, quality: cint): bool {.importc: "IMG_SaveJPG".}
proc saveJPG_IO*(surface: ptr Surface, dst: IOStream, closeio: bool, quality: cint): bool {.importc: "IMG_SaveJPG_IO".}
proc savePNG*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_SavePNG".}
proc savePNG_IO*(surface: ptr Surface, dst: IOStream, closeio: bool): bool {.importc: "IMG_SavePNG_IO".}
proc saveTGA*(surface: ptr Surface, file: cstring): bool {.importc: "IMG_SaveTGA".}
proc saveTGA_IO*(surface: ptr Surface, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveTGA_IO".}
proc saveWEBP*(surface: ptr Surface, file: cstring, quality: cfloat): bool {.importc: "IMG_SaveWEBP".}
proc saveWEBP_IO*(surface: ptr Surface, dst: IOStream, closeio: bool, quality: cfloat): bool {.importc: "IMG_SaveWEBP_IO".}

proc loadAnimation*(file: cstring): Animation {.importc: "IMG_LoadAnimation".}
proc loadAnimationIO*(src: IOStream, closeio: bool): Animation {.importc: "IMG_LoadAnimation_IO".}
proc loadAnimationTypedIO*(src: IOStream, closeio: bool, format: cstring): Animation {.importc: "IMG_LoadAnimationTyped_IO".}
proc loadANIAnimationIO*(src: IOStream): Animation {.importc: "IMG_LoadANIAnimation_IO".}
proc loadAPNGAnimationIO*(src: IOStream): Animation {.importc: "IMG_LoadAPNGAnimation_IO".}
proc loadAVIFAnimationIO*(src: IOStream): Animation {.importc: "IMG_LoadAVIFAnimation_IO".}
proc loadGIFAnimationIO*(src: IOStream): Animation {.importc: "IMG_LoadGIFAnimation_IO".}
proc loadWEBPAnimationIO*(src: IOStream): Animation {.importc: "IMG_LoadWEBPAnimation_IO".}

proc saveAnimation*(anim: Animation, file: cstring): bool {.importc: "IMG_SaveAnimation".}
proc saveAnimationTypedIO*(anim: Animation, dst: IOStream, closeio: bool, format: cstring): bool {.importc: "IMG_SaveAnimationTyped_IO".}
proc saveANIAnimationIO*(anim: Animation, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveANIAnimation_IO".}
proc saveAPNGAnimationIO*(anim: Animation, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveAPNGAnimation_IO".}
proc saveAVIFAnimationIO*(anim: Animation, dst: IOStream, closeio: bool, quality: cint): bool {.importc: "IMG_SaveAVIFAnimation_IO".}
proc saveGIFAnimationIO*(anim: Animation, dst: IOStream, closeio: bool): bool {.importc: "IMG_SaveGIFAnimation_IO".}
proc saveWEBPAnimationIO*(anim: Animation, dst: IOStream, closeio: bool, quality: cint): bool {.importc: "IMG_SaveWEBPAnimation_IO".}

proc createAnimatedCursor*(anim: Animation; hotX, hotY: cint): Cursor {.importc: "IMG_CreateAnimatedCursor".}

proc freeAnimation*(anim: Animation): void {.importc: "IMG_FreeAnimation".}

proc createAnimationEncoder*(file: cstring): AnimationEncoder {.importc: "IMG_CreateAnimationEncoder".}
proc createAnimationEncoderIO*(dst: IOStream, closeio: bool, format: cstring): AnimationEncoder {.importc: "IMG_CreateAnimationEncoder_IO".}
proc createAnimationEncoderWithProperties*(props: PropertiesID): AnimationEncoder {.importc: "IMG_CreateAnimationEncoderWithProperties".}
proc addAnimationEncoderFrame*(encoder: AnimationEncoder, surface: ptr Surface, duration: uint64): bool {.importc: "IMG_AddAnimationEncoderFrame".}
proc closeAnimationEncoder*(encoder: AnimationEncoder): bool {.importc: "IMG_CloseAnimationEncoder".}

proc createAnimationDecoder*(file: cstring): AnimationDecoder {.importc: "IMG_CreateAnimationDecoder".}
proc createAnimationDecoderIO*(src: IOStream, closeio: bool, format: cstring): AnimationDecoder {.importc: "IMG_CreateAnimationDecoder_IO".}
proc createAnimationDecoderWithProperties*(props: PropertiesID): AnimationDecoder {.importc: "IMG_CreateAnimationDecoderWithProperties".}
proc getAnimationDecoderProperties*(decoder: AnimationDecoder): PropertiesID {.importc: "IMG_GetAnimationDecoderProperties".}
proc getAnimationDecoderFrame*(decoder: AnimationDecoder, frame: ptr ptr Surface, duration: ptr uint64): bool {.importc: "IMG_GetAnimationDecoderFrame".}
proc getAnimationDecoderStatus*(decoder: AnimationDecoder): AnimationDecoderStatus {.importc: "IMG_GetAnimationDecoderStatus".}
proc resetAnimationDecoder*(decoder: AnimationDecoder): bool {.importc: "IMG_ResetAnimationDecoder".}
proc closeAnimationDecoder*(decoder: AnimationDecoder): bool {.importc: "IMG_CloseAnimationDecoder".}

{.pop.}
