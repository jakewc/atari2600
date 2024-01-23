    processor 6502

    include "vcs.h"
    include "macro.h"

    seg code
    org $F000

Start:
    CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start a new frame by turning on VBlank/VSync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NextFrame:
    lda #2  ;same as binary value 00000010
    sta VBLANK  ; turn on vblank
    sta VSYNC   ; turn on vsync

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Generate the three lines of vsync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    sta WSYNC       ;first scanline
    sta WSYNC       ;second scanline
    sta WSYNC       ;third scanline

    lda #0
    sta VSYNC ; turn off vsync

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let TIA output the 37 lines of the VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #37 ; X = 37
LoopVBlank:
    sta WSYNC      ; wait for next scanline
    dex             ; X--
    bne LoopVBlank  ; loop while X != 0

    lda #0
    sta VBLANK      ; turn off vblank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #192    ;counter for visible scanlines
LoopScanline:

    stx COLUBK  ; set background to X
    sta WSYNC   ; wait for next scanline
    dex
    bne LoopScanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 overscanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #2  ; turn on VBLANK again
    sta VBLANK

    ldx #30     ;counter for 30 scanlines
LoopOverscan:
    sta WSYNC
    dex
    bne LoopOverscan

    jmp NextFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COmplete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    org $FFFC
    .word Start
    .word Start
