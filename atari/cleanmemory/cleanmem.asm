    processor 6502

    seg code
    org $F000 ; define the code origin at $F000

Start:
    sei ;disable interrupts
    cld ;disable BCD decimal math mode
    ldx #$FF ; load X register with literal $FF
    txs ; transfer X register to S (stack register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Clear Zero Page memory region ($00 to $FF)
;Meaning the entire TIA register space and also RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #0 ; A=0
    ldx #$FF ; X = literal $FF
    sta $FF ; store A at $FF as we decrement before storing in the loop
MemLoop:
    dex ; X--
    sta $0,X ; store A register at address $0 + X
    bne MemLoop ; loop until X = 0 (z-flag set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Fill ROM size to 4Kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start ; reset vector at $FFFC (where program starts)
    .word Start ; interrupt vector $FFFE (unused in VCS)
