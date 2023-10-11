	processor 6502
	include "vcs.h"
	include "macro.h"
	ORG $F000

Reset:
	CLEAN_START
	
	ldx #$80	; Load blue
	stx COLUBK

	ldx #$1C	; load yellow
	stx COLUPF

StartFrame:
	lda #02
	sta VBLANK	; Turn on VBLANK
	sta VSYNC	; Turn on VSYNC

	REPEAT 3
		sta WSYNC
	REPEND
	
	lda #00
	sta VSYNC	; turn vsync off

	REPEAT 37
		sta WSYNC	; 37 lines of VBLANK
	REPEND

	lda #00
	sta VBLANK	; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up Playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldx #%00000001
	stx CTRLPF	; load 1 into CTRLPF to turn on playfield reflection

	; Skip the 7 scanlines with no PF set
	ldx #0
	stx PF0
	stx PF1
	stx PF2
	REPEAT 7
		sta WSYNC
	REPEND
	
	; Playfield top
	ldx #%11100000
	stx PF0
	ldx #%11111111
	stx PF1
	stx PF2

	; Draw the playfield
	REPEAT 7
		sta WSYNC
	REPEND

	; Playfield sides
	ldx #%01100000
	stx PF0
	ldx #0
	stx PF1
	ldx #%10000000
	stx PF2
	
	; Draw the next 164 lines
	REPEAT 164
		sta WSYNC
	REPEND

	
	; Playfield bottom
	ldx #%11100000
	stx PF0
	ldx #%11111111
	stx PF1
	stx PF2

	; Draw the playfield
	REPEAT 7
		sta WSYNC
	REPEND

	; Last 7 scanlines with no PF set
	ldx #0
	stx PF0
	stx PF1
	stx PF2
	
	REPEAT 7
		sta WSYNC
	REPEND

	; overscan region
	lda #2
	sta VBLANK
	
	REPEAT 30
		sta WSYNC
	REPEND
	
	jmp StartFrame

	; Complete ROM size
	org $FFFC
	.word Reset
	.word Reset
