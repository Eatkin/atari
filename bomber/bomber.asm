	processor 6502
	
	include "macro.h"
	include "vcs.h"

	seg.u Variables
	org $80

JetXPos		byte
JetYPos		byte
BomberXPos	byte
BomberYPos	byte

	seg Code
	org $F000

Reset:
	CLEAN_START

; Initialise RAM variables and TIA registers
	; Set initial player coordinates
	lda #10
	sta JetYPos
	lda #60
	sta JetXPos

; Begin rendering
StartFrame:
	; Turn on vblank and vsync
	ldx #2
	stx VBLANK
	stx VSYNC
	REPEAT 3
		stx WSYNC 
	REPEND
	; Turn off vsync
	ldx #0
	stx VSYNC

	; Render VBLANK
	REPEAT 37
		stx WSYNC
	REPEND
	; Turn off VBLANK
	stx VBLANK

; Render the visible scanlines
VisibleScanlines:
	; Set background colour
	lda #$84	; Blue
	sta COLUBK
	; Set playfield colour
	lda #$C2
	sta COLUPF
	
	; Setup playfield
	lda #%11110000	; PF0 uses first 4 bits and is reversed
	sta PF0
	lda #%11000000
	sta PF1
	lda #0
	sta PF2

	; Playfield reflection
	lda #%00000001
	sta CTRLPF

	ldx #192
.GameLineLoop:
	; Render the 192 visible scanline
	stx WSYNC
	dex
	bne .GameLineLoop	

; Display vblank
Overscan:
	lda #2
	sta VBLANK
	REPEAT 30
		sta WSYNC
	REPEND

	lda #0
	sta VBLANK

	; Loop forever
	jmp StartFrame

; Complete ROM with 4kb
	org $FFFC
	word Reset
	word Reset
