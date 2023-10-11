	processor 6502
	include "macro.h"
	include "vcs.h"

	seg.u Variables
	org $80
P0XPos	byte		; sprite x coordinate

	seg Code
	org $F000

Reset:
	CLEAN_START

	ldx #$00	; Load black for the background
	stx COLUBK

	lda #40
	sta P0XPos	; Set player x-pos at 40

StartFrame:
	lda #2
	sta VBLANK
	sta VSYNC

	; 3 lines of vsync
	REPEAT 3
		sta WSYNC
	REPEND

	lda #0
	sta VSYNC	; turn off vsync

	lda P0XPos
	and #%01111111	; Forces bit 7 to 0 meaning A is always positive

	sec

	sta WSYNC
	sta HMCLR	; clear old horizontal position values

DivideLoop:
	sbc #15
	bcs DivideLoop	; loop while carry is set

	eor #7		; exclusive OR with accumulator
			; A will be between -8 and 7
	REPEAT 4
		asl	; bitshift a, HMP0 uses only 4 bits
	REPEND

	sta HMP0	; set fine position value
	sta RESP0	; set coarse position value
	sta WSYNC	; wait for next scanline
	sta HMOVE	; apply fine position offset

	; Output the 37 (-2) lines of vblank
	REPEAT 35
		sta WSYNC
	REPEND

	lda #0
	sta VBLANK	; turn off VBLANK

	; Draw 192 visible scanline
	REPEAT 60
		sta WSYNC 	; 60 empty scanlines
	REPEND

	ldy #8		; Counter for the bitmap drawing
DrawBitmap:
	lda P0Bitmap,Y
	sta GRP0	; set graphics for P0 slice

	lda P0Color,Y
	sta COLUP0


	sta WSYNC

	dey
	bne DrawBitmap

	lda #0
	sta GRP0	; disable P0 bitmap graphics

	REPEAT 124
		sta WSYNC	; remaining 124 scalines
	REPEND

Overscan:
	lda #2
	sta VBLANK
	REPEAT 30
		sta WSYNC
	REPEND

CheckP0Up:
	lda #%00010000
	bit SWCHA
	bne CheckP0Down
	inc P0XPos
CheckP0Down:
	lda #%00100000
	bit SWCHA
	bne CheckP0Left
	dec P0XPos
CheckP0Left:
	lda #%01000000
	bit SWCHA
	bne CheckP0Right
	dec P0XPos
CheckP0Right:
	lda #%10000000
	bit SWCHA
	bne NoInput
	inc P0XPos
NoInput:
	; Do nothing

	jmp StartFrame


P0Bitmap:
	byte #%00111100
	byte #%01000010
	byte #%10011001
	byte #%10100101
	byte #%10100101
	byte #%10100101
	byte #%01000010
	byte #%00111100

P0Color:
	byte #$00
	byte #$02
	byte #$02
	byte #$52
	byte #$52
	byte #$52
	byte #$52
	byte #$52

	; Complete ROM
	org $FFFC
	word Reset
	word Reset
