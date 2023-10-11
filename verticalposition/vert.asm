	processor 6502

	include "macro.h"
	include "vcs.h"

	seg.u Variables
	org $80
P0Height	byte
PlayerYPos	byte

	seg Code
	org $F000

Reset:
	CLEAN_START
	
	ldx #$00
	stx COLUBK
	
	lda #180
	sta PlayerYPos

	lda #9
	sta P0Height

StartFrame:
	; Turn on vsync and vblank
	lda #2
	sta VBLANK
	sta VSYNC

	; 3 lines of vsync
	REPEAT 3
		sta WSYNC
	REPEND

	; Turn off vsync
	lda #0
	sta VSYNC

	; 37 lines of vblank
	REPEAT 37
		sta WSYNC 
	REPEND

	lda #0
	sta VBLANK

	; Draw 192 visible scanlines
	ldx #192

Scanline:
	txa
	sec		; Set carry for subtraction
	sbc PlayerYPos
	cmp P0Height	; Are we inside player boundary?
	bcc LoadBitmap	; if result < spriteheight then run subroutine
	lda #0		; else set a register to 0

LoadBitmap:
	tay
	lda P0Bitmap,Y

	sta WSYNC
	sta GRP0	; Set graphics for player 0 slice
	lda P0Colour,Y
	sta COLUP0
	
	dex
	bne Scanline

Overscan:
	lda #2
	sta VBLANK
	REPEAT 30
		sta WSYNC
	REPEND

	dec PlayerYPos

	jmp StartFrame

P0Bitmap:
	byte #%00000000
	byte #%00111100
	byte #%01000010
	byte #%10011001
	byte #%10100101
	byte #%10100101
	byte #%10011001
	byte #%01000010
	byte #%00111100

P0Colour:
	byte #$00
	byte #$C8
	byte #$84
	byte #$0F
	byte #$1F
	byte #$16
	byte #$60
	byte #$AA
	byte #$53

	org $FFFC
	word Reset
	word Reset
