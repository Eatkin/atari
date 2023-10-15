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
	.byte #%00000000
        .byte #%11111111;$16
        .byte #%11100111;$16
        .byte #%11100111;$18
        .byte #%01111110;$18
        .byte #%01011010;$1A
        .byte #%00111100;$1A
        .byte #%00111100;$1C
        .byte #%00011000;$1C

P0Colour:
	.byte #$00
        .byte #$16;
        .byte #$16;
        .byte #$18;
        .byte #$18;
        .byte #$1A;
        .byte #$1A;
        .byte #$1C;
        .byte #$1C;

	org $FFFC
	word Reset
	word Reset
