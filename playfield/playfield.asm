	processor 6502
	
	include "macro.h"
	include "vcs.h"

	seg
	org $F000

Reset:
	CLEAN_START
	
	LDX #$80	; load blue
	STX COLUBK

	LDA #$1C	; load yellow
	STA COLUPF

StartFrame:
	LDA #02
	STA VBLANK
	STA VSYNC

	REPEAT 3
		STA WSYNC	; 3 scanlines for vsync
	REPEND

	LDA #0
	STA VSYNC

	REPEAT 37
		STA WSYNC
	REPEND
	LDA #0
	STA VBLANK

	LDX #%0000001
	STX CTRLPF		; Reflect playfield

	; Skip 7 lines of PF
	LDX #0
	STX PF0
	STX PF1
	STX PF2
	REPEAT 7
		STA WSYNC
	REPEND

	; Playfield top
	LDX #%11100000
	STX PF0
	LDX #%11111111
	STX PF1
	STX PF2
	REPEAT 7
		STA WSYNC
	REPEND

	; Playfield sides
	LDX #%00100000
	STX PF0
	LDX #%0
	STX PF1
	STX PF2
	REPEAT 164
		STA WSYNC
	REPEND

	; Playfield bottom
	LDX #%11100000
	STX PF0
	LDX #%11111111
	STX PF1
	STX PF2
	REPEAT 7
		STA WSYNC
	REPEND

	; Skip 7 lines of PF
	LDX #0
	STX PF0
	STX PF1
	STX PF2
	REPEAT 7
		STA WSYNC
	REPEND

	; Overscan
	LDA #02
	STA VBLANK
	REPEAT 30
		STA WSYNC
	REPEND
	LDA #0
	STA VBLANK

	JMP StartFrame

	; Fill the ROM
	ORG $FFFC
	.word Reset
	.word Reset
