	processor 6502
	
	include "macro.h"
	include "vcs.h"

	seg.u Variables
	org $80

JetXPos		byte
JetYPos		byte
BomberXPos	byte
BomberYPos	byte
Score		byte
Timer		byte
Temp		byte
OnesDigitOffset	word
TensDigitOffset	word
BallDudePtr	word
AdrienPtr	word
BallDudeColPtr	word
AdrienColPtr	word
JetAnimOffset	byte
Random		byte

; Define constants
SPRITE_HEIGHT = 9
DIGIT_HEIGHT = 5

	seg Code
	org $F000

Reset:
	CLEAN_START

; Initialise RAM variables and TIA registers
	; Set initial player coordinates
	lda #80
	sta JetYPos
	lda #0
	sta JetXPos
	
	; Set initial bomber coordinates
	; Basically just some random coordinates as placeholder lol
	lda #83
	sta BomberYPos
	lda #80
	sta BomberXPos
	lda #%11010100
	sta Random
	lda #0
	sta Score
	sta Timer

; Initialise sprite and palette pointers
	lda #<BallDude
	sta BallDudePtr
	lda #>BallDude
	sta BallDudePtr+1

	lda #<Adrien
	sta AdrienPtr
	lda #>Adrien
	sta AdrienPtr+1

	lda #<BallDudeCol
	sta BallDudeColPtr
	lda #>BallDudeCol
	sta BallDudeColPtr+1

	lda #<AdrienCol
	sta AdrienColPtr
	lda #>AdrienCol
	sta AdrienColPtr+1

; Begin rendering
StartFrame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pre-vblank calculations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda JetXPos
	ldy #0		; Corresponds to P0
	jsr SetObjXPos

	lda BomberXPos
	ldy #1		; Corresponds to p1
	jsr SetObjXPos

	jsr CalculateDigitOffset	; Calculate scoreboard digit lookup table offset

	; Poke registers
	sta WSYNC
	sta HMOVE

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

; Render scoreboard
	; Clear playfield registers so nothing is drawn
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta GRP0
	sta GRP1
	lda #$1C
	sta COLUPF
	; Do not reflect scoreboard
	lda #0
	sta CTRLPF

	; Display 20 scanlines for scoreboard
	REPEAT 20
		sta WSYNC
	REPEND
	

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

	; 84 to account for the size of the scoreboard
	ldx #84

; Render the 96 visible scanline
; Using a 2 line kernel
.GameLineLoop:
.InsideBallDude:
	txa
	sec
	sbc JetYPos
	cmp SPRITE_HEIGHT
	bcc .DrawSpriteP0
	lda #0

.DrawSpriteP0:
	clc
	adc JetAnimOffset
	tay
	lda (BallDudePtr),Y	; Y register is only register that can work with pointers
	sta WSYNC		; Wait for scanline
	sta GRP0		; Set graphics for P0
	lda (BallDudeColPtr),Y
	sta COLUP0
	
.InsideAdrien:
	txa
	sec
	sbc BomberYPos
	cmp SPRITE_HEIGHT
	bcc .DrawSpriteP1
	lda #0

.DrawSpriteP1:
	tay

	; Set Adrien to be thicc because it'll be funny idk
	lda #%00000111
	sta NUSIZ1
	tya

	lda (AdrienPtr),Y	; Y register is only register that can work with pointers
	sta WSYNC		; Wait for scanline
	sta GRP1		; Set graphics for P0
	lda (AdrienColPtr),Y
	sta COLUP1

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Joystick input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldx JetXPos
CheckP0Up:
	lda #%00010000
	bit SWCHA
	bne CheckP0Down
	inc JetYPos
CheckP0Down:
	lda #%00100000
	bit SWCHA
	bne CheckP0Left
	dec JetYPos
CheckP0Left:
	lda #%01000000
	bit SWCHA
	bne CheckP0Right 
	dec JetXPos
CheckP0Right:
	lda #%10000000
	bit SWCHA
	bne NoInput
	inc JetXPos
NoInput:
	lda #0
	sta JetAnimOffset
	cpx JetXPos
	beq NoChange
	; Set the sprite pointer to BallDude Turning
	lda SPRITE_HEIGHT
	sta JetAnimOffset
NoChange:

UpdateBomberPosition:
	clc
	lda BomberYPos
	cmp #0
	bmi .ResetBomberPosition

	dec BomberYPos
	jmp EndPositionUpdate

.ResetBomberPosition:
	jsr SpawnBomber

EndPositionUpdate:
; Collision checks
CheckCollisionP0P1:
	lda #%10000000
	bit CXPPMM
	bne .CollisionP0P1	; Zero flag is set if a&bit is zero (lol)
	jmp CheckCollisionP0Playfield
.CollisionP0P1:
	jsr GameOver
CheckCollisionP0Playfield:
	lda #%10000000
	bit CXP0FB
	bne .CollisionP0Playfield
	jmp EndCollisionCheck
.CollisionP0Playfield:
	jsr GameOver
EndCollisionCheck:
	sta CXCLR		; Poke clear collision check register
	; Loop forever
	jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; accumulator contains x-posiiton
; y register contains object (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjXPos subroutine
	sta WSYNC
	sec
.DivideLoop:
	sbc #15
	bcs .DivideLoop	; loop while carry is set

	eor #7		; exclusive OR with accumulator
			; A will be between -8 and 7
	REPEAT 4
		asl	; bitshift a, HMP0 uses only 4 bits
	REPEND

	sta HMP0,Y	; set fine position value
	sta RESP0,Y	; set coarse position value
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GameOver Subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
	; Set background to red for one frame
	lda #$30
	sta COLUBK
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generate random number using LFSR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SpawnBomber subroutine
	; The LFSR
	lda Random
	asl
	eor Random
	asl
	eor Random
	asl
	asl
	eor Random
	asl
	rol Random
	
	; Divide by 4 to match playfield size
	lsr	
	lsr
	sta BomberXPos
	lda #30			; Add 30 for playfield bounds
	adc BomberXPos
	sta BomberXPos

	lda #96
	sta BomberYPos
	
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set scoreboard digits to be displayed to screen
; We do it in hex because it's easier or something idk lol
; I'll probably come back to this to try make a decimal display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
	ldx #1
.PrepareScoreLoop
	; Ones digit
	lda Score,X	; Timer when x = 1
	and #%00001111
	sta Temp
	asl
	asl
	adc Temp
	sta OnesDigitOffset,X
	; Tens digit
	lda Score,X
	and #%11110000
	sta Temp
	lsr
	lsr
	sta Temp
	lsr
	lsr
	adc Temp
	sta TensDigitOffset,X

	dex
	bpl .PrepareScoreLoop
	rts


; Sprites
Digits:
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #

	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %00110011          ;  ##  ##
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###

	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #
	.byte %00010001          ;   #   #

	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %00010001          ;   #   #
	.byte %01110111          ; ### ###

	.byte %00100010          ;  #   #
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #

	.byte %01110111          ; ### ###
	.byte %01010101          ; # # # #
	.byte %01100110          ; ##  ##
	.byte %01010101          ; # # # #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01000100          ; #   #
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###

	.byte %01100110          ; ##  ##
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01010101          ; # # # #
	.byte %01100110          ; ##  ##

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01110111          ; ### ###

	.byte %01110111          ; ### ###
	.byte %01000100          ; #   #
	.byte %01100110          ; ##  ##
	.byte %01000100          ; #   #
	.byte %01000100          ; #   #

BallDude:
	.byte #%00000000
        .byte #%01111110;$5A
        .byte #%11111111;$5A
        .byte #%11111111;$58
        .byte #%11111111;$56
        .byte #%10011111;$54
        .byte #%11011011;$54
        .byte #%11111011;$52
        .byte #%01110110;$52
BallDudeTurning:
	.byte #%00000000
        .byte #%00011000;$5A
        .byte #%00111100;$58
        .byte #%01111110;$56
        .byte #%01111110;$54
        .byte #%01001010;$54
        .byte #%01101010;$52
        .byte #%00110100;$52
        .byte #%00011000;$52
Adrien:
	.byte #%00000000
        .byte #%11111111;$16
        .byte #%11100111;$16
        .byte #%11100111;$18
        .byte #%01111110;$18
        .byte #%01011010;$1A
        .byte #%00111100;$1A
        .byte #%00111100;$1C
        .byte #%00011000;$1C

BallDudeCol:
	.byte #$00
        .byte #$5A;
        .byte #$58;
        .byte #$56;
        .byte #$54;
        .byte #$54;
        .byte #$52;
        .byte #$52;
        .byte #$52;
BallDudeTurningCol:
	.byte #$00
        .byte #$5A;
        .byte #$58;
        .byte #$56;
        .byte #$54;
        .byte #$54;
        .byte #$52;
        .byte #$52;
        .byte #$52;
AdrienCol:
	.byte #$00
        .byte #$16;
        .byte #$16;
        .byte #$18;
        .byte #$18;
        .byte #$1A;
        .byte #$1A;
        .byte #$1C;
        .byte #$1C;

; Complete ROM with 4kb
	org $FFFC
	word Reset
	word Reset
