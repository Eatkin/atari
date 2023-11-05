	processor 6502
	
	include "macro.h"
	include "vcs.h"

	seg.u Variables
	org $80

JetXPos		byte
JetYPos		byte
BomberXPos	byte
BomberYPos	byte
MissileXPos	byte
MissileYPos	byte
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
ScoreSprite	byte
TimerSprite	byte
TerrainColour	byte
RiverColour	byte

; Define constants
SPRITE_HEIGHT = 9
DIGIT_HEIGHT = 5
SCOREBOARD_HEIGHT = 8

	seg Code
	org $F000

Reset:
	CLEAN_START

; Initialise RAM variables and TIA registers
	; Set initial player coordinates
	lda #10
	sta JetYPos
	lda #80
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

; Declare macros
	MAC DRAW_MISSILE
		lda #0
		; x contains the scanline number
		cpx MissileYPos
		bne .SkipMissileDraw
.DrawMissile:
		lda #%00000010		; Enable missile 0
		inc MissileYPos
.SkipMissileDraw:
		sta ENAM0		; Store correct value in TIA missile register
	ENDM

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
	REPEAT 33
		stx WSYNC
	REPEND

	lda JetXPos
	ldy #0		; Corresponds to P0
	jsr SetObjXPos

	lda BomberXPos
	ldy #1		; Corresponds to p1
	jsr SetObjXPos

	lda MissileXPos
	ldy #2		; Corresponds to missile0
	jsr SetObjXPos

	jsr CalculateDigitOffset	; Calculate scoreboard digit lookup table offset

	; Poke registers
	sta WSYNC
	sta HMOVE

	ldx #0
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
	sta CTRLPF
	sta COLUBK

	; Set playfield colour
	lda #$1E
	sta COLUPF

	; Draw the scoreboard
	ldx #DIGIT_HEIGHT
.ScoreDigitLoop:
	ldy TensDigitOffset
	lda Digits,Y
	and #$F0	; Mask the graphics for the ones digits
	sta ScoreSprite
	
	ldy OnesDigitOffset
	lda Digits,Y
	and #$0F	; Mask the graphics for the tens digits
	ora ScoreSprite
	sta ScoreSprite

	sta WSYNC	; poke
	sta PF1		; Display score sprite
	
	ldy TensDigitOffset+1	; Tens digit for the timer
	lda Digits,Y
	and #$F0
	sta TimerSprite

	ldy OnesDigitOffset+1
	lda Digits,Y
	and #$0F
	ora TimerSprite
	sta TimerSprite

	; Waste clock cycles
	jsr Sleep12Cycles

	sta PF1
	
	; 2-kernel display so strobe
	ldy ScoreSprite
	sta WSYNC

	sty PF1
	inc TensDigitOffset
	inc TensDigitOffset+1
	inc OnesDigitOffset
	inc OnesDigitOffset+1

	jsr Sleep12Cycles

	dex
	sta PF1
	bne .ScoreDigitLoop
	
	sta WSYNC

	; Add padding under the scoreboard
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta WSYNC
	sta WSYNC
	sta WSYNC

; Render the visible scanlines
VisibleScanlines:
	; Set background colour
	lda RiverColour
	sta COLUBK
	; Set playfield colour
	lda TerrainColour
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

	; Number of remaining scanlines
	ldx #85

; Render the 96 visible scanline
; Using a 2 line kernel
.GameLineLoop:
	DRAW_MISSILE		; Macro to see if we will draw the missile
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

	sta WSYNC	

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
	bne ClampYHigh
	dec JetYPos
ClampYHigh:
	; Here we will limit jet's y-range
	; Max y is 96, min y is 0
	clc
	lda JetYPos
	cmp #76
	bcc ClampYLow
	lda #76
	sta JetYPos
ClampYLow:
	clc
	lda #1
	cmp JetYPos
	bcc CheckP0Left
	sta JetYPos
CheckP0Left:
	; Store the X position so we can restore it if we collide with the playfield
	ldy JetXPos
	lda #%01000000
	bit SWCHA
	bne CheckP0Right 
	dec JetXPos
CheckP0Right:
	lda #%10000000
	bit SWCHA
	bne ClampXPos
	inc JetXPos
ClampXPos:
	; Check if we've collided with the playfield and if we have restore our X position to that saved in the y register
	lda #%10000000
	bit CXP0FB
	beq CheckButtonPressed		;Zero flag is set if a&bit is zero (lol)
	; This will restore the jet x position from the y register if we have hit the playfield
	tya
	; Collisions are precise so we need to account for different collision masks
	; The moving sprite and static sprite are 1 pixel different
	clc
	cmp #80
	bcs ShiftRight
	adc #1
	jmp UpdateJetPos
ShiftRight:
	sbc #1
UpdateJetPos:
	sta JetXPos
CheckButtonPressed:
	lda #%10000000
	bit INPT4
	bne NoInput
	; Set missile coorindates
	lda JetXPos
	clc
	adc #5
	sta MissileXPos
	lda JetYPos
	clc
	adc #5
	sta MissileYPos
	
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
	jsr SetTerrainRiverColour
	jmp .CheckCollisionM0P1
.CollisionP0P1:
	jsr GameOver
.CheckCollisionM0P1:
	lda #%10000000
	bit CXM0P
	bne .CollisionM0P1
	jmp EndCollisionCheck
.CollisionM0P1:
	jsr SpawnBomber
	sed
	clc
	lda Score
	adc #1
	sta Score
	cld
	lda #0
	sta MissileYPos

EndCollisionCheck:
	sta CXCLR		; Poke clear collision check register
	; Loop forever
	jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set terrain/river colour to green/blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColour subroutine
	lda #$C2
	sta TerrainColour
	lda #$84
	sta RiverColour

	rts


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
	sta TerrainColour
	sta RiverColour

	lda #0
	sta Score
	sta Timer
	
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

.SetScoreValues:
	sed			; Turn on decimal mode
	lda Timer 
	clc
	adc #1
	sta Timer
	cld			; Turn off decimal mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sleep 12 cycles
; jsr takes 6 cycle, rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
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
