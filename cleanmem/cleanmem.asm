	processor 6502

	seg code
	org $f000	; Define the code origin

Start:
	sei		; Disable interrupts
	cld		; Disable the BCD decimal math mode
	ldx #$FF	; Load X register for $FF
	txs		; Transfer X register to stack pointer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear the page zero region $00 to $FF
; I.e. clear the RAM and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #0
	ldx #$FF

	sta $ff		; Set $FF to zero because MemLoop misses it

MemLoop:
	dex
	sta $0,X	; Store the value of A inside memory address $0+X
	bne MemLoop	; loop if X register is not 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fill ROM size to 4kb exactly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org $FFFC
	.word Start	; Reset vector at $FFFC (where the program starts)
	.word Start	; Interrupt vector at $FFFE
