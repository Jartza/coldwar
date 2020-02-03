 processor 6502
	ORG $1201
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $34,$36,$32,$34
	.byte    $29, $00, $00, $00
	ORG $1210
ColdWar
	jmp block69559
 ; Temp vars section
 ; Temp vars section ends
	org $2000
carbody_L	dc.w $03000, $03030, $03060, $03090
carbody_M	dc.w $03010, $03040, $03070, $030a0
carbody_R	dc.w $03020, $03050, $03080, $030b0
ground_T	dc.w $03100, $03108
updown_T	dc.w $03110, $03118, $03120, $03128, $03130, $03138, $03140, $03148
	dc.w 
wheel_L	dc.w $030c0, $030d0, $030e0, $030f0
wheel_R	dc.w $030c8, $030d8, $030e8, $030f8
drawTree	dc.b $00, $00
scnt	dc.b	$00
gcnt	dc.b	$00
mcnt	dc.b	$00
x	dc.b	$32
y	dc.b	$9c
yr	dc.b	$a0
yl	dc.b	$a0
oldyr	dc.b	$b4
oldyl	dc.b	$b4
oldx	dc.b	$a8
oldy	dc.b	$9c
dir	dc.b	$02
delay	dc.b	
clr	dc.b	$00
grndtile	dc.b	$00
updowntile	dc.b	$00
updowndir	dc.b	$01
groundpos	dc.b	$00
groundmoved	dc.b	$00
i	dc.b	$00
p1	= $64
yoff	dc.b	$00
yoff_r	dc.b	$00
yoff_l	dc.b	$00
wheelbump	dc.b	$00
message	
	dc.b	$03, $0f, $0c, $04, $20, $17, $01, $12, $20
	dc.b	$2d, $20, $13, $0f, $0d, $05, $20, $02, $15
	dc.b	$07, $07, $19, $20, $07, $01, $0d, $05, 0
	
	
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
mul16x8_num1Hi = $80
mul16x8_num1 = $82
mul16x8_num2 = $84
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop  ; accumulating multiply entry point (enter with .A=lo, .Y=hi)
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
	
	
	; ***********  Defining procedure : initVbm
	;    Procedure type : User-defined procedure
	
	; Initialise the core VBM (Vic20 Bitmap Mode) library
	; Created by Andy H - Hewco.uk for use in Turbo Rascal
	; See help to get started, all commmands begin with 'vbm'
; Screen address table Low byte / high byte
vbmScrL = $0c ; 20 bytes
;    dc.b 0,0,0,0,0,0,0,0,0,0
;    dc.b 0,0,0,0,0,0,0,0,0,0
vbmScrH = $20 ; 20 bytes
;    dc.b 0,0,0,0,0,0,0,0,0,0
;    dc.b 0,0,0,0,0,0,0,0,0,0
; ends at $32
vbm9000     = $00 ; store $9000 address value
vbm9001     = $01 ; store $9001 address value
vbm9005     = $02 ; store $9005 address value
vbmX        = $03 ; x position
vbmY        = $04 ; y position
vbmI        = $05 ; index
vbmJ        = $06 ; index
vbmT        = $07 ; index
vbmScroll   = $08    ; 16 - character scroll start
vbmNumColumns = $09  ; 20 -number of columns
vbmScrLstart = $0a   ; $00 - start address for bitmap L
vbmScrHstart = $0b   ; $11 - start address for bitmap H
vbmSetDisplayMode
    ; initialise
    lda #16 ; start char
    sta vbmScroll
    lda #$00 ; address L
    sta vbmScrLstart
    lda #$11 ; address H
    sta vbmScrHstart
    lda $9000
    sta vbm9000
    cmp #$c;keep
    beq vbmIsPal
    lda $9000	; Adjust horizontal position for NTSC
    clc
    adc #1
    sta $9000
    jmp vbmIsNtsc
	
vbmIsPal
    lda $9000	; Adjust horizontal position for PAL
    clc
    adc #2
    sta $9000
vbmIsNtsc
    lda #25		; (12x2) + 1
    sta $9003	; set screen height to 12 double height chars
    lda vbmNumColumns ;#20
    sta $9002	; set screen width to 20 characters
    cmp #20
    beq vbmSDM_noadjust
    lda $9000	; 19 column mode, move horiz position another 4 pixels right to centre
    clc
    adc #1
    sta $9000
vbmSDM_noadjust
    lda $9001
    sta vbm9001
    sec
    sbc #1
    sta $9001	; adjust vertical position
    lda $9005
    sta vbm9005
    lda #%11001100	; 204 - set screen and character to $1000
    sta $9005
    jsr vbmLayoutCharacters
    jsr vbmCreateColumnAddresses
    rts
;; Write column start addresses
vbmCreateColumnAddresses
    ; address table for logical character memory
    ; (bitmap x start addresses)
    ldx #0
    lda vbmScrLstart    ; L start address of bitmap
    sta vbmScrL,x
    ldy vbmScrHstart    ; H start address of bitmap
    tya
    sta vbmScrH,x
vbmAddrTable2
    lda vbmScrL,x
    inx
    clc
    adc #192 ; height of screen in pixels
    bcc vbmNoOverflow2
    iny
vbmNoOverflow2
    sta vbmScrL,x
    tya
    sta vbmScrH,x
    cpy #$20             ; address needs to wrap back around?
    bne vbmAddrTable3
    ; reset address to start of memory VIC bitmap
    lda #$00
    sta vbmScrL,x
    ldy #$11
    tya
    sta vbmScrH,x
vbmAddrTable3
    cpx #19		; width in characters-1
    bcc vbmAddrTable2
    rts
;; Draw character map to screen to form bitmap layout
vbmLayoutCharacters
    ; Layout characters on screen
    lda vbmScroll 	; character to start drawing with (16)
    sta vbmI            ; character
    lda #0
    sta vbmX
vbmXDrawForLoop
    lda #0
    sta vbmY
    ; screen start
    lda #$00
    ldy #$10
    sta screenmemory
    sty screenmemory+1
vbmYDrawForLoop
    lda vbmI
    ldy vbmX
    sta (screenmemory),y
    ; add 20 for next row (or 19)
    lda screenmemory
    clc
    adc vbmNumColumns ; 20
    sta screenmemory
    bcc vbmYDrawForLoopOverflow
    inc screenmemory+1
vbmYDrawForLoopOverflow
    inc vbmY
    inc vbmI    ; character
    bne vbmDrawForLoopResetChar
    lda #16     ; reset character
    sta vbmI
vbmDrawForLoopResetChar
    lda #12	; comapare 12 rows
    cmp vbmY ;keep
    bne vbmYDrawForLoop ; loop rows
    inc vbmX
    lda vbmNumColumns	; compare 20 columns
    cmp vbmX ;keep
    bne vbmXDrawForLoop	; loop columns
	rts
	
	
	; ***********  Defining procedure : initVbmBlot
	;    Procedure type : User-defined procedure
	
	; VBM Blot mask
vbmBlotBit    dc.b $c0, $30, $0c, $03
vbmDrawBlot
	; Accumulator contains X position
	lsr   ; divide by 8 to find column number
	lsr
	lsr
	tax
	lda vbmScrL,x   ; Address of table lo
	ldy vbmScrH,x   ; Address of table hi
	sta screenmemory   ; Set sceenmemory to start of column lo
	sty screenmemory+1 ; Set sceenmemory to start of column hi
	lda vbmX
	and #6   ; find offset for dot
	lsr
	tax
	lda vbmBlotBit,x   ; get blot pattern
	ldy vbmY  ; draw dot in row
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda vbmBlotBit,x   ; get blot pattern for second row
	ora (screenmemory),y
	sta (screenmemory),y
	rts
vbmClearBlot
	; Accumulator contains X position
	lsr   ; divide by 8 to find column number
	lsr
	lsr
	tax
	lda vbmScrL,x   ; Address of table lo
	ldy vbmScrH,x   ; Address of table hi
	sta screenmemory   ; Set sceenmemory to start of column lo
	sty screenmemory+1 ; Set sceenmemory to start of column hi
	lda vbmX
	and #6   ; find offset for dot
	lsr
	tax
	lda vbmBlotBit,x   ; get blot pattern
	eor #$ff  ; invert
	ldy vbmY  ; draw dot in row
	and (screenmemory),y
	sta (screenmemory),y
	iny
	lda vbmBlotBit,x   ; get blot pattern for second row
	eor #$ff  ; invert
	and (screenmemory),y
	sta (screenmemory),y
	rts
vbmDrawBlotE
	; Accumulator contains X position
	lsr   ; divide by 8 to find column number
	lsr
	lsr
	tax
	lda vbmScrL,x   ; Address of table lo
	ldy vbmScrH,x   ; Address of table hi
	sta screenmemory   ; Set sceenmemory to start of column lo
	sty screenmemory+1 ; Set sceenmemory to start of column hi
	lda vbmX
	and #6   ; find offset for dot
	lsr
	tax
	lda vbmBlotBit,x   ; get blot pattern
	ldy vbmY  ; draw dot in row
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda vbmBlotBit,x   ; get blot pattern for second row
	eor (screenmemory),y
	sta (screenmemory),y
	rts
vbmTestPixel2
	; Accumulator contains X position
	lsr   ; divide by 8 to find column number
	lsr
	lsr
	tax
	lda vbmScrL,x   ; Address of table lo
	ldy vbmScrH,x   ; Address of table hi
	sta screenmemory   ; Set sceenmemory to start of column lo
	sty screenmemory+1 ; Set sceenmemory to start of column hi
	lda vbmX
	and #6   ; find offset for dot
	lsr
	tax
	lda vbmBlotBit,x   ; get blot pattern (double pixel, even aligned)
	ldy vbmY  ; draw dot in row
	and (screenmemory),y
	rts
	
	
	; ***********  Defining procedure : initVbmClear
	;    Procedure type : User-defined procedure
	
	; VBM Clear bitmap routine
vbmClear
	; Address of bitmap $1100
	lda #0
	ldx #17
	sta screenmemory
	stx screenmemory+1
	sta vbmX
vbmClearForX
	lda #0
	sta vbmY
vbmClearForY
	lda vbmI ; vbmI contains byte to clear bitmap with
	ldy vbmY
	sta (screenmemory),y
	inc vbmY
	lda #192    ; reached end of row?
	cmp vbmY ;keep
	bne vbmClearForY
	lda screenmemory
	clc
	adc #192
	bcc vbmClearXOverflow
	inc screenmemory+1
vbmClearXOverflow
	sta screenmemory
	inc vbmX
	lda #20 ; reched end of column?
	cmp vbmX ;keep
	bne vbmClearForX
    ; bitmap has been cleared
	rts
	
	
	; ***********  Defining procedure : initVbmDrawSmallTextE
	;    Procedure type : User-defined procedure
	
	; Draw text characters to the bitmap using a zero terminated CSTRING with EOR operation
	; CSTRING    = $80
	; Font chars = $82
	; Temp addr  = $84 - used to calculate char address
vbmDrawSmallTextE
vbmDSTXE_Xloop
	; calculate next screen memory position
	lda vbmX
	lsr ; divde x by 2 (2 chars per character cell)
	tax
	; Wor out from LSR if odd or even pattern
	bcs vbmDSTXE_Odd
	lda #$f0 ; even, use left side of font
	bcc vbmDSTXE_Even ; we know carry will be clear
vbmDSTXE_Odd
	lda #$0f ; odd, use right side of font
vbmDSTXE_Even
	sta vbmT ; store mask to use for later
	lda vbmScrL,x   ; Address of table lo
	ldy vbmScrH,x   ; Address of table hi
	clc
	adc vbmY		; Add Y offset
	bcc vbmDSTXE_NSP_NoOverflow
	iny
vbmDSTXE_NSP_NoOverflow
	sta screenmemory
	sty screenmemory+1
vbmDSTXE_GetCharNum
	; convert text number (0-255) * 8 = memory offset
	ldy #0
	lda ($80),y		; get char from current position in CSTRING
	bne vbmDSTXE_NotEnd
	rts ; if =0, we are end of the cstring
vbmDSTXE_NotEnd
	sta $84
	sty $84+1
	asl $84
	rol $84+1 ;x2
	asl $84
	rol $84+1 ;x4
	asl $84
	rol $84+1 ;x8
	lda $84
	clc
	adc $82  ; add tile low address
	sta $84
	lda $84+1
	adc $82+1 ; add tile high address
	sta $84+1
vbmDSTXE_DrawChar
	; y reg is ZERO from ldy #0 in GetTileNum
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	and vbmT
	eor (screenmemory),y
	sta (screenmemory),y
vbmDSTXE_NextChar
	clc
	inc $80  ; low byte
	bne vbmDSTXE_NTM_NoOverflow
	inc $80+1  ; high byte
vbmDSTXE_NTM_NoOverflow
	; next x pos on screen
	inc vbmX
	lda #40   ; 0-39 columns, 40 means exceeded right of screen
	cmp vbmX  ; has x pos exceeded?
	beq vbmDSTXE_NextLine  ;
	jmp vbmDSTXE_Xloop  ; no, draw next char
vbmDSTXE_NextLine
	; yes, set x back to 0 and inc vbmY by line height (pixels)
	lda #0
	sta vbmX
	lda vbmY
	clc
	adc vbmI
	sta vbmY
	jmp vbmDSTXE_Xloop
	rts
	
	
	; ***********  Defining procedure : initVbmDrawSprite8E
	;    Procedure type : User-defined procedure
	
	; VBM - draw an 8x8 sprite with EOR - use vbmSetPosition first
	; Left Side = $80
	; Right side = $82
vbmDrawSprite8E
	; draw left side
	ldy #0
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	; move screenmemory to next column
	lda screenmemory
	clc
	adc #192 ; next column
	bcc vbmDS8E_overflow
	inc screenmemory+1
vbmDS8E_overflow
	sta screenmemory
	lda vbmX
	bne vbmDS8E_Right
	rts ; in position 0 there is no right side to draw
vbmDS8E_Right
	; draw right side
	ldy #0
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	eor (screenmemory),y
	sta (screenmemory),y
	rts
	
	
	; ***********  Defining procedure : initVbmDrawSpriteSliceE
	;    Procedure type : User-defined procedure
	
	; VBM - draw an 8 x h sprite with EOR - use vbmSetPosition first
	; Shifted sprite address = $80
vbmDrawSpriteSliceE
	ldy vbmI	; line in sprite to start drawing from
vbmDSSE_draw
	; draw one slice
	lda ($80),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	cpy vbmT	; line is sprite to draw to (height)
	bne vbmDSSE_draw
	rts
	
	
	; ***********  Defining procedure : initVbmNextColumn
	;    Procedure type : User-defined procedure
	
vbmNextColumn
	; move screenmemory to next column
	; WARNING: This is not safe to use with Screen Scrolling as the character map position is unknown
	lda screenmemory
	clc
	adc #192 ; next column
	bcc vbmCSS_overflow
	inc screenmemory+1
vbmCSS_overflow
	sta screenmemory
	rts
	
	
	; ***********  Defining procedure : initVbmScreenShiftLeft
	;    Procedure type : User-defined procedure
	
	; VBM - Shift the screen to the left
	; x reg = start line, vbmY = end line
vbmScreenShiftLeft
	;ldx vbmX	; line to start at
vbmLSP_loop
	lda $1100,x ; to get bit from left
	asl
	rol $1100+$E40,x
	rol $1100+$D80,x
	rol $1100+$CC0,x
	rol $1100+$C00,x
	rol $1100+$B40,x
	rol $1100+$A80,x
	rol $1100+$9C0,x
	rol $1100+$900,x
	rol $1100+$840,x
	rol $1100+$780,x
	rol $1100+$6C0,x
	rol $1100+$600,x
	rol $1100+$540,x
	rol $1100+$480,x
	rol $1100+$3C0,x
	rol $1100+$300,x
	rol $1100+$240,x
	rol $1100+$180,x
	rol $1100+$C0,x
	rol $1100,x
	inx
	cpx vbmY	; line to end at
	bne vbmLSP_loop
	rts
	
	
	; ***********  Defining procedure : initVbmSetPosition
	;    Procedure type : User-defined procedure
	
	; ----------
	; init vbmSetPosition
vbmSetPosition
    lda vbmX ; x position
    pha      ; store x position for later
    lsr      ; divide by 8 to get column number
    lsr
    lsr
    tax      ; locate address of start of column
    lda vbmScrL,x
    ldy vbmScrH,x
    clc
    adc vbmY ; add y position
    bcc vbmSPos_noOverflow
    iny ; overflow
vbmSPos_noOverflow
    sta screenmemory   ; set screenmemory to the correct address in the bitmap
    sty screenmemory+1
    pla      ; retrieve x position
    rts
; call to set position on bitmap supporting 8 shifted x positions
vbmSetPosition1
    jsr vbmSetPosition
    and #7   ; get x offset 0-7
    sta vbmX ; for use in sprite routines
    rts
; call to set position on bitmap supporting 4 shifted x positions
vbmSetPosition2
    jsr vbmSetPosition
    and #7   ; get x offset 0-7
    lsr
    sta vbmX ; for use in sprite routines
    rts
; call to set position on bitmap supporting 2 shifted x positions
vbmSetPosition4
    jsr vbmSetPosition
    and #7   ; get x offset 0-7
    lsr
    lsr
    sta vbmX ; for use in sprite routines
	rts
	
	
	; ***********  Defining procedure : init_viairq
	;    Procedure type : User-defined procedure
	
init_via_irq:
  ldx #0       ; wait for this raster line (times 2)
A0_vic_raster:
  cpx $9004
  bne A0_vic_raster        ; at this stage, the inaccuracy is 7 clock cycles
                ; the processor is in this place 2 to 9 cycles
                ; after $9004 has changed
  ldy #9
  bit $24
A1_vic_raster:
  ldx $9004
  txa
  bit $24
  ldx #24
  dex
  bne *-1       ; first spend some time (so that the whole
  cmp $9004     ; loop will be 2 raster lines)
  bcs *+2       ; save one cycle if $9004 changed too late
  dey
  bne A1_vic_raster
                ; now it is fully synchronized
                ; 6 cycles have passed since last $9004 change
                ; and we are on line 2(28+9)=74
;initialize the timers
  lda #$40      ; enable Timer A free run of both VIAs
  sta $911b
  sta $912b
        ; 312*71-2 = $568
timers_vic_raster:
  lda #$56
  ldx #$86
  sta $9116     ; load the timer low byte latches
  sta $9126
  ldy #7        ; make a little delay to get the raster effect to the
  dey           ; right place
  bne *-1
  nop
  nop
  stx $9125     ; start the IRQ timer A
                ; 6560-101: 65 cycles from $9004 change
                ; 6561-101: 77 cycles from $9004 change
  ldy #10       ; spend some time (1+5*9+4=55 cycles)
  dey           ; before starting the reference timer
  bne *-1
  stx $9115     ; start the reference timer
pointers_vic_raster:
  lda #00     ; set the raster IRQ routine pointer
  sta $314
  lda #00
  sta $315
  lda #$c0
  sta $912e     ; enable Timer A underflow interrupts
  rts
	rts
	
	
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
multiplier = $80
multiplier_a = $82
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $82
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
initeightbitmul_multiply_eightbit89778
	rts
	
	
	; ***********  Defining procedure : initjoy1
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	; ----------
	; ReadJoy1 and ReadJoy2 (on supported platforms)
	; populates joy1 and joy1pressed which can be tested by AND-ing with the following constants:
;JOY_DOWN  = %00000100
;JOY_UP    = %00000010
;JOY_LEFT  = %00001000
;JOY_RIGHT = %00000001
;JOY_FIRE  = %00010000
VIC20_PORTACASS = $911F
VIC20_PORTBVIA2 = $9120  ; Port B 6522 2 value (joystick)
VIC20_PORTBVIA2d = $9122 ; Port B 6522 2 direction (joystick)
joy1 .byte 0
joy1last .byte 0
joy1pressed .byte 0
callReadJoy1
	LDA VIC20_PORTACASS
	EOR #$FF
	AND #$3C
	LDX #$7F
	SEI
	STX VIC20_PORTBVIA2d
	LDY VIC20_PORTBVIA2
	BMI initjoy1_JoySkip96479
	ORA #$02
initjoy1_JoySkip96479
	LDX #$FF
	STX VIC20_PORTBVIA2d
	CLI
	LSR
	STA joy1
	eor joy1last
	and joy1
	sta joy1pressed
	lda joy1
	sta joy1last
	rts
	
	
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	jmp initmoveto_moveto38005
screenmemory =  $fe
screen_x = $80
screen_y = $82
SetScreenPosition
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop
	clc
	adc #22
	bcc sskip
	inc screenmemory+1
sskip
	dey
	bne syloop
sydone
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone
	sta screenmemory
	rts
initmoveto_moveto38005
	rts
	
; // Address of bitmap(screen and characters)
; // Export sprites
; // Resource memory
; // pre-shifted address-table for car body
; // Address-table for ground tile
; // Address-table for up'n'down tile
; // pre-shifter address-table for single wheel
	
	
	; ***********  Defining procedure : drawcar
	;    Procedure type : User-defined procedure
	
drawcar
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	
; // store the processors registers
; //screen_bg_color := BLACK + SCREEN_BG_BLACK;
; // Clear previous car sprite
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	lda oldy
	sta vbmY
	; x is complex
	lda oldx
	sta vbmX
	jsr vbmSetPosition2
	; Read address 1
	; read out the address
	lda vbmX	; x offset
	asl
	tax
	lda carbody_L,x
	sta $80
	lda carbody_L+1,x
	sta $80+1
	lda #0
	sta vbmI
	lda #8
	sta vbmT
	jsr vbmDrawSpriteSliceE
	; WARNING: Do not use if using character screen scrolling commands
	jsr vbmNextColumn
	; Read address 1
	; read out the address
	lda vbmX	; x offset
	asl
	tax
	lda carbody_M,x
	sta $80
	lda carbody_M+1,x
	sta $80+1
	lda #0
	sta vbmI
	lda #8
	sta vbmT
	jsr vbmDrawSpriteSliceE
	; WARNING: Do not use if using character screen scrolling commands
	jsr vbmNextColumn
	; Read address 1
	; read out the address
	lda vbmX	; x offset
	asl
	tax
	lda carbody_R,x
	sta $80
	lda carbody_R+1,x
	sta $80+1
	lda #0
	sta vbmI
	lda #8
	sta vbmT
	jsr vbmDrawSpriteSliceE
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	lda oldyl
	sta vbmY
	; x is complex
	lda oldx
	sta vbmX
	jsr vbmSetPosition2
	; Read address 1
	; Read address 2
	lda vbmX ; x offset 0-7
	asl ; for simplicty, storing lo, hi in one array
	tax
	lda wheel_L,x
	sta $80
	lda wheel_L+1,x
	sta $80+1
	lda wheel_R,x
	sta $82
	lda wheel_R+1,x
	sta $82+1
	jsr vbmDrawSprite8E
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	lda oldyr
	sta vbmY
	; x is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda oldx
	clc
	adc #10
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmSetPosition2
	; Read address 1
	; Read address 2
	lda vbmX ; x offset 0-7
	asl ; for simplicty, storing lo, hi in one array
	tax
	lda wheel_L,x
	sta $80
	lda wheel_L+1,x
	sta $80+1
	lda wheel_R,x
	sta $82
	lda wheel_R+1,x
	sta $82+1
	jsr vbmDrawSprite8E
	; Screen Shift Left
	lda #192
	sta vbmY
	ldx #176 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Screen Shift Left
	lda #192
	sta vbmY
	ldx #176 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne drawcar_elseblock33938
drawcar_binaryclausesuccess99650
	; Binary clause: EQUALS
	lda gcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_elseblock33938
drawcar_binaryclausesuccess23565
drawcar_ConditionalTrueBlock83567
	
; // Draw new ground piece
	; ----------
	; vbmSetPosition2 x, y
	lda #176
	sta vbmY
	lda #152
	sta vbmX
	jsr vbmSetPosition2
	; Assigning single variable : p1
	
	; Load Integer array
	ldx updowntile
	txa   ; watch for bug, Integer array has index range of 0 to 127
	asl
	tax
	lda updown_T,x
	ldy updown_T+1,x
	
	
	sta p1
	sty p1+1
	ldy #$0
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	; Assigning single variable : drawTree
	lda #0
	sta drawTree+#$1
drawcar_elseblock33938
drawcar_elsedoneblock46718
	
; // Draw new car sprite
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	lda y
	sta vbmY
	; x is complex
	lda x
	sta vbmX
	jsr vbmSetPosition2
	; Read address 1
	; read out the address
	lda vbmX	; x offset
	asl
	tax
	lda carbody_L,x
	sta $80
	lda carbody_L+1,x
	sta $80+1
	lda #0
	sta vbmI
	lda #8
	sta vbmT
	jsr vbmDrawSpriteSliceE
	; WARNING: Do not use if using character screen scrolling commands
	jsr vbmNextColumn
	; Read address 1
	; read out the address
	lda vbmX	; x offset
	asl
	tax
	lda carbody_M,x
	sta $80
	lda carbody_M+1,x
	sta $80+1
	lda #0
	sta vbmI
	lda #8
	sta vbmT
	jsr vbmDrawSpriteSliceE
	; WARNING: Do not use if using character screen scrolling commands
	jsr vbmNextColumn
	; Read address 1
	; read out the address
	lda vbmX	; x offset
	asl
	tax
	lda carbody_R,x
	sta $80
	lda carbody_R+1,x
	sta $80+1
	lda #0
	sta vbmI
	lda #8
	sta vbmT
	jsr vbmDrawSpriteSliceE
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	lda yl
	sta vbmY
	; x is complex
	lda x
	sta vbmX
	jsr vbmSetPosition2
	; Read address 1
	; Read address 2
	lda vbmX ; x offset 0-7
	asl ; for simplicty, storing lo, hi in one array
	tax
	lda wheel_L,x
	sta $80
	lda wheel_L+1,x
	sta $80+1
	lda wheel_R,x
	sta $82
	lda wheel_R+1,x
	sta $82+1
	jsr vbmDrawSprite8E
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	lda yr
	sta vbmY
	; x is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	clc
	adc #10
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmSetPosition2
	; Read address 1
	; Read address 2
	lda vbmX ; x offset 0-7
	asl ; for simplicty, storing lo, hi in one array
	tax
	lda wheel_L,x
	sta $80
	lda wheel_L+1,x
	sta $80+1
	lda wheel_R,x
	sta $82
	lda wheel_R+1,x
	sta $82+1
	jsr vbmDrawSprite8E
	; Assigning single variable : drawTree
	lda #0
	sta drawTree+#$0
	; Assigning single variable : oldx
	lda x
	sta oldx
	; Assigning single variable : oldy
	lda y
	sta oldy
	; Assigning single variable : oldyr
	lda yr
	sta oldyr
	; Assigning single variable : oldyl
	lda yl
	sta oldyl
	
; //screen_bg_color := BLUE + SCREEN_BG_BLACK;
; // Increase counters
	inc scnt
	; Assigning single variable : gcnt
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda gcnt
	clc
	adc #1
	 ; end add / sub var with constant
	
	and #3
	 ; end add / sub var with constant
	
	sta gcnt
	; Assigning single variable : mcnt
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda mcnt
	clc
	adc #1
	 ; end add / sub var with constant
	
	and #1
	 ; end add / sub var with constant
	
	sta mcnt
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	 jmp $eabf     ; return to normal IRQ	
	rts
block69559
	
; // restore the processor registers and complete our interrupt
	; Set special display mode for VBM bitmap graphics
	lda #19
	sta vbmNumColumns
	jsr vbmSetDisplayMode
	; Clear VBM bitmap
	lda #0
	sta vbmI ; byte to clear bitmap with
	jsr vbmClear
	lda #$93
	sta screenmemory+1
	lda #$ff
	sta screenmemory
	lda #9
	ldy #241 ; colour mem to clear (stops at zero so +1)
MainProgram_vbmCC_loop57960
	sta (screenmemory),y
	dey
	bne MainProgram_vbmCC_loop57960
	; Assigning memory location
	; Assigning single variable : $900e
	lda #45
	
	sta $900e
	; Assigning memory location
	; Assigning single variable : $900f
	lda #14
	
	sta $900f
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for13337
	; ----------
	; vbmSetPosition2 x, y
	lda #184
	sta vbmY
	; x is complex
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	
	lda i
	
	asl
	asl
	asl
	sta vbmX
	jsr vbmSetPosition2
	; Assigning single variable : p1
	
	; Load Integer array
	ldx grndtile
	txa   ; watch for bug, Integer array has index range of 0 to 127
	asl
	tax
	lda ground_T,x
	ldy ground_T+1,x
	
	
	sta p1
	sty p1+1
	ldy #$0
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	iny
	lda (p1),y
	sta (screenmemory),y
	; Assigning single variable : grndtile
	; 8 bit binop
	; Add/sub where right value is constant number
	lda grndtile
	eor #1
	 ; end add / sub var with constant
	
	sta grndtile
	inc i
	lda #20
	cmp i ;keep
	bne MainProgram_for13337
MainProgram_forLoopDone2751
	
; // draw text
	; Draw 4x8 text to the bitmap with EOR operation
	; Text to use:
	lda #<message
	sta $80
	lda #>message
	sta $80+1
	; Font characters to use:
	lda #<smallFont
	sta $82
	lda #>smallFont
	sta $82+1
	lda #6
	sta vbmX ; x position
	lda #1
	sta vbmY ; y position in pixels
	lda #6
	sta vbmI ; line height in pixels
	jsr vbmDrawSmallTextE
	lda #1
	ldx #0
MainProgram_fill24399
	sta $9400,x
	inx
	cpx #20
	bne MainProgram_fill24399
	lda #$7f
	sta $912e ; disable and acknowledge interrupts
	sta $912d
	
; // Set up an raster interrupt to call outiside visible screen,
; // params: IRQ function, raster line, PAL/NTSC(0/1)
	lda #<drawcar
	sta pointers_vic_raster+1
	lda #>drawcar
	sta pointers_vic_raster+6
	ldx #112 ; optimized, look out for bugs
	lda #0
	bne MainProgram_viarasterirq_ntsc_timing47048
	lda #$86
	sta timers_vic_raster+1
	lda #$56
	sta timers_vic_raster+3
	jsr A0_vic_raster
	jmp MainProgram_viarasterirq_end88959
MainProgram_viarasterirq_ntsc_timing47048
	lda #$43
	sta timers_vic_raster+1
	lda #$42
	sta timers_vic_raster+3
	jsr A0_vic_raster
MainProgram_viarasterirq_end88959
MainProgram_while5983
	; Binary clause: NOTEQUALS
	lda #1
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	beq MainProgram_binaryclausefailed3026
MainProgram_binaryclausesuccess21253
	lda #1; success
	jmp MainProgram_binaryclausefinished98889
MainProgram_binaryclausefailed3026
	lda #0 ; failed state
MainProgram_binaryclausefinished98889
	cmp #1
	beq MainProgram_ConditionalTrueBlock49815
	jmp MainProgram_elsedoneblock31181
MainProgram_ConditionalTrueBlock49815
	jsr callReadJoy1
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$0
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed8627
MainProgram_binaryclausesuccess36789
	lda #1; success
	jmp MainProgram_binaryclausefinished41408
MainProgram_binaryclausefailed8627
	lda #0 ; failed state
MainProgram_binaryclausefinished41408
	cmp #1
	beq MainProgram_ConditionalTrueBlock96862
	jmp MainProgram_elsedoneblock11529
MainProgram_ConditionalTrueBlock96862
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for56794
	; Binary clause Simplified: EQUALS
	; ----------
	; vbmTestPixel2 x, y  - can be used to test for multi-color mode pixels
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #176
	clc
	adc i
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	clc
	adc #8
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmTestPixel2
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock39929
MainProgram_ConditionalTrueBlock95489
	
; //screen_bg_color := BLACK + SCREEN_BG_BLACK;
; // calculate new y offset for buggy and wheels
	; Assigning single variable : yoff
	lda i
	sta yoff
MainProgram_elseblock45448
MainProgram_elsedoneblock39929
	inc i
	lda #8
	cmp i ;keep
	bne MainProgram_for56794
MainProgram_forLoopDone92301
	; Assigning single variable : y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #156
	clc
	adc yoff
	 ; end add / sub var with constant
	
	sta y
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for87482
	; Binary clause Simplified: EQUALS
	; ----------
	; vbmTestPixel2 x, y  - can be used to test for multi-color mode pixels
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #176
	clc
	adc i
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	lda x
	sta vbmX
	jsr vbmTestPixel2
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock77212
MainProgram_ConditionalTrueBlock31222
	; Assigning single variable : yoff
	lda i
	sta yoff
MainProgram_elseblock70876
MainProgram_elsedoneblock77212
	inc i
	lda #8
	cmp i ;keep
	bne MainProgram_for87482
MainProgram_forLoopDone39571
	; Assigning single variable : yl
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #167
	clc
	adc yoff
	 ; end add / sub var with constant
	
	clc
	adc wheelbump
	 ; end add / sub var with constant
	
	sta yl
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for52154
	; Binary clause Simplified: EQUALS
	; ----------
	; vbmTestPixel2 x, y  - can be used to test for multi-color mode pixels
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #176
	clc
	adc i
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	clc
	adc #16
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmTestPixel2
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock94424
MainProgram_ConditionalTrueBlock70826
	; Assigning single variable : yoff
	lda i
	sta yoff
MainProgram_elseblock71142
MainProgram_elsedoneblock94424
	inc i
	lda #8
	cmp i ;keep
	bne MainProgram_for52154
MainProgram_forLoopDone77791
	; Assigning single variable : yr
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #167
	clc
	adc yoff
	 ; end add / sub var with constant
	
	clc
	adc wheelbump
	 ; end add / sub var with constant
	
	sta yr
	; Assigning single variable : wheelbump
	; 8 bit binop
	; Add/sub where right value is constant number
	lda wheelbump
	eor #1
	 ; end add / sub var with constant
	
	sta wheelbump
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock4570
MainProgram_ConditionalTrueBlock20294
	; Assigning single variable : x
	; Optimizer: a = a +/- b
	lda x
	clc
	adc #2
	sta x
MainProgram_elseblock71509
MainProgram_elsedoneblock4570
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #8
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock87571
MainProgram_ConditionalTrueBlock49199
	; Assigning single variable : x
	; Optimizer: a = a +/- b
	lda x
	sec
	sbc #2
	sta x
MainProgram_elseblock77047
MainProgram_elsedoneblock87571
	; Binary clause Simplified: LESS
	lda x
	; Compare with pure num / var optimization
	cmp #$14;keep
	bcs MainProgram_elsedoneblock64035
MainProgram_ConditionalTrueBlock21098
	
; //if(x = oldx) then x:=x-2;
	; Assigning single variable : x
	lda #20
	sta x
MainProgram_elseblock35863
MainProgram_elsedoneblock64035
	; Binary clause Simplified: GREATER
	lda x
	; Compare with pure num / var optimization
	cmp #$78;keep
	bcc MainProgram_elsedoneblock68016
	beq MainProgram_elsedoneblock68016
MainProgram_ConditionalTrueBlock7886
	; Assigning single variable : x
	lda #120
	sta x
MainProgram_elseblock33082
MainProgram_elsedoneblock68016
	
; //screen_bg_color := BLUE + SCREEN_BG_BLACK;
	; Assigning single variable : drawTree
	lda #1
	sta drawTree+#$0
MainProgram_elseblock15659
MainProgram_elsedoneblock11529
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock41964
MainProgram_ConditionalTrueBlock18393
	
; // Set up next ground tile
	; Assigning single variable : drawTree
	lda #1
	sta drawTree+#$1
	; Assigning single variable : updowntile
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowntile
	clc
	adc updowndir
	 ; end add / sub var with constant
	
	sta updowntile
	; Binary clause: EQUALS
	; Compare with pure num / var optimization
	cmp #$7;keep
	; BC done
	bne MainProgram_tempfail34322
MainProgram_binaryclausesuccess43134
	jmp MainProgram_ConditionalTrueBlock73174
MainProgram_tempfail34322
	; Binary clause: EQUALS
	lda updowntile
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_elseblock49454
MainProgram_binaryclausesuccess94115
MainProgram_ConditionalTrueBlock73174
	; Assigning single variable : updowndir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowndir
	eor #254
	 ; end add / sub var with constant
	
	sta updowndir
MainProgram_elseblock49454
MainProgram_elsedoneblock66431
MainProgram_elseblock70515
MainProgram_elsedoneblock41964
	jmp MainProgram_while5983
MainProgram_elseblock70778
MainProgram_elsedoneblock31181
EndSymbol
EndBlock4807
	org $3000
carSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_carbody.bin"
	org $30c0
wheelSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_wheel.bin"
	org $3100
groundTile
	incbin "/Users/jartza/src/coldwar///export/sprite_ground.bin"
	org $3110
updownTile
	incbin "/Users/jartza/src/coldwar///export/sprite_updown.bin"
	org $3150
smallFont
	incbin "/Users/jartza/src/coldwar///export/font4x8.bin"
