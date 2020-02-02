 processor 6502
	ORG $1201
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $34,$36,$32,$34
	.byte    $29, $00, $00, $00
	ORG $1210
ColdWar
	jmp block94813
 ; Temp vars section
 ; Temp vars section ends
	org $2000
carbody_L	dc.w $03800, $03830, $03860, $03890
carbody_M	dc.w $03810, $03840, $03870, $038a0
carbody_R	dc.w $03820, $03850, $03880, $038b0
ground_T	dc.w $03900, $03908
updown_T	dc.w $03910, $03918, $03920, $03928, $03930, $03938, $03940, $03948
	dc.w 
wheel_L	dc.w $038c0, $038d0, $038e0, $038f0
wheel_R	dc.w $038c8, $038d8, $038e8, $038f8
drawTree	dc.b $00, $00
x	dc.b	$14
y	dc.b	$9c
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
scroffset	dc.b	$00
yoff	dc.b	$00
yoff_r	dc.b	$00
yoff_l	dc.b	$00
	
	
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
	
	
	; ***********  Defining procedure : initVbmDot
	;    Procedure type : User-defined procedure
	
	; VBM Dot mask
vbmDotBit    dc.b $80, $40, $20, $10, $08, $04, $02, $01
vbmDrawDot
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
	and #7   ; find offset for dot
	tax
	lda vbmDotBit,x   ; get dot pattern
	ldy vbmY  ; draw dot in row
	ora (screenmemory),y
	sta (screenmemory),y
	rts
vbmClearDot
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
	and #7   ; find offset for dot
	tax
	lda vbmDotBit,x   ; get dot pattern
	eor #$ff  ; invert it
	ldy vbmY  ; clear dot in row
	and (screenmemory),y
	sta (screenmemory),y
	rts
vbmDrawDotE
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
	and #7   ; find offset for dot
	tax
	lda vbmDotBit,x   ; get dot pattern
	ldy vbmY  ; draw dot in row
	eor (screenmemory),y
	sta (screenmemory),y
	rts
vbmTestPixel
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
	and #7   ; find offset for dot
	tax
	lda vbmDotBit,x   ; get dot pattern
	ldy vbmY  ; get row
	and (screenmemory),y ; AND with screenmemory to get pixel value
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
initeightbitmul_multiply_eightbit29057
	rts
	
	
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	jmp initmoveto_moveto14276
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
initmoveto_moveto14276
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
	; Assigning single variable : scroffset
	inc scroffset
	
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda oldy
	clc
	adc #10
	 ; end add / sub var with constant
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda oldy
	clc
	adc #10
	 ; end add / sub var with constant
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
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroffset
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq drawcar_elsedoneblock55864
drawcar_ConditionalTrueBlock25778
	
; // Scroll the landscape
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
drawcar_elseblock532
drawcar_elsedoneblock55864
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne drawcar_elseblock95361
drawcar_binaryclausesuccess99025
	; Binary clause: EQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroffset
	and #7
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_elseblock95361
drawcar_binaryclausesuccess29121
drawcar_ConditionalTrueBlock39479
	
; // Draw ground
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
drawcar_elseblock95361
drawcar_elsedoneblock14788
	
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc #10
	 ; end add / sub var with constant
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc #10
	 ; end add / sub var with constant
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
	
; //screen_bg_color := BLUE + SCREEN_BG_BLACK;
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	 jmp $eabf     ; return to normal IRQ	
	rts
block94813
	
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
MainProgram_vbmCC_loop97281
	sta (screenmemory),y
	dey
	bne MainProgram_vbmCC_loop97281
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
MainProgram_for27618
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
	bne MainProgram_for27618
MainProgram_forLoopDone64312
	lda #$7f
	sta $912e ; disable and acknowledge interrupts
	sta $912d
	
; // Set up an raster interrupt to call outiside visible screen,
; // params: IRQ function, raster line, PAL/NTSC(0/1)
	lda #<drawcar
	sta pointers_vic_raster+1
	lda #>drawcar
	sta pointers_vic_raster+6
	ldx #110 ; optimized, look out for bugs
	lda #0
	bne MainProgram_viarasterirq_ntsc_timing25119
	lda #$86
	sta timers_vic_raster+1
	lda #$56
	sta timers_vic_raster+3
	jsr A0_vic_raster
	jmp MainProgram_viarasterirq_end86748
MainProgram_viarasterirq_ntsc_timing25119
	lda #$43
	sta timers_vic_raster+1
	lda #$42
	sta timers_vic_raster+3
	jsr A0_vic_raster
MainProgram_viarasterirq_end86748
MainProgram_while9055
	; Binary clause: NOTEQUALS
	lda #1
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	beq MainProgram_binaryclausefailed48910
MainProgram_binaryclausesuccess12246
	lda #1; success
	jmp MainProgram_binaryclausefinished93438
MainProgram_binaryclausefailed48910
	lda #0 ; failed state
MainProgram_binaryclausefinished93438
	cmp #1
	beq MainProgram_ConditionalTrueBlock51692
	jmp MainProgram_elsedoneblock55758
MainProgram_ConditionalTrueBlock51692
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$0
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed88614
MainProgram_binaryclausesuccess37250
	lda #1; success
	jmp MainProgram_binaryclausefinished93124
MainProgram_binaryclausefailed88614
	lda #0 ; failed state
MainProgram_binaryclausefinished93124
MainProgram_logical_class_temp_var66711 = $88
	sta MainProgram_logical_class_temp_var66711
	; Binary clause: EQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroffset
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed37809
MainProgram_binaryclausesuccess41544
	lda #1; success
	jmp MainProgram_binaryclausefinished82538
MainProgram_binaryclausefailed37809
	lda #0 ; failed state
MainProgram_binaryclausefinished82538
	and MainProgram_logical_class_temp_var66711
	cmp #1
	beq MainProgram_ConditionalTrueBlock52461
	jmp MainProgram_elsedoneblock69728
MainProgram_ConditionalTrueBlock52461
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for22969
	; Binary clause Simplified: GREATER
	; ----------
	; vbmTestPixel x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #184
	sec
	sbc i
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
	jsr vbmTestPixel ; returns A = 1 or 0
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcc MainProgram_elsedoneblock76349
	beq MainProgram_elsedoneblock76349
MainProgram_ConditionalTrueBlock34894
	
; // calculate new y offset for buggy and wheels
	; Assigning single variable : yoff
	lda i
	sta yoff
MainProgram_elseblock64216
MainProgram_elsedoneblock76349
	inc i
	lda #8
	cmp i ;keep
	bne MainProgram_for22969
MainProgram_forLoopDone91456
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for6548
	; Binary clause Simplified: GREATER
	; ----------
	; vbmTestPixel x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #184
	sec
	sbc i
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	lda x
	sta vbmX
	jsr vbmTestPixel ; returns A = 1 or 0
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcc MainProgram_elsedoneblock92564
	beq MainProgram_elsedoneblock92564
MainProgram_ConditionalTrueBlock84524
	; Assigning single variable : yoff_l
	lda i
	sta yoff_l
MainProgram_elseblock40925
MainProgram_elsedoneblock92564
	inc i
	lda #8
	cmp i ;keep
	bne MainProgram_for6548
MainProgram_forLoopDone73181
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for9415
	; Binary clause Simplified: GREATER
	; ----------
	; vbmTestPixel x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #184
	sec
	sbc i
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
	jsr vbmTestPixel ; returns A = 1 or 0
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcc MainProgram_elsedoneblock89154
	beq MainProgram_elsedoneblock89154
MainProgram_ConditionalTrueBlock84140
	; Assigning single variable : yoff_r
	lda i
	sta yoff_r
MainProgram_elseblock35761
MainProgram_elsedoneblock89154
	inc i
	lda #8
	cmp i ;keep
	bne MainProgram_for9415
MainProgram_forLoopDone80146
	; Assigning single variable : y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #164
	sec
	sbc yoff
	 ; end add / sub var with constant
	
	sta y
	; Assigning single variable : x
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	clc
	adc dir
	 ; end add / sub var with constant
	
	sta x
	; Binary clause: EQUALS
	; Compare with pure num / var optimization
	cmp #$14;keep
	; BC done
	bne MainProgram_tempfail7852
MainProgram_binaryclausesuccess26639
	jmp MainProgram_ConditionalTrueBlock56586
MainProgram_tempfail7852
	; Binary clause: EQUALS
	lda x
	; Compare with pure num / var optimization
	cmp #$6e;keep
	; BC done
	bne MainProgram_elseblock68639
MainProgram_binaryclausesuccess35579
MainProgram_ConditionalTrueBlock56586
	; Assigning single variable : dir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda dir
	eor #252
	 ; end add / sub var with constant
	
	sta dir
MainProgram_elseblock68639
MainProgram_elsedoneblock49014
	; Assigning single variable : drawTree
	lda #1
	sta drawTree+#$0
MainProgram_elseblock17869
MainProgram_elsedoneblock69728
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock35449
MainProgram_ConditionalTrueBlock6280
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
	bne MainProgram_tempfail22815
MainProgram_binaryclausesuccess26505
	jmp MainProgram_ConditionalTrueBlock53575
MainProgram_tempfail22815
	; Binary clause: EQUALS
	lda updowntile
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_elseblock29044
MainProgram_binaryclausesuccess70217
MainProgram_ConditionalTrueBlock53575
	; Assigning single variable : updowndir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowndir
	eor #254
	 ; end add / sub var with constant
	
	sta updowndir
MainProgram_elseblock29044
MainProgram_elsedoneblock89088
MainProgram_elseblock33562
MainProgram_elsedoneblock35449
	jmp MainProgram_while9055
MainProgram_elseblock42387
MainProgram_elsedoneblock55758
EndSymbol
EndBlock6600
	org $3800
carSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_carbody.bin"
	org $38c0
wheelSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_wheel.bin"
	org $3900
groundTile
	incbin "/Users/jartza/src/coldwar///export/sprite_ground.bin"
	org $3910
updownTile
	incbin "/Users/jartza/src/coldwar///export/sprite_updown.bin"
