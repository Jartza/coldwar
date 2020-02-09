 processor 6502
	ORG $1201
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $34,$36,$32,$34
	.byte    $29, $00, $00, $00
	ORG $1210
ColdWar
	jmp block88145
 ; Temp vars section
 ; Temp vars section ends
	org $2000
carbody_L	dc.w $03400, $03418, $03430, $03448
carbody_M	dc.w $03408, $03420, $03438, $03450
carbody_R	dc.w $03410, $03428, $03440, $03458
ground_T	dc.w $034a0, $034a8
updown_T	dc.w $034b0, $034b8, $034c0, $034c8, $034d0, $034d8, $034e0, $034e8
	dc.w 
wheel_L	dc.w $03460, $03470, $03480, $03490
wheel_R	dc.w $03468, $03478, $03488, $03498
drawTree	dc.b $00, $00
stars	dc.b $00, $00, $00, $00, $00, $00
scnt	dc.b	$00
gcnt	dc.b	$00
mcnt	dc.b	$00
bcnt	dc.b	$00
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
t	dc.b	$00
p1	= $64
yoff	dc.b	$00
yoff_r	dc.b	$00
yoff_l	dc.b	$00
wheelbump	dc.b	$00
state	dc.b	$00
xdir	dc.b	$00
jdir	dc.b	$00
message	
	dc.b	$03, $0f, $0c, $04, $20, $17, $01, $12, $20
	dc.b	$2d, $20, $0c, $09, $14, $14, $0c, $05, $20
	dc.b	$02, $15, $07, $07, $19, $20, $07, $01, $0d
	dc.b	$05, 0
	
	
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
	
	
	; ***********  Defining procedure : initVbmDrawSprite2E
	;    Procedure type : User-defined procedure
	
	; VBM - draw a 16x8 sprite with EOR - use vbmSetPosition first
	; Left Side = $80
	; middle = $82
	; Right side = $84
vbmDrawSprite2E
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
	bcc vbmDS2E_overflow
	inc screenmemory+1
vbmDS2E_overflow
	sta screenmemory
vbmDS2E_Middle
	; draw middle
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
	; move screenmemory to next column
	lda screenmemory
	clc
	adc #192 ; next column
	bcc vbmDS2E_overflow2
	inc screenmemory+1
vbmDS2E_overflow2
	sta screenmemory
	lda vbmX
	bne vbmDS2E_Right
	rts ; in position 0 there is no right side to draw
vbmDS2E_Right
	; draw right side
	ldy #0
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	eor (screenmemory),y
	sta (screenmemory),y
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
initeightbitmul_multiply_eightbit19724
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
	BMI initjoy1_JoySkip75108
	ORA #$02
initjoy1_JoySkip75108
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
	
	jmp initmoveto_moveto54392
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
initmoveto_moveto54392
	rts
	
	
	; ***********  Defining procedure : initrandom256
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip16954
	eor #$4d
initrandom256_RandomSkip16954
	sta Random+1
	eor $9124
	rts
	
; // Interrupt function that handles all the screen drawing
; // starting from certain raster line to avoid flicker
; //
	
	
	; ***********  Defining procedure : drawcar
	;    Procedure type : User-defined procedure
	
drawcar
	
; // Store the processors registers
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	
; //				screen_bg_color := RED + SCREEN_BG_BLACK;
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
	; Read address 2
	; Read address 3
	lda vbmX ; x offset 0-7
	asl ; for simplicty, storing lo, hi in one array
	tax
	lda carbody_L,x
	sta $80
	lda carbody_L+1,x
	sta $80+1
	lda carbody_M,x
	sta $82
	lda carbody_M+1,x
	sta $82+1
	lda carbody_R,x
	sta $84
	lda carbody_R+1,x
	sta $84+1
	jsr vbmDrawSprite2E
	
; // Clear old wheels
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda oldy
	clc
	adc oldyl
	 ; end add / sub var with constant
	
	clc
	adc #6
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda oldy
	clc
	adc oldyr
	 ; end add / sub var with constant
	
	clc
	adc #7
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
	lda scnt
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq drawcar_elsedoneblock14311
drawcar_ConditionalTrueBlock55530
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
drawcar_elseblock98964
drawcar_elsedoneblock14311
	; Binary clause: EQUALS
	lda bcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_binaryclausefailed97582
drawcar_binaryclausesuccess34464
	lda #1; success
	jmp drawcar_binaryclausefinished22241
drawcar_binaryclausefailed97582
	lda #0 ; failed state
drawcar_binaryclausefinished22241
	cmp #1
	beq drawcar_ConditionalTrueBlock95917
	jmp drawcar_elsedoneblock22718
drawcar_ConditionalTrueBlock95917
	; Assigning single variable : t
	lda #0
	; Calling STOREVARIABLE
	sta t
drawcar_for60173
	
; // Stars
	; ----------
	; vbmDrawDotE x, y  - draw with Eor
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	lda t
	; Load right hand side
	tax
	lda #10
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
	
	clc
	adc #40
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	; Load Byte array
	ldx t
	lda stars,x
	
	sta vbmX
	jsr vbmDrawDotE
	ldx t ; optimized, look out for bugs
	dec stars,x
	lda stars
	bcs drawcar_incmax18710
	lda #160
	sta stars
drawcar_incmax18710
	; ----------
	; vbmDrawDotE x, y  - draw with Eor
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	lda t
	; Load right hand side
	tax
	lda #10
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
	
	clc
	adc #40
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	; Load Byte array
	ldx t
	lda stars,x
	
	sta vbmX
	jsr vbmDrawDotE
	inc t
	lda #5
	cmp t ;keep
	bcs drawcar_for60173
drawcar_forLoopDone16487
drawcar_elseblock91451
drawcar_elsedoneblock22718
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne drawcar_elseblock18017
drawcar_binaryclausesuccess87071
	; Binary clause: EQUALS
	lda gcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_elseblock18017
drawcar_binaryclausesuccess29183
drawcar_ConditionalTrueBlock81974
	
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
	; Calling STOREVARIABLE
	sta drawTree+#$1
drawcar_elseblock18017
drawcar_elsedoneblock50575
	
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
	; Read address 2
	; Read address 3
	lda vbmX ; x offset 0-7
	asl ; for simplicty, storing lo, hi in one array
	tax
	lda carbody_L,x
	sta $80
	lda carbody_L+1,x
	sta $80+1
	lda carbody_M,x
	sta $82
	lda carbody_M+1,x
	sta $82+1
	lda carbody_R,x
	sta $84
	lda carbody_R+1,x
	sta $84+1
	jsr vbmDrawSprite2E
	
; // Draw wheels
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc yl
	 ; end add / sub var with constant
	
	clc
	adc #6
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc yr
	 ; end add / sub var with constant
	
	clc
	adc #7
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
	; Calling STOREVARIABLE
	sta drawTree+#$0
	; Assigning single variable : oldx
	lda x
	; Calling STOREVARIABLE
	sta oldx
	; Assigning single variable : oldy
	lda y
	; Calling STOREVARIABLE
	sta oldy
	; Assigning single variable : oldyr
	lda yr
	; Calling STOREVARIABLE
	sta oldyr
	; Assigning single variable : oldyl
	lda yl
	; Calling STOREVARIABLE
	sta oldyl
	
; // Increase counters
	inc scnt
	inc gcnt
	lda gcnt
	cmp #$8
	bcc drawcar_incmax32094
	lda #0
	sta gcnt
drawcar_incmax32094
	inc mcnt
	lda mcnt
	cmp #$2
	bcc drawcar_incmax7738
	lda #0
	sta mcnt
drawcar_incmax7738
	inc bcnt
	lda bcnt
	cmp #$3
	bcc drawcar_incmax52021
	lda #0
	sta bcnt
drawcar_incmax52021
	
; //				screen_bg_color := BLUE + SCREEN_BG_BLACK;
; // Restore the processor registers and complete our interrupt
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	 jmp $eabf     ; return to normal IRQ	
	rts
	
; // Limit buggy x movement
	
	
	; ***********  Defining procedure : limit_x
	;    Procedure type : User-defined procedure
	
limit_x
	; Binary clause Simplified: LESS
	lda x
	; Compare with pure num / var optimization
	cmp #$8;keep
	bcs limit_x_elsedoneblock78856
limit_x_ConditionalTrueBlock43423
	; Assigning single variable : x
	lda #8
	; Calling STOREVARIABLE
	sta x
limit_x_elseblock24462
limit_x_elsedoneblock78856
	; Binary clause Simplified: GREATER
	lda x
	; Compare with pure num / var optimization
	cmp #$78;keep
	bcc limit_x_elsedoneblock57845
	beq limit_x_elsedoneblock57845
limit_x_ConditionalTrueBlock15425
	; Assigning single variable : x
	lda #120
	; Calling STOREVARIABLE
	sta x
limit_x_elseblock69885
limit_x_elsedoneblock57845
	rts
	
; // Move buggy right
	
	
	; ***********  Defining procedure : buggy_right
	;    Procedure type : User-defined procedure
	
buggy_right
	; Assigning single variable : x
	; Optimizer: a = a +/- b
	lda x
	clc
	adc #2
	sta x
	; Assigning single variable : xdir
	lda #2
	; Calling STOREVARIABLE
	sta xdir
	rts
	
; // Move buggy left
	
	
	; ***********  Defining procedure : buggy_left
	;    Procedure type : User-defined procedure
	
buggy_left
	; Assigning single variable : x
	; Optimizer: a = a +/- b
	lda x
	sec
	sbc #2
	sta x
	; Assigning single variable : xdir
	lda #1
	; Calling STOREVARIABLE
	sta xdir
	rts
	
; // Find ground offset at x+offset
	
	
	; ***********  Defining procedure : find_ground
	;    Procedure type : User-defined procedure
	
xoffset	dc.b	
find_ground_block89001
find_ground
	; Assigning single variable : i
	lda #0
	; Calling STOREVARIABLE
	sta i
find_ground_for38533
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
	adc xoffset
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmTestPixel2
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne find_ground_elsedoneblock22398
find_ground_ConditionalTrueBlock64436
	; Assigning single variable : yoff
	lda i
	; Calling STOREVARIABLE
	sta yoff
find_ground_elseblock52656
find_ground_elsedoneblock22398
	inc i
	lda #8
	cmp i ;keep
	bne find_ground_for38533
find_ground_forLoopDone94449
	rts
block88145
	
; // Main body, handle all initialization and
; // game logic and joystick controls.
; // Initialize display mode to 19 column
	; Set special display mode for VBM bitmap graphics
	lda #19
	sta vbmNumColumns
	jsr vbmSetDisplayMode
	
; // Clear VBM
	; Clear VBM bitmap
	lda #0
	sta vbmI ; byte to clear bitmap with
	jsr vbmClear
	
; // Clear color memory with WHITE and set MULTICOLOR flag
	lda #$93
	sta screenmemory+1
	lda #$ff
	sta screenmemory
	lda #9
	ldy #241 ; colour mem to clear (stops at zero so +1)
MainProgram_vbmCC_loop6011
	sta (screenmemory),y
	dey
	bne MainProgram_vbmCC_loop6011
	
; // Set other colors, plus audio volume
	; Assigning memory location
	; Assigning single variable : $900e
	lda #45
	
	; Calling STOREVARIABLE
	sta $900e
	; Assigning memory location
	; Assigning single variable : $900f
	lda #14
	
	; Calling STOREVARIABLE
	sta $900f
	; Assigning single variable : i
	lda #0
	; Calling STOREVARIABLE
	sta i
MainProgram_for30375
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
	
	; Calling STOREVARIABLE
	sta grndtile
	inc i
	lda #20
	cmp i ;keep
	bne MainProgram_for30375
MainProgram_forLoopDone63150
	; Assigning single variable : i
	lda #0
	; Calling STOREVARIABLE
	sta i
MainProgram_for24023
	
; // Draw "stars"
	; Assigning single variable : stars
	; 8 bit binop
	; Add/sub right value is variable/expression
	jsr Random
	
	lsr
	lsr
	lsr
	
MainProgram_rightvarAddSub_var94759 = $88
	sta MainProgram_rightvarAddSub_var94759
	jsr Random
	
	lsr
	
	clc
	adc MainProgram_rightvarAddSub_var94759
	
	; Calling STOREVARIABLE
	pha
	ldx i ; optimized, look out for bugs
	pla
	sta stars,x
	; ----------
	; vbmDrawDot x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	lda i
	; Load right hand side
	tax
	lda #10
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
	
	clc
	adc #40
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	; Load Byte array
	ldx i
	lda stars,x
	
	sta vbmX
	jsr vbmDrawDot
	inc i
	lda #5
	cmp i ;keep
	bcs MainProgram_for24023
MainProgram_forLoopDone84339
	
; // Draw "title" text
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
	lda #5
	sta vbmX ; x position
	lda #1
	sta vbmY ; y position in pixels
	lda #6
	sta vbmI ; line height in pixels
	jsr vbmDrawSmallTextE
	
; // Set upper two rows to hires single color
	lda #1
	ldx #0
MainProgram_fill19443
	sta $9400,x
	inx
	cpx #20
	bne MainProgram_fill19443
	
; // Set up an raster interrupt to call outside visible screen,
; // params: IRQ function, raster line, PAL/NTSC(0/1)
	lda #$7f
	sta $912e ; disable and acknowledge interrupts
	sta $912d
	lda #<drawcar
	sta pointers_vic_raster+1
	lda #>drawcar
	sta pointers_vic_raster+6
	ldx #112 ; optimized, look out for bugs
	lda #0
	bne MainProgram_viarasterirq_ntsc_timing86294
	lda #$86
	sta timers_vic_raster+1
	lda #$56
	sta timers_vic_raster+3
	jsr A0_vic_raster
	jmp MainProgram_viarasterirq_end67185
MainProgram_viarasterirq_ntsc_timing86294
	lda #$43
	sta timers_vic_raster+1
	lda #$42
	sta timers_vic_raster+3
	jsr A0_vic_raster
MainProgram_viarasterirq_end67185
MainProgram_while17293
	; Binary clause: NOTEQUALS
	lda #1
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	beq MainProgram_binaryclausefailed32673
MainProgram_binaryclausesuccess90573
	lda #1; success
	jmp MainProgram_binaryclausefinished91621
MainProgram_binaryclausefailed32673
	lda #0 ; failed state
MainProgram_binaryclausefinished91621
	cmp #1
	beq MainProgram_ConditionalTrueBlock25862
	jmp MainProgram_elsedoneblock72463
MainProgram_ConditionalTrueBlock25862
	
; // Main game logic runs here in loop
; // Read joystick
	jsr callReadJoy1
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$0
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed52874
MainProgram_binaryclausesuccess27176
	lda #1; success
	jmp MainProgram_binaryclausefinished51378
MainProgram_binaryclausefailed52874
	lda #0 ; failed state
MainProgram_binaryclausefinished51378
MainProgram_logical_class_temp_var87327 = $88
	sta MainProgram_logical_class_temp_var87327
	; Binary clause: EQUALS
	lda mcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed73999
MainProgram_binaryclausesuccess10079
	lda #1; success
	jmp MainProgram_binaryclausefinished55313
MainProgram_binaryclausefailed73999
	lda #0 ; failed state
MainProgram_binaryclausefinished55313
	and MainProgram_logical_class_temp_var87327
MainProgram_logical_class_temp_var21780 = $88
	sta MainProgram_logical_class_temp_var21780
	; Binary clause: EQUALS
	lda state
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed16757
MainProgram_binaryclausesuccess32310
	lda #1; success
	jmp MainProgram_binaryclausefinished60631
MainProgram_binaryclausefailed16757
	lda #0 ; failed state
MainProgram_binaryclausefinished60631
	and MainProgram_logical_class_temp_var21780
	cmp #1
	beq MainProgram_ConditionalTrueBlock47436
	jmp MainProgram_elsedoneblock21380
MainProgram_ConditionalTrueBlock47436
	
; // Basic movement and wheel wobble
; //screen_bg_color := GREEN + SCREEN_BG_BLACK;
; // calculate new y offset for buggy and wheels
	; Assigning single variable : xoffset
	lda #8
	; Calling STOREVARIABLE
	sta xoffset
	jsr find_ground
	; Assigning single variable : y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #156
	clc
	adc yoff
	 ; end add / sub var with constant
	
	; Calling STOREVARIABLE
	sta y
	; Assigning single variable : xoffset
	lda #0
	; Calling STOREVARIABLE
	sta xoffset
	jsr find_ground
	; Assigning single variable : yl
	; 8 bit binop
	; Add/sub where right value is constant number
	lda yoff
	lsr
	
	clc
	adc wheelbump
	 ; end add / sub var with constant
	
	; Calling STOREVARIABLE
	sta yl
	; Assigning single variable : xoffset
	lda #16
	; Calling STOREVARIABLE
	sta xoffset
	jsr find_ground
	; Assigning single variable : yr
	; 8 bit binop
	; Add/sub where right value is constant number
	lda yoff
	lsr
	
	clc
	adc wheelbump
	 ; end add / sub var with constant
	
	; Calling STOREVARIABLE
	sta yr
	; Assigning single variable : wheelbump
	; 8 bit binop
	; Add/sub where right value is constant number
	lda wheelbump
	eor #1
	 ; end add / sub var with constant
	
	; Calling STOREVARIABLE
	sta wheelbump
	
; // Joystick handling
	; Assigning single variable : xdir
	lda #0
	; Calling STOREVARIABLE
	sta xdir
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock55927
MainProgram_ConditionalTrueBlock53411
	jsr buggy_right
MainProgram_elseblock71591
MainProgram_elsedoneblock55927
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #8
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock33351
MainProgram_ConditionalTrueBlock33343
	jsr buggy_left
MainProgram_elseblock43301
MainProgram_elsedoneblock33351
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #4
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock91783
MainProgram_ConditionalTrueBlock35113
	; Assigning single variable : state
	lda #2
	; Calling STOREVARIABLE
	sta state
MainProgram_elseblock85264
MainProgram_elsedoneblock91783
	
; //if(x = oldx) then x:=x-2;
	jsr limit_x
	
; //screen_bg_color := BLUE + SCREEN_BG_BLACK;
	; Assigning single variable : drawTree
	lda #1
	; Calling STOREVARIABLE
	sta drawTree+#$0
MainProgram_elseblock21236
MainProgram_elsedoneblock21380
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$0
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed534
MainProgram_binaryclausesuccess62604
	lda #1; success
	jmp MainProgram_binaryclausefinished68233
MainProgram_binaryclausefailed534
	lda #0 ; failed state
MainProgram_binaryclausefinished68233
MainProgram_logical_class_temp_var60604 = $88
	sta MainProgram_logical_class_temp_var60604
	; Binary clause: EQUALS
	lda mcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed68549
MainProgram_binaryclausesuccess76795
	lda #1; success
	jmp MainProgram_binaryclausefinished44966
MainProgram_binaryclausefailed68549
	lda #0 ; failed state
MainProgram_binaryclausefinished44966
	and MainProgram_logical_class_temp_var60604
MainProgram_logical_class_temp_var76815 = $88
	sta MainProgram_logical_class_temp_var76815
	; Binary clause: EQUALS
	lda state
	; Compare with pure num / var optimization
	cmp #$2;keep
	; BC done
	bne MainProgram_binaryclausefailed95748
MainProgram_binaryclausesuccess76522
	lda #1; success
	jmp MainProgram_binaryclausefinished21976
MainProgram_binaryclausefailed95748
	lda #0 ; failed state
MainProgram_binaryclausefinished21976
	and MainProgram_logical_class_temp_var76815
	cmp #1
	beq MainProgram_ConditionalTrueBlock15479
	jmp MainProgram_elsedoneblock2324
MainProgram_ConditionalTrueBlock15479
	; Binary clause Simplified: EQUALS
	lda xdir
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne MainProgram_elsedoneblock12974
MainProgram_ConditionalTrueBlock20071
	
; // Jump
	jsr buggy_right
MainProgram_elseblock92012
MainProgram_elsedoneblock12974
	; Binary clause Simplified: EQUALS
	lda xdir
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainProgram_elsedoneblock35318
MainProgram_ConditionalTrueBlock29010
	jsr buggy_left
MainProgram_elseblock45826
MainProgram_elsedoneblock35318
	jsr limit_x
	; Binary clause Simplified: EQUALS
	lda jdir
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elseblock73436
MainProgram_ConditionalTrueBlock79967
	; Assigning single variable : y
	; Optimizer: a = a +/- b
	lda y
	sec
	sbc #2
	sta y
	jmp MainProgram_elsedoneblock90031
MainProgram_elseblock73436
	; Assigning single variable : y
	; Optimizer: a = a +/- b
	lda y
	clc
	adc #2
	sta y
MainProgram_elsedoneblock90031
	; Binary clause Simplified: LESS
	lda y
	; Compare with pure num / var optimization
	cmp #$8c;keep
	bcs MainProgram_elsedoneblock83205
MainProgram_ConditionalTrueBlock7099
	; Assigning single variable : jdir
	lda #1
	; Calling STOREVARIABLE
	sta jdir
MainProgram_elseblock99861
MainProgram_elsedoneblock83205
	; Binary clause Simplified: GREATER
	lda y
	; Compare with pure num / var optimization
	cmp #$a4;keep
	bcc MainProgram_elsedoneblock95162
	beq MainProgram_elsedoneblock95162
MainProgram_ConditionalTrueBlock4459
	; Assigning single variable : jdir
	lda #0
	; Calling STOREVARIABLE
	sta jdir
	; Assigning single variable : state
	; Calling STOREVARIABLE
	sta state
MainProgram_elseblock26029
MainProgram_elsedoneblock95162
	; Assigning single variable : drawTree
	lda #1
	; Calling STOREVARIABLE
	sta drawTree+#$0
MainProgram_elseblock39450
MainProgram_elsedoneblock2324
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock85663
MainProgram_ConditionalTrueBlock11215
	
; // Set up next ground tile
	; Assigning single variable : drawTree
	lda #1
	; Calling STOREVARIABLE
	sta drawTree+#$1
	; Assigning single variable : updowntile
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowntile
	clc
	adc updowndir
	 ; end add / sub var with constant
	
	; Calling STOREVARIABLE
	sta updowntile
	; Binary clause: EQUALS
	; Compare with pure num / var optimization
	cmp #$7;keep
	; BC done
	bne MainProgram_tempfail2574
MainProgram_binaryclausesuccess45109
	jmp MainProgram_ConditionalTrueBlock96613
MainProgram_tempfail2574
	; Binary clause: EQUALS
	lda updowntile
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_elseblock9848
MainProgram_binaryclausesuccess36722
MainProgram_ConditionalTrueBlock96613
	; Assigning single variable : updowndir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowndir
	eor #254
	 ; end add / sub var with constant
	
	; Calling STOREVARIABLE
	sta updowndir
MainProgram_elseblock9848
MainProgram_elsedoneblock12726
MainProgram_elseblock11861
MainProgram_elsedoneblock85663
	jmp MainProgram_while17293
MainProgram_elseblock78671
MainProgram_elsedoneblock72463
EndSymbol
EndBlock2064
	org $3400
carSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_carbody.bin"
	org $3460
wheelSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_wheel.bin"
	org $34a0
groundTile
	incbin "/Users/jartza/src/coldwar///export/sprite_ground.bin"
	org $34b0
updownTile
	incbin "/Users/jartza/src/coldwar///export/sprite_updown.bin"
	org $34f0
smallFont
	incbin "/Users/jartza/src/coldwar///export/font4x8.bin"
