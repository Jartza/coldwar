 processor 6502
	ORG $1201
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $34,$36,$32,$34
	.byte    $29, $00, $00, $00
	ORG $1210
ColdWar
	jmp block1
 ; Temp vars section
 ; Temp vars section ends
	org $2000
carbody_L	dc.w $03000, $03018, $03030, $03048
carbody_M	dc.w $03008, $03020, $03038, $03050
carbody_R	dc.w $03010, $03028, $03040, $03058
ground_T	dc.w $030a0, $030a8
updown_T	dc.w $030b0, $030b8, $030c0, $030c8, $030d0, $030d8, $030e0, $030e8
	dc.w 
upper_T	dc.w $032f0, $032f8, $03300, $03308, $03310, $03318, $03320, $03328
	dc.w 
wheel_L	dc.w $03060, $03070, $03080, $03090
wheel_R	dc.w $03068, $03078, $03088, $03098
drawTree	dc.b $00, $00, $00
suspension	dc.b $00, $00, $09c
sus_y	dc.b	$00
sus_wy	dc.b	$02
scnt	dc.b	$00
gcnt	dc.b	$00
mcnt	dc.b	$00
bcnt	dc.b	$00
ucnt	dc.b	$00
uscnt	dc.b	$00
x	dc.b	$32
y	dc.b	$9c
y_off	dc.b	$00
wh_y	dc.b	$9c
wr_y_off	dc.b	$00
wl_y_off	dc.b	$00
old_x	dc.b	$a8
old_y	dc.b	$9c
old_y_off	dc.b	$00
old_wh_y	dc.b	$b4
old_wr_y_off	dc.b	$00
old_wl_y_off	dc.b	$00
i	dc.b	$00
t	dc.b	$00
xdir	dc.b	$00
state	dc.b	$00
p1	= $64
yoff	dc.b	$00
jdir	dc.b	$00
grndtile	dc.b	$00
updowntile	dc.b	$00
updowndir	dc.b	$01
uppertile	dc.b	$00
upperdir	dc.b	$01
scoretitle	
	dc.b	$13, $03, $0f, $12, $05, $3a, 0
score	dc.b $00, $00, $00
scoreadd	dc.b $01, $00, $00
fis1 = $36
fis2 = $37
fis3 = $38
fis4 = $39
	
	
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
	
	
	; ***********  Defining procedure : initVbmDrawSmallBCD
	;    Procedure type : User-defined procedure
	
	; Draw small text characters to the bitmap using a zero terminated CSTRING
	; Font chars = $82
	; Temp addr  = $84 - used to calculate char address
vbmDrawSmallBCDDigit
	; calculate next screen memory position
	lda vbmX
	lsr ; divde x by 2 (2 chars per character cell)
	tax
	; Work out from LSR if odd or even pattern
	bcs vbmDSBCD_Odd
	lda #$f0 ; even, use left side of font
	bcc vbmDSBCD_Even ; we know carry will be clear
vbmDSBCD_Odd
	lda #$0f ; odd, use right side of font
vbmDSBCD_Even
	sta vbmJ ; store mask to use for later
	lda vbmScrL,x   ; Address of table lo
	ldy vbmScrH,x   ; Address of table hi
	clc
	adc vbmY		; Add Y offset
	bcc vbmDSBCD_NSP_NoOverflow
	iny
vbmDSBCD_NSP_NoOverflow
	sta screenmemory
	sty screenmemory+1
vbmDSBCD_GetCharNum
	; convert text number (0-255) * 8 = memory offset
	ldy #0
	lda vbmI ; get digit to display
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
	adc $82  ; add char low address
	sta $84
	lda $84+1
	adc $82+1 ; add char high address
	sta $84+1
	lda vbmT
	pha ; store vbmT on stack for a minute
	lda vbmJ ; take mask
	eor #$ff ; and invert
	sta vbmT ; to use to blank out new char pos
vbmDSBCD_DrawDigit
	; y reg is ZERO from ldy #0 in GetTileNum
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	beq vbmDSBCD_Done // special case - on 6th line if empty skip rest
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda (screenmemory),y
	and vbmT
	sta (screenmemory),y
	lda ($84),y
	and vbmJ
	ora (screenmemory),y
	sta (screenmemory),y
vbmDSBCD_Done
	pla ; store vbmT on stack for a minute
	sta vbmT
	inc vbmX
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
	; Work out from LSR if odd or even pattern
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
	
	
	; ***********  Defining procedure : initVbmDrawSprite2
	;    Procedure type : User-defined procedure
	
	; VBM - draw a 16x8 sprite with OR - use vbmSetPosition first
	; Left Side = $80
	; middle = $82
	; Right side = $84
vbmDrawSprite2
	; draw left side
	ldy #0
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($80),y
	ora (screenmemory),y
	sta (screenmemory),y
	; move screenmemory to next column
	lda screenmemory
	clc
	adc #192 ; next column
	bcc vbmDS2_overflow
	inc screenmemory+1
vbmDS2_overflow
	sta screenmemory
vbmDS2_Middle
	; draw middle
	ldy #0
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($82),y
	ora (screenmemory),y
	sta (screenmemory),y
	; move screenmemory to next column
	lda screenmemory
	clc
	adc #192 ; next column
	bcc vbmDS2_overflow2
	inc screenmemory+1
vbmDS2_overflow2
	sta screenmemory
	lda vbmX
	bne vbmDS2_Right
	rts ; in position 0 there is no right side to draw
vbmDS2_Right
	; draw right side
	ldy #0
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
	iny
	lda ($84),y
	ora (screenmemory),y
	sta (screenmemory),y
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
initeightbitmul_multiply_eightbit2
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
joy1 = $5f
joy1last dc.b 0
joy1pressed dc.b 0
callReadJoy1
	LDA VIC20_PORTACASS
	EOR #$FF
	AND #$3C
	LDX #$7F
	SEI
	STX VIC20_PORTBVIA2d
	LDY VIC20_PORTBVIA2
	BMI initjoy1_JoySkip3
	ORA #$02
initjoy1_JoySkip3
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
	
	jmp initmoveto_moveto4
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
initmoveto_moveto4
	rts
	
	
	; ***********  Defining procedure : initrandom256
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip5
	eor #$4d
initrandom256_RandomSkip5
	sta Random+1
	eor $9124
	rts
	
; // x direction during jump
; // Game main loop state-machine state
; // Generic pointer, used for tile drawing
; // Used for y offset calculations
; // Jump direction
; // Some misc stuff, probably to be removed later
; // Include music player code
; //
; // Fisichella player zero page variables
; //
; // Fisichella player variables in tape buffer.
; // Not reserved (hardcoded in asm), just displayed here for reminder
; //
	
	
	; ***********  Defining procedure : musicplayer
	;    Procedure type : User-defined procedure
	
setstate	dc.b	
musicplayer_block6
musicplayer
	lda setstate
	cmp #$1 ;keep
	bne musicplayer_casenext8
	; 
	; ****** Inline assembler section
    jmp L1D9C
	
	jmp musicplayer_caseend7
musicplayer_casenext8
	lda setstate
	cmp #$2 ;keep
	bne musicplayer_casenext10
	; 
	; ****** Inline assembler section
    jmp L1DDD
	
	jmp musicplayer_caseend7
musicplayer_casenext10
musicplayer_caseend7
	; 
	; ****** Inline assembler section
MUSICPLAY
        dec     $03EC
        beq     L1C2F
L1C0B
        lda     #$0A
        sta     fis3
        ldx     #$00
        jsr     L1D1A
        inc     fis3
        ldx     #$04
        jsr     L1D1A
        inc     fis3
        ldx     #$08
        jsr     L1D1A
        inc     fis3
        ldx     #$0C
        jsr     L1D1A
        lda     $03EF
        bne     L1C84
        rts
L1C2F   
        lda     $03ED
        sta     $03EC
        lda     $03EA
        cmp     #$FF
        beq     L1C0B
        ldx     #$00
        jsr     L1CCD
        ldx     #$04
        jsr     L1CCD
        ldx     #$08
        jsr     L1CCD
        ldx     #$0C
        jsr     L1CCD
        inc     $03EB
        lda     $03EB
        and     #$0F
        bne     L1C0B
        sta     $03EB
        lda     $03EA
        clc
        adc     #$04
        sta     $03EA
L1C66
        tay
        lda     musicData,y
        cmp     #$FE
        beq     L1C77
        sta     $03F0
        jsr     L1DCA
        jmp     L1C0B
L1C77
        lda     musicData+1,y
        sta     $03EA
        cmp     #$FF
        bne     L1C66
        jmp     L1C0B
L1C84
        dec     $03EE
        beq     L1C8A
        rts
L1C8A
        lda     #$80
        sta     fis1
        lda     #$37
        sta     fis2
        ldy     $03EF
L1C95
        lda     (fis1),y
        ror
        ror
        ror
        ror
        and     #$0F
        sta     fis3
        lda     (fis1),y
        and     #$0F
        beq     L1CB7
        sta     $03EE
        lda     $900E
        and     #$F0
        ora     fis3
        sta     $900E
        iny
        sty     $03EF
        rts
L1CB7
        lda     fis3
        beq     L1CC9
        lda     $03EF
        and     #$F0
        ora     fis3
        sta     $03EF
        tay
        jmp     L1C95
L1CC9
        sta     $03EF
        rts
L1CCD
        ldy     $03EB
        lda     $03F0,x
        bmi     L1CE5
        sta     fis3
        and     #$70
        ora     #$80
        sta     fis1
        lda     #>musicData
        sta     fis2
        lda     (fis1),y
        bne     L1CE6
L1CE5
        rts
L1CE6
        and     #$1F
        sta     fis4
        lda     fis3
        and     #$0F
        sec
        sbc     #$08
        clc
        adc     fis4
        sta     $03F1,x
        lda     #$01
        sta     $03F2,x
        lda     (fis1),y
        ror
        and     #$70
        sta     $03F3,x
        ror
        ror
        ror
        ror
        and     #$07
        tay
        lda     musicData+392,y
        bne     L1D11
        rts
L1D11
        sta     $03EF
        lda     #$01
        sta     $03EE
        rts
L1D1A
        lda     $03F3,x
        cmp     #$FF
        beq     L1D31
        dec     $03F2,x
        beq     L1D32
        rts
L1D27
        ldy     fis3
        sta     $9000,y
        lda     #$FF
        sta     $03F3,x
L1D31
        rts
L1D32
        lda     $03F3,x
        sta     fis1
        ror
        ror
        ror
        ror
        and     #$0F
        tay
        lda     musicData+384,y
        sta     $03F2,x
L1D44
        lda     #>musicData+1
        sta     fis2
        ldy     #$00
        lda     (fis1),y
        beq     L1D27
        cmp     #$10
        bcc     L1D8A
        and     #$1F
        clc
        adc     #$10
        sta     fis4
        lda     $03F1,x
        clc
        adc     fis4
        tay
        cpy     #$40
        bcc     L1D66
        ldy     #$3F
L1D66
        cpy     #$C0
        bcc     L1D6C
        ldy     #$00
L1D6C
        lda     musicData+448,y
        sta     fis4
        ldy     #$00
        lda     (fis1),y
        rol
        rol
        rol
        rol
        and     #$07
        sec
        sbc     #$04
        clc
        adc     fis4
        ldy     fis3
        sta     $9000,y
        inc     $03F3,x
        rts
L1D8A
        and     #$0F
        sta     fis4
        lda     fis1
        and     #$F0
        ora     fis4
        sta     fis1
        sta     $03F3,x
        jmp     L1D44
L1D9C
        jsr     L1DDD
        lda     #$06
        sta     $03ED
        lda     #$01
        sta     $03EC
        lda     #$FF
        sta     $03F3
        sta     $03F7
        sta     $03FB
        sta     $03FF
        lda     $900E
        and     #$F0
        ora     #$08
        sta     $900E
        lda     musicData
        sta     $03F0
        ldy     #$00
L1DCA
        lda     musicData+1,y
        sta     $03F4
        lda     musicData+2,y
        sta     $03F8
        lda     musicData+3,y
        sta     $03FC
        rts
L1DDD
        lda     #$00
        ldy     #$15
L1DE1
        sta     $03EA,y
        cpy     #$04
        bcs     L1DEB
        sta     $900A,y
L1DEB
        dey
        bpl     L1DE1
        rts
	
	
	rts
	
; // Music player originally from Fisichella by Aleksi Eeben.
; // Player disassembled with da65 and modified to be suitable with TRSE + VBM.
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
	; 
	; ****** Inline assembler section
    jsr MUSICPLAY
	
	
; // Play song
; //				screen_bg_color := WHITE + SCREEN_BG_BLACK;
; //				screen_bg_color := BLACK + SCREEN_BG_BLACK;
; // Clear previous car sprite
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda old_y
	clc
	adc old_y_off
	 ; end add / sub var with constant
	
	clc
	adc #2
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	lda old_x
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
	lda old_wh_y
	clc
	adc old_wl_y_off
	 ; end add / sub var with constant
	
	clc
	adc #10
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	lda old_x
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
	lda old_wh_y
	clc
	adc old_wr_y_off
	 ; end add / sub var with constant
	
	clc
	adc #10
	 ; end add / sub var with constant
	sta vbmY
	; x is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda old_x
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
	
; //	if(scnt & 1) then
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
	; Binary clause Simplified: EQUALS
	lda uscnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne drawcar_elsedoneblock16
drawcar_ConditionalTrueBlock14
	; Screen Shift Left
	lda #142
	sta vbmY
	ldx #134 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Screen Shift Left
	lda #142
	sta vbmY
	ldx #134 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
drawcar_elseblock15
drawcar_elsedoneblock16
	; Binary clause Simplified: EQUALS
	lda bcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne drawcar_elsedoneblock22
drawcar_ConditionalTrueBlock20
	
; // Stars
	; Screen Shift Left
	lda #51
	sta vbmY
	ldx #50 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Screen Shift Left
	lda #71
	sta vbmY
	ldx #70 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Screen Shift Left
	lda #91
	sta vbmY
	ldx #90 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
drawcar_elseblock21
drawcar_elsedoneblock22
	; Binary clause Simplified: EQUALS
	lda mcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne drawcar_elsedoneblock28
drawcar_ConditionalTrueBlock26
	; Screen Shift Left
	lda #41
	sta vbmY
	ldx #40 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Screen Shift Left
	lda #61
	sta vbmY
	ldx #60 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
	; Screen Shift Left
	lda #81
	sta vbmY
	ldx #80 ; optimized, look out for bugs
	jsr vbmScreenShiftLeft
drawcar_elseblock27
drawcar_elsedoneblock28
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne drawcar_elseblock33
drawcar_binaryclausesuccess37
	; Binary clause: EQUALS
	lda gcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_elseblock33
drawcar_binaryclausesuccess39
drawcar_ConditionalTrueBlock32
	
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
	; Store Variable simplified optimization : right-hand term is pure
	ldx #1 ; optimized, look out for bugs
	lda #0
	sta drawTree,x
drawcar_elseblock33
drawcar_elsedoneblock34
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$2
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne drawcar_elseblock43
drawcar_binaryclausesuccess47
	; Binary clause: EQUALS
	lda ucnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_elseblock43
drawcar_binaryclausesuccess49
drawcar_ConditionalTrueBlock42
	
; // Draw new upper level piece
	; ----------
	; vbmSetPosition2 x, y
	lda #134
	sta vbmY
	lda #152
	sta vbmX
	jsr vbmSetPosition2
	; Assigning single variable : p1
	
	; Load Integer array
	ldx uppertile
	txa   ; watch for bug, Integer array has index range of 0 to 127
	asl
	tax
	lda upper_T,x
	ldy upper_T+1,x
	
	
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
	; Store Variable simplified optimization : right-hand term is pure
	ldx #2 ; optimized, look out for bugs
	lda #0
	sta drawTree,x
drawcar_elseblock43
drawcar_elsedoneblock44
	
; // Draw new car sprite
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc y_off
	 ; end add / sub var with constant
	
	clc
	adc #2
	 ; end add / sub var with constant
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
	jsr vbmDrawSprite2
	
; // Draw wheels
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda wh_y
	clc
	adc wl_y_off
	 ; end add / sub var with constant
	
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda wh_y
	clc
	adc wr_y_off
	 ; end add / sub var with constant
	
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
	; Store Variable simplified optimization : right-hand term is pure
	ldx #0 ; optimized, look out for bugs
	lda #0
	sta drawTree,x
	; Assigning single variable : old_x
	lda x
	sta old_x
	; Assigning single variable : old_y
	lda y
	sta old_y
	; Assigning single variable : old_y_off
	lda y_off
	sta old_y_off
	; Assigning single variable : old_wh_y
	lda wh_y
	sta old_wh_y
	; Assigning single variable : old_wl_y_off
	lda wl_y_off
	sta old_wl_y_off
	; Assigning single variable : old_wr_y_off
	lda wr_y_off
	sta old_wr_y_off
	
; // Increase counters
	inc scnt
	inc gcnt
	lda gcnt
	cmp #$4 ; keep
	bne drawcar_incmax53
	lda #0
	
	sta gcnt
drawcar_incmax53
	inc mcnt
	lda mcnt
	cmp #$2 ; keep
	bne drawcar_incmax55
	lda #0
	
	sta mcnt
drawcar_incmax55
	inc bcnt
	lda bcnt
	cmp #$3 ; keep
	bne drawcar_incmax57
	lda #0
	
	sta bcnt
drawcar_incmax57
	inc ucnt
	lda ucnt
	cmp #$10 ; keep
	bne drawcar_incmax59
	lda #0
	
	sta ucnt
drawcar_incmax59
	inc uscnt
	lda uscnt
	cmp #$4 ; keep
	bne drawcar_incmax61
	lda #0
	
	sta uscnt
drawcar_incmax61
	
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
	bcs limit_x_elsedoneblock66
limit_x_ConditionalTrueBlock64
	; Assigning single variable : x
	lda #8
	sta x
limit_x_elseblock65
limit_x_elsedoneblock66
	; Binary clause Simplified: GREATER
	lda x
	; Compare with pure num / var optimization
	cmp #$78;keep
	bcc limit_x_elsedoneblock72
	beq limit_x_elsedoneblock72
limit_x_ConditionalTrueBlock70
	; Assigning single variable : x
	lda #120
	sta x
limit_x_elseblock71
limit_x_elsedoneblock72
	rts
	
; // Move buggy right
	
	
	; ***********  Defining procedure : buggy_right
	;    Procedure type : User-defined procedure
	
incamount	dc.b	
buggy_right_block75
buggy_right
	; Assigning single variable : x
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	clc
	adc incamount
	 ; end add / sub var with constant
	
	sta x
	; Assigning single variable : xdir
	lda #2
	sta xdir
	rts
	
; // Move buggy left
	
	
	; ***********  Defining procedure : buggy_left
	;    Procedure type : User-defined procedure
	
decamount	dc.b	
buggy_left_block76
buggy_left
	; Assigning single variable : x
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	sec
	sbc decamount
	 ; end add / sub var with constant
	
	sta x
	; Assigning single variable : xdir
	lda #1
	sta xdir
	rts
	
; // Jump suspension
	
	
	; ***********  Defining procedure : jumpsuspend
	;    Procedure type : User-defined procedure
	
jumpsuspend
	
; // Suspension
	; Assigning single variable : suspension
	; Store Variable simplified optimization : right-hand term is pure
	ldx sus_y ; optimized, look out for bugs
	lda y
	sta suspension,x
	; Assigning single variable : wh_y
	; Load Byte array
	ldx sus_wy
	lda suspension,x
	
	sta wh_y
	inc sus_y
	lda sus_y
	cmp #$3 ; keep
	bne jumpsuspend_incmax79
	lda #0
	
	sta sus_y
jumpsuspend_incmax79
	inc sus_wy
	lda sus_wy
	cmp #$3 ; keep
	bne jumpsuspend_incmax81
	lda #0
	
	sta sus_wy
jumpsuspend_incmax81
	rts
	
; // Find ground offset at x+offset
	
	
	; ***********  Defining procedure : find_ground
	;    Procedure type : User-defined procedure
	
xoffset	dc.b	
find_ground_block82
find_ground
	; Assigning single variable : i
	lda #0
	sta i
find_ground_for83
	; Binary clause Simplified: EQUALS
	; ----------
	; vbmTestPixel2 x, y  - can be used to test for multi-color mode pixels
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #175
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
	bne find_ground_elsedoneblock96
find_ground_ConditionalTrueBlock94
	; Assigning single variable : yoff
	lda i
	sta yoff
find_ground_elseblock95
find_ground_elsedoneblock96
	inc i
	lda #8
	cmp i ;keep
	bne find_ground_for83
find_ground_forLoopDone91
	rts
block1
	
; // Main body, handle all initialization and
; // game logic and joystick controls.
; // Init music player
	; Assigning single variable : setstate
	lda #1
	sta setstate
	jsr musicplayer
	
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
MainProgram_vbmCC_loop99
	sta (screenmemory),y
	dey
	bne MainProgram_vbmCC_loop99
	
; // Set other colors, plus audio volume
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
MainProgram_for100
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
	bne MainProgram_for100
MainProgram_forLoopDone102
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for104
	
; // Draw "stars"
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
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	
	lsr
	lsr
	
	clc
	adc #8
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmDrawDot
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
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	
	lsr
	lsr
	
	clc
	adc #88
	 ; end add / sub var with constant
	sta vbmX
	jsr vbmDrawDot
	inc i
	lda #5
	cmp i ;keep
	bcs MainProgram_for104
MainProgram_forLoopDone110
	
; // Draw "title" text
	; Draw 4x8 text to the bitmap with EOR operation
	; Text to use:
	lda #<scoretitle
	sta $80
	lda #>scoretitle
	sta $80+1
	; Font characters to use:
	lda #<smallFont
	sta $82
	lda #>smallFont
	sta $82+1
	lda #0
	sta vbmX ; x position
	sta vbmY ; y position in pixels
	lda #6
	sta vbmI ; line height in pixels
	jsr vbmDrawSmallTextE
	
; // Set upper two rows to hires single color
	lda #1
	ldx #0
MainProgram_fill116
	sta $9400,x
	inx
	cpx #20
	bne MainProgram_fill116
	
; // Set up an raster interrupt to call outside visible screen,
; // params: IRQ function, raster line, PAL/NTSC(0/1)
	lda #$7f
	sta $912e ; disable and acknowledge interrupts
	sta $912d
	lda #<drawcar
	sta pointers_vic_raster+1
	lda #>drawcar
	sta pointers_vic_raster+6
	ldx #124 ; optimized, look out for bugs
	lda #0
	bne MainProgram_viarasterirq_ntsc_timing117
	lda #$86
	sta timers_vic_raster+1
	lda #$56
	sta timers_vic_raster+3
	jsr A0_vic_raster
	jmp MainProgram_viarasterirq_end118
MainProgram_viarasterirq_ntsc_timing117
	lda #$43
	sta timers_vic_raster+1
	lda #$42
	sta timers_vic_raster+3
	jsr A0_vic_raster
MainProgram_viarasterirq_end118
MainProgram_while119
	; Full binary clause
	; Binary clause: NOTEQUALS
	lda #1
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	beq MainProgram_binaryclausefailed306
MainProgram_binaryclausesuccess308
	lda #1; success
	jmp MainProgram_binaryclausefinished307
MainProgram_binaryclausefailed306
	lda #0 ; failed state
MainProgram_binaryclausefinished307
	cmp #1
	beq MainProgram_ConditionalTrueBlock120
	jmp MainProgram_elsedoneblock122
MainProgram_ConditionalTrueBlock120
	
; // Main game logic runs here in loop
; // Read joystick
	jsr callReadJoy1
	; Full binary clause
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$0
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed369
MainProgram_binaryclausesuccess371
	lda #1; success
	jmp MainProgram_binaryclausefinished370
MainProgram_binaryclausefailed369
	lda #0 ; failed state
MainProgram_binaryclausefinished370
MainProgram_logical_class_temp_var372 = $88
	sta MainProgram_logical_class_temp_var372
	; Binary clause: EQUALS
	lda mcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed373
MainProgram_binaryclausesuccess375
	lda #1; success
	jmp MainProgram_binaryclausefinished374
MainProgram_binaryclausefailed373
	lda #0 ; failed state
MainProgram_binaryclausefinished374
	and MainProgram_logical_class_temp_var372
	cmp #1
	beq MainProgram_ConditionalTrueBlock311
	jmp MainProgram_elsedoneblock313
MainProgram_ConditionalTrueBlock311
	lda state
	cmp #$0 ;keep
	bne MainProgram_casenext378
	
; // Handle buggy drawing and moving when IRQ says the car has
; // been redrawn
; // Stationery
	; Assigning single variable : xdir
	lda #0
	sta xdir
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock383
MainProgram_ConditionalTrueBlock381
	; Assigning single variable : incamount
	lda #2
	sta incamount
	jsr buggy_right
MainProgram_elseblock382
MainProgram_elsedoneblock383
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #8
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock389
MainProgram_ConditionalTrueBlock387
	; Assigning single variable : decamount
	lda #2
	sta decamount
	jsr buggy_left
MainProgram_elseblock388
MainProgram_elsedoneblock389
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #2
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock395
MainProgram_ConditionalTrueBlock393
	; Assigning single variable : state
	lda #2
	sta state
MainProgram_elseblock394
MainProgram_elsedoneblock395
	; Assigning single variable : drawTree
	; Store Variable simplified optimization : right-hand term is pure
	ldx #0 ; optimized, look out for bugs
	lda #1
	sta drawTree,x
	jmp MainProgram_caseend377
MainProgram_casenext378
	lda state
	cmp #$2 ;keep
	bne MainProgram_casenext398
	; Binary clause Simplified: EQUALS
	lda xdir
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne MainProgram_elsedoneblock403
MainProgram_ConditionalTrueBlock401
	
; // In jump
	; Assigning single variable : incamount
	lda #1
	sta incamount
	jsr buggy_right
MainProgram_elseblock402
MainProgram_elsedoneblock403
	; Binary clause Simplified: EQUALS
	lda xdir
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainProgram_elsedoneblock409
MainProgram_ConditionalTrueBlock407
	; Assigning single variable : decamount
	lda #1
	sta decamount
	jsr buggy_left
MainProgram_elseblock408
MainProgram_elsedoneblock409
	; Binary clause Simplified: EQUALS
	lda jdir
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elseblock414
MainProgram_ConditionalTrueBlock413
	; Assigning single variable : y
	; Optimizer: a = a +/- b
	lda y
	sec
	sbc #2
	sta y
	jmp MainProgram_elsedoneblock415
MainProgram_elseblock414
	; Assigning single variable : y
	; Optimizer: a = a +/- b
	lda y
	clc
	adc #2
	sta y
MainProgram_elsedoneblock415
	; Binary clause Simplified: LESS
	lda y
	; Compare with pure num / var optimization
	cmp #$6e;keep
	bcs MainProgram_elsedoneblock422
MainProgram_ConditionalTrueBlock420
	; Assigning single variable : jdir
	lda #1
	sta jdir
MainProgram_elseblock421
MainProgram_elsedoneblock422
	; Binary clause Simplified: EQUALS
	lda y
	; Compare with pure num / var optimization
	cmp #$9c;keep
	bne MainProgram_elsedoneblock428
MainProgram_ConditionalTrueBlock426
	; Assigning single variable : jdir
	lda #0
	sta jdir
	; Assigning single variable : state
	sta state
MainProgram_elseblock427
MainProgram_elsedoneblock428
	; Assigning single variable : drawTree
	; Store Variable simplified optimization : right-hand term is pure
	ldx #0 ; optimized, look out for bugs
	lda #1
	sta drawTree,x
	jmp MainProgram_caseend377
MainProgram_casenext398
MainProgram_caseend377
	
; // make buggy stay inside limits			
	jsr limit_x
	
; // calculate new y offset for buggy and wheels
	; Assigning single variable : xoffset
	lda #8
	sta xoffset
	jsr find_ground
	; Assigning single variable : y_off
	lda yoff
	sta y_off
	; Assigning single variable : xoffset
	lda #2
	sta xoffset
	jsr find_ground
	; Assigning single variable : wl_y_off
	lda yoff
	sta wl_y_off
	; Assigning single variable : xoffset
	lda #12
	sta xoffset
	jsr find_ground
	; Assigning single variable : wr_y_off
	lda yoff
	sta wr_y_off
	jsr jumpsuspend
MainProgram_elseblock312
MainProgram_elsedoneblock313
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock434
MainProgram_ConditionalTrueBlock432
	
; // Set up next ground tile when IRQ says ground needs new
; // tile
	; Assigning single variable : drawTree
	; Store Variable simplified optimization : right-hand term is pure
	ldx #1 ; optimized, look out for bugs
	lda #1
	sta drawTree,x
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
	bne MainProgram_tempfail453
MainProgram_binaryclausesuccess455
	jmp MainProgram_ConditionalTrueBlock449
MainProgram_tempfail453
	; Binary clause: EQUALS
	lda updowntile
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_elseblock450
MainProgram_binaryclausesuccess457
MainProgram_ConditionalTrueBlock449
	; Assigning single variable : updowndir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowndir
	eor #254
	 ; end add / sub var with constant
	
	sta updowndir
MainProgram_elseblock450
MainProgram_elsedoneblock451
MainProgram_elseblock433
MainProgram_elsedoneblock434
	; Full binary clause
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$2
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed476
MainProgram_binaryclausesuccess478
	lda #1; success
	jmp MainProgram_binaryclausefinished477
MainProgram_binaryclausefailed476
	lda #0 ; failed state
MainProgram_binaryclausefinished477
	cmp #1
	beq MainProgram_ConditionalTrueBlock460
	jmp MainProgram_elsedoneblock462
MainProgram_ConditionalTrueBlock460
	
; // Set up next upper level tile when IRQ says ground needs new
; // tile
	; Assigning single variable : uppertile
	; 8 bit binop
	; Add/sub where right value is constant number
	lda uppertile
	clc
	adc upperdir
	 ; end add / sub var with constant
	
	sta uppertile
	; Binary clause: EQUALS
	; Compare with pure num / var optimization
	cmp #$7;keep
	; BC done
	bne MainProgram_tempfail485
MainProgram_binaryclausesuccess487
	jmp MainProgram_ConditionalTrueBlock481
MainProgram_tempfail485
	; Binary clause: EQUALS
	lda uppertile
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_elseblock482
MainProgram_binaryclausesuccess489
MainProgram_ConditionalTrueBlock481
	; Assigning single variable : upperdir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda upperdir
	eor #254
	 ; end add / sub var with constant
	
	sta upperdir
MainProgram_elseblock482
MainProgram_elsedoneblock483
	; ----------
	; BcdAdd address, address, number
	sed
	clc
	lda score
	adc scoreadd
	sta score
	lda score+$01
	adc scoreadd+$01
	sta score+$01
	lda score+$02
	adc scoreadd+$02
	sta score+$02
	cld
	; ----------
	; VBM DrawSBCD BCD array, Font, X, Y, number of BCD bytes
	; Font characters to use:
	lda #<$3270
	sta $82
	lda #>$3270
	sta $82+1
	lda #6
	sta vbmX ; x position
	lda #0
	sta vbmY ; y position in pixels
	lda #$02 ; BCD array - highest byte (in reverse order)
	sta vbmT
MainProgram_vbmDrawSBCDloop491
	ldx vbmT
	lda score,x
	pha
	lsr ; get high nibble
	lsr
	lsr
	lsr
	sta vbmI ; digit to display
	jsr vbmDrawSmallBCDDigit
	pla
	and #$0f ; get low nibble
	sta vbmI ; digit to display
	jsr vbmDrawSmallBCDDigit
	dec vbmT
	bpl MainProgram_vbmDrawSBCDloop491 ; loop until all bytes displayed
	; Assigning single variable : drawTree
	; Store Variable simplified optimization : right-hand term is pure
	ldx #2 ; optimized, look out for bugs
	lda #1
	sta drawTree,x
MainProgram_elseblock461
MainProgram_elsedoneblock462
	jmp MainProgram_while119
MainProgram_elseblock121
MainProgram_elsedoneblock122
EndSymbol
EndBlock6919
	org $3000
carSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_carbody.bin"
	org $3060
wheelSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_wheel.bin"
	org $30a0
groundTile
	incbin "/Users/jartza/src/coldwar///export/sprite_ground.bin"
	org $30b0
updownTile
	incbin "/Users/jartza/src/coldwar///export/sprite_updown.bin"
	org $30f0
smallFont
	incbin "/Users/jartza/src/coldwar///export/font4x8.bin"
	org $32f0
upperLevel
	incbin "/Users/jartza/src/coldwar///export/sprite_upperlevel.bin"
	org $3600
musicData
	incbin "/Users/jartza/src/coldwar///export/imono.bin"
