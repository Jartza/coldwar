 processor 6502
	ORG $1201
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $34,$36,$32,$34
	.byte    $29, $00, $00, $00
	ORG $1210
ColdWar
	jmp block86431
 ; Temp vars section
 ; Temp vars section ends
	org $2000
carbody_L	dc.w $02d00, $02d18, $02d30, $02d48
carbody_M	dc.w $02d08, $02d20, $02d38, $02d50
carbody_R	dc.w $02d10, $02d28, $02d40, $02d58
ground_T	dc.w $02da0, $02da8
updown_T	dc.w $02db0, $02db8, $02dc0, $02dc8, $02dd0, $02dd8, $02de0, $02de8
	dc.w 
wheel_L	dc.w $02d60, $02d70, $02d80, $02d90
wheel_R	dc.w $02d68, $02d78, $02d88, $02d98
drawTree	dc.b $00, $00
suspension	dc.b $00, $00, $00, $09c
sus_y	dc.b	$00
sus_wy	dc.b	$03
scnt	dc.b	$00
gcnt	dc.b	$00
mcnt	dc.b	$00
bcnt	dc.b	$00
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
lag	dc.b	$00
dir	dc.b	$02
clr	dc.b	$00
grndtile	dc.b	$00
updowntile	dc.b	$00
updowndir	dc.b	$01
wheelbump	dc.b	$00
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
initeightbitmul_multiply_eightbit72238
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
	BMI initjoy1_JoySkip51293
	ORA #$02
initjoy1_JoySkip51293
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
	
	jmp initmoveto_moveto33165
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
initmoveto_moveto33165
	rts
	
	
	; ***********  Defining procedure : initrandom256
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip1965
	eor #$4d
initrandom256_RandomSkip1965
	sta Random+1
	eor $9124
	rts
	
; // x direction during jump
; // Game main loop state-machine state
; // Generic pointer, used for tile drawing
; // Used for y offset calculations
; // Jump direction
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda old_y
	clc
	adc old_y_off
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
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scnt
	and #1
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq drawcar_elsedoneblock84869
drawcar_ConditionalTrueBlock36591
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
drawcar_elseblock96113
drawcar_elsedoneblock84869
	; Binary clause Simplified: EQUALS
	lda bcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne drawcar_elsedoneblock83791
drawcar_ConditionalTrueBlock99237
	
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
drawcar_elseblock92862
drawcar_elsedoneblock83791
	; Binary clause Simplified: EQUALS
	lda mcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne drawcar_elsedoneblock71074
drawcar_ConditionalTrueBlock68550
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
drawcar_elseblock41375
drawcar_elsedoneblock71074
	; Binary clause: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne drawcar_elseblock37177
drawcar_binaryclausesuccess57548
	; Binary clause: EQUALS
	lda gcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne drawcar_elseblock37177
drawcar_binaryclausesuccess47144
drawcar_ConditionalTrueBlock37433
	
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
drawcar_elseblock37177
drawcar_elsedoneblock41986
	
; // Draw new car sprite
	; ----------
	; vbmSetPosition2 x, y
	; y is complex
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc y_off
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
	bne drawcar_incmax73072
	lda #0
	sta gcnt
drawcar_incmax73072
	inc mcnt
	lda mcnt
	cmp #$2 ; keep
	bne drawcar_incmax24497
	lda #0
	sta mcnt
drawcar_incmax24497
	inc bcnt
	lda bcnt
	cmp #$3 ; keep
	bne drawcar_incmax99537
	lda #0
	sta bcnt
drawcar_incmax99537
	
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
	bcs limit_x_elsedoneblock80491
limit_x_ConditionalTrueBlock6230
	; Assigning single variable : x
	lda #8
	sta x
limit_x_elseblock78729
limit_x_elsedoneblock80491
	; Binary clause Simplified: GREATER
	lda x
	; Compare with pure num / var optimization
	cmp #$78;keep
	bcc limit_x_elsedoneblock81407
	beq limit_x_elsedoneblock81407
limit_x_ConditionalTrueBlock55114
	; Assigning single variable : x
	lda #120
	sta x
limit_x_elseblock26834
limit_x_elsedoneblock81407
	rts
	
; // Move buggy right
	
	
	; ***********  Defining procedure : buggy_right
	;    Procedure type : User-defined procedure
	
incamount	dc.b	
buggy_right_block54081
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
buggy_left_block56709
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
	
; // Calculate wheel offset difference to car body
	
	
	; ***********  Defining procedure : wheeldiff
	;    Procedure type : User-defined procedure
	
wheeldiff
	; Binary clause Simplified: GREATER
	lda wh_y
	; Compare with pure num / var optimization
	cmp y;keep
	bcc wheeldiff_elseblock23913
	beq wheeldiff_elseblock23913
wheeldiff_ConditionalTrueBlock77321
	; Binary clause Simplified: GREATER
	; 8 bit binop
	; Add/sub where right value is constant number
	lda wh_y
	sec
	sbc y
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$3;keep
	bcc wheeldiff_elsedoneblock25501
	beq wheeldiff_elsedoneblock25501
wheeldiff_ConditionalTrueBlock79851
	; Assigning single variable : wh_y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc #3
	 ; end add / sub var with constant
	
	sta wh_y
wheeldiff_elseblock60229
wheeldiff_elsedoneblock25501
	jmp wheeldiff_elsedoneblock21124
wheeldiff_elseblock23913
	; Binary clause Simplified: GREATER
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	sec
	sbc wh_y
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$3;keep
	bcc wheeldiff_elsedoneblock97735
	beq wheeldiff_elsedoneblock97735
wheeldiff_ConditionalTrueBlock538
	; Assigning single variable : wh_y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc #3
	 ; end add / sub var with constant
	
	sta wh_y
wheeldiff_elseblock14263
wheeldiff_elsedoneblock97735
wheeldiff_elsedoneblock21124
	rts
	
; // Find ground offset at x+offset
	
	
	; ***********  Defining procedure : find_ground
	;    Procedure type : User-defined procedure
	
xoffset	dc.b	
find_ground_block54437
find_ground
	; Assigning single variable : i
	lda #0
	sta i
find_ground_for17794
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
	bne find_ground_elsedoneblock84092
find_ground_ConditionalTrueBlock13180
	; Assigning single variable : yoff
	lda i
	sta yoff
find_ground_elseblock11006
find_ground_elsedoneblock84092
	inc i
	lda #8
	cmp i ;keep
	bne find_ground_for17794
find_ground_forLoopDone55125
	rts
block86431
	
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
MainProgram_vbmCC_loop2127
	sta (screenmemory),y
	dey
	bne MainProgram_vbmCC_loop2127
	
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
MainProgram_for91567
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
	bne MainProgram_for91567
MainProgram_forLoopDone50108
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for32396
	
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
	bcs MainProgram_for32396
MainProgram_forLoopDone57572
	
; // Test
	; Assigning single variable : t
	lda #100
	sta t
	; Assigning single variable : i
	lda #0
	sta i
MainProgram_for42400
	; ----------
	; vbmDrawDot x, y
	lda #12
	sta vbmY
	; x is complex
	lda t
	sta vbmX
	jsr vbmDrawDot
	dec t
	lda t
	cmp #$ff ; keep
	bne MainProgram_incmax8308
	lda #159
	sta t
MainProgram_incmax8308
	inc i
	lda #255
	cmp i ;keep
	bne MainProgram_for42400
MainProgram_forLoopDone34783
	
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
MainProgram_fill58288
	sta $9400,x
	inx
	cpx #20
	bne MainProgram_fill58288
	
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
	bne MainProgram_viarasterirq_ntsc_timing92688
	lda #$86
	sta timers_vic_raster+1
	lda #$56
	sta timers_vic_raster+3
	jsr A0_vic_raster
	jmp MainProgram_viarasterirq_end64184
MainProgram_viarasterirq_ntsc_timing92688
	lda #$43
	sta timers_vic_raster+1
	lda #$42
	sta timers_vic_raster+3
	jsr A0_vic_raster
MainProgram_viarasterirq_end64184
MainProgram_while30235
	; Full binary clause
	; Binary clause: NOTEQUALS
	lda #1
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	beq MainProgram_binaryclausefailed32584
MainProgram_binaryclausesuccess23435
	lda #1; success
	jmp MainProgram_binaryclausefinished64689
MainProgram_binaryclausefailed32584
	lda #0 ; failed state
MainProgram_binaryclausefinished64689
	cmp #1
	beq MainProgram_ConditionalTrueBlock37769
	jmp MainProgram_elsedoneblock69042
MainProgram_ConditionalTrueBlock37769
	
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
	bne MainProgram_binaryclausefailed99620
MainProgram_binaryclausesuccess98096
	lda #1; success
	jmp MainProgram_binaryclausefinished99373
MainProgram_binaryclausefailed99620
	lda #0 ; failed state
MainProgram_binaryclausefinished99373
MainProgram_logical_class_temp_var48642 = $88
	sta MainProgram_logical_class_temp_var48642
	; Binary clause: EQUALS
	lda mcnt
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_binaryclausefailed58026
MainProgram_binaryclausesuccess90130
	lda #1; success
	jmp MainProgram_binaryclausefinished74279
MainProgram_binaryclausefailed58026
	lda #0 ; failed state
MainProgram_binaryclausefinished74279
	and MainProgram_logical_class_temp_var48642
	cmp #1
	beq MainProgram_ConditionalTrueBlock33311
	jmp MainProgram_elsedoneblock67684
MainProgram_ConditionalTrueBlock33311
	lda state
	cmp #$0 ;keep
	bne MainProgram_casenext52771
	
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
	beq MainProgram_elsedoneblock96076
MainProgram_ConditionalTrueBlock50692
	; Assigning single variable : incamount
	lda #2
	sta incamount
	jsr buggy_right
MainProgram_elseblock44351
MainProgram_elsedoneblock96076
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #8
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock24830
MainProgram_ConditionalTrueBlock61127
	; Assigning single variable : decamount
	lda #2
	sta decamount
	jsr buggy_left
MainProgram_elseblock97535
MainProgram_elsedoneblock24830
	; Binary clause Simplified: NOTEQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy1
	and #4
	 ; end add / sub var with constant
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock44983
MainProgram_ConditionalTrueBlock26695
	; Assigning single variable : state
	lda #2
	sta state
MainProgram_elseblock86126
MainProgram_elsedoneblock44983
	; Assigning single variable : wheelbump
	; 8 bit binop
	; Add/sub where right value is constant number
	lda wheelbump
	eor #1
	 ; end add / sub var with constant
	
	sta wheelbump
	; Assigning single variable : drawTree
	; Store Variable simplified optimization : right-hand term is pure
	ldx #0 ; optimized, look out for bugs
	lda #1
	sta drawTree,x
	jmp MainProgram_caseend11307
MainProgram_casenext52771
	lda state
	cmp #$2 ;keep
	bne MainProgram_casenext8171
	; Binary clause Simplified: EQUALS
	lda xdir
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne MainProgram_elsedoneblock70440
MainProgram_ConditionalTrueBlock38751
	
; // In jump
	; Assigning single variable : incamount
	lda #1
	sta incamount
	jsr buggy_right
MainProgram_elseblock28352
MainProgram_elsedoneblock70440
	; Binary clause Simplified: EQUALS
	lda xdir
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainProgram_elsedoneblock47034
MainProgram_ConditionalTrueBlock28659
	; Assigning single variable : decamount
	lda #1
	sta decamount
	jsr buggy_left
MainProgram_elseblock78463
MainProgram_elsedoneblock47034
	; Binary clause Simplified: EQUALS
	lda jdir
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elseblock33981
MainProgram_ConditionalTrueBlock45702
	; Assigning single variable : y
	; Optimizer: a = a +/- b
	lda y
	sec
	sbc #2
	sta y
	jmp MainProgram_elsedoneblock41212
MainProgram_elseblock33981
	; Assigning single variable : y
	; Optimizer: a = a +/- b
	lda y
	clc
	adc #2
	sta y
MainProgram_elsedoneblock41212
	; Binary clause Simplified: LESS
	lda y
	; Compare with pure num / var optimization
	cmp #$78;keep
	bcs MainProgram_elsedoneblock56657
MainProgram_ConditionalTrueBlock80965
	; Assigning single variable : jdir
	lda #1
	sta jdir
MainProgram_elseblock45383
MainProgram_elsedoneblock56657
	; Binary clause Simplified: EQUALS
	lda y
	; Compare with pure num / var optimization
	cmp #$9c;keep
	bne MainProgram_elsedoneblock60608
MainProgram_ConditionalTrueBlock10065
	; Assigning single variable : jdir
	lda #0
	sta jdir
	; Assigning single variable : state
	sta state
MainProgram_elseblock8446
MainProgram_elsedoneblock60608
	jsr wheeldiff
	; Assigning single variable : drawTree
	; Store Variable simplified optimization : right-hand term is pure
	ldx #0 ; optimized, look out for bugs
	lda #1
	sta drawTree,x
	jmp MainProgram_caseend11307
MainProgram_casenext8171
MainProgram_caseend11307
	
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
	lda #0
	sta xoffset
	jsr find_ground
	; Assigning single variable : wl_y_off
	; 8 bit binop
	; Add/sub where right value is constant number
	lda yoff
	clc
	adc wheelbump
	 ; end add / sub var with constant
	
	sta wl_y_off
	; Assigning single variable : xoffset
	lda #18
	sta xoffset
	jsr find_ground
	; Assigning single variable : wr_y_off
	; 8 bit binop
	; Add/sub where right value is constant number
	lda yoff
	clc
	adc wheelbump
	 ; end add / sub var with constant
	
	sta wr_y_off
	
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
	cmp #$4 ; keep
	bne MainProgram_incmax61660
	lda #0
	sta sus_y
MainProgram_incmax61660
	inc sus_wy
	lda sus_wy
	cmp #$4 ; keep
	bne MainProgram_incmax58011
	lda #0
	sta sus_wy
MainProgram_incmax58011
MainProgram_elseblock41289
MainProgram_elsedoneblock67684
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx #$1
	lda drawTree,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainProgram_elsedoneblock72692
MainProgram_ConditionalTrueBlock48441
	
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
	bne MainProgram_tempfail90941
MainProgram_binaryclausesuccess55204
	jmp MainProgram_ConditionalTrueBlock36531
MainProgram_tempfail90941
	; Binary clause: EQUALS
	lda updowntile
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	bne MainProgram_elseblock68134
MainProgram_binaryclausesuccess68098
MainProgram_ConditionalTrueBlock36531
	; Assigning single variable : updowndir
	; 8 bit binop
	; Add/sub where right value is constant number
	lda updowndir
	eor #254
	 ; end add / sub var with constant
	
	sta updowndir
MainProgram_elseblock68134
MainProgram_elsedoneblock5292
MainProgram_elseblock41848
MainProgram_elsedoneblock72692
	jmp MainProgram_while30235
MainProgram_elseblock46701
MainProgram_elsedoneblock69042
EndSymbol
EndBlock5513
	org $2d00
carSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_carbody.bin"
	org $2d60
wheelSprite
	incbin "/Users/jartza/src/coldwar///export/sprite_wheel.bin"
	org $2da0
groundTile
	incbin "/Users/jartza/src/coldwar///export/sprite_ground.bin"
	org $2db0
updownTile
	incbin "/Users/jartza/src/coldwar///export/sprite_updown.bin"
	org $2df0
smallFont
	incbin "/Users/jartza/src/coldwar///export/font4x8.bin"
