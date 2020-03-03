; da65 V2.18 - Git N/A
; Created:    2020-03-03 14:46:12
; Input file: player.bin
; Page:       1


        .setcpu "6502"

L5641           := $5641
        jmp     L1D9C

        jmp     L1DDD

        dec     $03EC
        beq     L1C2F
L1C0B:  lda     #$0A
        sta     $FD
        ldx     #$00
        jsr     L1D1A
        inc     $FD
        ldx     #$04
        jsr     L1D1A
        inc     $FD
        ldx     #$08
        jsr     L1D1A
        inc     $FD
        ldx     #$0C
        jsr     L1D1A
        lda     $03EF
        bne     L1C84
        rts

L1C2F:  lda     $03ED
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
L1C66:  tay
        lda     $1A00,y
        cmp     #$FE
        beq     L1C77
        sta     $03F0
        jsr     L1DCA
        jmp     L1C0B

L1C77:  lda     $1A01,y
        sta     $03EA
        cmp     #$FF
        bne     L1C66
        jmp     L1C0B

L1C84:  dec     $03EE
        beq     L1C8A
        rts

L1C8A:  lda     #$80
        sta     $FB
        lda     #$1B
        sta     $FC
        ldy     $03EF
L1C95:  lda     ($FB),y
        ror     a
        ror     a
        ror     a
        ror     a
        and     #$0F
        sta     $FD
        lda     ($FB),y
        and     #$0F
        beq     L1CB7
        sta     $03EE
        lda     $900E
        and     #$F0
        ora     $FD
        sta     $900E
        iny
        sty     $03EF
        rts

L1CB7:  lda     $FD
        beq     L1CC9
        lda     $03EF
        and     #$F0
        ora     $FD
        sta     $03EF
        tay
        jmp     L1C95

L1CC9:  sta     $03EF
        rts

L1CCD:  ldy     $03EB
        lda     $03F0,x
        bmi     L1CE5
        sta     $FD
        and     #$70
        ora     #$80
        sta     $FB
        lda     #$1A
        sta     $FC
        lda     ($FB),y
        bne     L1CE6
L1CE5:  rts

L1CE6:  and     #$1F
        sta     $FE
        lda     $FD
        and     #$0F
        sec
        sbc     #$08
        clc
        adc     $FE
        sta     $03F1,x
        lda     #$01
        sta     $03F2,x
        lda     ($FB),y
        ror     a
        and     #$70
        sta     $03F3,x
        ror     a
        ror     a
        ror     a
        ror     a
        and     #$07
        tay
        lda     $1B88,y
        bne     L1D11
        rts

L1D11:  sta     $03EF
        lda     #$01
        sta     $03EE
        rts

L1D1A:  lda     $03F3,x
        cmp     #$FF
        beq     L1D31
        dec     $03F2,x
        beq     L1D32
        rts

L1D27:  ldy     $FD
        sta     $9000,y
        lda     #$FF
        sta     $03F3,x
L1D31:  rts

L1D32:  lda     $03F3,x
        sta     $FB
        ror     a
        ror     a
        ror     a
        ror     a
        and     #$0F
        tay
        lda     $1B80,y
        sta     $03F2,x
L1D44:  lda     #$1B
        sta     $FC
        ldy     #$00
        lda     ($FB),y
        beq     L1D27
        cmp     #$10
        bcc     L1D8A
        and     #$1F
        clc
        adc     #$10
        sta     $FE
        lda     $03F1,x
        clc
        adc     $FE
        tay
        cpy     #$40
        bcc     L1D66
        ldy     #$3F
L1D66:  cpy     #$C0
        bcc     L1D6C
        ldy     #$00
L1D6C:  lda     $1BC0,y
        sta     $FE
        ldy     #$00
        lda     ($FB),y
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$07
        sec
        sbc     #$04
        clc
        adc     $FE
        ldy     $FD
        sta     $9000,y
        inc     $03F3,x
        rts

L1D8A:  and     #$0F
        sta     $FE
        lda     $FB
        and     #$F0
        ora     $FE
        sta     $FB
        sta     $03F3,x
        jmp     L1D44

L1D9C:  jsr     L1DDD
        lda     L1DFF
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
        lda     $1A00
        sta     $03F0
        ldy     #$00
L1DCA:  lda     $1A01,y
        sta     $03F4
        lda     $1A02,y
        sta     $03F8
        lda     $1A03,y
        sta     $03FC
        rts

L1DDD:  lda     #$00
        ldy     #$15
L1DE1:  sta     $03EA,y
        cpy     #$04
        bcs     L1DEB
        sta     $900A,y
L1DEB:  dey
        bpl     L1DE1
        rts

        brk
        lsr     $49
        .byte   $53
        eor     #$43
        pha
        eor     $4C
        jmp     L5641

        and     ($2E),y
        bmi     L1DFF
L1DFF:  .byte   $06
