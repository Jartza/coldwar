*=$1001 "BASIC installer and song data"
    .word basic_line2
    .word $0010
    .byte $9e 
    .text "4119"        // TRIPLE CHECK THIS
    .byte $00
basic_line2:
    .word end_basic
    .word $0020
    .byte $9e 
    .text "7024"
// now its installed
    .byte $00
end_basic:
    .word $0000                   
//*=$1900 // SONG MUST BE ALIGNED TO A 256 BYTE BOUNDARY!
//.text "SONG DATA GOES HERE 512 BYTES "
//.text "SET SONGLOCATION VAR IN SOURCE TO WHEREVER YOU"
//.text "WANT TO LOAD YOUR SONG DATA TO."
//.text " $03E0 TO $03FF AND $FB TO $FE ARE USED BY THIS APP!"
.var SongLocation=$1c00 
.var PlayerLocation=$1900
Installer:
.print "Setup driver/memory loc: "+toIntString(Installer)
    ldx #0
!:  lda START,x 
    sta SongLocation,x 
    lda START+256,x 
    sta SongLocation+256,x 
    lda START+512,x 
    sta PlayerLocation,x 
    lda START+768,x 
    sta PlayerLocation+256,x 
    lda START+1024,x 
    sta PlayerLocation+512,x
    dex 
    dey 
    bne !-
    jsr PlayerLocation //init
    rts

START: 
//The song binary can be relocated but be careful
// to ensure all locations and variables are
// accurate.
.import binary "easygoing.prg",2,512

// PLAYER VARIABLES USED
.var zpvar1=$fb 
.var zpvar2=$fc 
.var zpvar3=$fd 
.var zpvar4=$fe
.var var1=$03ea 
.var var2=$03f0 
.var VOL_AUXCLR=$900e
.var VIC_REGS=$9000
.var VIC_FREQ=$900a

/* d-asm of the song format:

000-07f: 128 bytes
tracks list
4bit x 4bit, one byte per channel, 4 channels, 32 rows of tracks
bits 4-7: block # (0-7)
bits 0-3: mod (-7/+7)

080-0ff: 128 bytes
block definitions
bits 0-3: 
5-F#?
8-G#?
A - A
B - B
C - C
bits 4-7:
instrument? octave?

100-180: 128 bytes
SND defs. 16 bytes per sound. 
same bit format as block definitions.

180-18f - 16 bytes
e/r setting for SNDs 
bits 0-3:
bits 4-7:

190-1c0: 48 bytes
note and value for envelopes 1-2-3
bits 4-7: note
bits 1-3: value
(env 0 = all $00)

1c0-1fe: 
maybe waveform data
1ff = $fe
*/

*=$123f "Player code"
.pseudopc PlayerLocation {

// Install should also set MusicAddress?

MusicInit:
.print "Initialize: SYS"+toIntString(MusicInit)
    jmp Init_Main   // 39c
Rewind:
.print "Restart: SYS"+toIntString(MusicInit+3)
    jsr play_restart
MusicPlay:
.print "Pause/Resume: SYS"+toIntString(MusicInit+6)
    jmp PauseResume
MusicStop:
.print "Stop: SYS"+toIntString(MusicInit+9)
    jmp Stop_Main   // 3dd
MusicFrame:
    dec var1+2
    beq _d
_a: //1c0bh
    lda #%00001010
    sta zpvar3      // current register number: A
    ldx #0
    jsr _i  // 31a
    inc zpvar3     //B
    ldx #4
    jsr _i 
    inc zpvar3      //C
    ldx #8
    jsr _i 
    inc zpvar3      //D
    ldx #12         
    jsr _i 
    lda var1+5      // volume
    bne _e
    rts 
_d:
    lda var1+3
    sta var1+2
    lda var1
    cmp #$ff 
    beq _a
    ldx #0
    jsr _g
    ldx #4
    jsr _g
    ldx #8
    jsr _g 
    ldx #12         // 4-bytes per frame
    jsr _g
    inc var1+1
    lda var1+1
    and #$0f
    bne _a
    sta var1+1
    lda var1
    clc 
    adc #4
    sta var1
_b:
    tay 
    lda SongLocation+0,y     // 1a00 is music data start
    cmp #$fe        // fe = end of data
    beq !+
    sta var1+6
    jsr _j
    jmp _a
!:
    lda SongLocation+1,y     // music data+offset
    sta var1 
    cmp #$ff        // keep going if empty
    bne _b
    jmp _a

_e:
    dec var1+4      // length of mod?
    beq _c
    rts 
_c:
    lda #$80
    sta zpvar1 
    lda #>SongLocation+256
    sta zpvar2 
    ldy var1+5
_f:
    lda (zpvar1),y     // from 1b80, special codes
    ror 
    ror 
    ror 
    ror 
    and #$0f
    sta zpvar3          // top bits to 4-bit and into zp3
    lda (zpvar1),y      // 1b80
    and #$0f            // bottom bits
    beq !+
    sta var1+4          // bottom bits to var+4
    lda VOL_AUXCLR           // volume reg
    and #$f0 
    ora zpvar3          // vol bits are top bits
    sta VOL_AUXCLR 
    iny 
    sty var1+5
    rts 
!:
    lda zpvar3 
    beq !+
    lda var1+5          // progress counter?
    and #$f0 
    ora zpvar3 
    sta var1+5
    tay 
    jmp _f           //2cdh
!:
    sta var1+5
    rts 

_g:
    ldy var1+1
    lda var2+0,x 
    bmi _ret
    sta zpvar3 
    and #%01110000
    ora #%10000000
    sta zpvar1         // 8x-fx 
    lda #>SongLocation
    sta zpvar2 
    lda (zpvar1),y    
    bne !+
_ret:rts
!:
    and #$1f
    sta zpvar4 
    lda zpvar3 
    and #$0f 
    sec 
    sbc #8
    clc 
    adc zpvar4 
    sta var2+1,x 
    lda #1 
    sta var2+2,x 
    lda (zpvar1),y
    ror 
    and #%01110000
    sta var2+3,x 
    ror 
    ror 
    ror 
    ror 
    and #%00000111
    tay 
    lda SongLocation+392,y // +$188
    bne !+
    rts
!:
    sta var1+5
    lda #1
    sta var1+4 
    rts

_i:
    lda var2+3,x 
    cmp #$ff
    beq _n
    dec var2+2,x 
    beq _o
    rts 

_k:
    ldy zpvar3 
    sta VIC_REGS,y 
    lda #$ff
    sta var2+3,x 
_n: rts 

_o: // frequency change this frame for this reg
    lda var2+3,x 
    sta zpvar1 
    ror 
    ror 
    ror 
    ror
    and #$0f 
    tay 
    lda SongLocation+384,y     
    sta var2+2,x    // whatever 1b80-8f is it goes in var2+2
_m:    
    lda #>SongLocation+256 
    sta zpvar2 
    ldy #0
    lda (zpvar1),y     // 1b00
    beq _k
    cmp #16
    bcc _l
    and #$1f
    clc 
    adc #16
    sta zpvar4 
    lda var2+1,x 
    clc 
    adc zpvar4 
    tay 
    cpy #$40
    bcc !+
    ldy #$3f
!:  cpy #$c0
    bcc !+
    ldy #0
!:  lda SongLocation+448,y
    sta zpvar4 
    ldy #0
    lda (zpvar1),y
    rol 
    rol 
    rol 
    rol 
    and #%00000111
    sec 
    sbc #4
    clc 
    adc zpvar4 
    ldy zpvar3 
    sta VIC_REGS,y
    inc var2+3,x
    rts 

_l:
    and #$0f 
    sta zpvar4 
    lda zpvar1 
    and #$f0 
    ora zpvar4 
    sta zpvar1 
    sta var2+3,x
    jmp _m

Interrupt:

    pha 
    tya 
    pha 
    txa 
    pha 
    
    lda EOF-2
    cmp #1
    bne !skip+      // if "playing"==0 then skip ISR

    lda $9126       //acknowledge VIA2 T1 IRQ
    
    lda $9004       //b1-8 of raster#
    pha

    lda $900f
    and #%11111000
    ora #%00000001
    sta $900f       // border color white
    
    jsr MusicFrame
    
    lda $900f
    and #%11111000
    sta $900f       // border color black
    
    pla 
    sta $02
    lda #$95
    sec 
    sbc $02         // store (95h-raster ct) as timer
    sta $9124       // next interval in low latch
!skip:
    pla 
    tax 
    pla 
    tay 
    pla 

    jmp $eabf 

PauseResume:
    lda var1
    bne !+ // if zero, not loaded.
    jsr Init_Main
!:
    lda EOF-2
    bne !+
    lda #1
    jmp z 
!:  ldy #4
_loop:
    sta VIC_FREQ,y         // if y loop is 3 or less, also zero out frequency
    dey 
    bpl _loop
    lda #0    
    // play = ON
z:  sta EOF-2
    rts 

Init_Main:
// setup irq
.var VIA2IER = $912e
.var VIA2ACR = $912b
    // Crystal         14318181 Hz
    // c/14 = 1022727.2 Hz
    // Hz/60 = 17045.45 cycles/s
    // ~284.09 cycles per frame
    // ~0.888 cycles per raster line (320)
    // ~1.084 cycles per raster (262)
    // 4295h == ~1/60s
    sei 

    lda #$0
    sta $9126       // low - VIA2 Timer 1
    sta $9127       // high (initial countdown)
    lda #$95
    sta $9124       // low (VIA2T1 latch)
    lda #$42
    sta $9125       // high (VIA2T1 latch)

    lda VIA2ACR 
    and #%01111111 // $7f
    ora #%01000000 // $40
    sta VIA2ACR 
    
    lda #$7f 
    sta VIA2IER     //clear all IRQs 
    lda #%11000000
    sta VIA2IER     //set only T1 IRQ
    
    lda #<Interrupt
    sta $0314
    lda #>Interrupt 
    sta $0315           // hardware IRQ vec
    
    cli 
play_restart:
    jsr Stop_Main
    
    lda EOF
    sta var1+3          // 7
    lda #1
    sta var1+2          // 1
    lda #$ff 
    sta var2+3 
    sta var2+7
    sta var2+11
    sta var2+15
    lda VOL_AUXCLR
    and #$f0 
    ora #%00001000
    sta VOL_AUXCLR      // vol=8
    lda SongLocation
    sta var2
    ldy #0 
_j:    
    lda SongLocation+1,y 
    sta var2+4
    lda SongLocation+2,y
    sta var2+8
    lda SongLocation+3,y
    sta var2+12 

    rts 

SetMem:
.print(SetMem)
    ldx #$ff
    ldy #$18
    clc
    jsr $ff99 // memtop to $18ff (player data starts at $1900)
    ldx #$01
    ldy #$10
    clc 
    jsr $ff9c // membtm to $1001
    ldx #16
    lda #0
!:  sta $1001,x // clear basic "loader" listing
    dex 
    bpl !-
    rts

Stop_Main:
    lda #0 
    sta EOF-2
    ldy #21
_stoploop:
    sta var1,y 
    cpy #4
    bcs !+
    sta VIC_FREQ,y         // if y loop is 3 or less, also zero out frequency
!:  dey 
    bpl _stoploop
    rts

.byte $00
.text "FISICHELLA+  "
.byte $00 // playing var 
.byte $00
EOF:  .byte $07

} //end pseudopc
EndFile: