; ----------------------------------------------------------------------------------------------------

.define imgchars				$c000	; 40 * 64 = $0a00

.define imgscreen				$a000	; size = 80*50*2 = $1f40

.define fontdata				$10000  ; 1024*16 = $04000
.define scrollimgdata			$14000  ;  512*16 = $02000
.define imgdata					$20000  ; 512*328 = $29000
.define moddata					$49000  ;           $12000

.define emptychar				$ff80
.define screen1					$b000

.define zp0						$f8

.define imgwidth				512
.define imgxoffset				0 ; 96-64		; (512-320)/2

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #$41										; enable 40MHz
		sta $00

		;lda #$70										; Disable C65 rom protection using hypervisor trap (see mega65 manual)
		;sta $d640
		;eom

		lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
		trb $d030

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		trb $d031

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		ldx #$00
		lda #$00
:		sta emptychar,x
		inx
		cpx #64
		bne :-

		ldx #$00
:		lda #<(emptychar/64)
		sta screen1+0*$0100+0,x
		sta screen1+1*$0100+0,x
		sta screen1+2*$0100+0,x
		sta screen1+3*$0100+0,x
		sta screen1+4*$0100+0,x
		sta screen1+5*$0100+0,x
		sta screen1+6*$0100+0,x
		sta screen1+7*$0100+0,x
		lda #>(emptychar/64)
		sta screen1+0*$0100+1,x
		sta screen1+1*$0100+1,x
		sta screen1+2*$0100+1,x
		sta screen1+3*$0100+1,x
		sta screen1+4*$0100+1,x
		sta screen1+5*$0100+1,x
		sta screen1+6*$0100+1,x
		sta screen1+7*$0100+1,x
		inx
		inx
		bne :-

		DMA_RUN_JOB clearcolorramjob

		lda #<.loword(screen1)							; set pointer to screen ram
		sta $d060
		lda #>.loword(screen1)
		sta $d061
		lda #<.hiword(screen1)
		sta $d062
		lda #>.hiword(screen1)
		sta $d063

		lda #<$0800										; set (offset!) pointer to colour ram
		sta $d064
		lda #>$0800
		sta $d065

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$00
		sta $d012
		lda #<fastload_irq_handler
		sta $fffe
		lda #>fastload_irq_handler
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

		jsr fl_init
		jsr fl_waiting

		FLOPPY_IFFL_FAST_LOAD_INIT "MEGAINT.DATA"
		FLOPPY_IFFL_FAST_LOAD_ADDRESS imgdata			; test1.mim
		FLOPPY_IFFL_FAST_LOAD_ADDRESS moddata			; song.mod
		FLOPPY_IFFL_FAST_LOAD_ADDRESS fontdata			; font.mim

		jsr fl_exit
		
		sei

		lda #$35
		sta $01

		lda #<.loword(moddata)
		sta adrPepMODL+0
		lda #>.loword(moddata)
		sta adrPepMODL+1
		lda #<.hiword(moddata)
		sta adrPepMODH+0
		lda #>.hiword(moddata)
		sta adrPepMODH+1

		jsr peppitoInit

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		trb $d031

		lda #80											; set to 80 for etherload
		sta $d05e

		lda #$02
		sta $d020
		lda #$10
		sta $d021

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		sta $d04c

		lda #$32										; pal screen start
		sta palntscscreenstart
		bit $d06f
		bpl :+
		lda #$1a										; ntsc screen start
		sta palntscscreenstart
:

		jsr img_rendinit

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a
			
		lda palntscscreenstart
		sta $d012
		lda #<img_render_irq
		sta $fffe
		lda #>img_render_irq
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli
		
loop
		lda $d020
		jmp loop

; ----------------------------------------------------------------------------------------------------

screencolumn	.byte 0
screenrow		.byte 0

img_fcblock
		.repeat 8
			.byte 0, 1, 2, 3, 4, 5, 6, 7
		.endrepeat

img_misccounter
		.byte 0

img_rowchars
		.byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,    1,2,3,4,5,6,7,8,9,10

img_rendinit

		; WHY THE HELL DO I NEED TO FILL 2 PALETTES HERE???

		lda $d070										; BANK IN PALETTE 0 - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

		lda #$00
		ldx #$00										; set palette data
:		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		lda $d070
		and #%11001111									; set palette to 0
		ora #%00000000
		sta $d070



		lda $d070										; BANK IN PALETTE 1 - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%01000000
		sta $d070

		lda #$00
		ldx #$00										; set palette data
:		txa
		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		lda $d070
		and #%11111100									; set alt palette to 2
		ora #%00000001
		sta $d070

		DMA_RUN_JOB imgrender_clearcolorramjob

		lda #<.loword(SAFE_COLOR_RAM_PLUS_ONE)
		sta zp0+0
		lda #>.loword(SAFE_COLOR_RAM_PLUS_ONE)
		sta zp0+1
		lda #<.hiword(SAFE_COLOR_RAM_PLUS_ONE)
		sta zp0+2
		lda #>.hiword(SAFE_COLOR_RAM_PLUS_ONE)
		sta zp0+3

		lda #0
		sta img_misccounter

img_fillsetaltpalbits

		ldz #30*2										; set columns 30-40 to use alt palette
		lda #%01101111
:		sta [zp0],z
		inz
		inz
		cpz #40*2
		bne :-

		clc
		lda zp0+0
		adc #80
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1
		lda zp0+2
		adc #0
		sta zp0+2
		lda zp0+3
		adc #0
		sta zp0+3

		inc img_misccounter
		lda img_misccounter
		cmp #25
		bne img_fillsetaltpalbits


		; fill full colour char pattern ($f000)

		lda #<imgchars
		sta imgri1+1
		lda #>imgchars
		sta imgri1+2

		lda #$00
		sta img_misccounter

		ldx #$00
		ldy #$00
:		tya
		adc img_fcblock,x
imgri1	sta imgchars,x
		inx
		cpx #64
		bne :-

		clc
		lda imgri1+1
		adc #64
		sta imgri1+1
		lda imgri1+2
		adc #0
		sta imgri1+2
		clc
		tya
		adc #$08
		tay
		inc img_misccounter
		lda img_misccounter
		cmp #42
		bne :-

		; fill screen ($e000)

		lda #$00
		sta screencolumn
		sta screenrow

		lda #<(imgscreen+0)
		sta put0+1
		lda #>(imgscreen+0)
		sta put0+2
		lda #<(imgscreen+1)
		sta put1+1
		lda #>(imgscreen+1)
		sta put1+2

		; imgchars = $f000
		; imgchars/64 = $0800

putstart
		ldx screencolumn
		clc
		lda img_rowchars,x								; char to plot
		adc #<(imgchars/64)
put0	sta imgscreen+0									; plot left of 2 chars

		lda #>(imgchars/64)
put1	sta imgscreen+1									; plot right of 2 chars

		clc												; add 2 to screenpos low
		lda put0+1
		adc #02
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc												; add 2 to screenpos high
		lda put1+1
		adc #02
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

		inc screencolumn								; increase screen column until 40
		lda screencolumn
		cmp #40
		bne putstart

		lda #0											; reset screencolumn to 0, increase row until 25
		sta screencolumn
		inc screenrow
		lda screenrow
		cmp #25
		beq endscreenplot

		jmp putstart

endscreenplot

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #%00100000									; set H320, V200, ATTR
		sta $d031

		lda #$00
		sta $d016

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		lda $d04c
		lda #$42
		sta $d05c

		lda #$01
		sta $d05b										; Set display to V200
		lda #25
		sta $d07b										; Display 25 rows of text

		lda #<imgscreen									; set pointer to screen ram
		sta $d060
		lda #>imgscreen
		sta $d061
		lda #(imgscreen & $ff0000) >> 16
		sta $d062
		lda #$00
		sta $d063

        rts

; ----------------------------------------------------------------------------------------------------------------------------------------

img_render_irq

		php
		pha
		phx
		phy
		phz

		lda #$00
		sta $d020

		; start render loop -----------------------

		ldx #0

		lda palntscscreenstart
:		cmp $d012
		bne :-

		ldy $d012

img_render_irq_loop

		lda $d070										; BANK IN PALETTE 0 - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

			sta $d707										; inline DMA copy
ir_rls		.byte $82, 64									; Source skip rate (256ths of bytes)
			.byte $83, 1									; Source skip rate (whole bytes)
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
ir_rl		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
ir_gls		.byte $82, 64									; Source skip rate (256ths of bytes)
			.byte $83, 1									; Source skip rate (whole bytes)
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
ir_gl		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
ir_bls		.byte $82, 0									; Source skip rate (256ths of bytes)
			.byte $83, 1									; Source skip rate (whole bytes)
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
ir_bl		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d308										; dst
			.byte (($d308 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

		lda $d070										; BANK IN PALETTE 1 - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%01000000
		sta $d070

			sta $d707										; inline DMA copy
ir_rrs		.byte $82, 64									; Source skip rate (256ths of bytes)
			.byte $83, 1									; Source skip rate (whole bytes)
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
ir_rr		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
ir_grs		.byte $82, 64									; Source skip rate (256ths of bytes)
			.byte $83, 1									; Source skip rate (whole bytes)
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
ir_gr		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
ir_brs		.byte $82, 0									; Source skip rate (256ths of bytes)
			.byte $83, 1									; Source skip rate (whole bytes)
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
ir_br		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d308										; dst
			.byte (($d308 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

		lda tab_rl1,x
		sta ir_rl+1
		lda tab_rl2,x
		sta ir_rl+2
		lda tab_rr1,x
		sta ir_rr+1
		lda tab_rr2,x
		sta ir_rr+2

		lda tab_gl1,x
		sta ir_gl+1
		lda tab_gl2,x
		sta ir_gl+2
		lda tab_gr1,x
		sta ir_gr+1
		lda tab_gr2,x
		sta ir_gr+2

		lda tab_bl1,x
		sta ir_bl+1
		lda tab_bl2,x
		sta ir_bl+2
		lda tab_br1,x
		sta ir_br+1
		lda tab_br2,x
		sta ir_br+2

		iny
:		cpy $d012
		bne :-

		inx
		cpx #201
		lbne img_render_irq_loop

		jsr peppitoPlay

		inc frame

		lda #$80
		sta $d020

		jsr calcredoffsets
		jsr calctab_red_horizontal
		jsr calctab_red_vertical
		jsr setfirstlineredvalues

		jsr calcgreenoffsets
		jsr calctab_green_horizontal
		jsr calctab_green_vertical
		jsr setfirstlinegreenvalues

		jsr calcblueoffsets
		jsr calctab_blue_horizontal
		jsr calctab_blue_vertical
		jsr setfirstlinebluevalues

		lda #$00
		sta $d020

		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

palntscscreenstart	.byte $32

frame				.byte 0

xoffsetred			.byte 0
yoffsetred			.byte 0
xscalered			.byte 0
yscalered			.byte 0

xoffsetgreen		.byte 0
yoffsetgreen		.byte 0
xscalegreen			.byte 0
yscalegreen			.byte 0

xoffsetblue			.byte 100
yoffsetblue			.byte 64
xscaleblue			.byte 0
yscaleblue			.byte 0

yscaletmplo			.byte 0

; ----------------------------------------------------------------------------------------------------------------------------------------

setfirstlineredvalues

		lda xoffsetred
		sta ir_rl+0
		ldy	xscalered
		sty ir_rls+1
		sty ir_rrs+1
		clc
		adc xscaleaddlo,y
		sta ir_rr+0

		lda tab_rl1
		sta ir_rl+1
		lda tab_rl2
		sta ir_rl+2
		lda tab_rr1
		sta ir_rr+1
		lda tab_rr2
		sta ir_rr+2

		rts

setfirstlinegreenvalues

		lda xoffsetgreen
		sta ir_gl+0
		ldy	xscalegreen
		sty ir_gls+1
		sty ir_grs+1
		clc
		adc xscaleaddlo,y
		sta ir_gr+0

		lda tab_gl1
		sta ir_gl+1
		lda tab_gl2
		sta ir_gl+2
		lda tab_gr1
		sta ir_gr+1
		lda tab_gr2
		sta ir_gr+2

		rts

setfirstlinebluevalues

		lda xoffsetblue
		sta ir_bl+0
		ldy	xscaleblue
		sty ir_bls+1
		sty ir_brs+1
		clc
		adc xscaleaddlo,y
		sta ir_br+0

		lda tab_bl1
		sta ir_bl+1
		lda tab_bl2
		sta ir_bl+2
		lda tab_br1
		sta ir_br+1
		lda tab_br2
		sta ir_br+2

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

calcredoffsets

		clc					; calculate red x offset
		lda frame
		adc #32
		asl
		tay
		lda sine,y
		sta xoffsetred

		ldy frame			; calculate red y offset
		lda sine,y
		lsr
		sta yoffsetred

		clc					; calculate red x scale
		lda frame
		adc #128
		tay
		lda sine,y
		lsr
		sta xscalered

		lda xscalered
		sta yscalered

		rts

calcgreenoffsets

		clc					; calculate green x offset
		lda frame
		adc #32
		tay
		lda sine+64,y
		sta xoffsetgreen

		clc					; calculate green y offset
		lda frame
		asl
		adc #128
		tay
		lda sine,y
		lsr
		sta yoffsetgreen

		clc					; calculate green x scale
		lda frame
		asl
		tay
		lda sine,y
		lsr
		sta xscalegreen

		lda xscalegreen
		sta yscalegreen

		rts

calcblueoffsets

		clc					; calculate blue x offset
		lda frame
		asl
		adc #32
		tay
		lda sine+64,y
		sta xoffsetblue

		clc					; calculate blue y offset
		lda frame
		asl
		adc #64
		tay
		lda sine,y
		lsr
		sta yoffsetblue

		clc					; calculate blue x scale
		lda frame
		asl
		adc #128+64
		tay
		lda sine,y
		lsr
		sta xscaleblue

		lda xscaleblue
		sta yscaleblue

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

calctab_red_horizontal

		ldy yoffsetred
		lda lumaleftmid,y
		sta tab_rl1
		lda lumalefthi,y
		sta tab_rl2
		sta tab_rr2

		clc
		lda xoffsetred
		ldy	xscalered
		adc xscaleaddlo,y
		lda tab_rl1
		adc xscaleaddhi,y
		sta tab_rr1

		rts

calctab_green_horizontal

		ldy yoffsetgreen
		lda lumaleftmid,y
		sta tab_gl1
		lda lumalefthi,y
		sta tab_gl2
		sta tab_gr2

		clc
		lda xoffsetgreen
		ldy	xscalegreen
		adc xscaleaddlo,y
		lda tab_gl1
		adc xscaleaddhi,y
		sta tab_gr1

		rts

calctab_blue_horizontal

		ldy yoffsetblue
		lda lumaleftmid,y
		sta tab_bl1
		lda lumalefthi,y
		sta tab_bl2
		sta tab_br2

		clc
		lda xoffsetblue
		ldy	xscaleblue
		adc xscaleaddlo,y
		lda tab_bl1
		adc xscaleaddhi,y
		sta tab_br1

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

calctab_red_vertical		

		lda #0
		sta yscaletmplo

		ldx #0
ctrv_loop
		ldy #2

		clc
		lda yscaletmplo
		adc yscalered
		sta yscaletmplo
		bcc :+
		tya
		asl
		tay

:		tya
		clc
		adc tab_rl1+0,x
		sta tab_rl1+1,x
		lda tab_rl2+0,x
		adc #0
		sta tab_rl2+1,x

		tya
		clc
		adc tab_rr1+0,x
		sta tab_rr1+1,x
		lda tab_rr2+0,x
		adc #0
		sta tab_rr2+1,x

		inx
		cpx #199
		bne ctrv_loop

		rts

calctab_green_vertical		

		lda #0
		sta yscaletmplo

		ldx #0
ctgv_loop
		ldy #2

		clc
		lda yscaletmplo
		adc yscalegreen
		sta yscaletmplo
		bcc :+
		tya
		asl
		tay

:		tya
		clc
		adc tab_gl1+0,x
		sta tab_gl1+1,x
		lda tab_gl2+0,x
		adc #0
		sta tab_gl2+1,x

		tya
		clc
		adc tab_gr1+0,x
		sta tab_gr1+1,x
		lda tab_gr2+0,x
		adc #0
		sta tab_gr2+1,x

		inx
		cpx #199
		bne ctgv_loop

		rts

calctab_blue_vertical		

		lda #0
		sta yscaletmplo

		ldx #0
ctbv_loop
		ldy #2

		clc
		lda yscaletmplo
		adc yscaleblue
		sta yscaletmplo
		bcc :+
		tya
		asl
		tay

:		tya
		clc
		adc tab_bl1+0,x
		sta tab_bl1+1,x
		lda tab_bl2+0,x
		adc #0
		sta tab_bl2+1,x

		tya
		clc
		adc tab_br1+0,x
		sta tab_br1+1,x
		lda tab_br2+0,x
		adc #0
		sta tab_br2+1,x

		inx
		cpx #199
		bne ctbv_loop

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

imgrender_clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (SAFE_COLOR_RAM) >> 20				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.byte %00000000										; this is normally the source addres, but contains the fill value now
				.byte 0
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM) & $ffff					; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.word $000f										; ff = red = transparency. this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000								; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((SAFE_COLOR_RAM) >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 40*26										; Count LSB + Count MSB

				.word $0007										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 40*26										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM)+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte ((((SAFE_COLOR_RAM)+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

.segment "TABLES"

.align 256
lumaleftmid
		.repeat 328, I
			.byte >.loword((imgdata) + I*imgwidth + imgxoffset)
		.endrepeat

.align 256
lumalefthi
		.repeat 328, I
			.byte <.hiword((imgdata) + I*imgwidth + imgxoffset)
		.endrepeat

.align 256
tab_rl1
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_rl2
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_rr1
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_rr2
		.repeat 200
			.byte 0
		.endrepeat

.align 256
tab_gl1
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_gl2
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_gr1
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_gr2
		.repeat 200
			.byte 0
		.endrepeat

.align 256
tab_bl1
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_bl2
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_br1
		.repeat 200
			.byte 0
		.endrepeat
.align 256
tab_br2
		.repeat 200
			.byte 0
		.endrepeat

.align 256
charxsizes
		.byte   10, 13, 10, 14, 11, 8,  8, 15, 11,  4,  8, 11,  8, 16, 11, 14, 10, 15, 10, 12, 11, 12, 13, 16, 13, 13, 11,  8,  5,  5,  5,  5, 12, 6, 10, 10, 12, 11, 10, 11, 10, 10,  4,  9,  12, 10, 7,  7
		;       @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z   _   .   ,   :   ;   0   1   2   3   4   5   6   7   8   9   !   ?   $   *   (   )

scrolltext
		.byte "@abcdefghijklmnopqrstuvwxyz .,:;0123456789!?$*()"

charstrip
		.byte 0

scrollposlo
		.byte 0
scrollposhi
		.byte 0

charremap
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $1b, $2a, $00, $00, $2c, $00, $00, $00, $2e, $2f, $2d, $00, $1d, $00, $1c, $00
		.byte $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $1e, $1f, $00, $00, $00, $2b
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f ; @ a b c d e f g h i j k l m n o
		.byte $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $00, $00, $00, $00, $00 ; p q r s t u v w x y z

.align 256
sine
		.repeat 2
			.byte 000, 000, 000, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
			.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
			.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
			.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254
			.byte 255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
			.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
			.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
			.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 000
		.endrepeat

.align 256
xscaleaddlo
		.repeat 256, I
			.byte <(240 + I * 240/256)
		.endrepeat

.align 256
xscaleaddhi
		.repeat 256, I
			.byte >(240 + I * 240/256)
		.endrepeat

