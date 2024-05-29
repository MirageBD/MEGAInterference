; ----------------------------------------------------------------------------------------------------

.define imgchars				$c000	; 40 * 64 = $0a00

.define imgscreen				$a000	; size = 80*50*2 = $1f40

.define imgdata					$21000  ; 320*200*3 = $2ee00
.define moddata					$10000

.define emptychar				$ff80
.define screen1					$b000

.define zp0						$f8

.define imgwidth				512
.define imgxoffset				96-64		; (512-320)/2

.define yoffset					32

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main
main_restart

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
		;lda #$ff										; setup IRQ interrupt
		;sta $d012
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

		;lda #$00
		;sta $d015

		lda #$32										; pal screen start
		sta palntscscreenstart
		bit $d06f
		bpl :+
		lda #$1a										; ntsc screen start
		sta palntscscreenstart
:

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

		; calculate red x offset
		lda frame
		asl
		tay
		lda sine,y
		lsr
		sta xoffsetred

		; calculate red y offset
		ldy frame
		lda sine,y
		lsr
		tay

		; red left
		clc
		lda lumaleftlo+yoffset,y
		adc xoffsetred
		sta imgrucr+0
		lda lumaleftmid+yoffset,y
		adc #0
		sta imgrucr+1
		lda lumalefthi+yoffset,y
		adc #0
		sta imgrucr+2

		; red right
		clc
		lda lumarightlo+yoffset,y
		adc xoffsetred
		sta imgrucrr+0
		lda lumarightmid+yoffset,y
		adc #0
		sta imgrucrr+1
		lda lumarighthi+yoffset,y
		adc #0
		sta imgrucrr+2

		; calculate green x offset
		lda frame
		asl
		tay
		lda sine+64,y
		lsr
		sta xoffsetgreen

		; calculate green y offset
		clc
		lda frame
		asl
		adc #128
		tay
		lda sine,y
		lsr
		tay

		; green left
		clc
		lda lumaleftlo+yoffset,y
		adc xoffsetgreen
		sta imgrucg+0
		lda lumaleftmid+yoffset,y
		adc #0
		sta imgrucg+1
		lda lumalefthi+yoffset,y
		adc #0
		sta imgrucg+2

		; green right
		clc
		lda lumarightlo+yoffset,y
		adc xoffsetgreen
		sta imgrucgr+0
		lda lumarightmid+yoffset,y
		adc #0
		sta imgrucgr+1
		lda lumarighthi+yoffset,y
		adc #0
		sta imgrucgr+2

		; blue left
		clc		
		lda lumaleftlo+100
		adc #64
		sta imgrucb+0
		lda lumaleftmid+100
		adc #0
		sta imgrucb+1
		lda lumalefthi+100
		adc #0
		sta imgrucb+2

		; blue right
		clc
		lda lumarightlo+100
		adc #64
		sta imgrucbr+0
		lda lumarightmid+100
		adc #0
		sta imgrucbr+1
		lda lumarighthi+100
		adc #0
		sta imgrucbr+2

		ldx #0

		lda palntscscreenstart
:		cmp $d012
		bne :-

img_render_irq_loop

		lda $d070										; BANK IN PALETTE 0 - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
imgrucr		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
imgrucg		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
imgrucb		.word $0000										; src
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
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
imgrucrr	.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
imgrucgr	.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
imgrucbr	.word $0000										; src
			.byte $00										; src bank and flags
			.word $d308										; dst
			.byte (($d308 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

		ldy $d012
		iny

		inx

		clc
		lda imgrucr+1
		adc #2
		sta imgrucr+1
		lda imgrucr+2
		adc #0
		sta imgrucr+2

		clc
		lda imgrucg+1
		adc #2
		sta imgrucg+1
		lda imgrucg+2
		adc #0
		sta imgrucg+2

		clc
		lda imgrucb+1
		adc #2
		sta imgrucb+1
		lda imgrucb+2
		adc #0
		sta imgrucb+2

		clc
		lda imgrucrr+1
		adc #2
		sta imgrucrr+1
		lda imgrucrr+2
		adc #0
		sta imgrucrr+2

		clc
		lda imgrucgr+1
		adc #2
		sta imgrucgr+1
		lda imgrucgr+2
		adc #0
		sta imgrucgr+2

		clc
		lda imgrucbr+1
		adc #2
		sta imgrucbr+1
		lda imgrucbr+2
		adc #0
		sta imgrucbr+2

:		cpy $d012
		bne :-

		cpx #200
		lbne img_render_irq_loop

		jsr peppitoPlay

		inc frame

		;lda palntscscreenstart
		;sta $d012

		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

palntscscreenstart
		.byte $32

frame
		.byte 0
framewait
		.byte 64

xoffsetred
		.byte 0

xoffsetgreen
		.byte 0

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
lumaleftlo
		.repeat 400, I
			.byte <.loword((imgdata) + I*imgwidth + imgxoffset)
		.endrepeat

.align 256
lumaleftmid
		.repeat 400, I
			.byte >.loword((imgdata) + I*imgwidth + imgxoffset)
		.endrepeat

.align 256
lumalefthi
		.repeat 400, I
			.byte <.hiword((imgdata) + I*imgwidth + imgxoffset)
		.endrepeat

.align 256
lumarightlo
		.repeat 400, I
			.byte <.loword((imgdata) + I*imgwidth + imgxoffset + 240)
		.endrepeat

.align 256
lumarightmid
		.repeat 400, I
			.byte >.loword((imgdata) + I*imgwidth + imgxoffset + 240)
		.endrepeat

.align 256
lumarighthi
		.repeat 400, I
			.byte <.hiword((imgdata) + I*imgwidth + imgxoffset + 240)
		.endrepeat




/*

.align 256
lineredleftlo
		.repeat 200
			.byte 0
		.endrepeat

.align 256
lineredleftmid
		.repeat 200
			.byte 0
		.endrepeat

.align 256
lineredlefthi
		.repeat 200
			.byte 0
		.endrepeat



.align 256
linegreenleftlo
		.repeat 200
			.byte 0
		.endrepeat

.align 256
linegreenleftmid
		.repeat 200
			.byte 0
		.endrepeat

.align 256
linegreenlefthi
		.repeat 200
			.byte 0
		.endrepeat


.align 256
lineblueleftlo
		.repeat 200
			.byte 0
		.endrepeat

.align 256
lineblueleftmid
		.repeat 200
			.byte 0
		.endrepeat

.align 256
linebluelefthi
		.repeat 200
			.byte 0
		.endrepeat

*/


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
