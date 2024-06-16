# -----------------------------------------------------------------------------

megabuild		= 1
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f
CAT				= cat

SRC_DIR			= ./src
UI_SRC_DIR		= ./src/ui
UIELT_SRC_DIR	= ./src/ui/uielements
DRVRS_SRC_DIR	= ./src/drivers
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g -D megabuild=$(megabuild) --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
GCC				= gcc
CC1541			= cc1541
MC				= MegaConvert
MEGAADDRESS		= megatool -a
MEGACRUNCH		= megatool -c
MEGAIFFL		= megatool -i
MEGAIMAGE		= megatool -x
MEGAMOD			= MegaMod
EL				= etherload
XMEGA65			= D:\PCTOOLS\xemu\xmega65.exe
MEGAFTP			= mega65_ftp -e

CONVERTBREAK	= 's/al [0-9A-F]* \.br_\([a-z]*\)/\0\nbreak \.br_\1/'
CONVERTWATCH	= 's/al [0-9A-F]* \.wh_\([a-z]*\)/\0\nwatch store \.wh_\1/'

CONVERTVICEMAP	= 's/al //'

.SUFFIXES: .o .s .out .bin .pu .b2 .a

default: all

OBJS = $(EXE_DIR)/boot.o $(EXE_DIR)/main.o

BINFILES = $(BIN_DIR)/test1.mim
BINFILES += $(BIN_DIR)/song.mod
BINFILES += $(BIN_DIR)/font.mim

BINFILESMC  = $(BIN_DIR)/test1.mim.addr.mc
BINFILESMC += $(BIN_DIR)/song.mod.addr.mc
BINFILESMC += $(BIN_DIR)/font.mim.addr.mc

$(BIN_DIR)/test1.mim: $(BIN_DIR)/test1.raw
	$(MEGAIMAGE) 512 328 1 $(BIN_DIR)/test1.raw $(BIN_DIR)/test1.mim

$(BIN_DIR)/font.mim: $(BIN_DIR)/font.raw
	$(MEGAIMAGE) 1024 16 1 $(BIN_DIR)/font.raw $(BIN_DIR)/font.mim

$(EXE_DIR)/boot.o:	$(SRC_DIR)/boot.s \
					$(SRC_DIR)/main.s \
					$(SRC_DIR)/irqload.s \
					$(SRC_DIR)/decruncher.s \
					$(SRC_DIR)/iffl.s \
					$(SRC_DIR)/macros.s \
					$(SRC_DIR)/mathmacros.s \
					$(SRC_DIR)/modplay.s \
					Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

$(BIN_DIR)/alldata.bin: $(BINFILES)
	$(MEGAADDRESS) $(BIN_DIR)/test1.mim               00000000
	$(MEGAADDRESS) $(BIN_DIR)/song.mod                00000000
	$(MEGAADDRESS) $(BIN_DIR)/font.mim                00000000
	$(MEGACRUNCH) $(BIN_DIR)/test1.mim.addr
	$(MEGACRUNCH) $(BIN_DIR)/song.mod.addr
	$(MEGACRUNCH) $(BIN_DIR)/font.mim.addr
	$(MEGAIFFL) $(BINFILESMC) $(BIN_DIR)/alldata.bin

$(EXE_DIR)/boot.prg.addr.mc: $(BINFILES) $(EXE_DIR)/boot.o Linkfile
	$(LD) -Ln $(EXE_DIR)/boot.maptemp --dbgfile $(EXE_DIR)/boot.dbg -C Linkfile -o $(EXE_DIR)/boot.prg $(EXE_DIR)/boot.o
	$(MEGAADDRESS) $(EXE_DIR)/boot.prg 00000400
	$(MEGACRUNCH) -e 00002100 $(EXE_DIR)/boot.prg.addr

$(EXE_DIR)/megaint.d81: $(EXE_DIR)/boot.prg.addr.mc $(BIN_DIR)/alldata.bin
	$(RM) $@
	$(CC1541) -n "megaint" -i " 2024" -d 19 -v\
	 \
	 -f "megaint" -w $(EXE_DIR)/boot.prg.addr.mc \
	 -f "megaint.data" -w $(BIN_DIR)/alldata.bin \
	$@

# -----------------------------------------------------------------------------

run: $(EXE_DIR)/megaint.d81

ifeq ($(megabuild), 1)
	m65 -l COM3 -T 'list'
	$(MEGAFTP) -c "put D:\Mega\MegaInterference\exe\megaint.d81 megaint.d81" -c "quit"
	$(EL) -m MEGAINT.D81 -r $(EXE_DIR)/boot.prg.addr.mc
ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif
else
	cmd.exe /c $(XMEGA65) -autoload -8 $(EXE_DIR)/megaint.d81
endif

clean:
	$(RM) $(EXE_DIR)/*.*
	$(RM) $(EXE_DIR)/*

