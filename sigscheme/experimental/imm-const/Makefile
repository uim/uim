GCCOPT=-c -Os -fomit-frame-pointer
OBJDUMPOPT=-td

PROG=imm-test
TARGETS=imm-test-gcc34-Os-i486.txt imm-test-gcc34-Os-ppro.txt \
  imm-test-gcc34-Os-ppro-i486.txt \
  imm-test-gcc32-Os-armv5te.txt imm-test-gcc32-Os-armv5te-thumb.txt \
  imm-test-gcc32-Os-mips.txt imm-test-gcc32-Os-mips16.txt \
  imm-test-gcc32-Os-sh2.txt imm-test-gcc32-Os-sh3.txt \
  imm-test-gcc32-Os-sh4.txt \
  imm-test-gcc32-Os-m68k.txt \
  imm-test-gcc34-Os-avr.txt

# disabled
#  imm-test-gcc32-Os-sh5-compact.txt

all: $(TARGETS)

clean:
	rm -f $(TARGETS) $(PROG).o

imm-test-gcc34-Os-i486.txt:
	gcc -march=i486 $(GCCOPT) $(PROG).c
	objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc34-Os-ppro.txt:
	gcc -march=pentiumpro $(GCCOPT) $(PROG).c
	objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc34-Os-ppro-i486.txt:
	gcc -march=pentiumpro -mtune=i486 $(GCCOPT) $(PROG).c
	objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-armv5te.txt:
	arm-rtems-gcc -march=armv5te $(GCCOPT) $(PROG).c
	arm-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-armv5te-thumb.txt:
	arm-rtems-gcc -march=armv5te -mthumb $(GCCOPT) $(PROG).c
	arm-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-mips.txt:
	mips-rtems-gcc $(GCCOPT) $(PROG).c
	mips-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-mips16.txt:
	mips-rtems-gcc -mips16 $(GCCOPT) $(PROG).c
	mips-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-sh2.txt:
	sh-rtems-gcc -m2 $(GCCOPT) $(PROG).c
	sh-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-sh3.txt:
	sh-rtems-gcc -m3 $(GCCOPT) $(PROG).c
	sh-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-sh4.txt:
	sh-rtems-gcc -m4 $(GCCOPT) $(PROG).c
	sh-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-sh5-compact.txt:
	sh-rtems-gcc -m5-compact $(GCCOPT) $(PROG).c
	sh-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc32-Os-m68k.txt:
	m68k-rtems-gcc $(GCCOPT) $(PROG).c
	m68k-rtems-objdump $(OBJDUMPOPT) $(PROG).o >$@

imm-test-gcc34-Os-avr.txt:
	avr-gcc $(GCCOPT) $(PROG).c
	avr-objdump $(OBJDUMPOPT) $(PROG).o >$@
