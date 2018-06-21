SHELL := /bin/bash # Use bash syntax
include ./Makefile.inc

ifneq (,$(findstring F,$(ISA)))
  FLOAT=--float
endif
ifneq (,$(findstring D,$(ISA)))
  define_macros += -D dpfpu=True
  FLOAT=--float
endif
ifneq (,$(findstring RV64,$(ISA)))
  XLEN=64
endif
ifneq (,$(findstring RV32,$(ISA)))
  XLEN=32
endif

all:
	@ln -f -s $(SHAKTI_C_HOME)/bin/* .
	@n=1 ; while [[ $$n -le $(RUNS) ]] ; do \
	echo "----------------Test $$n ----------------";\
	make spike || { echo SKIP; make remove; continue; } ;\
	cut -c1-8 code.mem > code.mem.MSB ;\
	cut -c9-16 code.mem > code.mem.LSB ;\
	echo "Running test on SHAKTI C-$(XLEN)";\
	timeout 1m ./out > log || { echo SIM TIMED OUT; exit; };\
	cat app_log; echo " ";\
	diff -iqw app_log temp/output.riscv || { echo RTL AND SPIKE DO NOT MATCH; exit; } ;\
	mkdir -p testdir_$$n ;\
	mv code.mem.* log app_log temp testdir_$$n/ ; \
	((n=n+1)) ; \
	done;


spike-regress:
	@ln -f -s $(SHAKTI_C_HOME)/bin/* .
	@n=1 ; while [[ $$n -le $(RUNS) ]] ; do \
	echo "----------------Test $$n ----------------";\
	make spike || { echo SKIP; make remove; continue; } ;\
	mv temp testdir_$$n/ ; \
	((n=n+1)) ; \
	done;

spike: test.host test.riscv
	@echo "Running test on HOST";
	@cd temp; timeout 2 ./test.host > output.host 
	@cd temp; cat output.host
	@echo "Running test on SPIKE"
	@cd temp; spike -c test.riscv > output.riscv 
	@cd temp; cat output.riscv
	@cd temp; diff -iqw output.host output.riscv

test.host: test.c
	@cd temp;gcc -m$(XLEN) -w -Os -I $(CSMITH_INCDIR) -o test.host test.c

test.riscv: test.c crt.o syscalls.spike syscalls.shakti
	@cd temp; riscv$(XLEN)-unknown-elf-gcc -w -Os -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) -I ../src/ test.c -o test.o -march=rv$(XLEN)imafd 
	@cd temp; riscv$(XLEN)-unknown-elf-gcc -T ../src/link.ld -I $(CSMITH_INCDIR) -I ../src/ test.o syscalls.spike crt.o -o test.riscv -static -nostdlib -nostartfiles -lgcc -lm 
	@cd temp; riscv$(XLEN)-unknown-elf-gcc -T ../src/link.ld -I $(CSMITH_INCDIR) -I ../src/ test.o syscalls.shakti crt.o -o test.shakti -static -nostdlib -nostartfiles -lgcc -lm
	@cd temp; riscv$(XLEN)-unknown-elf-objdump -D test.riscv > test.riscv.dump
	@cd temp; riscv$(XLEN)-unknown-elf-objdump -D test.shakti > test.shakti.dump
	@cd temp; elf2hex 8 32768 test.shakti 2147483648 > code.mem

test.c:
	@rm -rf temp
	@mkdir -p temp
	@cd temp; csmith --no-packed-struct $(FLOAT) -o test.c
	@cd temp; gawk '/Seed:/ {print$$2,$$3;}' test.c

crt.o:
	@cd temp; riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c -I $(CSMITH_INCDIR) ../src/crt.S -o crt.o

syscalls.spike:
	@cp src/syscalls.c.spike temp/syscalls.c
	@cd temp; riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) -I ../src/ syscalls.c -o syscalls.spike
	@cd temp; rm -f syscalls.c

syscalls.shakti:
	@cp src/syscalls.c.shakti temp/syscalls.c
	@cd temp; riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) -I ../src/ syscalls.c -o syscalls.shakti
	@cd temp; rm -f syscalls.c

clean: 
	@rm -rf platform.info test.* output.host output.riscv app_log code.mem* crt.o syscalls.spike  syscalls.shakti testdir* out out.so rtl_* log config_string64.hex* *.txt rtl.dump spike.dump temp
	@find -type l -delete
	@echo "cleaned"
remove:
	@rm -f platform.info test.c test.host test.riscv output.host output.riscv test.o
